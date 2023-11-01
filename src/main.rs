use protobuf::descriptor::FileDescriptorProto;
use protobuf_parse;
use std::{error::Error, path};

use lsp_types::{
    notification::{DidOpenTextDocument, DidSaveTextDocument, Notification, PublishDiagnostics},
    request::GotoDefinition,
    Diagnostic, DiagnosticServerCapabilities, DiagnosticSeverity, GotoDefinitionResponse,
    InitializeParams, Range, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncOptions, TextDocumentSyncSaveOptions, Url,
};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};

fn main() -> Result<(), Box<dyn Error>> {
    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                ..Default::default()
            },
        )),
        // completion_provider: Some(lsp_types::CompletionOptions::default()),
        diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
            lsp_types::DiagnosticOptions {
                identifier: Some(String::from("spelgud")),
                ..Default::default()
            },
        )),
        ..Default::default()
    })
    .unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    Ok(())
}

fn parse(path: &str) -> Result<Vec<FileDescriptorProto>, Box<dyn Error>> {
    let mut parser = protobuf_parse::Parser::new();
    parser.pure();
    parser.capture_stderr();
    parser.input(path);
    parser.include(path::Path::new(".").canonicalize()?);
    Ok(parser.file_descriptor_set()?.file)
}

// Parse a single error line from the pure parser into a diagnostic.
// Error lines look like:
// error in `./example.proto`: at 4:13: label required
fn parse_diag_pure(line: &str) -> Result<lsp_types::Diagnostic, Box<dyn Error>> {
    let line = line
        .strip_prefix("error in")
        .ok_or(format!("Unexpected error line prefix: {}", line))?;
    let mut parts = line.split(":");
    let _ = parts
        .next()
        .ok_or(format!("Error line missing filename: {}", line))?
        .trim_matches('`');
    let lineno = parts
        .next()
        .ok_or(format!("Error line missing line number: {}", line))?
        .strip_prefix(" at ")
        .ok_or(format!("Error line missing location prefix: {}", line))?
        .parse::<u32>()?;
    let _colno = parts
        .next()
        .ok_or(format!("Error line missing column number: {}", line))?
        .parse::<u32>()?;
    let msg = parts.next().ok_or("Error line missing message")?;
    Ok(lsp_types::Diagnostic {
        range: Range {
            start: lsp_types::Position {
                line: lineno - 1,
                character: 0,
            },
            end: lsp_types::Position {
                line: lineno - 1,
                character: line.len().try_into()?,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some(String::from("pbls")),
        message: msg.into(),
        ..Default::default()
    })
}

fn get_diagnostics(err: &dyn Error) -> Result<Vec<Diagnostic>, Box<dyn Error>> {
    let mut vec = Vec::<Diagnostic>::new();
    for diag in err.to_string().lines().map(|l| parse_diag_pure(l)) {
        vec.push(diag?);
    }
    Ok(vec)
}

fn main_loop(connection: Connection, params: serde_json::Value) -> Result<(), Box<dyn Error>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                match cast::<GotoDefinition>(req) {
                    Ok((id, _)) => {
                        let result = Some(GotoDefinitionResponse::Array(Vec::new()));
                        let result = serde_json::to_value(&result).unwrap();
                        let resp = Response {
                            id,
                            result: Some(result),
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                // ...
            }
            Message::Response(_) => {}
            Message::Notification(not) => match not.method.as_str() {
                DidOpenTextDocument::METHOD => {
                    if let Ok(params) = notification::<DidOpenTextDocument>(not) {
                        let resp = on_open(params.text_document.uri)?;
                        connection.sender.send(Message::Notification(resp))?;
                    }
                }
                DidSaveTextDocument::METHOD => {
                    if let Ok(params) = notification::<DidSaveTextDocument>(not) {
                        let resp = on_open(params.text_document.uri)?;
                        connection.sender.send(Message::Notification(resp))?;
                    }
                }
                _ => {}
            },
        }
    }
    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn notification<N>(
    not: lsp_server::Notification,
) -> Result<N::Params, ExtractError<lsp_server::Notification>>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    not.extract(N::METHOD)
}

fn method<N>() -> String
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    String::from(N::METHOD)
}

fn on_open(uri: Url) -> Result<lsp_server::Notification, Box<dyn Error>> {
    if uri.scheme() != "file" {
        Err(format!("Unsupported scheme: {}", uri))?
    }
    let diags = match parse(uri.path()) {
        Ok(_) => Vec::<Diagnostic>::new(),
        Err(err) => {
            let err = err.source().ok_or("Parse error missing source")?;
            get_diagnostics(err)?
        }
    };

    let params = lsp_types::PublishDiagnosticsParams {
        uri,
        diagnostics: diags,
        version: None,
    };

    Ok(lsp_server::Notification {
        method: method::<PublishDiagnostics>(),
        params: serde_json::to_value(&params)?,
    })
}
