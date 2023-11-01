use protobuf::descriptor::FileDescriptorProto;
use protobuf_parse;
use std::error::Error;

use lsp_types::{
    notification::PublishDiagnostics, request::GotoDefinition, DiagnosticSeverity,
    GotoDefinitionResponse, InitializeParams, Range, ServerCapabilities,
};

use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};

fn main() -> Result<(), Box<dyn Error + Send + Sync>> {
    eprintln!("{}", parse("").unwrap_err().source().unwrap());
    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        // completion_provider: Some(lsp_types::CompletionOptions::default()),
        diagnostic_provider: Some(lsp_types::DiagnosticServerCapabilities::Options(
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

    eprintln!("Shutting down server");
    Ok(())
}

fn parse(path: &str) -> Result<Vec<FileDescriptorProto>, Box<dyn Error>> {
    let mut parser = protobuf_parse::Parser::new();
    parser.pure();
    parser.capture_stderr();
    parser.input("./example.proto");
    parser.include(".");
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
    let colno = parts
        .next()
        .ok_or(format!("Error line missing column number: {}", line))?
        .parse::<u32>()?;
    let msg = parts.next().ok_or("Error line missing message")?;
    eprintln!("Parsed: line={}, col={}, msg={}", lineno, colno, msg);
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

fn get_diagnostics(err: &dyn Error) -> Result<Vec<lsp_types::Diagnostic>, Box<dyn Error>> {
    eprintln!("Parsing diagnostics from: {}", err);
    let mut vec = Vec::<lsp_types::Diagnostic>::new();
    for diag in err.to_string().lines().map(|l| parse_diag_pure(l)) {
        vec.push(diag?);
    }
    eprintln!("Returning diagnostics {:?}", vec);
    Ok(vec)
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    eprintln!("Starting example main loop");
    for msg in &connection.receiver {
        eprintln!("Got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("Got request: {req:?}");
                match cast::<GotoDefinition>(req) {
                    Ok((id, params)) => {
                        eprintln!("got gotoDefinition request #{id}: {params:?}");
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
            Message::Response(resp) => {
                eprintln!("got response: {resp:?}");
            }
            Message::Notification(not) => {
                eprintln!("got notification: {not:?}");
                match notification::<lsp_types::notification::DidOpenTextDocument>(not) {
                    Ok(params) => {
                        eprintln!("Got DidOpenTextDocument: {params:?}");
                        match on_open(params) {
                            Ok(Some(resp)) => {
                                connection.sender.send(Message::Notification(resp))?
                            }
                            Ok(None) => {}
                            Err(err) => eprintln!("DidOpenTextDocument error: {err:?}"),
                        }
                    }
                    Err(err) => {
                        eprintln!("DidOpenTextDocument error: {err:?}");
                    }
                }
            }
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

fn notification<N>(not: Notification) -> Result<N::Params, ExtractError<Notification>>
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

fn on_open(
    params: lsp_types::DidOpenTextDocumentParams,
) -> Result<Option<Notification>, Box<dyn Error>> {
    let uri = params.text_document.uri;
    if uri.scheme() != "file" {
        Err(format!("Unsupported scheme: {}", uri))?
    }
    let path = uri.path();
    match parse("") {
        Ok(_) => Ok(None),
        Err(err) => {
            let err = err.source().ok_or("Parse error missing source")?;
            eprintln!("Failed: {}", err);
            let diags = get_diagnostics(err)?;
            let result = Some(lsp_types::PublishDiagnosticsParams {
                uri,
                diagnostics: diags,
                version: None,
            });
            Ok(Some(Notification {
                method: method::<PublishDiagnostics>(),
                params: serde_json::to_value(&result)?,
            }))
        }
    }
}
