use lsp_types::request::DocumentSymbolRequest;
use lsp_types::{DocumentSymbolResponse, Location, OneOf, Position, SymbolInformation, SymbolKind};
use protobuf::descriptor::{source_code_info, DescriptorProto, FileDescriptorProto};
use protobuf_parse;
use std::{error::Error, path};

// Field numbers from https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/descriptor.proto#L100-L101
const MESSAGE_TYPE: i32 = 4;
const ENUM_TYPE: i32 = 5;
const NESTED_MESSAGE_TYPE: i32 = 3;
const NESTED_ENUM_TYPE: i32 = 4;

use lsp_types::{
    notification::{DidOpenTextDocument, DidSaveTextDocument, Notification, PublishDiagnostics},
    Diagnostic, DiagnosticServerCapabilities, DiagnosticSeverity, InitializeParams, Range,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions, Url,
};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};

fn main() -> Result<(), Box<dyn Error>> {
    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        document_symbol_provider: Some(OneOf::Left(true)),
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
    // The protoc parser gives more useful and consistent error messages
    parser.protoc();
    parser.protoc_extra_args(vec!["--include_source_info"]);
    parser.capture_stderr();
    parser.input(path::Path::new(path).canonicalize()?);
    parser.include(path::Path::new(".").canonicalize()?);
    Ok(parser.file_descriptor_set()?.file)
}

// Parse a single error line from the protoc parser into a diagnostic.
// Error lines look like:
// "/usr/bin/protoc" "-I/home/rcorre/src/pbls" ... "--include_imports" "/home/rcorre/src/pbls/foo.proto"", "foo.proto:4:13: "int" is not defined".
fn parse_diag(line: &str) -> Result<lsp_types::Diagnostic, Box<dyn Error>> {
    let (_, rest) = line.split_once(".proto:").ok_or("Failed to parse error")?;
    let (linestr, rest) = rest
        .split_once(':')
        .ok_or("Failed to parse line number from error")?;
    let (_, msg) = rest
        .split_once(':')
        .ok_or("Failed to parse message from error")?;
    let msg = msg.strip_suffix(".\"").unwrap_or(msg).replace("\\\"", "\"");

    let lineno = linestr.parse::<u32>()?;

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
    // Errors are delineated by literal \n.
    for diag in err.to_string().split("\\n").map(|l| parse_diag(l)) {
        vec.push(diag?);
    }
    Ok(vec)
}

fn location_to_symbol(
    uri: Url,
    loc: &source_code_info::Location,
    fd: &FileDescriptorProto,
) -> Option<SymbolInformation> {
    // See https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/descriptor.proto#L1097-L1120
    // The first element is the type, followed by the index of that type in the descriptor.
    let (name, kind) = match loc.path[..] {
        [MESSAGE_TYPE, idx, NESTED_MESSAGE_TYPE, subidx] => (
            fd.message_type
                .get(usize::try_from(idx).ok()?)?
                .nested_type
                .get(usize::try_from(subidx).ok()?)?
                .name
                .clone(),
            SymbolKind::STRUCT,
        ),
        [MESSAGE_TYPE, idx, NESTED_ENUM_TYPE, subidx] => (
            fd.message_type
                .get(usize::try_from(idx).ok()?)?
                .enum_type
                .get(usize::try_from(subidx).ok()?)?
                .name
                .clone(),
            SymbolKind::ENUM,
        ),
        [MESSAGE_TYPE, idx] => (
            fd.message_type
                .get(usize::try_from(idx).ok()?)?
                .name
                .clone(),
            SymbolKind::STRUCT,
        ),
        [ENUM_TYPE, idx] => (
            fd.enum_type.get(usize::try_from(idx).ok()?)?.name.clone(),
            SymbolKind::ENUM,
        ),
        _ => {
            return None;
        }
    };
    let start = Position {
        line: loc.span[0].try_into().unwrap(),
        character: loc.span[1].try_into().unwrap(),
    };
    let end = Position {
        line: loc.span[2].try_into().unwrap(),
        character: loc.span[3].try_into().unwrap(),
    };

    // deprecated field is deprecated, but cannot be omitted
    #[allow(deprecated)]
    Some(SymbolInformation {
        // TODO: no clone
        name: name.clone().unwrap_or("Unknown".into()),
        kind,
        location: Location {
            uri: uri.clone(),
            range: Range { start, end },
        },
        tags: None,
        deprecated: None,
        container_name: None,
    })
}

fn get_symbols(uri: Url) -> Result<DocumentSymbolResponse, Box<dyn Error>> {
    let parsed = parse(uri.path())?;
    let first = parsed.first().ok_or("No info")?;
    eprintln!(
        "messages={:?}, locations={:?}",
        first.message_type, first.source_code_info
    );
    Ok(DocumentSymbolResponse::Flat(
        first
            .source_code_info
            .location
            .iter()
            .filter_map(|loc| location_to_symbol(uri.clone(), loc, first))
            .collect(),
    ))
}

fn main_loop(connection: Connection, params: serde_json::Value) -> Result<(), Box<dyn Error>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                match cast::<DocumentSymbolRequest>(req) {
                    Ok((id, params)) => {
                        let result = Some(get_symbols(params.text_document.uri)?);
                        let result = serde_json::to_value(&result)?;
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
