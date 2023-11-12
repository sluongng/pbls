use lsp_types::request::DocumentSymbolRequest;
use lsp_types::{DocumentSymbolResponse, Location, OneOf, Position, SymbolInformation, SymbolKind};
use protobuf::descriptor::{source_code_info, DescriptorProto, FileDescriptorProto};
use protobuf_parse;
use std::fs;
use std::{error::Error, path};

use lsp_types::{
    notification::{DidOpenTextDocument, DidSaveTextDocument, Notification, PublishDiagnostics},
    Diagnostic, DiagnosticServerCapabilities, DiagnosticSeverity, InitializeParams, Range,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions, Url,
};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};

// Field numbers from https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/descriptor.proto#L100-L101
const MESSAGE_TYPE: i32 = 4;
const ENUM_TYPE: i32 = 5;
const NESTED_MESSAGE_TYPE: i32 = 3;
const NESTED_ENUM_TYPE: i32 = 4;

fn default_proto_paths() -> Vec<String> {
    vec![".".into()]
}

#[derive(Debug, serde::Deserialize, Default)]
struct Config {
    #[serde(default = "default_proto_paths")]
    proto_paths: Vec<String>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let (connection, io_threads) = Connection::stdio();
    start(connection)?;
    io_threads.join()?;
    Ok(())
}

fn start(connection: Connection) -> Result<(), Box<dyn Error>> {
    let conf = if let Ok(s) = fs::read_to_string(".pbls.toml") {
        toml::from_str(s.as_str())?
    } else {
        Config::default()
    };
    eprintln!("Using config {:?}", conf);

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

    let init_params = connection.initialize(server_capabilities)?;
    let _params: InitializeParams = serde_json::from_value(init_params).unwrap();

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                match cast::<DocumentSymbolRequest>(req) {
                    Ok((id, params)) => {
                        let resp = match get_symbols(params.text_document.uri, &conf) {
                            Ok(result) => Response {
                                id,
                                result: Some(serde_json::to_value(&result)?),
                                error: None,
                            },
                            Err(err) => Response {
                                id,
                                result: None,
                                error: Some(lsp_server::ResponseError {
                                    code: lsp_server::ErrorCode::InternalError as i32,
                                    message: err.to_string(),
                                    data: None,
                                }),
                            },
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
                        let resp = on_open(params.text_document.uri, &conf)?;
                        connection.sender.send(Message::Notification(resp))?;
                    }
                }
                DidSaveTextDocument::METHOD => {
                    if let Ok(params) = notification::<DidSaveTextDocument>(not) {
                        let resp = on_open(params.text_document.uri, &conf)?;
                        connection.sender.send(Message::Notification(resp))?;
                    }
                }
                _ => {}
            },
        }
    }
    Ok(())
}

fn parse(path: &str, conf: &Config) -> Result<Vec<FileDescriptorProto>, Box<dyn Error>> {
    let mut parser = protobuf_parse::Parser::new();
    // The protoc parser gives more useful and consistent error messages
    parser.protoc();
    parser.protoc_extra_args(vec!["--include_source_info"]);
    parser.capture_stderr();
    parser.input(path::Path::new(path).canonicalize()?);
    parser.includes(
        conf.proto_paths
            .iter()
            .map(|p| path::Path::new(p).canonicalize().unwrap()),
    );
    Ok(parser.file_descriptor_set()?.file)
}

// Parse a single error line from the protoc parser into a diagnostic.
// Usually each error has a line containing a location, like:
// foo.proto:4:13: "int" is not defined
// Other lines do not contain location info.
// We'll return None to skip these, as usually another line contains the location.
fn parse_diag(line: &str) -> Option<lsp_types::Diagnostic> {
    eprintln!("Parsing diag {line}");
    let (_, rest) = line.split_once(".proto:")?;
    let (linestr, rest) = rest.split_once(':')?;
    let (_, msg) = rest.split_once(':')?;
    let msg = msg.strip_suffix(".\"").unwrap_or(msg).replace("\\\"", "\"");

    let lineno = linestr.parse::<u32>().unwrap();

    Some(lsp_types::Diagnostic {
        range: Range {
            start: lsp_types::Position {
                line: lineno - 1,
                character: 0,
            },
            end: lsp_types::Position {
                line: lineno - 1,
                character: line.len().try_into().unwrap(),
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
    for diag in err.to_string().split("\\n").filter_map(|l| parse_diag(l)) {
        vec.push(diag);
    }
    Ok(vec)
}

fn location_to_name_kind_nested(
    path: &[i32],
    proto: &DescriptorProto,
) -> Option<(String, SymbolKind)> {
    match path {
        [NESTED_ENUM_TYPE, idx] => Some((
            proto
                .enum_type
                .get(usize::try_from(*idx).ok()?)?
                .name
                .clone()
                .unwrap_or("<EnumMissingName>".into()),
            SymbolKind::ENUM,
        )),
        [NESTED_MESSAGE_TYPE, idx] => Some((
            proto
                .nested_type
                .get(usize::try_from(*idx).ok()?)?
                .name
                .clone()?,
            SymbolKind::STRUCT,
        )),
        [NESTED_MESSAGE_TYPE, idx, tail @ ..] => {
            let parent_name = proto
                .nested_type
                .get(usize::try_from(*idx).ok()?)?
                .name
                .clone()
                .unwrap_or("<MessageMissingName>".into());
            let (name, kind) = location_to_name_kind_nested(
                tail,
                proto.nested_type.get(usize::try_from(*idx).ok()?)?,
            )?;
            Some((parent_name + "." + &name, kind))
        }
        _ => None,
    }
}

fn location_to_name_kind(path: &[i32], fd: &FileDescriptorProto) -> Option<(String, SymbolKind)> {
    match path {
        // top-level enum
        [ENUM_TYPE, idx] => Some((
            fd.enum_type
                .get(usize::try_from(*idx).ok()?)?
                .name
                .clone()
                .unwrap_or("<EnumMissingName>".into()),
            SymbolKind::ENUM,
        )),
        // top-level message
        [MESSAGE_TYPE, idx] => Some((
            fd.message_type
                .get(usize::try_from(*idx).ok()?)?
                .name
                .clone()
                .unwrap_or("<MessageMissingName>".into()),
            SymbolKind::STRUCT,
        )),
        // nested type
        [MESSAGE_TYPE, idx, tail @ ..] => {
            let parent_name = fd
                .message_type
                .get(usize::try_from(*idx).ok()?)?
                .name
                .clone()
                .unwrap_or("<MessageMissingName>".into());
            let (name, kind) = location_to_name_kind_nested(
                &tail,
                fd.message_type.get(usize::try_from(*idx).ok()?)?,
            )?;
            Some((parent_name + "." + &name, kind))
        }
        _ => None,
    }
}

fn location_to_symbol(
    uri: Url,
    loc: &source_code_info::Location,
    fd: &FileDescriptorProto,
) -> Option<SymbolInformation> {
    // See https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/descriptor.proto#L1097-L1120
    // The first element is the type, followed by the index of that type in the descriptor.
    let (name, kind) = location_to_name_kind(&loc.path[..], fd)?;
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
        name,
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

fn get_symbols(uri: Url, conf: &Config) -> Result<DocumentSymbolResponse, Box<dyn Error>> {
    let parsed = parse(uri.path(), &conf)?;
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

fn on_open(uri: Url, conf: &Config) -> Result<lsp_server::Notification, Box<dyn Error>> {
    if uri.scheme() != "file" {
        Err(format!("Unsupported scheme: {}", uri))?
    }
    let diags = match parse(uri.path(), conf) {
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
