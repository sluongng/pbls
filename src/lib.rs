use lsp_server::{Connection, ExtractError, Message, Response};
use lsp_types::request::{DocumentSymbolRequest, GotoDefinition, Request};
use lsp_types::{
    notification::{DidOpenTextDocument, DidSaveTextDocument, Notification, PublishDiagnostics},
    Diagnostic, DiagnosticServerCapabilities, DiagnosticSeverity, InitializeParams, Range,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions, Url,
};
use lsp_types::{
    DocumentSymbolParams, DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse,
    Location, OneOf, Position, SymbolInformation, SymbolKind,
};
use protobuf::descriptor::{source_code_info, DescriptorProto, FileDescriptorProto};
use protobuf_parse;
use std::fs;
use std::{error::Error, path};

// Field numbers from https://github.com/protocolbuffers/protobuf/blob/main/src/google/protobuf/descriptor.proto#L100-L101
const MESSAGE_TYPE: i32 = 4;
const ENUM_TYPE: i32 = 5;
const NESTED_MESSAGE_TYPE: i32 = 3;
const NESTED_ENUM_TYPE: i32 = 4;

pub type Result<T> = std::result::Result<T, Box<dyn Error>>;

#[derive(Debug, serde::Deserialize)]
struct Config {
    proto_paths: Vec<String>,
}

pub fn run(connection: Connection) -> Result<()> {
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        document_symbol_provider: Some(OneOf::Left(true)),
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                ..Default::default()
            },
        )),
        definition_provider: Some(OneOf::Left(true)),
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

    eprintln!("Initializing");
    let init_params = connection.initialize(server_capabilities)?;
    eprintln!("Initialized");
    let params: InitializeParams = serde_json::from_value(init_params).unwrap();
    let root = params
        .root_uri
        .map(|u| u.path().to_string())
        .unwrap_or(".".into());
    let path = std::path::Path::new(&root).join(".pbls.toml");
    let conf = if path.is_file() {
        eprintln!("Reading config from {path:?}");
        toml::from_str(fs::read_to_string(path)?.as_str())?
    } else {
        eprintln!("Using default config");
        Config {
            proto_paths: vec![root],
        }
    };
    eprintln!("Using config {:?}", conf);

    for msg in &connection.receiver {
        eprintln!("Handling message {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    eprintln!("Shutting down");
                    return Ok(());
                }
                match req.method.as_str() {
                    DocumentSymbolRequest::METHOD => {
                        let (id, params) =
                            req.extract::<DocumentSymbolParams>(DocumentSymbolRequest::METHOD)?;
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
                    GotoDefinition::METHOD => {
                        let (id, params) =
                            req.extract::<GotoDefinitionParams>(GotoDefinition::METHOD)?;
                        let resp = match get_definition(params, &conf) {
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
                    _ => {}
                }
            }
            Message::Response(_) => {}
            Message::Notification(not) => match not.method.as_str() {
                DidOpenTextDocument::METHOD => {
                    if let Ok(params) = notification::<DidOpenTextDocument>(not) {
                        eprintln!("Handling DidOpenTextDocument: {}", params.text_document.uri);
                        let resp = on_open(params.text_document.uri, &conf)?;
                        connection.sender.send(Message::Notification(resp))?;
                    }
                }
                DidSaveTextDocument::METHOD => {
                    if let Ok(params) = notification::<DidSaveTextDocument>(not) {
                        eprintln!("Handling DidSaveTextDocument: {}", params.text_document.uri);
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

fn parse(path: &str, conf: &Config) -> Result<Vec<FileDescriptorProto>> {
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
        message: msg.trim().into(),
        ..Default::default()
    })
}

fn get_diagnostics(err: &dyn Error) -> Result<Vec<Diagnostic>> {
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

    // Always has exactly three or four elements: start line, start column,
    // end line (optional, otherwise assumed same as start line), end column.
    let (start_line, start_col, end_line, end_col) = match loc.span[..] {
        [start_line, start_col, end_line, end_col] => (start_line, start_col, end_line, end_col),
        [start_line, start_col, end_col] => (start_line, start_col, start_line, end_col),
        _ => None?,
    };

    let start = Position {
        line: start_line.try_into().unwrap(),
        character: start_col.try_into().unwrap(),
    };
    let end = Position {
        line: end_line.try_into().unwrap(),
        character: end_col.try_into().unwrap(),
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

fn get_definition(params: GotoDefinitionParams, conf: &Config) -> Result<GotoDefinitionResponse> {
    let uri = params.text_document_position_params.text_document.uri;
    let path = uri.path();
    let pos = params.text_document_position_params.position;

    // Find the word dunder the cursor
    let text = fs::read_to_string(uri.path())?;
    let lineno: usize = pos.line.try_into()?;
    let charno: usize = pos.character.try_into()?;
    let line = text
        .lines()
        .skip(lineno)
        .next()
        .ok_or(format!("Line {lineno} out of range in file {path}"))?;
    let fore = line[..charno]
        .rfind(|c: char| c.is_whitespace())
        .map(|n| n + 1)
        .unwrap_or(0);
    let aft = line[charno..]
        .find(|c: char| c.is_whitespace())
        .map(|n| n + charno)
        .unwrap_or(line.len());
    let word = &line[fore..aft];
    eprintln!("Getting definition for {word}");

    // Find the symbol matching the word
    let syms = file_to_symbols(uri.clone(), &conf)?;
    let sym = syms
        .iter()
        .find(|x| x.name == word)
        .ok_or(format!("Symbol for '{word}' not found"))?;

    Ok(GotoDefinitionResponse::Scalar(Location {
        uri,
        range: sym.location.range,
    }))
}

// fn file_to_symbols_with_imports(uri: Url, conf: &Config) -> Result<Vec<SymbolInformation>> {
//     let parsed = parse(uri.path(), &conf)?;
//     let first = parsed.first().ok_or("No info")?;
//     let imports = first.dependency.iter().map(|x| find_import(x));
//     Ok(first
//         .source_code_info
//         .location
//         .iter()
//         .filter_map(|loc| location_to_symbol(uri.clone(), loc, first))
//         .collect())
// }

fn file_to_symbols(uri: Url, conf: &Config) -> Result<Vec<SymbolInformation>> {
    let parsed = parse(uri.path(), &conf)?;
    let first = parsed.first().ok_or("No info")?;
    Ok(first
        .source_code_info
        .location
        .iter()
        .filter_map(|loc| location_to_symbol(uri.clone(), loc, first))
        .collect())
}

fn get_symbols(uri: Url, conf: &Config) -> Result<DocumentSymbolResponse> {
    Ok(DocumentSymbolResponse::Flat(file_to_symbols(uri, conf)?))
}

fn notification<N>(
    not: lsp_server::Notification,
) -> std::result::Result<N::Params, ExtractError<lsp_server::Notification>>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    not.extract(N::METHOD)
}

fn on_open(uri: Url, conf: &Config) -> Result<lsp_server::Notification> {
    if uri.scheme() != "file" {
        Err(format!("Unsupported scheme: {}", uri))?
    }
    let diags = match parse(uri.path(), conf) {
        Ok(_) => Vec::<Diagnostic>::new(),
        Err(err) => {
            let err = err.source().ok_or("Parse error missing source")?;
            eprintln!("Parsing diagnostics from {}", err);
            get_diagnostics(err)?
        }
    };

    let params = lsp_types::PublishDiagnosticsParams {
        uri,
        diagnostics: diags,
        version: None,
    };

    Ok(lsp_server::Notification {
        method: PublishDiagnostics::METHOD.into(),
        params: serde_json::to_value(&params)?,
    })
}
