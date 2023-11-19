mod parser;
use parser::ParseResult;
use parser::Parser;

use lsp_server::{Connection, Message, Response};
use lsp_types::request::{DocumentSymbolRequest, GotoDefinition, Request, WorkspaceSymbolRequest};
use lsp_types::{
    notification::{DidOpenTextDocument, DidSaveTextDocument, Notification, PublishDiagnostics},
    Diagnostic, DiagnosticServerCapabilities, InitializeParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncOptions, TextDocumentSyncSaveOptions, Url,
};
use lsp_types::{
    DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentSymbolParams,
    DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse, OneOf,
    WorkspaceSymbolParams,
};
use std::error::Error;
use std::fs;

pub type Result<T> = std::result::Result<T, Box<dyn Error>>;

#[derive(Debug, serde::Deserialize)]
struct Config {
    proto_paths: Vec<String>,
}

pub fn run(connection: Connection) -> Result<()> {
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        document_symbol_provider: Some(OneOf::Left(true)),
        workspace_symbol_provider: Some(OneOf::Left(true)),
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

    let mut parser = parser::Parser::new(conf.proto_paths);

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
                        let resp = match parser.parse(params.text_document.uri)? {
                            ParseResult::Syms(syms) => Response {
                                id,
                                result: Some(serde_json::to_value(DocumentSymbolResponse::Flat(
                                    syms,
                                ))?),
                                error: None,
                            },
                            ParseResult::Diags(_) => Response {
                                id,
                                result: None,
                                error: Some(lsp_server::ResponseError {
                                    code: lsp_server::ErrorCode::InternalError as i32,
                                    message: "File cannot be parsed".into(),
                                    data: None,
                                }),
                            },
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    WorkspaceSymbolRequest::METHOD => {
                        let (id, params) =
                            req.extract::<WorkspaceSymbolParams>(WorkspaceSymbolRequest::METHOD)?;
                        let resp = match parser.parse_all() {
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
                        let resp = match get_definition(&mut parser, params) {
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
                    let params =
                        not.extract::<DidOpenTextDocumentParams>(DidOpenTextDocument::METHOD)?;
                    eprintln!("Handling DidOpenTextDocument: {}", params.text_document.uri);
                    let resp = on_open(params.text_document.uri, &mut parser)?;
                    connection.sender.send(Message::Notification(resp))?;
                }
                DidSaveTextDocument::METHOD => {
                    let params =
                        not.extract::<DidSaveTextDocumentParams>(DidSaveTextDocument::METHOD)?;
                    eprintln!("Handling DidSaveTextDocument: {}", params.text_document.uri);
                    let resp = on_open(params.text_document.uri, &mut parser)?;
                    connection.sender.send(Message::Notification(resp))?;
                }
                _ => {}
            },
        }
    }
    Ok(())
}

fn get_definition(
    parser: &mut Parser,
    params: GotoDefinitionParams,
) -> Result<GotoDefinitionResponse> {
    let uri = params.text_document_position_params.text_document.uri;
    let path = uri.path();
    let pos = params.text_document_position_params.position;

    // Find the word under the cursor
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
    let fullname = &line[fore..aft];
    eprintln!("Getting definition for {fullname}");

    let (pkg, name) = fullname.rsplit_once(".").unwrap_or(("", fullname));

    // Find the symbol matching the word
    let syms = parser.parse_all()?;

    // If pkg is empty, it refers to a type within the same package as the edited proto file.
    // Otherwise, we check for that name in another package.
    let sym = syms
        .iter()
        .find(|x| {
            x.name == name && (pkg == "" || x.container_name.as_ref().map_or(false, |c| c == pkg))
        })
        .ok_or(format!("Symbol for '{fullname}' not found"))?;

    Ok(GotoDefinitionResponse::Scalar(sym.location.clone()))
}

// fn get_workspace_symbols(conf: &Config) -> Result<WorkspaceSymbolResponse> {
//     Ok(WorkspaceSymbolResponse::Flat(workspace_symbols(conf)?))
// }

fn on_open(uri: Url, parser: &mut Parser) -> Result<lsp_server::Notification> {
    let diags = match parser.parse(uri.clone())? {
        ParseResult::Syms(_) => Vec::<Diagnostic>::new(),
        ParseResult::Diags(diags) => diags,
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
