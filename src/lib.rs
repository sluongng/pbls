mod parser;
use parser::ParseResult;
use parser::Parser;

use lsp_server::{Connection, Message};
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

fn handle<Req>(
    parser: &mut parser::Parser,
    req: lsp_server::Request,
    handler: impl Fn(&mut parser::Parser, Req::Params) -> Result<Req::Result>,
) -> Result<lsp_server::Message>
where
    Req: lsp_types::request::Request,
{
    let (id, params) = req.extract::<Req::Params>(Req::METHOD)?;
    Ok(Message::Response(match handler(parser, params) {
        Ok(resp) => lsp_server::Response {
            id,
            result: Some(serde_json::to_value(resp)?),
            error: None,
        },
        Err(err) => lsp_server::Response {
            id,
            result: None,
            error: Some(lsp_server::ResponseError {
                code: lsp_server::ErrorCode::InternalError as i32,
                message: err.to_string(),
                data: None,
            }),
        },
    }))
}

fn handle_document_symbols(
    parser: &mut parser::Parser,
    params: DocumentSymbolParams,
) -> Result<Option<DocumentSymbolResponse>> {
    match parser.parse(params.text_document.uri)? {
        ParseResult::Syms(syms) => Ok(Some(DocumentSymbolResponse::Flat(syms))),
        ParseResult::Diags(_) => Err("File cannot be parsed".into()),
    }
}

fn handle_workspace_symbols(
    parser: &mut parser::Parser,
    _: WorkspaceSymbolParams,
) -> Result<Option<lsp_types::WorkspaceSymbolResponse>> {
    Ok(Some(lsp_types::WorkspaceSymbolResponse::Flat(
        parser.parse_all()?,
    )))
}

fn handle_goto_definition(
    parser: &mut Parser,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
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
    let name = &line[fore..aft];
    eprintln!("Getting definition for {name}");

    let syms = parser.parse_all()?;

    let sym = syms
        .iter()
        .find(|x| {
            name == x.name
                || match &x.container_name {
                    Some(pkg) => name == pkg.to_owned() + "." + &x.name,
                    None => false,
                }
        })
        .ok_or(format!("Symbol for '{name}' not found"))?;

    Ok(Some(GotoDefinitionResponse::Scalar(sym.location.clone())))
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
                let resp = match req.method.as_str() {
                    DocumentSymbolRequest::METHOD => Some(handle::<DocumentSymbolRequest>(
                        &mut parser,
                        req,
                        handle_document_symbols,
                    )),
                    WorkspaceSymbolRequest::METHOD => Some(handle::<WorkspaceSymbolRequest>(
                        &mut parser,
                        req,
                        handle_workspace_symbols,
                    )),
                    GotoDefinition::METHOD => Some(handle::<GotoDefinition>(
                        &mut parser,
                        req,
                        handle_goto_definition,
                    )),
                    _ => None,
                };
                if let Some(resp) = resp {
                    connection.sender.send(resp?)?;
                }
            }
            Message::Response(_) => {}
            Message::Notification(not) => match not.method.as_str() {
                DidOpenTextDocument::METHOD => {
                    let params =
                        not.extract::<DidOpenTextDocumentParams>(DidOpenTextDocument::METHOD)?;
                    eprintln!("Handling DidOpenTextDocument: {}", params.text_document.uri);
                    match on_open(params.text_document.uri, &mut parser) {
                        Ok(resp) => connection.sender.send(Message::Notification(resp)),
                        Err(err) => connection.sender.send(Message::Notification(
                            lsp_server::Notification {
                                method: lsp_types::notification::ShowMessage::METHOD.into(),
                                params: serde_json::to_value(lsp_types::ShowMessageParams {
                                    typ: lsp_types::MessageType::ERROR,
                                    message: err.to_string(),
                                })?,
                            },
                        )),
                    }?;
                }
                DidSaveTextDocument::METHOD => {
                    let params =
                        not.extract::<DidSaveTextDocumentParams>(DidSaveTextDocument::METHOD)?;
                    eprintln!("Handling DidSaveTextDocument: {}", params.text_document.uri);
                    let resp = on_save(params.text_document.uri, &mut parser)?;
                    connection.sender.send(Message::Notification(resp))?;
                }
                _ => {}
            },
        }
    }
    Ok(())
}

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

fn on_save(uri: Url, parser: &mut Parser) -> Result<lsp_server::Notification> {
    let diags = match parser.reparse(uri.clone())? {
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
