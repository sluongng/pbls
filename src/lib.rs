mod parser;
mod syntax;
mod workspace;
use lsp_types::request::Completion;
use lsp_types::CompletionItem;
use lsp_types::CompletionItemKind;
use lsp_types::CompletionParams;
use lsp_types::CompletionResponse;
use lsp_types::SymbolKind;
use parser::ParseResult;

use lsp_server::{Connection, Message};
use lsp_types::request::{DocumentSymbolRequest, GotoDefinition, Request, WorkspaceSymbolRequest};
use lsp_types::{
    notification::{DidOpenTextDocument, DidSaveTextDocument, Notification, PublishDiagnostics},
    Diagnostic, DiagnosticServerCapabilities, InitializeParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncOptions, TextDocumentSyncSaveOptions,
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
    proto_paths: Vec<std::path::PathBuf>,
}

// Handle a request, returning the response to send.
fn handle<Req>(
    workspace: &mut workspace::Workspace,
    req: lsp_server::Request,
    handler: impl Fn(&mut workspace::Workspace, Req::Params) -> Result<Req::Result>,
) -> Result<lsp_server::Message>
where
    Req: lsp_types::request::Request,
{
    let (id, params) = req.extract::<Req::Params>(Req::METHOD)?;
    Ok(Message::Response(match handler(workspace, params) {
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

// Handle a notification, optionally returning a notification to send in response.
fn notify<N>(
    workspace: &mut workspace::Workspace,
    not: lsp_server::Notification,
    handler: impl Fn(&mut workspace::Workspace, N::Params) -> Result<Option<lsp_server::Notification>>,
) -> Result<Option<lsp_server::Message>>
where
    N: lsp_types::notification::Notification,
{
    let params = not.extract::<N::Params>(N::METHOD)?;
    Ok(match handler(workspace, params) {
        Ok(Some(resp)) => Some(Message::Notification(resp)),
        Ok(None) => None,
        // If we get an error, we can't respond directly as with a Request.
        // Instead, send a ShowMessage notification with the error.
        Err(err) => Some(Message::Notification(lsp_server::Notification {
            method: lsp_types::notification::ShowMessage::METHOD.into(),
            params: serde_json::to_value(lsp_types::ShowMessageParams {
                typ: lsp_types::MessageType::ERROR,
                message: err.to_string(),
            })?,
        })),
    })
}

fn handle_document_symbols(
    workspace: &mut workspace::Workspace,
    params: DocumentSymbolParams,
) -> Result<Option<DocumentSymbolResponse>> {
    Ok(Some(DocumentSymbolResponse::Flat(
        workspace.symbols(params.text_document.uri)?,
    )))
}

fn handle_workspace_symbols(
    workspace: &mut workspace::Workspace,
    _: WorkspaceSymbolParams,
) -> Result<Option<lsp_types::WorkspaceSymbolResponse>> {
    Ok(Some(lsp_types::WorkspaceSymbolResponse::Flat(
        workspace.all_symbols()?,
    )))
}

fn handle_goto_definition(
    workspace: &mut workspace::Workspace,
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

    let all = workspace.all_symbols()?;
    let sym = all
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

fn handle_completion(
    workspace: &mut workspace::Workspace,
    params: CompletionParams,
) -> Result<Option<CompletionResponse>> {
    let doc = params.text_document_position.clone();
    match workspace.completion_context(
        &doc.text_document.uri,
        doc.position.line.try_into()?,
        doc.position.character.try_into()?,
    )? {
        Some(syntax::CompletionContext::Message(_)) => {
            complete_types(workspace, doc.text_document.uri)
        }
        Some(syntax::CompletionContext::Enum(_)) => todo!(),
        Some(syntax::CompletionContext::Import) => todo!(),
        None => todo!(),
    }
}

fn complete_types(
    workspace: &mut workspace::Workspace,
    uri: lsp_types::Url,
) -> Result<Option<CompletionResponse>> {
    let syms = workspace.symbols(uri)?;
    let items = syms.iter().map(|s| CompletionItem {
        label: s.name.clone(),
        label_details: None,
        kind: Some(match s.kind {
            SymbolKind::ENUM => CompletionItemKind::ENUM,
            _ => CompletionItemKind::STRUCT,
        }),
        detail: None,
        documentation: None,
        ..Default::default()
    });
    Ok(Some(CompletionResponse::Array(items.collect())))
}

fn notify_did_open(
    workspace: &mut workspace::Workspace,
    params: DidOpenTextDocumentParams,
) -> Result<Option<lsp_server::Notification>> {
    let uri = params.text_document.uri;
    let diags = match workspace.open(uri.clone(), params.text_document.text)? {
        ParseResult::Syms(_) => Vec::<Diagnostic>::new(),
        ParseResult::Diags(diags) => diags,
    };

    let params = lsp_types::PublishDiagnosticsParams {
        uri,
        diagnostics: diags,
        version: None,
    };

    Ok(Some(lsp_server::Notification {
        method: PublishDiagnostics::METHOD.into(),
        params: serde_json::to_value(&params)?,
    }))
}

fn notify_did_save(
    workspace: &mut workspace::Workspace,
    params: DidSaveTextDocumentParams,
) -> Result<Option<lsp_server::Notification>> {
    let uri = params.text_document.uri;
    let diags = match workspace.save(
        uri.clone(),
        params.text.ok_or("Save notification missing text")?,
    )? {
        ParseResult::Syms(_) => Vec::<Diagnostic>::new(),
        ParseResult::Diags(diags) => diags,
    };

    let params = lsp_types::PublishDiagnosticsParams {
        uri,
        diagnostics: diags,
        version: None,
    };

    Ok(Some(lsp_server::Notification {
        method: PublishDiagnostics::METHOD.into(),
        params: serde_json::to_value(&params)?,
    }))
}

fn has_proto_files(path: impl AsRef<std::path::Path>) -> Result<bool> {
    Ok(std::fs::read_dir(path)?
        .find(|x| match x {
            Ok(entry) => entry
                .path()
                .extension()
                .map_or(false, |e| e.to_str() == Some("proto")),
            Err(_) => false,
        })
        .is_some())
}

fn find_dirs(root: std::path::PathBuf) -> Result<Vec<std::path::PathBuf>> {
    let mut res = vec![];
    for entry in std::fs::read_dir(&root)? {
        if let Ok(entry) = entry {
            if entry.metadata().is_ok_and(|m| m.is_dir()) {
                let mut dirs = find_dirs(entry.path())?;
                res.append(&mut dirs);
            }
        }
    }
    res.push(root);
    Ok(res)
}

fn find_import_paths(root: std::path::PathBuf) -> Result<Vec<std::path::PathBuf>> {
    let dirs = find_dirs(root)?;
    Ok(dirs
        .iter()
        .filter(|&x| has_proto_files(x).unwrap_or(false))
        .map(|x| x.to_owned())
        .collect())
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_find_import_paths() {
        let root = std::fs::canonicalize("testdata").unwrap();
        assert_eq!(super::find_import_paths(root.clone()).unwrap(), vec![root]);
    }
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
        completion_provider: Some(lsp_types::CompletionOptions::default()),
        diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
            lsp_types::DiagnosticOptions {
                identifier: Some(String::from("pbls")),
                workspace_diagnostics: true,
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
        .map(|u| u.to_file_path().unwrap())
        .unwrap_or(std::env::current_dir().unwrap());

    let path = root.join(".pbls.toml");
    let conf = if path.is_file() {
        eprintln!("Reading config from {path:?}");
        toml::from_str(fs::read_to_string(path)?.as_str())?
    } else {
        eprintln!("Using default config");
        Config {
            proto_paths: find_import_paths(root)?,
        }
    };
    eprintln!("Using config {:?}", conf);

    let mut workspace = workspace::Workspace::new(conf.proto_paths);

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
                        &mut workspace,
                        req,
                        handle_document_symbols,
                    )),
                    WorkspaceSymbolRequest::METHOD => Some(handle::<WorkspaceSymbolRequest>(
                        &mut workspace,
                        req,
                        handle_workspace_symbols,
                    )),
                    GotoDefinition::METHOD => Some(handle::<GotoDefinition>(
                        &mut workspace,
                        req,
                        handle_goto_definition,
                    )),
                    Completion::METHOD => {
                        Some(handle::<Completion>(&mut workspace, req, handle_completion))
                    }
                    _ => None,
                };
                if let Some(resp) = resp {
                    connection.sender.send(resp?)?;
                }
            }
            Message::Response(_) => {}
            Message::Notification(not) => {
                let resp = match not.method.as_str() {
                    DidOpenTextDocument::METHOD => {
                        notify::<DidOpenTextDocument>(&mut workspace, not, notify_did_open)?
                    }
                    DidSaveTextDocument::METHOD => {
                        notify::<DidSaveTextDocument>(&mut workspace, not, notify_did_save)?
                    }
                    _ => None,
                };
                if let Some(resp) = resp {
                    connection.sender.send(resp)?;
                }
            }
        }
    }
    Ok(())
}
