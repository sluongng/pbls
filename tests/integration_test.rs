use lsp_server::{Connection, Message};
use lsp_types::notification::{DidOpenTextDocument, PublishDiagnostics};
use lsp_types::request::{DocumentSymbolRequest, GotoDefinition, Shutdown};
use lsp_types::{notification::Initialized, request::Initialize, InitializedParams};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DidOpenTextDocumentParams, DocumentSymbolParams,
    GotoDefinitionParams, GotoDefinitionResponse, InitializeParams, Location, Position,
    PublishDiagnosticsParams, Range, SymbolInformation, SymbolKind, TextDocumentIdentifier,
    TextDocumentItem, TextDocumentPositionParams, Url,
};
use pbls::Result;
use std::error::Error;

struct TestClient {
    conn: Connection,
    thread: Option<std::thread::JoinHandle<()>>,
    id: i32,
}

impl TestClient {
    fn new() -> Result<TestClient> {
        let (client, server) = Connection::memory();
        let thread = std::thread::spawn(|| {
            pbls::run(server).unwrap();
        });
        let mut client = TestClient {
            conn: client,
            thread: Some(thread),
            id: 0,
        };

        client.request::<Initialize>(InitializeParams {
            root_uri: Some(
                Url::from_file_path(std::path::Path::new("testdata").canonicalize()?).unwrap(),
            ),
            ..Default::default()
        })?;
        client.notify::<Initialized>(InitializedParams {})?;

        Ok(client)
    }

    fn recv<T>(&self) -> std::result::Result<T::Params, Box<dyn Error>>
    where
        T: lsp_types::notification::Notification,
    {
        match self.conn.receiver.recv()? {
            Message::Request(r) => Err(format!("Expected notification, got: {r:?}"))?,
            Message::Response(r) => Err(format!("Expected notification, got: {r:?}"))?,
            Message::Notification(resp) => {
                assert_eq!(resp.method, T::METHOD);
                Ok(serde_json::from_value(resp.params)?)
            }
        }
    }

    fn request<T>(&mut self, params: T::Params) -> pbls::Result<T::Result>
    where
        T: lsp_types::request::Request,
        T::Params: serde::de::DeserializeOwned,
    {
        let req = Message::Request(lsp_server::Request {
            id: self.id.into(),
            method: T::METHOD.to_string(),
            params: serde_json::to_value(params)?,
        });
        eprintln!("Sending {:?}", req);
        self.id += 1;
        self.conn.sender.send(req)?;
        eprintln!("Waiting");
        match self.conn.receiver.recv()? {
            Message::Request(r) => Err(format!("Expected response, got: {r:?}"))?,
            Message::Notification(r) => Err(format!("Expected response, got: {r:?}"))?,
            Message::Response(resp) => Ok(serde_json::from_value(
                resp.result.ok_or("Missing result from response")?,
            )?),
        }
    }

    fn notify<T>(&self, params: T::Params) -> pbls::Result<()>
    where
        T: lsp_types::notification::Notification,
        T::Params: serde::de::DeserializeOwned,
    {
        self.conn
            .sender
            .send(Message::Notification(lsp_server::Notification {
                method: T::METHOD.to_string(),
                params: serde_json::to_value(params)?,
            }))?;
        Ok(())
    }
}

impl Drop for TestClient {
    fn drop(&mut self) {
        self.request::<Shutdown>(()).unwrap();
        self.notify::<lsp_types::notification::Exit>(()).unwrap();
        self.thread.take().unwrap().join().unwrap();
    }
}

#[test]
fn test_start_stop() -> pbls::Result<()> {
    TestClient::new()?;
    Ok(())
}

#[test]
fn test_open_ok() -> pbls::Result<()> {
    let client = TestClient::new()?;

    let uri =
        Url::from_file_path(std::path::Path::new("testdata/simple.proto").canonicalize()?).unwrap();

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    let diags = client.recv::<PublishDiagnostics>()?;
    assert_eq!(
        diags,
        PublishDiagnosticsParams {
            uri: uri,
            diagnostics: vec![],
            version: None,
        }
    );
    Ok(())
}

#[test]
fn test_diagnostics() -> pbls::Result<()> {
    let client = TestClient::new()?;

    let uri =
        Url::from_file_path(std::path::Path::new("testdata/error.proto").canonicalize()?).unwrap();

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    let diags = client.recv::<PublishDiagnostics>()?;
    let base_diag = Diagnostic {
        range: Range {
            start: Position {
                line: 16,
                character: 0,
            },
            end: Position {
                line: 16,
                character: 318,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("pbls".into()),
        ..Default::default()
    };
    assert_eq!(
        diags,
        PublishDiagnosticsParams {
            uri,
            diagnostics: vec![
                Diagnostic {
                    range: Range {
                        start: Position {
                            line: 16,
                            character: 0
                        },
                        end: Position {
                            line: 16,
                            character: 318
                        }
                    },
                    message: "\"f\" is already defined in \"main.Bar\".".into(),
                    ..base_diag.clone()
                },
                Diagnostic {
                    range: Range {
                        start: Position {
                            line: 11,
                            character: 0
                        },
                        end: Position {
                            line: 11,
                            character: 44
                        }
                    },
                    message: "\"Thingy\" is not defined.".into(),
                    ..base_diag.clone()
                },
                Diagnostic {
                    range: Range {
                        start: Position {
                            line: 16,
                            character: 0
                        },
                        end: Position {
                            line: 16,
                            character: 88
                        }
                    },
                    message: "Field number 1 has already been used in \"main.Bar\" by field \"f\""
                        .into(),
                    ..base_diag.clone()
                }
            ],
            version: None
        }
    );
    Ok(())
}

#[test]
fn test_no_diagnostics() -> pbls::Result<()> {
    let client = TestClient::new()?;

    let uri =
        Url::from_file_path(std::path::Path::new("testdata/simple.proto").canonicalize()?).unwrap();

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    let diags = client.recv::<PublishDiagnostics>()?;

    assert_eq!(
        diags,
        PublishDiagnosticsParams {
            uri,
            diagnostics: vec![],
            version: None
        }
    );
    Ok(())
}

#[test]
fn test_symbols() -> pbls::Result<()> {
    let mut client = TestClient::new()?;

    let uri =
        Url::from_file_path(std::path::Path::new("testdata/simple.proto").canonicalize()?).unwrap();

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    client.recv::<PublishDiagnostics>()?;

    let syms = client.request::<DocumentSymbolRequest>(DocumentSymbolParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        work_done_progress_params: lsp_types::WorkDoneProgressParams {
            work_done_token: None,
        },
        partial_result_params: lsp_types::PartialResultParams {
            partial_result_token: None,
        },
    })?;
    assert_eq!(
        syms,
        Some(lsp_types::DocumentSymbolResponse::Flat(vec![
            // deprecated field is deprecated, but cannot be omitted
            #[allow(deprecated)]
            SymbolInformation {
                name: "Thing".into(),
                kind: SymbolKind::ENUM,
                tags: None,
                deprecated: None,
                location: Location {
                    uri: uri.clone(),
                    range: Range {
                        start: Position {
                            line: 4,
                            character: 0
                        },
                        end: Position {
                            line: 8,
                            character: 1
                        }
                    }
                },
                container_name: None
            },
            // deprecated field is deprecated, but cannot be omitted
            #[allow(deprecated)]
            SymbolInformation {
                name: "Foo".into(),
                kind: SymbolKind::STRUCT,
                tags: None,
                deprecated: None,
                location: Location {
                    uri: uri.clone(),
                    range: Range {
                        start: Position {
                            line: 10,
                            character: 0
                        },
                        end: Position {
                            line: 13,
                            character: 1
                        }
                    }
                },
                container_name: None
            },
            // deprecated field is deprecated, but cannot be omitted
            #[allow(deprecated)]
            SymbolInformation {
                name: "Bar".into(),
                kind: SymbolKind::STRUCT,
                tags: None,
                deprecated: None,
                location: Location {
                    uri: uri.clone(),
                    range: Range {
                        start: Position {
                            line: 15,
                            character: 0
                        },
                        end: Position {
                            line: 17,
                            character: 1
                        }
                    }
                },
                container_name: None
            },
            #[allow(deprecated)]
            SymbolInformation {
                name: "Empty".into(),
                kind: SymbolKind::STRUCT,
                tags: None,
                deprecated: None,
                location: Location {
                    uri: uri.clone(),
                    range: Range {
                        start: Position {
                            line: 19,
                            character: 0
                        },
                        end: Position {
                            line: 19,
                            character: 16
                        }
                    }
                },
                container_name: None
            }
        ]))
    );
    Ok(())
}

#[test]
fn test_goto_definition_same_file() -> pbls::Result<()> {
    let mut client = TestClient::new()?;

    let uri =
        Url::from_file_path(std::path::Path::new("testdata/simple.proto").canonicalize()?).unwrap();

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    client.recv::<PublishDiagnostics>()?;

    // goto Thing enum
    {
        let resp = client.request::<GotoDefinition>(GotoDefinitionParams {
            work_done_progress_params: lsp_types::WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: lsp_types::PartialResultParams {
                partial_result_token: None,
            },
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: Position {
                    line: 12,
                    character: 5,
                },
            },
        })?;
        assert_eq!(
            resp,
            Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: Range {
                    start: Position {
                        line: 4,
                        character: 0
                    },
                    end: Position {
                        line: 8,
                        character: 1
                    }
                }
            }))
        );
    }

    // goto Foo message
    {
        let resp = client.request::<GotoDefinition>(GotoDefinitionParams {
            work_done_progress_params: lsp_types::WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: lsp_types::PartialResultParams {
                partial_result_token: None,
            },
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: Position {
                    line: 16,
                    character: 2,
                },
            },
        })?;
        assert_eq!(
            resp,
            Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range: Range {
                    start: Position {
                        line: 10,
                        character: 0
                    },
                    end: Position {
                        line: 13,
                        character: 1
                    }
                }
            }))
        );
    }
    Ok(())
}
