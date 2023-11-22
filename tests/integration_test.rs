use core::panic;
use lsp_server::{Connection, Message};
use lsp_types::notification::{DidOpenTextDocument, PublishDiagnostics};
use lsp_types::request::{DocumentSymbolRequest, GotoDefinition, Shutdown, WorkspaceSymbolRequest};
use lsp_types::{notification::Initialized, request::Initialize, InitializedParams};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DidOpenTextDocumentParams, DocumentSymbolParams,
    DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse, InitializeParams,
    Location, Position, PublishDiagnosticsParams, Range, SymbolInformation, SymbolKind,
    TextDocumentIdentifier, TextDocumentItem, TextDocumentPositionParams, Url,
    WorkspaceSymbolParams, WorkspaceSymbolResponse,
};
use pbls::Result;
use pretty_assertions::assert_eq;
use std::error::Error;

fn base_uri() -> Url {
    Url::from_file_path(std::fs::canonicalize("./testdata/simple.proto").unwrap()).unwrap()
}

fn other_uri() -> Url {
    Url::from_file_path(std::fs::canonicalize("./testdata/other.proto").unwrap()).unwrap()
}

fn dep_uri() -> Url {
    Url::from_file_path(std::fs::canonicalize("./testdata/dep.proto").unwrap()).unwrap()
}

// Generate a GotoDefinition request for a line containing `text`,
// with the cursor offset from the start of the search string by `offset`
fn goto(uri: Url, text: &str, column: u32) -> GotoDefinitionParams {
    let filetext = std::fs::read_to_string(uri.to_file_path().unwrap()).unwrap();
    let (lineno, line) = filetext
        .lines()
        .enumerate()
        .skip_while(|(_, l)| !l.contains(text))
        .next()
        .unwrap_or_else(|| panic!("{text} not found in {uri}"));

    let character = line.find(|c: char| !c.is_whitespace()).unwrap_or(0);
    GotoDefinitionParams {
        work_done_progress_params: lsp_types::WorkDoneProgressParams {
            work_done_token: None,
        },
        partial_result_params: lsp_types::PartialResultParams {
            partial_result_token: None,
        },
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: base_uri() },
            position: Position {
                line: lineno.try_into().unwrap(),
                character: column + u32::try_from(character).unwrap(),
            },
        },
    }
}

// Return the Location for the definition of the type `name`.
// `name` should include the "message" or "enum" prefix.
// Assumes that the leading { is on the same line as the enum/message.
fn locate(uri: Url, name: &str) -> Location {
    let filetext = std::fs::read_to_string(uri.to_file_path().unwrap()).unwrap();
    let (start_line, start_col) = filetext
        .lines()
        .enumerate()
        .find_map(|(i, l)| match l.find(name) {
            Some(col) => Some((i, col)),
            None => None,
        })
        .unwrap_or_else(|| panic!("{name} not found in {uri}"));

    let mut nesting = 0;
    let mut end_line = 0;
    let mut end_col = 0;
    for (lineno, line) in filetext.lines().skip(start_line).enumerate() {
        nesting += line.chars().filter(|c| c == &'{').count();
        nesting -= line.chars().filter(|c| c == &'}').count();
        if nesting == 0 {
            end_line = lineno + start_line;
            end_col = line.len();
            break;
        }
    }

    Location {
        uri,
        range: Range {
            start: Position {
                line: start_line.try_into().unwrap(),
                character: start_col.try_into().unwrap(),
            },
            end: Position {
                line: end_line.try_into().unwrap(),
                character: end_col.try_into().unwrap(),
            },
        },
    }
}

struct TestClient {
    conn: Connection,
    thread: Option<std::thread::JoinHandle<()>>,
    id: i32,
}

fn assert_elements_equal<T, K, F>(mut a: Vec<T>, mut b: Vec<T>, key: F)
where
    T: Clone + std::fmt::Debug + std::cmp::PartialEq,
    K: Ord,
    F: Clone + FnMut(&T) -> K,
{
    a.sort_by_key(key.clone());
    b.sort_by_key(key);

    assert_eq!(a, b);
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
    assert_eq!(diags.uri, uri);
    let base_diag = Diagnostic {
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("pbls".into()),
        ..Default::default()
    };
    assert_elements_equal(
        diags.diagnostics,
        vec![
            Diagnostic {
                range: locate(uri.clone(), "Thingy t =").range,
                message: "\"Thingy\" is not defined.".into(),
                ..base_diag.clone()
            },
            Diagnostic {
                range: locate(uri, "int32 foo =").range,
                message: "Field number 1 has already been used in \"main.Bar\" by field \"f\""
                    .into(),
                ..base_diag.clone()
            },
        ],
        |s| s.message.clone(),
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
fn test_document_symbols() -> pbls::Result<()> {
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

    let Some(DocumentSymbolResponse::Flat(actual)) =
        client.request::<DocumentSymbolRequest>(DocumentSymbolParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            work_done_progress_params: lsp_types::WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: lsp_types::PartialResultParams {
                partial_result_token: None,
            },
        })?
    else {
        panic!("Expected DocumentSymbolResponse::Flat")
    };
    assert_elements_equal(
        actual,
        vec![
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
                            line: 7,
                            character: 0,
                        },
                        end: Position {
                            line: 11,
                            character: 1,
                        },
                    },
                },
                container_name: Some("main".into()),
            },
            // deprecated field is deprecated, but cannot be omitted
            #[allow(deprecated)]
            SymbolInformation {
                name: "Foo".into(),
                kind: SymbolKind::STRUCT,
                tags: None,
                deprecated: None,
                location: locate(base_uri(), "message Foo"),
                container_name: Some("main".into()),
            },
            #[allow(deprecated)]
            SymbolInformation {
                name: "Foo.Buz".into(),
                kind: SymbolKind::STRUCT,
                tags: None,
                deprecated: None,
                location: locate(base_uri(), "message Buz"),
                container_name: Some("main".into()),
            },
            // deprecated field is deprecated, but cannot be omitted
            #[allow(deprecated)]
            SymbolInformation {
                name: "Bar".into(),
                kind: SymbolKind::STRUCT,
                tags: None,
                deprecated: None,
                location: locate(base_uri(), "message Bar"),
                container_name: Some("main".into()),
            },
            #[allow(deprecated)]
            SymbolInformation {
                name: "Empty".into(),
                kind: SymbolKind::STRUCT,
                tags: None,
                deprecated: None,
                location: locate(base_uri(), "message Empty"),
                container_name: Some("main".into()),
            },
        ],
        |s| s.name.clone(),
    );
    Ok(())
}

#[test]
fn test_workspace_symbols() -> pbls::Result<()> {
    let mut client = TestClient::new()?;

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: base_uri(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    client.recv::<PublishDiagnostics>()?;

    let Some(WorkspaceSymbolResponse::Flat(actual)) =
        client.request::<WorkspaceSymbolRequest>(WorkspaceSymbolParams {
            query: "".into(),
            work_done_progress_params: lsp_types::WorkDoneProgressParams {
                work_done_token: None,
            },
            partial_result_params: lsp_types::PartialResultParams {
                partial_result_token: None,
            },
        })?
    else {
        panic!("Symbols response is not Flat")
    };
    let expected = vec![
        // deprecated field is deprecated, but cannot be omitted
        #[allow(deprecated)]
        SymbolInformation {
            name: "Other".into(),
            kind: SymbolKind::STRUCT,
            tags: None,
            deprecated: None,
            location: locate(other_uri(), "message Other"),
            container_name: Some("other".into()),
        },
        // deprecated field is deprecated, but cannot be omitted
        #[allow(deprecated)]
        SymbolInformation {
            name: "Dep".into(),
            kind: SymbolKind::STRUCT,
            tags: None,
            deprecated: None,
            location: locate(dep_uri(), "message Dep"),
            container_name: Some("main".into()),
        },
        // deprecated field is deprecated, but cannot be omitted
        #[allow(deprecated)]
        SymbolInformation {
            name: "Dep2".into(),
            kind: SymbolKind::ENUM,
            tags: None,
            deprecated: None,
            location: locate(dep_uri(), "enum Dep2"),
            container_name: Some("main".into()),
        },
        // deprecated field is deprecated, but cannot be omitted
        #[allow(deprecated)]
        SymbolInformation {
            name: "Thing".into(),
            kind: SymbolKind::ENUM,
            tags: None,
            deprecated: None,
            location: locate(base_uri(), "enum Thing"),
            container_name: Some("main".into()),
        },
        // deprecated field is deprecated, but cannot be omitted
        #[allow(deprecated)]
        SymbolInformation {
            name: "Foo".into(),
            kind: SymbolKind::STRUCT,
            tags: None,
            deprecated: None,
            location: locate(base_uri(), "message Foo"),
            container_name: Some("main".into()),
        },
        // deprecated field is deprecated, but cannot be omitted
        #[allow(deprecated)]
        SymbolInformation {
            name: "Foo.Buz".into(),
            kind: SymbolKind::STRUCT,
            tags: None,
            deprecated: None,
            location: locate(base_uri(), "message Buz"),
            container_name: Some("main".into()),
        },
        // deprecated field is deprecated, but cannot be omitted
        #[allow(deprecated)]
        SymbolInformation {
            name: "Bar".into(),
            kind: SymbolKind::STRUCT,
            tags: None,
            deprecated: None,
            location: locate(base_uri(), "message Bar"),
            container_name: Some("main".into()),
        },
        #[allow(deprecated)]
        SymbolInformation {
            name: "Empty".into(),
            kind: SymbolKind::STRUCT,
            tags: None,
            deprecated: None,
            location: locate(base_uri(), "message Empty"),
            container_name: Some("main".into()),
        },
        #[allow(deprecated)]
        SymbolInformation {
            name: "Other.Nested".into(),
            kind: SymbolKind::STRUCT,
            tags: None,
            deprecated: None,
            location: locate(other_uri(), "message Nested"),
            container_name: Some("other".into()),
        },
    ];
    assert_elements_equal(actual, expected, |s| s.name.clone());
    Ok(())
}

#[test]
fn test_goto_definition_same_file() -> pbls::Result<()> {
    let mut client = TestClient::new()?;

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: base_uri(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    client.recv::<PublishDiagnostics>()?;

    assert_eq!(
        client.request::<GotoDefinition>(goto(base_uri(), "Thing t =", 3))?,
        Some(GotoDefinitionResponse::Scalar(locate(
            base_uri(),
            "enum Thing"
        )))
    );

    assert_eq!(
        client.request::<GotoDefinition>(goto(base_uri(), "Foo f =", 2))?,
        Some(GotoDefinitionResponse::Scalar(locate(
            base_uri(),
            "message Foo",
        )))
    );

    Ok(())
}

#[test]
fn test_goto_definition_same_file_nested() -> pbls::Result<()> {
    let mut client = TestClient::new()?;

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: base_uri(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    client.recv::<PublishDiagnostics>()?;

    assert_eq!(
        client.request::<GotoDefinition>(goto(base_uri(), "Foo.Buz buz =", 6))?,
        Some(GotoDefinitionResponse::Scalar(locate(
            base_uri(),
            "message Buz"
        )))
    );

    Ok(())
}

#[test]
fn test_goto_definition_different_file() -> pbls::Result<()> {
    let mut client = TestClient::new()?;

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: base_uri().clone(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    client.recv::<PublishDiagnostics>()?;

    let resp = client.request::<GotoDefinition>(goto(base_uri(), "Dep d =", 0))?;
    assert_eq!(
        resp,
        Some(GotoDefinitionResponse::Scalar(locate(
            dep_uri(),
            "message Dep",
        )))
    );

    Ok(())
}

#[test]
fn test_goto_definition_different_file_nested() -> pbls::Result<()> {
    let mut client = TestClient::new()?;

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: base_uri().clone(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    client.recv::<PublishDiagnostics>()?;

    let resp = client.request::<GotoDefinition>(goto(
        base_uri(),
        "other.Other.Nested other_nested =",
        0,
    ))?;
    assert_eq!(
        resp,
        Some(GotoDefinitionResponse::Scalar(locate(
            other_uri(),
            "message Nested",
        )))
    );

    Ok(())
}
