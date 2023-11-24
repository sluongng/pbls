use core::panic;
use lsp_server::{Connection, Message};
use lsp_types::notification::{DidOpenTextDocument, DidSaveTextDocument, PublishDiagnostics};
use lsp_types::request::{
    DocumentDiagnosticRequest, DocumentSymbolRequest, GotoDefinition, Shutdown,
    WorkspaceSymbolRequest,
};
use lsp_types::{notification::Initialized, request::Initialize, InitializedParams};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    DocumentDiagnosticParams, DocumentSymbolParams, DocumentSymbolResponse, GotoDefinitionParams,
    GotoDefinitionResponse, InitializeParams, Location, Position, PublishDiagnosticsParams, Range,
    SymbolInformation, SymbolKind, TextDocumentIdentifier, TextDocumentItem,
    TextDocumentPositionParams, Url, WorkspaceSymbolParams, WorkspaceSymbolResponse,
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

fn error_uri() -> Url {
    Url::from_file_path(std::fs::canonicalize("./testdata/error.proto").unwrap()).unwrap()
}

fn diag(uri: Url, target: &str, message: &str) -> Diagnostic {
    Diagnostic {
        range: locate(uri, target).range,
        message: message.into(),
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("pbls".into()),
        ..Default::default()
    }
}

fn sym(uri: Url, pkg: &str, name: &str, text: &str) -> SymbolInformation {
    // deprecated field is deprecated, but cannot be omitted
    let kind = text
        .split_once(" ")
        .unwrap_or_else(|| panic!("Invalid symbol {text}"))
        .0;
    #[allow(deprecated)]
    SymbolInformation {
        name: name.into(),
        kind: match kind {
            "enum" => SymbolKind::ENUM,
            "message" => SymbolKind::STRUCT,
            _ => panic!("Invalid symbol {text}"),
        },
        tags: None,
        deprecated: None,
        location: locate(uri, text),
        container_name: Some(pkg.into()),
    }
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

struct TempDir(std::path::PathBuf);

impl TempDir {
    fn new() -> TempDir {
        let path = std::env::temp_dir().join("pbls-test");
        std::fs::create_dir_all(&path).unwrap();
        TempDir(path)
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        std::fs::remove_dir_all(&self.0).unwrap();
    }
}

impl AsRef<std::path::Path> for TempDir {
    fn as_ref(&self) -> &std::path::Path {
        &self.0
    }
}

impl std::ops::Deref for TempDir {
    type Target = std::path::PathBuf;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for TempDir {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

struct TestClient {
    conn: Connection,
    thread: Option<std::thread::JoinHandle<()>>,
    id: i32,
}

impl TestClient {
    fn new() -> Result<TestClient> {
        Self::new_with_root("testdata")
    }

    fn new_with_root(path: impl AsRef<std::path::Path>) -> Result<TestClient> {
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
            root_uri: Some(Url::from_file_path(std::fs::canonicalize(path).unwrap()).unwrap()),
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
fn test_open() -> pbls::Result<()> {
    let client = TestClient::new()?;

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: base_uri(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    let diags = client.recv::<PublishDiagnostics>()?;
    assert_eq!(
        diags,
        PublishDiagnosticsParams {
            uri: base_uri(),
            diagnostics: vec![],
            version: None,
        }
    );
    Ok(())
}

#[test]
fn test_diagnostics_on_open() -> pbls::Result<()> {
    let client = TestClient::new()?;

    let uri = Url::from_file_path(std::fs::canonicalize("testdata/error.proto")?).unwrap();

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
    assert_elements_equal(
        diags.diagnostics,
        vec![
            diag(uri.clone(), "Thingy t =", "\"Thingy\" is not defined."),
            diag(
                uri,
                "int32 foo =",
                "Field number 1 has already been used in \"main.Bar\" by field \"f\"",
            ),
        ],
        |s| s.message.clone(),
    );
    Ok(())
}

#[test]
fn test_diagnostics_on_save() -> pbls::Result<()> {
    let tmp = TempDir::new();
    let path = tmp.join("example.proto");
    let uri = Url::from_file_path(&path).unwrap();
    let client = TestClient::new_with_root(&tmp)?;

    std::fs::write(
        &path,
        r#"
syntax = "proto3";
package main;
message Foo{}
        "#,
    )?;

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
            uri: uri.clone(),
            diagnostics: vec![],
            version: None,
        }
    );

    // modify the file, check that we pick up the change
    std::fs::write(
        &path,
        r#"
syntax = "proto3";
package main;
message Foo{Flob flob = 1;}
        "#,
    )?;

    client.notify::<DidSaveTextDocument>(DidSaveTextDocumentParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        text: None, // we just re-read the file from disk
    })?;
    let diags = client.recv::<PublishDiagnostics>()?;
    assert_eq!(
        diags,
        PublishDiagnosticsParams {
            uri: uri.clone(),
            diagnostics: vec![diag(
                uri,
                "message Foo{Flob flob = 1;}",
                "\"Flob\" is not defined",
            )],
            version: None,
        }
    );

    Ok(())
}

#[test]
fn test_no_diagnostics_on_open() -> pbls::Result<()> {
    let client = TestClient::new()?;

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: base_uri(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    let diags = client.recv::<PublishDiagnostics>()?;

    assert_eq!(
        diags,
        PublishDiagnosticsParams {
            uri: base_uri(),
            diagnostics: vec![],
            version: None
        }
    );
    Ok(())
}

#[test]
fn test_document_diagnostics() -> pbls::Result<()> {
    let mut client = TestClient::new()?;

    let uri = error_uri();

    let resp = client.request::<DocumentDiagnosticRequest>(DocumentDiagnosticParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        identifier: None,
        previous_result_id: None,
        work_done_progress_params: lsp_types::WorkDoneProgressParams {
            work_done_token: None,
        },
        partial_result_params: lsp_types::PartialResultParams {
            partial_result_token: None,
        },
    })?;

    let lsp_types::DocumentDiagnosticReportResult::Report(
        lsp_types::DocumentDiagnosticReport::Full(resp),
    ) = resp
    else {
        panic!("Unexpected response {resp:?}")
    };

    assert_elements_equal(
        resp.full_document_diagnostic_report.items,
        vec![
            diag(uri.clone(), "Thingy t =", "\"Thingy\" is not defined."),
            diag(
                uri,
                "int32 foo =",
                "Field number 1 has already been used in \"main.Bar\" by field \"f\"",
            ),
        ],
        |s| s.message.clone(),
    );
    Ok(())
}
#[test]
fn test_document_symbols() -> pbls::Result<()> {
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

    let Some(DocumentSymbolResponse::Flat(actual)) =
        client.request::<DocumentSymbolRequest>(DocumentSymbolParams {
            text_document: TextDocumentIdentifier {
                uri: base_uri().clone(),
            },
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
            sym(base_uri(), "main", "Thing", "enum Thing"),
            sym(base_uri(), "main", "Foo", "message Foo"),
            sym(base_uri(), "main", "Foo.Buz", "message Buz"),
            sym(base_uri(), "main", "Bar", "message Bar"),
            sym(base_uri(), "main", "Empty", "message Empty"),
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
        sym(other_uri(), "other", "Other", "message Other"),
        sym(dep_uri(), "main", "Dep", "message Dep"),
        sym(dep_uri(), "main", "Dep2", "enum Dep2"),
        sym(base_uri(), "main", "Thing", "enum Thing"),
        sym(base_uri(), "main", "Foo", "message Foo"),
        sym(base_uri(), "main", "Foo.Buz", "message Buz"),
        sym(base_uri(), "main", "Bar", "message Bar"),
        sym(base_uri(), "main", "Empty", "message Empty"),
        sym(other_uri(), "other", "Other.Nested", "message Nested"),
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
