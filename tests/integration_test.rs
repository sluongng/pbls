use core::panic;
use lsp_server::{Connection, Message};
use lsp_types::notification::{
    DidChangeTextDocument, DidOpenTextDocument, DidSaveTextDocument, PublishDiagnostics,
};
use lsp_types::request::{
    Completion, DocumentSymbolRequest, GotoDefinition, Shutdown, WorkspaceSymbolRequest,
};
use lsp_types::{notification::Initialized, request::Initialize, InitializedParams};
use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, Diagnostic, DiagnosticSeverity,
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    DocumentSymbolParams, DocumentSymbolResponse, GotoDefinitionParams, GotoDefinitionResponse,
    InitializeParams, Location, PartialResultParams, Position, PublishDiagnosticsParams, Range,
    SymbolInformation, SymbolKind, TextDocumentContentChangeEvent, TextDocumentIdentifier,
    TextDocumentItem, TextDocumentPositionParams, Url, WorkDoneProgressParams,
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

fn completion_params(uri: Url, position: Position) -> CompletionParams {
    CompletionParams {
        text_document_position: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri },
            position,
        },
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: None,
        },
        partial_result_params: PartialResultParams {
            partial_result_token: None,
        },
        context: None,
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
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let path = std::env::temp_dir().join(format!("pbls-test-{now}"));
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
        match self
            .conn
            .receiver
            .recv_timeout(std::time::Duration::from_secs(5))?
        {
            Message::Request(r) => Err(format!("Expected notification, got: {r:?}"))?,
            Message::Response(r) => Err(format!("Expected notification, got: {r:?}"))?,
            Message::Notification(resp) => {
                assert_eq!(resp.method, T::METHOD, "Unexpected response {resp:?}");
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
        match self
            .conn
            .receiver
            .recv_timeout(std::time::Duration::from_secs(5))?
        {
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

    fn open(&self, uri: Url) -> pbls::Result<PublishDiagnosticsParams> {
        let text = std::fs::read_to_string(uri.path())?;
        self.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri,
                language_id: "".into(),
                version: 0,
                text,
            },
        })?;
        self.recv::<PublishDiagnostics>()
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

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: error_uri(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    let diags = client.recv::<PublishDiagnostics>()?;
    assert_eq!(diags.uri, error_uri());
    assert_elements_equal(
        diags.diagnostics,
        vec![
            diag(error_uri(), "Thingy t =", "\"Thingy\" is not defined."),
            diag(
                error_uri(),
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

    let text = r#"
syntax = "proto3";
package main;
message Foo{}
"#;
    std::fs::write(&path, text)?;

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: uri.clone(),
            language_id: "".into(),
            version: 0,
            text: text.into(),
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
    let text = r#"
syntax = "proto3";
package main;
message Foo{Flob flob = 1;}
"#;
    std::fs::write(&path, text)?;

    client.notify::<DidSaveTextDocument>(DidSaveTextDocumentParams {
        text_document: TextDocumentIdentifier { uri: uri.clone() },
        text: Some(text.into()),
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

#[test]
fn test_complete_import() -> pbls::Result<()> {
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

    let text = vec![
        "syntax = \"proto3\";",
        "import \"other.proto\";",
        "import \"",
    ]
    .join("\n");
    let change = TextDocumentContentChangeEvent {
        text: text.into(),
        range: None,
        range_length: None,
    };
    client.notify::<DidChangeTextDocument>(DidChangeTextDocumentParams {
        text_document: lsp_types::VersionedTextDocumentIdentifier {
            uri: base_uri().clone(),
            version: 0,
        },
        content_changes: vec![change],
    })?;

    let resp = client.request::<Completion>(completion_params(
        base_uri(),
        Position {
            line: 2,
            character: "import \"".len().try_into().unwrap(),
        },
    ))?;

    let Some(lsp_types::CompletionResponse::Array(actual)) = resp else {
        panic!("Unexpected completion response {resp:?}");
    };

    // excludes simple.proto (the current file)
    // excludes other.proto (already imported)
    assert_elements_equal(
        actual,
        vec![
            CompletionItem {
                label: "dep.proto".into(),
                kind: Some(CompletionItemKind::FILE),
                insert_text: Some("dep.proto\";".into()),
                ..Default::default()
            },
            CompletionItem {
                label: "error.proto".into(),
                kind: Some(CompletionItemKind::FILE),
                insert_text: Some("error.proto\";".into()),
                ..Default::default()
            },
        ],
        |s| s.label.clone(),
    );

    Ok(())
}

#[test]
fn test_complete_keyword() -> pbls::Result<()> {
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

    // get completion on the line after "package"
    let loc = locate(base_uri(), "package main;");

    let resp = client.request::<Completion>(completion_params(
        base_uri(),
        Position {
            line: loc.range.end.line + 1,
            character: 0,
        },
    ))?;

    let Some(lsp_types::CompletionResponse::Array(actual)) = resp else {
        panic!("Unexpected completion response {resp:?}");
    };

    assert_elements_equal(
        actual,
        vec![
            CompletionItem {
                label: "message".into(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            },
            CompletionItem {
                label: "enum".into(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            },
            CompletionItem {
                label: "import".into(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            },
        ],
        |s| s.label.clone(),
    );

    Ok(())
}

#[test]
fn test_complete_type() -> pbls::Result<()> {
    let mut client = TestClient::new()?;
    client.open(base_uri())?;

    // get completion in the body of message Foo.
    let pos = locate(base_uri(), "message Foo {").range.start;

    let resp = client.request::<Completion>(completion_params(
        base_uri(),
        Position {
            line: pos.line + 1,
            character: 0,
        },
    ))?;

    let Some(lsp_types::CompletionResponse::Array(actual)) = resp else {
        panic!("Unexpected completion response {resp:?}");
    };

    assert_elements_equal(
        actual,
        vec![
            CompletionItem {
                label: "message".into(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            },
            CompletionItem {
                label: "enum".into(),
                kind: Some(CompletionItemKind::KEYWORD),
                ..Default::default()
            },
            CompletionItem {
                label: "Bar".into(),
                kind: Some(CompletionItemKind::STRUCT),
                ..Default::default()
            },
            CompletionItem {
                label: "Empty".into(),
                kind: Some(CompletionItemKind::STRUCT),
                ..Default::default()
            },
            CompletionItem {
                label: "Foo".into(),
                kind: Some(CompletionItemKind::STRUCT),
                ..Default::default()
            },
            CompletionItem {
                label: "Foo.Buz".into(),
                kind: Some(CompletionItemKind::STRUCT),
                ..Default::default()
            },
            CompletionItem {
                label: "Thing".into(),
                kind: Some(CompletionItemKind::ENUM),
                ..Default::default()
            },
        ],
        |s| s.label.clone(),
    );

    Ok(())
}

#[test]
fn test_import_discovery() -> pbls::Result<()> {
    // Test the following structure
    // tmp
    // ├── root.proto
    // ├── sibling.proto
    // ├── a
    // │   ├── a.txt
    // │   ├── a.proto
    // │   └── f
    // │       └── af.proto
    // ├── b
    // │   ├── c
    // │   │   ├── bc.proto
    // │   │   └── bc.txt
    // │   └── d
    // │       └── bd.txt
    // ├── e
    // │
    // └── loop
    //     └── loop -> ../loop

    let tmp = TempDir::new();

    std::fs::create_dir_all(tmp.join("a/f"))?;
    std::fs::create_dir_all(tmp.join("b/c"))?;
    std::fs::create_dir_all(tmp.join("b/d"))?;
    std::fs::create_dir_all(tmp.join("e"))?;
    std::fs::create_dir_all(tmp.join("loop"))?;

    #[cfg(unix)]
    std::os::unix::fs::symlink(tmp.join("loop"), tmp.join("loop/loop"))?;

    #[cfg(windows)]
    std::os::windows::fs::symlink_dir(tmp.join("loop"), tmp.join("loop/loop"))?;

    std::fs::write(
        &tmp.join("root.proto"),
        r#"
        syntax = "proto3"; 

        import "sibling.proto"; 
        import "a.proto"; 
        import "af.proto"; 
        import "bc.proto"; 

        message Root{
            Sibling s = 1;
            A a = 2;
            AF af = 3;
            BC bc = 4;
        }"#,
    )?;

    std::fs::write(
        &tmp.join("sibling.proto"),
        "syntax = \"proto3\"; message Sibling{}",
    )?;

    std::fs::write(&tmp.join("a/a.txt"), "not a proto")?;
    std::fs::write(&tmp.join("a/a.proto"), "syntax = \"proto3\"; message A{}")?;
    std::fs::write(
        &tmp.join("a/f/af.proto"),
        "syntax = \"proto3\"; message AF{}",
    )?;

    std::fs::write(&tmp.join("b/c/bc.txt"), "not a proto")?;
    std::fs::write(
        &tmp.join("b/c/bc.proto"),
        "syntax = \"proto3\"; message BC{}",
    )?;

    std::fs::write(&tmp.join("b/d/bd.txt"), "not a proto")?;

    let root_uri = Url::from_file_path(&tmp.join("root.proto")).unwrap();
    let client = TestClient::new_with_root(&tmp)?;

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: root_uri.clone(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    let diags = client.recv::<PublishDiagnostics>()?;
    assert_eq!(
        diags,
        PublishDiagnosticsParams {
            uri: root_uri,
            diagnostics: vec![],
            version: None,
        }
    );
    Ok(())
}
