use lsp_server::{Connection, Message, Notification};
use lsp_types::notification::{DidOpenTextDocument, PublishDiagnostics};
use lsp_types::request::Shutdown;
use lsp_types::{notification::Initialized, request::Initialize, InitializedParams};
use lsp_types::{DidOpenTextDocumentParams, InitializeParams, TextDocumentItem, Url};
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
            pbls::start(server).unwrap();
        });
        let mut client = TestClient {
            conn: client,
            thread: Some(thread),
            id: 0,
        };

        client.request::<Initialize>(InitializeParams::default())?;
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
fn test_diagnostics() -> pbls::Result<()> {
    let mut client = TestClient::new()?;

    client.notify::<DidOpenTextDocument>(DidOpenTextDocumentParams {
        text_document: TextDocumentItem {
            uri: Url::from_file_path(std::path::Path::new("example.proto").canonicalize()?)
                .unwrap(),
            language_id: "".into(),
            version: 0,
            text: "".into(),
        },
    })?;
    let diags = client.recv::<PublishDiagnostics>()?;
    // assert_eq!(
    //     InitializeResult::from(),
    //     InitializeResult {
    //         capabilities: ServerCapabilities {
    //             document_symbol_provider: Some(OneOf::Left(true)),
    //             text_document_sync: Some(TextDocumentSyncCapability::Options(
    //                 TextDocumentSyncOptions {
    //                     save: Some(TextDocumentSyncSaveOptions::Supported(true)),
    //                     ..Default::default()
    //                 },
    //             )),
    //             // completion_provider: Some(lsp_types::CompletionOptions::default()),
    //             diagnostic_provider: Some(DiagnosticServerCapabilities::Options(
    //                 lsp_types::DiagnosticOptions {
    //                     identifier: Some(String::from("spelgud")),
    //                     ..Default::default()
    //                 },
    //             )),
    //             ..Default::default()
    //         },
    //         server_info: None,
    //     }
    // );
    Ok(())
}
