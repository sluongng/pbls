use std::io::{BufRead, BufReader, Read, Write};
use std::{error::Error, process::Stdio};

use lsp_types::{
    notification::PublishDiagnostics, request::GotoDefinition, DiagnosticSeverity,
    GotoDefinitionResponse, InitializeParams, Range, ServerCapabilities,
};

use lsp_server::{Connection, ExtractError, Message, Notification, Request, RequestId, Response};

// fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
//     // Note that  we must have our logging only write out to stderr.
//     eprintln!("Starting generic LSP server");

//     let (connection, io_threads) = Connection::stdio();

//     let server_capabilities = serde_json::to_value(&ServerCapabilities {
//         // completion_provider: Some(lsp_types::CompletionOptions::default()),
//         diagnostic_provider: Some(lsp_types::DiagnosticServerCapabilities::Options(
//             lsp_types::DiagnosticOptions {
//                 identifier: Some(String::from("spelgud")),
//                 ..Default::default()
//             },
//         )),
//         ..Default::default()
//     })
//     .unwrap();
//     let initialization_params = connection.initialize(server_capabilities)?;
//     main_loop(connection, initialization_params)?;
//     io_threads.join()?;

//     eprintln!("Shutting down server");
//     Ok(())
// }

struct Correction {
    start: u32,
    end: u32,
}

fn send_line(
    input: &str,
    writer: &mut std::process::ChildStdin,
    reader: &mut BufReader<std::process::ChildStdout>,
) -> Result<String, Box<dyn Error>> {
    eprint!("Sending to aspell: {}", input);
    writer.write(input.as_bytes())?;
    for line in reader.lines() {
        let ln = line?;
        eprintln!("Got {}", ln);
        if ln.is_empty() {
            return Ok(String::new());
        }

        let mut splits = ln.split_whitespace();
        match splits.next() {
            Some("&") => {
                // & badword suggestion_count offset: suggestion1, suggestion2, ...
                let word = splits.next().unwrap();

                // skip suggestion count
                if splits.next().is_none() {
                    Err("Early end of line")?
                }

                let offset = splits
                    .next()
                    .unwrap()
                    .to_string()
                    .strip_suffix(":")
                    .unwrap()
                    .parse::<u32>()?;
                eprintln!("Got mispell word:{} offset:{}", word, offset);
            }
            Some(sym) => {
                Err(format!("Unexpected aspell prefix: {}", sym))?;
            }
            None => {
                return Ok(String::new());
            }
        }
    }
    Ok(String::new())
}

fn main() -> Result<(), Box<dyn Error>> {
    let mut aspell = std::process::Command::new("aspell")
        .arg("pipe")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;
    eprintln!("Started aspell with pid {}", aspell.id());
    // let mut stdin = aspell.stdin.take().expect("Failed to open stdin");
    // let mut stdout = aspell.stdout.take().expect("Failed to open stdin");
    let mut stdin = aspell.stdin.take().unwrap();
    let stdout = aspell.stdout.take().unwrap();
    let mut reader = BufReader::new(stdout);

    // put aspell into "terse" mode, so it does not send "*" for correct words
    stdin.write_all("!\n".as_bytes())?;

    // first line from aspell is version info
    let mut intro = String::new();
    reader.read_line(&mut intro)?;
    eprintln!("aspell: {}", intro);

    {
        let res = send_line("henlo hai world world\n", &mut stdin, &mut reader)?;
        eprintln!("read '{}'", res);
    }
    {
        let res = send_line("wurld\n", &mut stdin, &mut reader)?;
        eprintln!("read '{}'", res);
    }

    Ok(())
}

fn main_loop(
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();
    eprintln!("Starting example main loop");
    for msg in &connection.receiver {
        eprintln!("Got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("Got request: {req:?}");
                match cast::<GotoDefinition>(req) {
                    Ok((id, params)) => {
                        eprintln!("got gotoDefinition request #{id}: {params:?}");
                        let result = Some(GotoDefinitionResponse::Array(Vec::new()));
                        let result = serde_json::to_value(&result).unwrap();
                        let resp = Response {
                            id,
                            result: Some(result),
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                // ...
            }
            Message::Response(resp) => {
                eprintln!("got response: {resp:?}");
            }
            Message::Notification(not) => {
                eprintln!("got notification: {not:?}");
                match notification::<lsp_types::notification::DidOpenTextDocument>(not) {
                    Ok(params) => {
                        eprintln!("Got DidOpenTextDocument: {params:?}");
                        match on_open(params) {
                            Ok(resp) => connection.sender.send(Message::Notification(resp))?,
                            Err(err) => eprintln!("DidOpenTextDocument error: {err:?}"),
                        }
                    }
                    Err(err) => {
                        eprintln!("DidOpenTextDocument error: {err:?}");
                    }
                }
            }
        }
    }
    Ok(())
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn notification<N>(not: Notification) -> Result<N::Params, ExtractError<Notification>>
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

fn on_open(params: lsp_types::DidOpenTextDocumentParams) -> Result<Notification, Box<dyn Error>> {
    let diags = vec![lsp_types::Diagnostic {
        range: Range {
            start: lsp_types::Position {
                line: 0,
                character: 0,
            },
            end: lsp_types::Position {
                line: 0,
                character: 10,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some(String::from("spell")),
        message: String::from("misspelling"),
        ..Default::default()
    }];
    let result = Some(lsp_types::PublishDiagnosticsParams {
        uri: params.text_document.uri,
        diagnostics: diags,
        version: None,
    });
    Ok(Notification {
        method: method::<PublishDiagnostics>(),
        params: serde_json::to_value(&result)?,
    })
}
