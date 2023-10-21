use lsp_types;
use serde::{Deserialize, Serialize};
use serde_json as json;
use std::error::Error;
use std::io::{self, Read};

#[derive(Serialize, Deserialize, Debug)]
struct Request {
    id: u32,
    method: String,
    params: json::Value,
}

#[derive(Serialize, Deserialize, Debug)]
struct Response {
    id: u32,
    result: json::Value,
}

#[derive(Serialize, Deserialize, Debug)]
struct ServerInfo {
    name: String,
    version: String,
}

#[derive(Serialize, Deserialize, Debug)]
struct InitializeResult {
    capabilities: ServerInfo,
    server_info: ServerInfo,
}

fn read_len() -> io::Result<usize> {
    let mut len = 0;

    for line in io::stdin().lines() {
        let l = line?;
        if l.is_empty() {
            break;
        }
        match l.strip_prefix("Content-Length: ") {
            Some(l) => match l.parse::<usize>() {
                Ok(sz) => len = sz,
                Err(err) => panic!("Failed to parse: {}", err),
            },
            None => {}
        }
    }

    return Ok(len);
}

fn read_request(len: usize) -> Result<Request, Box<dyn Error>> {
    let mut buf = vec![0; len];
    io::stdin().read_exact(&mut buf)?;
    let req: Request = json::from_slice(&buf)?;
    return Ok(req);
}

fn reply(resp: Response) -> json::Result<()> {
    let str = json::to_string(&resp)?;
    print!("Content-Length: {}\r\n\r\n", str.len());
    print!("{}\r\n\r\n", str);
    return Ok(());
}

fn main() {
    let result = lsp_types::InitializeResult {
        capabilities: lsp_types::ServerCapabilities {
            ..lsp_types::ServerCapabilities::default()
        },
        server_info: Some(lsp_types::ServerInfo {
            name: String::from("spelgud"),
            version: Some(String::from("0.1")),
        }),
    };
    eprintln!("starting spelgud {:?}", json::to_string(&result));
    loop {
        let len = read_len().unwrap();
        eprintln!("got len: {}", len);
        let req = read_request(len).unwrap();
        eprintln!("got msg: {:?}", req);
        reply(Response {
            id: req.id,
            result: json::to_value(&result).unwrap(),
        })
        .unwrap();
    }
}
