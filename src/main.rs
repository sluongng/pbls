use serde::{Deserialize, Serialize};
use serde_json as json;
use std::io;

#[derive(Serialize, Deserialize, Debug)]
struct Request {
    id: u32,
    method: String,
    params: serde_json::Value,
}

#[derive(Serialize, Deserialize, Debug)]
struct Response {
    id: u32,
}

fn read_len() -> io::Result<usize> {
    for line in io::stdin().lines() {
        match line?.strip_prefix("Content-Length: ") {
            Some(l) => match l.parse::<usize>() {
                Ok(sz) => return Ok(sz),
                Err(err) => panic!("Failed to parse: {}", err),
            },
            None => {}
        }
    }

    return Ok(0);
}

fn read_request() -> json::Result<Request> {
    let req: Request = json::from_reader(io::stdin())?;
    return Ok(req);
}

fn reply(resp: Response) -> json::Result<()> {
    let str = json::to_string(&resp)?;
    print!("Content-Length: {}\r\n\r\n", str.len());
    print!("{}\r\n\r\n", str);
    return Ok(());
}

fn main() {
    eprintln!("starting spelgud");
    loop {
        eprintln!("got len: {}", read_len().unwrap());
        let req = read_request().unwrap();
        eprintln!("got msg: {:?}", req);
        reply(Response { id: req.id }).unwrap();
    }
}
