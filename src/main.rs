use serde_json as json;
use std::io;

fn read_len() -> io::Result<usize> {
    for line in io::stdin().lines() {
        match line.unwrap().strip_prefix("Content-Length: ") {
            Some(l) => match l.parse::<usize>() {
                Ok(sz) => return Ok(sz),
                Err(err) => panic!("Failed to parse: {}", err),
            },
            None => {}
        }
    }

    return Ok(0);
}

fn read_msg() -> json::Result<json::Value> {
    let val: json::Value = serde_json::from_reader(io::stdin())?;
    return Ok(val);
}

fn main() {
    eprintln!("starting spelgud");
    eprintln!("got len: {}", read_len().unwrap());
    eprintln!("got msg: {}", read_msg().unwrap());
    return;
}
