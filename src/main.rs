use serde_json::Value;
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

fn main() {
    eprintln!("starting spelgud");
    eprintln!("got len: {}", read_len().unwrap());
    return;
    // // Parse the string of data into serde_json::Value.
    // let v: Value = match serde_json::from_reader(io::stdin()) {
    //     Ok(j) => j,
    //     Err(err) => {
    //         panic!("Failed to deserialize: {:?}", err)
    //     }
    // };

    // eprintln!("{}", v);
}
