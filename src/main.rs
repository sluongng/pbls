use serde_json::Value;
use std::io;

fn main() {
    // Parse the string of data into serde_json::Value.
    let v: Value = match serde_json::from_reader(io::stdin()) {
        Ok(j) => j,
        Err(err) => {
            panic!("Failed to deserialize: {:?}", err)
        }
    };

    // Access parts of the data by indexing with square brackets.
    println!("{}", v);
}
