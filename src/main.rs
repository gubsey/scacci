use std::io::stdin;

use scacci::{aparsetheid::*, notation::parse_note};

fn main() {
    let mut lines = stdin().lines();
    while let Some(Ok(line)) = lines.next() {
        parse_note(&line);
    }
}
