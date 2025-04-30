#![feature(if_let_guard)]

mod defs;
mod lexer;

use std::path::Path;

const CODE_FILE: &str = "test.stak";

fn main() {
    Path::new(env!("CARGO_MANIFEST_DIR")).join(CODE_FILE);
    let code = std::fs::read_to_string(CODE_FILE).unwrap();

    let tokens = lexer::parse_tokens(&code);
    dbg!(tokens);
}
