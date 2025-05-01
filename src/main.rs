#![feature(if_let_guard)]

mod asm;
mod defs;
mod ir;
mod lexer;

use std::path::Path;

const ASSEMBLY_FILE: &str = "test.asm";
const CODE_FILE: &str = "test.stak";

fn main() {
    let crate_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let code = std::fs::read_to_string(crate_dir.join(CODE_FILE)).unwrap();

    let tokens = lexer::parse_tokens(&code);
    dbg!(&tokens);

    let ops = ir::generate_ops(&tokens);
    dbg!(&ops);

    let assembly_code = asm::generate_assembly_code(&ops);
    std::fs::write(crate_dir.join(ASSEMBLY_FILE), assembly_code);
    println!("{}", &assembly_code);
}
