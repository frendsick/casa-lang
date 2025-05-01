#![feature(if_let_guard)]

mod asm;
mod compile;
mod defs;
mod ir;
mod lexer;

use compile::compile_assembly_code;
use std::io;
use std::path::Path;

const ASSEMBLY_FILE: &str = "test.asm";
const CODE_FILE: &str = "test.stak";

fn main() -> io::Result<()> {
    let crate_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let code = std::fs::read_to_string(crate_dir.join(CODE_FILE)).unwrap();

    let tokens = lexer::parse_tokens(&code);
    dbg!(&tokens);

    let ops = ir::generate_ops(&tokens);
    dbg!(&ops);

    let assembly_code = asm::generate_assembly_code(&ops);
    let assembly_file = crate_dir.join(ASSEMBLY_FILE);
    std::fs::write(&assembly_file, &assembly_code);
    println!("{}", &assembly_code);

    compile_assembly_code(&assembly_file)?;

    Ok(())
}
