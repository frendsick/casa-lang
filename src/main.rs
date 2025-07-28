#![feature(if_let_guard)]
#![feature(let_chains)]
// TODO: Re-enable warnings when the language is more mature
#![allow(unused_variables)]
#![allow(dead_code)]

mod asm;
mod cli;
mod common;
mod compile;
mod error;
mod lexer;
mod type_check;

use clap::Parser;
use std::io;
use std::process::Command;

use crate::cli::CasaCli;
use crate::compile::compile_assembly_code;
use crate::type_check::type_check_program;

const ASSEMBLY_FILE: &str = "test.asm";
const CODE_FILE: &str = "test.casa";

fn main() -> io::Result<()> {
    let cli = CasaCli::parse();
    let args = cli::parse_args(&cli);
    let input_path = cli::parse_input_path(&args);

    let segments = lexer::parse_code_file(&input_path);

    type_check_program(&segments);

    let assembly_code = asm::generate_assembly_code(&segments);
    let executable = compile_assembly_code(&assembly_code, &input_path)?;

    // Run the generated executable
    if let CasaCli::Run(_) = cli {
        Command::new(executable).status()?;
    }

    Ok(())
}
