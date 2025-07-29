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
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::cli::CasaCli;
use crate::compile::compile_assembly_code;
use crate::error::{CasaError, fatal_error_short, print_if_verbose};
use crate::type_check::type_check_program;

fn main() -> io::Result<()> {
    let cli = CasaCli::parse();
    let args = cli::parse_args(&cli);
    let input_path = canonicalize_path_must_exist(&args.input);

    print_if_verbose("Parsing included files", &args);
    let segments = lexer::parse_segments_from_included_files(&input_path);

    print_if_verbose("Type checking the program", &args);
    type_check_program(&segments);

    print_if_verbose("Generating assembly code", &args);
    let assembly_code = asm::generate_assembly_code(&segments);

    print_if_verbose("Compiling assembly code", &args);
    let executable = compile_assembly_code(&assembly_code, &input_path, &args)?;

    if matches!(cli, CasaCli::Run(_)) {
        print_if_verbose("Running the program", &args);
        Command::new(executable).status()?;
    }

    Ok(())
}

fn canonicalize_path_must_exist(path: &Path) -> PathBuf {
    match path.canonicalize() {
        Ok(path) => path,
        Err(error) => fatal_error_short(
            CasaError::FileNotFound,
            &format!("Cannot read file '{}': {}", path.display(), error),
        ),
    }
}
