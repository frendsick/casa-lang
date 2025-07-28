use crate::common::{Ansi, Location};
use std::process::exit;
use strum_macros::Display;

#[derive(Debug, Display)]
pub enum CasaError {
    BranchModifiedStack,
    FileNotFound,
    InvalidSignature,
    InvalidStackState,
    StackUnderflow,
    SyntaxError,
    UnknownFileExtension,
    UnknownIdentifier,
    ValueError,
}

pub fn colored_error_tag(error: CasaError) -> String {
    format!("[{}{}{}]", Ansi::Red, error, Ansi::Reset)
}

pub fn print_error(location: &Location, error: CasaError, message: &str) {
    eprintln!("{} {}\n\n{}", colored_error_tag(error), location, message);
}

pub fn print_error_short(error: CasaError, message: &str) {
    eprintln!("{} {}", colored_error_tag(error), message);
}

pub fn fatal_error(location: &Location, error: CasaError, message: &str) -> ! {
    print_error(location, error, message);
    exit(1);
}

pub fn fatal_error_short(error: CasaError, message: &str) -> ! {
    print_error_short(error, message);
    exit(1);
}
