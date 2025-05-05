use crate::common::{Ansi, Location, Token};
use std::process::exit;
use strum_macros::Display;

#[derive(Debug, Display)]
pub enum CasaError {
    BranchModifiedStack,
    InvalidSignature,
    InvalidStackState,
    StackUnderflow,
    SyntaxError,
    UnknownIdentifier,
    ValueError,
}

pub fn print_error(location: &Location, error: CasaError, message: &str) {
    eprintln!(
        "[{}{}{}] {}\n{}",
        Ansi::Red,
        error,
        Ansi::Reset,
        location,
        message
    );
}

pub fn fatal_error(location: &Location, error: CasaError, message: &str) -> ! {
    print_error(location, error, message);
    exit(1);
}
