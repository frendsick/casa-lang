use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};

use strum_macros::{Display, EnumString};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Intrinsic(Intrinsic),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i32),
    String(String),
}

#[derive(Debug, Clone, PartialEq, EnumString, Display)]
#[strum(serialize_all = "lowercase")]
pub enum Intrinsic {
    Syscall3,
}

#[derive(Debug, Clone)]
pub struct Location {
    pub file: PathBuf,
    pub row: usize,
    pub col: usize,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: String,
    pub ty: TokenType,
    pub loc: Location,
}

impl Token {
    pub fn new(value: &str, ty: TokenType, loc: Location) -> Self {
        Self {
            value: value.to_string(),
            ty,
            loc,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum OpType {
    Intrinsic(Intrinsic),
    PushInt,
    PushStr,
}

#[derive(Debug)]
pub struct Op {
    pub id: usize,
    pub ty: OpType,
    pub token: Token,
}

impl Op {
    pub fn new(id: usize, ty: OpType, token: &Token) -> Self {
        Self {
            id,
            ty,
            token: token.clone(),
        }
    }
}

pub struct Counter {
    count: AtomicUsize,
}

impl Counter {
    pub const fn new() -> Self {
        Counter {
            count: AtomicUsize::new(0),
        }
    }

    /// Adds to the current value, returning the previous value
    pub fn fetch_add(&self) -> usize {
        self.count.fetch_add(1, Ordering::SeqCst)
    }
}
