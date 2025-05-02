use phf::phf_map;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use strum_macros::{Display, EnumString};

pub static DELIMITERS: phf::Map<char, Delimiter> = phf_map! {
    ':' => Delimiter::Colon,
    '(' => Delimiter::OpenParen,
    ')' => Delimiter::CloseParen,
};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Delimiter(Delimiter),
    Identifier,
    Intrinsic(Intrinsic),
    Keyword(Keyword),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Delimiter {
    CloseParen,
    Colon,
    OpenParen,
}

#[derive(Debug, Clone, PartialEq, EnumString, Display)]
#[strum(serialize_all = "lowercase")]
pub enum Keyword {
    Break,
    Cast,
    Const,
    Continue,
    Do,
    Done,
    Elif,
    Else,
    Endif,
    End,
    Enum,
    Fi,
    Function,
    If,
    Inline,
    Return,
    Then,
    Typeof,
    While,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i32),
    String(String),
}

#[derive(Debug, Clone, PartialEq, EnumString, Display)]
#[strum(serialize_all = "lowercase")]
pub enum Intrinsic {
    Add,
    And,
    Div,
    Drop,
    Dup,
    Eq,
    Ge,
    Gt,
    Le,
    LoadByte,
    LoadWord,
    LoadDword,
    LoadQword,
    Lt,
    Sub,
    Mod,
    Mul,
    Ne,
    Or,
    Over,
    Rot,
    Shl,
    Shr,
    StoreByte,
    StoreWord,
    StoreDword,
    StoreQword,
    Swap,
    Syscall0,
    Syscall1,
    Syscall2,
    Syscall3,
    Syscall4,
    Syscall5,
    Syscall6,
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
    pub location: Location,
}

impl Token {
    pub fn new(value: &str, ty: TokenType, location: Location) -> Self {
        Self {
            value: value.to_string(),
            ty,
            location,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum OpType {
    FunctionCall,
    FunctionEpilogue,
    FunctionPrologue,
    Intrinsic(Intrinsic),
    PushInt,
    PushStr,
    Return,

    // If block
    If,
    Then,
    Fi,

    // Must be resolved later
    Unknown,
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

#[derive(Debug)]
pub struct Parameter {
    pub name: Option<String>,
    pub ty: String,
}

/// # Examples
///
/// `str`
/// `str int -> bool`
/// `num1:int num2:int -> int bool`
#[derive(Debug)]
pub struct Signature {
    pub params: Vec<Parameter>,
    pub returns: Vec<String>,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub signature: Signature,
    pub location: Location,
    pub ops: Vec<Op>,
}

#[derive(Debug)]
pub enum Segment {
    Function(Function),
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
