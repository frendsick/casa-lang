use indexmap::IndexSet;
use phf::phf_map;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use strum_macros::{Display, EnumString};

#[derive(Display)]
pub enum Ansi {
    #[strum(to_string = "\x1B[0m")]
    Reset,
    #[strum(to_string = "\x1B[33m")]
    Yellow,
}

pub static DELIMITERS: phf::Map<char, Delimiter> = phf_map! {
    ':' => Delimiter::Colon,
    '(' => Delimiter::OpenParen,
    ')' => Delimiter::CloseParen,
};

pub type IdentifierTable = HashMap<String, Identifier>;

#[derive(Debug)]
pub enum Identifier {
    Function(Function),
}

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
    Bind,
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
    Fun,
    If,
    Inline,
    Peek,
    Return,
    Take,
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
#[strum(serialize_all = "snake_case")]
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

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}{}{}:{}{}{}",
            self.file.display(),
            Ansi::Yellow,
            self.row,
            Ansi::Reset,
            Ansi::Yellow,
            self.col,
            Ansi::Reset
        )
    }
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
    InlineFunctionCall,
    Intrinsic(Intrinsic),
    PushInt,
    PushStr,
    Return,

    // If block
    If,
    Then,
    Fi,

    // While block
    While,
    Do,
    Done,
    Break,
    Continue,

    // Variables
    Take,
    Peek,
    Bind,
    TakeBind,
    PeekBind,
    PushBind,

    // Must be resolved later
    Unknown,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Option<String>,
    pub ty: String,
}

pub trait ParameterSlice {
    fn get_types(&self) -> Vec<String>;
}

impl ParameterSlice for [Parameter] {
    fn get_types(&self) -> Vec<String> {
        self.iter().map(|param| param.ty.clone()).collect()
    }
}

/// # Examples
///
/// `str`
/// `str int -> bool`
/// `num1:int num2:int -> int bool`
#[derive(Debug, Clone)]
pub struct Signature {
    pub params: Vec<Parameter>,
    pub returns: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub signature: Signature,
    pub location: Location,
    pub is_inline: bool,
    pub ops: Vec<Op>,
    // name, type
    pub variables: IndexSet<String>,
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

pub fn get_related_fi_id(op: &Op, function: &Function) -> Option<usize> {
    assert!(op.ty == OpType::Then);

    let op_index = function.ops.binary_search_by_key(&op.id, |op| op.id).ok()?;
    let mut nested_ifs = 0;

    for other_op in &function.ops[op_index + 1..] {
        match other_op.ty {
            OpType::Fi if nested_ifs == 0 => return Some(other_op.id),
            OpType::Fi => nested_ifs -= 1,
            OpType::If => nested_ifs += 1,
            _ => {}
        }
    }
    None
}

pub fn get_related_done_id(op: &Op, function: &Function) -> Option<usize> {
    assert!(op.ty == OpType::Break || op.ty == OpType::Do);

    let op_index = function.ops.binary_search_by_key(&op.id, |op| op.id).ok()?;
    let mut nested_whiles = 0;

    for other_op in &function.ops[op_index + 1..] {
        match other_op.ty {
            OpType::Done if nested_whiles == 0 => return Some(other_op.id),
            OpType::Done => nested_whiles -= 1,
            OpType::While => nested_whiles += 1,
            _ => {}
        }
    }
    None
}

pub fn get_related_while_id(op: &Op, function: &Function) -> Option<usize> {
    assert!(op.ty == OpType::Continue || op.ty == OpType::Done);

    let op_index = function.ops.binary_search_by_key(&op.id, |op| op.id).ok()?;
    let mut nested_whiles = 0;

    for other_op in function.ops[..op_index].iter().rev() {
        match other_op.ty {
            OpType::While if nested_whiles == 0 => return Some(other_op.id),
            OpType::Done => nested_whiles += 1,
            OpType::While => nested_whiles -= 1,
            _ => {}
        }
    }
    None
}
