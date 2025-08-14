use indexmap::IndexSet;
use itertools::Itertools;
use phf::phf_map;
use std::collections::HashMap;
use std::fmt;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, OnceLock, RwLock};
use strum_macros::{Display, EnumString};

// A table of identifiers declared at the global scope (e.g., functions)
pub type IdentifierTable = HashMap<String, Identifier>;
pub static GLOBAL_IDENTIFIERS: OnceLock<IdentifierTable> = OnceLock::new();

pub static DELIMITERS: phf::Map<char, Delimiter> = phf_map! {
    ')' => Delimiter::CloseParen,
    ':' => Delimiter::Colon,
    '.' => Delimiter::Dot,
    '(' => Delimiter::OpenParen,
};

#[derive(Display)]
pub enum Ansi {
    #[strum(to_string = "\x1B[0m")]
    Reset,
    #[strum(to_string = "\x1B[91m")]
    Red,
    #[strum(to_string = "\x1B[33m")]
    Yellow,
    #[strum(to_string = "\x1B[94m")]
    Blue,
    #[strum(to_string = "\x1B[95m")]
    Purple,
}

#[derive(Debug)]
pub enum Identifier {
    Constant(Constant),
    Function(Function),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Delimiter(Delimiter),
    EndOfFile,
    Identifier,
    Intrinsic(Intrinsic),
    Keyword(Keyword),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Delimiter {
    CloseParen,
    Colon,
    Dot,
    OpenParen,
}

#[derive(Debug, Clone, PartialEq, EnumString, Display)]
#[strum(serialize_all = "lowercase")]
pub enum Keyword {
    Assign,
    Bind,
    Break,
    Cast,
    Const,
    Continue,
    Do,
    Done,
    Elif,
    Else,
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
    Boolean(bool),
    Integer(i32),
    String(String),
}

#[derive(Debug, EnumString, Display)]
#[strum(serialize_all = "UPPERCASE")]
pub enum DataSize {
    Byte,
    Word,
    Dword,
    Qword,
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
    Neg,
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
        write!(f, "{}:{}:{}", self.file.display(), self.row, self.col,)
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
    Cast(String),
    FunctionEpilogue,
    FunctionPrologue,
    Intrinsic(Intrinsic),
    MethodCall,
    PushBool,
    PushInt,
    PushStr,
    Return,

    // If block
    If,
    Then,
    Elif,
    Else,
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
    AssignBind,
    TakeBind,
    PeekBind,

    // Must be resolved later
    Identifier,
}

#[derive(Debug, Clone)]
pub struct Op {
    pub id: usize,
    pub ty: OpType,
    pub token: Token,
    pub receiver: Arc<RwLock<Option<Type>>>,
}

impl Op {
    pub fn new(id: usize, ty: OpType, token: &Token) -> Self {
        Self {
            id,
            ty,
            token: token.clone(),
            receiver: Arc::new(RwLock::new(None)),
        }
    }
}

pub type Type = String;

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: Option<String>,
    pub ty: Type,
}

impl fmt::Display for Parameter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{}:", name)?;
        }
        write!(f, "{}", self.ty)
    }
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
    pub return_types: Vec<Type>,
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.params.is_empty() {
            write!(f, "{}", self.params.iter().join(" "))?;
        } else {
            write!(f, "()")?;
        }

        if !self.return_types.is_empty() {
            write!(f, " -> {}", self.return_types.iter().join(" "))?;
        } else {
            write!(f, " -> ()")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Constant {
    pub name: String,
    pub literal: Literal,
    pub location: Location,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub signature: Signature,
    pub location: Location,
    pub is_inline: bool,
    pub is_used: Arc<RwLock<bool>>,
    pub ops: Vec<Op>,
    pub variables: IndexSet<Type>,
}

#[derive(Debug, Clone)]
pub enum Segment {
    Constant(Constant),
    Function(Function),
    Include(PathBuf),
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

fn find_op_index_from_nested_ifs(op: &Op, function: &Function, op_type: OpType) -> Option<usize> {
    let op_index = function.ops.binary_search_by_key(&op.id, |op| op.id).ok()?;
    let mut nested_ifs = 0;

    for other_op in &function.ops[op_index + 1..] {
        match &other_op.ty {
            ty if *ty == op_type && nested_ifs == 0 => return Some(other_op.id),
            OpType::Fi => nested_ifs -= 1,
            OpType::If => nested_ifs += 1,
            _ => {}
        }
    }
    None
}

pub fn get_related_elif_id(op: &Op, function: &Function) -> Option<usize> {
    assert!(op.ty == OpType::Elif || op.ty == OpType::Then);
    find_op_index_from_nested_ifs(op, function, OpType::Elif)
}

pub fn get_related_else_id(op: &Op, function: &Function) -> Option<usize> {
    assert!(op.ty == OpType::Elif || op.ty == OpType::Then);
    find_op_index_from_nested_ifs(op, function, OpType::Else)
}

pub fn get_related_fi_id(op: &Op, function: &Function) -> Option<usize> {
    assert!(op.ty == OpType::Elif || op.ty == OpType::Else || op.ty == OpType::Then);
    find_op_index_from_nested_ifs(op, function, OpType::Fi)
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
