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
pub struct Token {
    pub value: String,
    pub ty: TokenType,
}

impl Token {
    pub fn new(value: &str, ty: TokenType) -> Self {
        Self {
            value: value.to_string(),
            ty,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum OpType {
    Intrinsic,
    PushInt,
    PushStr,
}

#[derive(Debug)]
pub struct Op {
    ty: OpType,
    token: Token,
}

impl Op {
    pub fn new(ty: OpType, token: &Token) -> Self {
        Self {
            ty,
            token: token.clone(),
        }
    }
}
