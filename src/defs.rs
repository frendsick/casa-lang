use strum_macros::{Display, EnumString};

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Intrinsic(Intrinsic),
    Literal(Literal),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i32),
    String(String),
}

#[derive(Debug, PartialEq, EnumString, Display)]
#[strum(serialize_all = "lowercase")]
pub enum Intrinsic {
    Syscall3,
}

#[derive(Debug)]
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
