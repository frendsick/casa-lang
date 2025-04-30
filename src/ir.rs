use crate::defs::{Intrinsic, Literal, Op, OpType, Token, TokenType};

pub fn generate_ops(tokens: &[Token]) -> Vec<Op> {
    let mut ops: Vec<Op> = Vec::new();

    for token in tokens {
        match &token.ty {
            TokenType::Intrinsic(intrinsic) => ops.push(get_intrinsic_op(intrinsic, token)),
            TokenType::Literal(literal) => ops.push(get_literal_op(literal, token)),
        }
    }

    ops
}

fn get_intrinsic_op(intrinsic: &Intrinsic, token: &Token) -> Op {
    match intrinsic {
        Intrinsic::Syscall3 => Op::new(OpType::Intrinsic, token),
    }
}

fn get_literal_op(literal: &Literal, token: &Token) -> Op {
    match literal {
        Literal::Integer(_) => Op::new(OpType::PushInt, token),
        Literal::String(_) => Op::new(OpType::PushStr, token),
    }
}
