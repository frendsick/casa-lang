use crate::defs::{Counter, Intrinsic, Literal, Op, OpType, Token, TokenType};

static OP_COUNTER: Counter = Counter::new();

pub fn generate_ops(tokens: &[Token]) -> Vec<Op> {
    let mut ops: Vec<Op> = Vec::new();

    for token in tokens {
        match &token.ty {
            TokenType::Intrinsic(v) => ops.push(get_intrinsic_op(v, token)),
            TokenType::Literal(v) => ops.push(get_literal_op(v, token)),
        }
    }

    ops
}

fn get_intrinsic_op(intrinsic: &Intrinsic, token: &Token) -> Op {
    match intrinsic {
        Intrinsic::Syscall3 => Op::new(
            OP_COUNTER.fetch_add(),
            OpType::Intrinsic(intrinsic.clone()),
            token,
        ),
    }
}

fn get_literal_op(literal: &Literal, token: &Token) -> Op {
    match literal {
        Literal::Integer(_) => Op::new(OP_COUNTER.fetch_add(), OpType::PushInt, token),
        Literal::String(_) => Op::new(OP_COUNTER.fetch_add(), OpType::PushStr, token),
    }
}
