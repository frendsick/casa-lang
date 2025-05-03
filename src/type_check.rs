use crate::defs::{
    Function, IdentifierTable, Intrinsic, Op, OpType, Parameter, ParameterSlice, Segment,
};

#[derive(Debug)]
pub enum TypeCheckError {
    InvalidSignature,
    StackUnderflow,
}

pub fn type_check_program(
    segments: &[Segment],
    global_identifiers: &IdentifierTable,
) -> Result<(), TypeCheckError> {
    for segment in segments {
        match segment {
            Segment::Function(f) => type_check_function(f, global_identifiers)?,
        }
    }

    Ok(())
}

fn type_check_function(
    function: &Function,
    _global_identifiers: &IdentifierTable,
) -> Result<(), TypeCheckError> {
    let mut type_stack = function.signature.params.get_types();

    for op in &function.ops {
        dbg!(&op);
        match &op.ty {
            OpType::FunctionEpilogue => {}
            OpType::FunctionPrologue => {}
            OpType::Intrinsic(intrinsic) => type_check_intrinsic(&mut type_stack, &intrinsic)?,
            OpType::PushInt => type_stack.push("int".to_string()),
            OpType::PushStr => type_stack.push("str".to_string()),
            _ => todo!(),
        }
    }

    if type_stack != function.signature.returns {
        return Err(TypeCheckError::InvalidSignature);
    }

    Ok(())
}

fn type_check_intrinsic(
    type_stack: &mut Vec<String>,
    intrinsic: &Intrinsic,
) -> Result<(), TypeCheckError> {
    match intrinsic {
        Intrinsic::Drop => pop(type_stack).map(|_| ()),
        _ => todo!(),
    }
}

fn pop(type_stack: &mut Vec<String>) -> Result<String, TypeCheckError> {
    type_stack.pop().ok_or(TypeCheckError::StackUnderflow)
}
