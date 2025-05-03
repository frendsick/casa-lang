use crate::defs::{
    Function, IdentifierTable, Intrinsic, Op, OpType, Parameter, ParameterSlice, Segment,
};

#[derive(Debug)]
pub enum TypeCheckError {
    InvalidSignature,
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
            OpType::PushInt => type_stack.push("int".to_string()),
            _ => todo!(),
        }
    }

    if type_stack != function.signature.returns {
        return Err(TypeCheckError::InvalidSignature);
    }

    Ok(())
}
