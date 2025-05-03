use crate::defs::{Function, IdentifierTable, Intrinsic, OpType, ParameterSlice, Segment};

#[derive(Debug)]
pub enum TypeCheckError {
    InvalidSignature,
    StackUnderflow,
    ValueError,
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
        Intrinsic::And => type_check_boolean_operator(type_stack),
        Intrinsic::Add => type_check_arithmetic(type_stack),
        Intrinsic::Drop => pop(type_stack).map(|_| ()),
        Intrinsic::Dup => type_check_dup(type_stack),
        Intrinsic::Div => type_check_arithmetic(type_stack),
        Intrinsic::Eq => type_check_comparison_operator(type_stack),
        Intrinsic::Ge => type_check_comparison_operator(type_stack),
        Intrinsic::Gt => type_check_comparison_operator(type_stack),
        Intrinsic::Le => type_check_comparison_operator(type_stack),
        Intrinsic::Lt => type_check_comparison_operator(type_stack),
        Intrinsic::Mod => type_check_arithmetic(type_stack),
        Intrinsic::Mul => type_check_arithmetic(type_stack),
        Intrinsic::Ne => type_check_comparison_operator(type_stack),
        Intrinsic::Or => type_check_boolean_operator(type_stack),
        Intrinsic::Sub => type_check_arithmetic(type_stack),
        _ => todo!(),
    }
}

fn pop(type_stack: &mut Vec<String>) -> Result<String, TypeCheckError> {
    type_stack.pop().ok_or(TypeCheckError::StackUnderflow)
}

fn pop_type(type_stack: &mut Vec<String>, expected_type: &str) -> Result<(), TypeCheckError> {
    let ty = pop(type_stack)?;
    (&ty == expected_type)
        .then(|| ())
        .ok_or(TypeCheckError::ValueError)
}

fn peek(type_stack: &[String]) -> Result<&str, TypeCheckError> {
    type_stack
        .last()
        .map(String::as_str)
        .ok_or(TypeCheckError::StackUnderflow)
}

fn peek_type(type_stack: &[String], expected_type: &str) -> Result<(), TypeCheckError> {
    let ty = peek(type_stack)?;
    (ty == expected_type)
        .then(|| ())
        .ok_or(TypeCheckError::ValueError)
}

fn type_check_arithmetic(type_stack: &mut Vec<String>) -> Result<(), TypeCheckError> {
    pop_type(type_stack, "int")?;
    peek_type(type_stack, "int")?;
    Ok(())
}

fn type_check_boolean_operator(type_stack: &mut Vec<String>) -> Result<(), TypeCheckError> {
    pop_type(type_stack, "bool")?;
    peek_type(type_stack, "bool")?;
    Ok(())
}

fn type_check_comparison_operator(type_stack: &mut Vec<String>) -> Result<(), TypeCheckError> {
    pop_type(type_stack, "int")?;
    pop_type(type_stack, "int")?;
    type_stack.push("bool".to_string());
    Ok(())
}

fn type_check_dup(type_stack: &mut Vec<String>) -> Result<(), TypeCheckError> {
    let ty = peek(&type_stack)?;
    type_stack.push(ty.to_string());
    Ok(())
}
