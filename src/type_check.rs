use indexmap::IndexMap;

use crate::defs::{
    Function, Identifier, IdentifierTable, Intrinsic, Location, Op, OpType, Parameter,
    ParameterSlice, Segment,
};

#[derive(Debug)]
pub enum TypeCheckError {
    BranchModifiedStack,
    InvalidSignature,
    StackUnderflow,
    SyntaxError,
    UnknownIdentifier,
    ValueError,
}

#[derive(Debug, Clone)]
struct TypeNode {
    ty: String,
    location: Location,
}

trait TypeStack {
    fn from(types: &[String], location: &Location) -> Vec<TypeNode>;
    fn peek_nth(&self, n: usize) -> Result<&TypeNode, TypeCheckError>;
    fn peek_stack(&self) -> Result<&TypeNode, TypeCheckError>;
    fn peek_type(&self, expected_type: &str) -> Result<(), TypeCheckError>;
    fn pop_stack(&mut self) -> Result<TypeNode, TypeCheckError>;
    fn pop_type(&mut self, expected_type: &str) -> Result<(), TypeCheckError>;
    fn push_node(&mut self, node: TypeNode);
    fn push_type(&mut self, ty: &str, location: &Location);
}

impl TypeStack for Vec<TypeNode> {
    fn from(types: &[String], location: &Location) -> Vec<TypeNode> {
        types
            .iter()
            .map(|ty| TypeNode {
                ty: ty.to_string(),
                location: location.clone(),
            })
            .collect()
    }

    fn peek_nth(&self, n: usize) -> Result<&TypeNode, TypeCheckError> {
        self.iter()
            .rev()
            .nth(n)
            .ok_or(TypeCheckError::StackUnderflow)
    }

    fn peek_stack(&self) -> Result<&TypeNode, TypeCheckError> {
        self.last().ok_or(TypeCheckError::StackUnderflow)
    }

    fn peek_type(&self, expected_type: &str) -> Result<(), TypeCheckError> {
        let node = self.peek_stack()?;
        (node.ty == expected_type)
            .then(|| ())
            .ok_or(TypeCheckError::ValueError)
    }

    fn pop_stack(&mut self) -> Result<TypeNode, TypeCheckError> {
        self.pop().ok_or(TypeCheckError::StackUnderflow)
    }

    fn pop_type(&mut self, expected_type: &str) -> Result<(), TypeCheckError> {
        let node = self.pop_stack()?;
        (node.ty == expected_type)
            .then(|| ())
            .ok_or(TypeCheckError::ValueError)
    }

    fn push_node(&mut self, node: TypeNode) {
        self.push(node)
    }

    fn push_type(&mut self, ty: &str, location: &Location) {
        self.push(TypeNode {
            ty: ty.to_string(),
            location: location.clone(),
        })
    }
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
    global_identifiers: &IdentifierTable,
) -> Result<(), TypeCheckError> {
    let mut type_stack = <Vec<TypeNode> as TypeStack>::from(
        &function.signature.params.get_types(),
        &function.location,
    );
    let return_stack =
        <Vec<TypeNode> as TypeStack>::from(&function.signature.returns, &function.location);

    let mut variables = IndexMap::new();
    let mut peek_index = 0;
    let mut op_index = 0;

    type_check_ops(
        &mut type_stack,
        &mut variables,
        &mut op_index,
        &mut peek_index,
        &function.ops,
        global_identifiers,
        None,
    )?;

    // Verify that stack matches the function's return types
    matching_stacks(&type_stack, &return_stack)
        .then(|| ())
        .ok_or(TypeCheckError::InvalidSignature)
}

fn type_check_ops(
    type_stack: &mut Vec<TypeNode>,
    variables: &mut IndexMap<String, String>,
    op_index: &mut usize,
    peek_index: &mut usize,
    ops: &[Op],
    global_identifiers: &IdentifierTable,
    stack_before_branch: Option<&[TypeNode]>,
) -> Result<(), TypeCheckError> {
    while let Some(op) = ops.get(*op_index) {
        dbg!(&op.token.value);
        *op_index += 1;
        match &op.ty {
            OpType::Bind => *peek_index = 0,
            OpType::Do => match stack_before_branch {
                Some(stack) => type_check_do(type_stack, stack)?,
                None => Err(TypeCheckError::SyntaxError)?,
            },
            OpType::Done => match stack_before_branch {
                Some(stack) => type_check_done(type_stack, stack)?,
                None => Err(TypeCheckError::SyntaxError)?,
            },
            OpType::Fi => match stack_before_branch {
                Some(stack) => type_check_fi(type_stack, stack)?,
                None => Err(TypeCheckError::SyntaxError)?,
            },
            OpType::FunctionCall | OpType::InlineFunctionCall => {
                match global_identifiers.get(&op.token.value) {
                    Some(Identifier::Function(function)) => {
                        type_check_function_call(type_stack, &op.token.location, &function)?;
                    }
                    _ => Err(TypeCheckError::UnknownIdentifier)?,
                }
            }
            OpType::FunctionEpilogue => {}
            OpType::FunctionPrologue => {}
            OpType::If => {}
            OpType::Intrinsic(intrinsic) => {
                type_check_intrinsic(type_stack, &op.token.location, &intrinsic)?
            }
            OpType::Peek => {}
            OpType::PeekBind => {
                type_check_peek_bind(type_stack, variables, &op.token.value, *peek_index)?;
                *peek_index += 1;
            }
            OpType::PushBind => {
                type_check_push_bind(type_stack, &variables, &op.token.location, &op.token.value)?
            }
            OpType::PushInt => type_stack.push_type("int", &op.token.location),
            OpType::PushStr => type_stack.push_type("str", &op.token.location),
            OpType::Take => {}
            OpType::TakeBind => type_check_take_bind(type_stack, variables, &op.token.value)?,
            OpType::Then => {
                type_stack.pop_type("bool")?;
                type_check_ops(
                    type_stack,
                    variables,
                    op_index,
                    peek_index,
                    ops,
                    global_identifiers,
                    Some(&type_stack.clone()),
                )?;
            }
            OpType::While => {
                type_check_ops(
                    type_stack,
                    variables,
                    op_index,
                    peek_index,
                    ops,
                    global_identifiers,
                    Some(&type_stack.clone()),
                )?;
            }
            // All unknown ops should be resolved before type checking
            OpType::Unknown => {
                dbg!(op);
                todo!()
            }
            _ => todo!(),
        }
    }
    Ok(())
}

fn type_check_do(
    type_stack: &mut Vec<TypeNode>,
    stack_before_while: &[TypeNode],
) -> Result<(), TypeCheckError> {
    type_stack.pop_type("bool")?;
    matching_stacks(type_stack, stack_before_while)
        .then(|| ())
        .ok_or(TypeCheckError::BranchModifiedStack)
}

fn type_check_done(
    type_stack: &[TypeNode],
    stack_before_while: &[TypeNode],
) -> Result<(), TypeCheckError> {
    matching_stacks(type_stack, stack_before_while)
        .then(|| ())
        .ok_or(TypeCheckError::BranchModifiedStack)
}

fn type_check_fi(
    type_stack: &[TypeNode],
    stack_before_while: &[TypeNode],
) -> Result<(), TypeCheckError> {
    matching_stacks(type_stack, stack_before_while)
        .then(|| ())
        .ok_or(TypeCheckError::BranchModifiedStack)
}

fn type_check_function_call(
    type_stack: &mut Vec<TypeNode>,
    location: &Location,
    function: &Function,
) -> Result<(), TypeCheckError> {
    for param in &function.signature.params {
        type_stack.pop_type(&param.ty)?;
    }
    for return_type in &function.signature.returns {
        type_stack.push_type(&return_type, location);
    }
    Ok(())
}

fn type_check_push_bind(
    type_stack: &mut Vec<TypeNode>,
    variables: &IndexMap<String, String>,
    location: &Location,
    variable_name: &str,
) -> Result<(), TypeCheckError> {
    let ty = match variables.get(variable_name) {
        Some(ty) => ty,
        None => Err(TypeCheckError::UnknownIdentifier)?,
    };
    type_stack.push_type(ty, location);
    Ok(())
}

fn type_check_peek_bind(
    type_stack: &mut Vec<TypeNode>,
    variables: &mut IndexMap<String, String>,
    variable_name: &str,
    peek_index: usize,
) -> Result<(), TypeCheckError> {
    let node = type_stack.peek_nth(peek_index)?;
    variables.insert(variable_name.to_string(), node.ty.clone());
    Ok(())
}

fn type_check_take_bind(
    type_stack: &mut Vec<TypeNode>,
    variables: &mut IndexMap<String, String>,
    variable_name: &str,
) -> Result<(), TypeCheckError> {
    let node = type_stack.pop_stack()?;
    variables.insert(variable_name.to_string(), node.ty);
    Ok(())
}

fn type_check_intrinsic(
    type_stack: &mut Vec<TypeNode>,
    location: &Location,
    intrinsic: &Intrinsic,
) -> Result<(), TypeCheckError> {
    match intrinsic {
        Intrinsic::And => type_check_boolean_operator(type_stack, location),
        Intrinsic::Add => type_check_arithmetic(type_stack, location),
        Intrinsic::Div => type_check_arithmetic(type_stack, location),
        Intrinsic::Drop => type_stack.pop_stack().map(|_| ()),
        Intrinsic::Dup => type_check_dup(type_stack, location),
        Intrinsic::Eq => type_check_comparison_operator(type_stack, location),
        Intrinsic::Ge => type_check_comparison_operator(type_stack, location),
        Intrinsic::Gt => type_check_comparison_operator(type_stack, location),
        Intrinsic::Le => type_check_comparison_operator(type_stack, location),
        Intrinsic::LoadByte => type_check_load(type_stack, location),
        Intrinsic::LoadWord => type_check_load(type_stack, location),
        Intrinsic::LoadDword => type_check_load(type_stack, location),
        Intrinsic::LoadQword => type_check_load(type_stack, location),
        Intrinsic::Lt => type_check_comparison_operator(type_stack, location),
        Intrinsic::Mod => type_check_arithmetic(type_stack, location),
        Intrinsic::Mul => type_check_arithmetic(type_stack, location),
        Intrinsic::Ne => type_check_comparison_operator(type_stack, location),
        Intrinsic::Or => type_check_boolean_operator(type_stack, location),
        Intrinsic::Over => type_check_over(type_stack, location),
        Intrinsic::Rot => type_check_rot(type_stack),
        Intrinsic::Shl => type_check_bitshift(type_stack, location),
        Intrinsic::Shr => type_check_bitshift(type_stack, location),
        Intrinsic::StoreByte => type_check_store(type_stack),
        Intrinsic::StoreWord => type_check_store(type_stack),
        Intrinsic::StoreDword => type_check_store(type_stack),
        Intrinsic::StoreQword => type_check_store(type_stack),
        Intrinsic::Sub => type_check_arithmetic(type_stack, location),
        Intrinsic::Swap => type_check_swap(type_stack),
        Intrinsic::Syscall0 => type_check_syscall(type_stack, location, 0),
        Intrinsic::Syscall1 => type_check_syscall(type_stack, location, 1),
        Intrinsic::Syscall2 => type_check_syscall(type_stack, location, 2),
        Intrinsic::Syscall3 => type_check_syscall(type_stack, location, 3),
        Intrinsic::Syscall4 => type_check_syscall(type_stack, location, 4),
        Intrinsic::Syscall5 => type_check_syscall(type_stack, location, 5),
        Intrinsic::Syscall6 => type_check_syscall(type_stack, location, 6),
    }
}

fn matching_stacks(stack1: &[TypeNode], stack2: &[TypeNode]) -> bool {
    if stack1.len() != stack2.len() {
        return false;
    }
    for (node1, node2) in stack1.iter().zip(stack2) {
        if node1.ty != node2.ty {
            return false;
        }
    }
    return true;
}

fn type_check_arithmetic(
    type_stack: &mut Vec<TypeNode>,
    location: &Location,
) -> Result<(), TypeCheckError> {
    type_stack.pop_type("int")?;
    type_stack.peek_type("int")?;
    Ok(())
}

fn type_check_boolean_operator(
    type_stack: &mut Vec<TypeNode>,
    location: &Location,
) -> Result<(), TypeCheckError> {
    type_stack.pop_type("bool")?;
    type_stack.peek_type("bool")?;
    Ok(())
}

fn type_check_comparison_operator(
    type_stack: &mut Vec<TypeNode>,
    location: &Location,
) -> Result<(), TypeCheckError> {
    type_stack.pop_type("int")?;
    type_stack.pop_type("int")?;
    type_stack.push_type("bool", location);
    Ok(())
}

fn type_check_dup(
    type_stack: &mut Vec<TypeNode>,
    location: &Location,
) -> Result<(), TypeCheckError> {
    let node = type_stack.peek_stack()?.clone();
    type_stack.push_type(&node.ty, location);
    Ok(())
}

fn type_check_load(
    type_stack: &mut Vec<TypeNode>,
    location: &Location,
) -> Result<(), TypeCheckError> {
    type_stack.pop_type("ptr")?;
    type_stack.push_type("any", location);
    Ok(())
}

fn type_check_over(
    type_stack: &mut Vec<TypeNode>,
    location: &Location,
) -> Result<(), TypeCheckError> {
    let t1 = type_stack.pop_stack()?.clone();
    let t2 = type_stack.peek_stack()?.clone();
    type_stack.push_node(t1);
    type_stack.push_node(t2);
    Ok(())
}

fn type_check_rot(type_stack: &mut Vec<TypeNode>) -> Result<(), TypeCheckError> {
    let t1 = type_stack.pop_stack()?;
    let t2 = type_stack.pop_stack()?;
    let t3 = type_stack.pop_stack()?;
    type_stack.push_node(t2);
    type_stack.push_node(t1);
    type_stack.push_node(t3);
    Ok(())
}

fn type_check_bitshift(
    type_stack: &mut Vec<TypeNode>,
    location: &Location,
) -> Result<(), TypeCheckError> {
    type_stack.pop_type("int")?;
    type_stack.pop_type("int")?;
    type_stack.push_type("int", location);
    Ok(())
}

fn type_check_store(type_stack: &mut Vec<TypeNode>) -> Result<(), TypeCheckError> {
    type_stack.pop_type("ptr")?;
    type_stack.pop_stack()?;
    Ok(())
}

fn type_check_swap(type_stack: &mut Vec<TypeNode>) -> Result<(), TypeCheckError> {
    let t1 = type_stack.pop_stack()?;
    let t2 = type_stack.pop_stack()?;
    type_stack.push_node(t1);
    type_stack.push_node(t2);
    Ok(())
}

fn type_check_syscall(
    type_stack: &mut Vec<TypeNode>,
    location: &Location,
    argc: u8,
) -> Result<(), TypeCheckError> {
    assert!(argc <= 6);

    let syscall = type_stack.pop_type("int")?;
    for _ in 0..argc {
        type_stack.pop_stack()?;
    }

    // Return value of the syscall
    type_stack.push_type("int", location);
    Ok(())
}
