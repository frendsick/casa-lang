use crate::common::{
    Ansi, Function, GLOBAL_IDENTIFIERS, Identifier, Intrinsic, Literal, Location, Op, OpType,
    ParameterSlice, Segment,
};
use crate::error::{CasaError, fatal_error};
use indexmap::IndexMap;
use std::fmt;
use strum_macros::Display;

#[derive(Debug, Clone)]
struct TypeNode {
    ty: String,
    location: Location,
}

#[derive(Debug, Clone)]
enum PopError {
    EmptyStack,
    WrongType(String),
}

trait TypeStack {
    fn from_types(types: &[String], location: &Location) -> Vec<TypeNode>;
    fn peek_nth(&self, n: usize) -> Option<&TypeNode>;
    fn peek_stack(&self) -> Option<&TypeNode>;
    fn peek_type(&self, expected_type: &str) -> Result<&TypeNode, PopError>;
    fn pop_stack(&mut self) -> Option<TypeNode>;
    fn pop_type(&mut self, expected_type: &str) -> Result<TypeNode, PopError>;
    fn push_node(&mut self, node: &TypeNode);
    fn push_type(&mut self, ty: &str, location: &Location);
}

impl TypeStack for Vec<TypeNode> {
    fn from_types(types: &[String], location: &Location) -> Vec<TypeNode> {
        types
            .iter()
            .map(|ty| TypeNode {
                ty: ty.to_string(),
                location: location.clone(),
            })
            .collect()
    }

    fn peek_nth(&self, n: usize) -> Option<&TypeNode> {
        self.iter().rev().nth(n)
    }

    fn peek_stack(&self) -> Option<&TypeNode> {
        self.last()
    }

    fn peek_type(&self, expected_type: &str) -> Result<&TypeNode, PopError> {
        match self.peek_stack() {
            Some(node) if node.ty == expected_type => Ok(node),
            Some(node) => Err(PopError::WrongType(node.ty.clone())),
            None => Err(PopError::EmptyStack),
        }
    }

    fn pop_stack(&mut self) -> Option<TypeNode> {
        self.pop()
    }

    fn pop_type(&mut self, expected_type: &str) -> Result<TypeNode, PopError> {
        match self.pop_stack() {
            Some(node) if node.ty == expected_type => Ok(node),
            Some(node) => Err(PopError::WrongType(node.ty)),
            None => Err(PopError::EmptyStack),
        }
    }

    fn push_node(&mut self, node: &TypeNode) {
        self.push(node.clone())
    }

    fn push_type(&mut self, ty: &str, location: &Location) {
        self.push(TypeNode {
            ty: ty.to_string(),
            location: location.clone(),
        })
    }
}

struct TypeStackSlice<'a>(&'a [TypeNode]);

impl fmt::Display for TypeStackSlice<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.is_empty() {
            f.write_str("[empty]")?;
        } else {
            for (i, node) in self.0.iter().rev().enumerate() {
                if i > 0 {
                    f.write_str("\n")?;
                }
                write!(f, "[{}] {} ({})", i + 1, node.ty, node.location)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Display, PartialEq, Eq)]
enum BranchType {
    IfBlock,
    WhileLoop,
}

#[derive(Debug)]
struct BranchedStack {
    ty: BranchType,
    stack_before: Vec<TypeNode>,
    stack_after: Vec<TypeNode>,
}

impl BranchedStack {
    fn new(ty: BranchType, type_stack: &[TypeNode]) -> Self {
        Self {
            ty,
            stack_before: type_stack.to_vec(),
            stack_after: type_stack.to_vec(),
        }
    }
}

pub fn type_check_program(segments: &[Segment]) {
    for segment in segments {
        match segment {
            Segment::Function(f) => {
                type_check_function(&f, segments);
            }
            Segment::Constant(_) | Segment::Include(_) => {}
        }
    }
}

fn type_check_function(function: &Function, segments: &[Segment]) {
    let params = &function.signature.params;
    let param_types_rev: Vec<String> = params.get_types().iter().rev().cloned().collect();
    let mut type_stack = Vec::from_types(&param_types_rev, &function.location);
    let mut branched_stacks: Vec<BranchedStack> = Vec::new();
    let mut variables: IndexMap<String, String> = IndexMap::new();
    let mut peek_index: usize = 0;

    // Type check ops
    for op in &function.ops {
        match &op.ty {
            OpType::Bind => peek_index = 0,
            OpType::Cast(ty) => type_check_cast(op, &mut type_stack, ty),
            OpType::Break => type_check_break(op, &type_stack, &branched_stacks),
            OpType::Continue => type_check_continue(op, &type_stack, &branched_stacks),
            OpType::Do => type_check_do(op, &mut type_stack, &branched_stacks),
            OpType::Done => {
                type_check_done(op, &type_stack, &branched_stacks);
                assert!(branched_stacks.pop().is_some());
            }
            OpType::Elif => type_check_elif(op, &mut type_stack, &mut branched_stacks),
            OpType::Else => type_check_else(op, &mut type_stack, &mut branched_stacks),
            OpType::Fi => {
                type_check_fi(op, &type_stack, &branched_stacks);
                assert!(branched_stacks.pop().is_some());
            }
            OpType::FunctionEpilogue => {}
            OpType::FunctionPrologue => {}
            OpType::Identifier => {
                // Global identifiers
                let global_identifiers = GLOBAL_IDENTIFIERS.get().unwrap();
                match global_identifiers.get(&op.token.value) {
                    Some(Identifier::Constant(c)) => match &c.literal {
                        Literal::Boolean(b) => type_stack.push_type("bool", &op.token.location),
                        Literal::Integer(i) => type_stack.push_type("int", &op.token.location),
                        Literal::String(s) => type_stack.push_type("str", &op.token.location),
                    },
                    Some(Identifier::Function(f)) => {
                        type_check_function_call(op, &mut type_stack, f);

                        // Set function as used
                        for segment in segments {
                            match segment {
                                Segment::Function(other_f) if f.name == other_f.name => {
                                    let mut is_used = other_f.is_used.write().unwrap();
                                    *is_used = true;
                                }
                                _ => {}
                            }
                        }
                    }
                    None => match variables.get(&op.token.value) {
                        Some(ty) => type_stack.push_type(ty, &op.token.location),
                        None => fatal_error(
                            &op.token.location,
                            CasaError::UnknownIdentifier,
                            &format!("Unknown identifier: {:?}", op),
                        ),
                    },
                }
            }
            OpType::If => {
                branched_stacks.push(BranchedStack::new(BranchType::IfBlock, &type_stack))
            }
            OpType::Intrinsic(intrinsic) => type_check_intrinsic(op, &mut type_stack, intrinsic),
            OpType::Peek => {}
            OpType::PeekBind => {
                type_check_peek_bind(op, &mut type_stack, &mut variables, peek_index);
                peek_index += 1;
            }
            OpType::PushBool => type_stack.push_type("bool", &op.token.location),
            OpType::PushInt => type_stack.push_type("int", &op.token.location),
            OpType::PushStr => type_stack.push_type("str", &op.token.location),
            OpType::Return => type_check_return(op, &type_stack, function),
            OpType::Take => {}
            OpType::TakeBind => type_check_take_bind(op, &mut type_stack, &mut variables),
            OpType::Then => type_check_then(op, &mut type_stack, &branched_stacks),
            OpType::While => {
                branched_stacks.push(BranchedStack::new(BranchType::WhileLoop, &type_stack))
            }
        }
    }

    // Verify that all branch blocks are closed
    if !branched_stacks.is_empty() {
        dbg!(&branched_stacks);
        fatal_error(
            &function.ops.last().unwrap().token.location,
            CasaError::SyntaxError,
            "Some branching blocks were not closed",
        )
    }

    // Verify that stack matches the function's return types
    let return_stack = Vec::from_types(&function.signature.return_types, &function.location);
    if !matching_stacks(&type_stack, &return_stack) {
        fatal_error(
            &function.location,
            CasaError::InvalidSignature,
            &format!(
                "Function '{}' has invalid signature

Signature: {}

Stack state at the end of the function:
{}",
                function.name,
                function.signature,
                TypeStackSlice(&type_stack),
            ),
        )
    }
}

fn type_check_stack_state(
    op: &Op,
    type_stack: &[TypeNode],
    branched_stacks: &[BranchedStack],
    expected_branch_type: BranchType,
) {
    match branched_stacks.last() {
        Some(stack) if stack.ty == expected_branch_type => {
            if !matching_stacks(type_stack, &stack.stack_after) {
                fatal_error(
                    &op.token.location,
                    CasaError::BranchModifiedStack,
                    &format!(
                        "The branch state was changed from the beginning of the {expected_branch_type}"
                    ),
                )
            }
        }
        Some(stack) => fatal_error(
            &op.token.location,
            CasaError::SyntaxError,
            &format!(
                "The '{}' keyword should be used in {} but got {}",
                op.token.value, expected_branch_type, stack.ty
            ),
        ),
        None => fatal_error(
            &op.token.location,
            CasaError::SyntaxError,
            &format!(
                "The '{}' keyword is used outside of {}",
                op.token.value, expected_branch_type
            ),
        ),
    }
}

fn type_check_break(op: &Op, type_stack: &[TypeNode], branched_stacks: &[BranchedStack]) {
    type_check_stack_state(op, type_stack, branched_stacks, BranchType::WhileLoop);
}

fn type_check_cast(op: &Op, type_stack: &mut Vec<TypeNode>, casted_type: &str) {
    let old_node = match type_stack.pop() {
        Some(node) => node,
        None => fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            "Cannot cast value from an empty stack",
        ),
    };

    type_stack.push_type(casted_type, &old_node.location);
}

fn type_check_continue(op: &Op, type_stack: &[TypeNode], branched_stacks: &[BranchedStack]) {
    type_check_stack_state(op, type_stack, branched_stacks, BranchType::WhileLoop);
}

fn type_check_do(op: &Op, type_stack: &mut Vec<TypeNode>, branched_stacks: &[BranchedStack]) {
    match type_stack.pop_type("bool") {
        Ok(_) => {}
        Err(PopError::EmptyStack) => fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!(
                "The '{}' keyword expects bool but the stack is empty",
                op.token.value
            ),
        ),
        Err(PopError::WrongType(ty)) => fatal_error(
            &op.token.location,
            CasaError::ValueError,
            &format!("Expected 'bool' but got '{}'", ty),
        ),
    }
    type_check_stack_state(op, type_stack, branched_stacks, BranchType::WhileLoop);
}

fn type_check_done(op: &Op, type_stack: &[TypeNode], branched_stacks: &[BranchedStack]) {
    type_check_stack_state(op, type_stack, branched_stacks, BranchType::WhileLoop);
}

fn type_check_elif(
    op: &Op,
    type_stack: &mut Vec<TypeNode>,
    branched_stacks: &mut Vec<BranchedStack>,
) {
    let original_branched_stack = match branched_stacks.last_mut() {
        Some(stack) => stack,
        None => fatal_error(
            &op.token.location,
            CasaError::SyntaxError,
            &format!(
                "The '{}' keyword is used outside of {}",
                op.token.value,
                BranchType::IfBlock,
            ),
        ),
    };
    *type_stack = original_branched_stack.stack_before.clone();
}

fn type_check_else(
    op: &Op,
    type_stack: &mut Vec<TypeNode>,
    branched_stacks: &mut Vec<BranchedStack>,
) {
    let original_branched_stack = match branched_stacks.last_mut() {
        Some(stack) => stack,
        None => fatal_error(
            &op.token.location,
            CasaError::SyntaxError,
            &format!(
                "The '{}' keyword is used outside of {}",
                op.token.value,
                BranchType::IfBlock,
            ),
        ),
    };
    if !matching_stacks(type_stack, &original_branched_stack.stack_before) {
        original_branched_stack.stack_after = type_stack.to_vec();
        *type_stack = original_branched_stack.stack_before.clone();
    }
}

fn type_check_fi(op: &Op, type_stack: &[TypeNode], branched_stacks: &[BranchedStack]) {
    type_check_stack_state(op, type_stack, branched_stacks, BranchType::IfBlock);
}

fn type_check_function_call(op: &Op, type_stack: &mut Vec<TypeNode>, function: &Function) {
    let function_name = &op.token.value;
    let global_identifiers = GLOBAL_IDENTIFIERS.get().unwrap();
    let function = match global_identifiers.get(function_name) {
        Some(Identifier::Function(function)) => function,
        _ => fatal_error(
            &op.token.location,
            CasaError::UnknownIdentifier,
            &format!("Function '{function_name}' was not found from the global identifiers"),
        ),
    };

    if type_stack.len() < function.signature.params.len() {
        fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!(
                "Function '{}' takes {} values as parameters but there is only {} values in the stack

Signature: {}",
                function.name,
                function.signature.params.len(),
                type_stack.len(),
                function.signature,
            ),
        )
    }

    for param in &function.signature.params {
        type_stack.pop_type(&param.ty).unwrap();
    }
    for return_type in &function.signature.return_types {
        type_stack.push_type(return_type, &op.token.location);
    }
}

fn type_check_push_bind(
    op: &Op,
    type_stack: &mut Vec<TypeNode>,
    variables: &IndexMap<String, String>,
) {
    let variable_name = &op.token.value;
    let ty = match variables.get(variable_name) {
        Some(ty) => ty,
        None => fatal_error(
            &op.token.location,
            CasaError::UnknownIdentifier,
            &format!("Variable '{variable_name}' is not defined"),
        ),
    };
    type_stack.push_type(ty, &op.token.location);
}

fn type_check_peek_bind(
    op: &Op,
    type_stack: &mut Vec<TypeNode>,
    variables: &mut IndexMap<String, String>,
    peek_index: usize,
) {
    let variable_name = &op.token.value;
    match type_stack.peek_nth(peek_index) {
        Some(node) => variables.insert(variable_name.to_string(), node.ty.clone()),
        None => fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!(
                "Cannot peek the {}. type from the stack with {} items into the variable '{}'",
                peek_index + 1,
                type_stack.len(),
                variable_name
            ),
        ),
    };
}

fn type_check_return(op: &Op, type_stack: &[TypeNode], function: &Function) {
    let return_stack = Vec::from_types(&function.signature.return_types, &op.token.location);
    if !matching_stacks(type_stack, &return_stack) {
        fatal_error(
            &function.location,
            CasaError::InvalidStackState,
            &format!(
                "Cannot return from '{}' function with invalid stack state

{}Hint{}: The stack should only contain the types defined by the function signature

Signature: {}

Stack state at the 'return' keyword:
{}",
                function.name,
                Ansi::Blue,
                Ansi::Reset,
                function.signature,
                TypeStackSlice(type_stack),
            ),
        )
    }
}

fn type_check_take_bind(
    op: &Op,
    type_stack: &mut Vec<TypeNode>,
    variables: &mut IndexMap<String, String>,
) {
    let variable_name = &op.token.value;
    match type_stack.pop_stack() {
        Some(node) => variables.insert(variable_name.to_string(), node.ty),
        None => fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!("Cannot take from empty stack into the variable '{variable_name}'"),
        ),
    };
}

fn type_check_then(op: &Op, type_stack: &mut Vec<TypeNode>, branched_stacks: &[BranchedStack]) {
    match type_stack.pop_type("bool") {
        Ok(_) => {}
        Err(PopError::EmptyStack) => fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!(
                "The '{}' keyword expects bool but the stack is empty",
                op.token.value
            ),
        ),
        Err(PopError::WrongType(ty)) => fatal_error(
            &op.token.location,
            CasaError::ValueError,
            &format!("Expected 'bool' but got '{}'", ty),
        ),
    }
    type_check_stack_state(op, type_stack, branched_stacks, BranchType::IfBlock);
}

fn type_check_intrinsic(op: &Op, type_stack: &mut Vec<TypeNode>, intrinsic: &Intrinsic) {
    match intrinsic {
        Intrinsic::And => type_check_boolean_operator(op, type_stack),
        Intrinsic::Add => type_check_arithmetic(op, type_stack),
        Intrinsic::Div => type_check_arithmetic(op, type_stack),
        Intrinsic::Drop => type_check_drop(op, type_stack),
        Intrinsic::Dup => type_check_dup(op, type_stack),
        Intrinsic::Eq => type_check_comparison_operator(op, type_stack),
        Intrinsic::Ge => type_check_comparison_operator(op, type_stack),
        Intrinsic::Gt => type_check_comparison_operator(op, type_stack),
        Intrinsic::Le => type_check_comparison_operator(op, type_stack),
        Intrinsic::LoadByte => type_check_load(op, type_stack),
        Intrinsic::LoadWord => type_check_load(op, type_stack),
        Intrinsic::LoadDword => type_check_load(op, type_stack),
        Intrinsic::LoadQword => type_check_load(op, type_stack),
        Intrinsic::Lt => type_check_comparison_operator(op, type_stack),
        Intrinsic::Mod => type_check_arithmetic(op, type_stack),
        Intrinsic::Mul => type_check_arithmetic(op, type_stack),
        Intrinsic::Ne => type_check_comparison_operator(op, type_stack),
        Intrinsic::Or => type_check_boolean_operator(op, type_stack),
        Intrinsic::Over => type_check_over(op, type_stack),
        Intrinsic::Rot => type_check_rot(op, type_stack),
        Intrinsic::Shl => type_check_bitshift(op, type_stack),
        Intrinsic::Shr => type_check_bitshift(op, type_stack),
        Intrinsic::StoreByte => type_check_store(op, type_stack),
        Intrinsic::StoreWord => type_check_store(op, type_stack),
        Intrinsic::StoreDword => type_check_store(op, type_stack),
        Intrinsic::StoreQword => type_check_store(op, type_stack),
        Intrinsic::Sub => type_check_arithmetic(op, type_stack),
        Intrinsic::Swap => type_check_swap(op, type_stack),
        Intrinsic::Syscall0 => type_check_syscall(op, type_stack, 0),
        Intrinsic::Syscall1 => type_check_syscall(op, type_stack, 1),
        Intrinsic::Syscall2 => type_check_syscall(op, type_stack, 2),
        Intrinsic::Syscall3 => type_check_syscall(op, type_stack, 3),
        Intrinsic::Syscall4 => type_check_syscall(op, type_stack, 4),
        Intrinsic::Syscall5 => type_check_syscall(op, type_stack, 5),
        Intrinsic::Syscall6 => type_check_syscall(op, type_stack, 6),
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
    true
}

fn type_check_arithmetic(op: &Op, type_stack: &mut Vec<TypeNode>) {
    let required_values = 2;
    if type_stack.len() < required_values {
        fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!(
                "The '{}' intrinsic requires {} values of type 'int' but the stack only has {} values",
                op.token.value,
                required_values,
                type_stack.len()
            ),
        )
    }

    type_stack.pop_type("int").unwrap();
    type_stack.pop_type("int").unwrap();
    type_stack.push_type("int", &op.token.location);
}

fn type_check_boolean_operator(op: &Op, type_stack: &mut Vec<TypeNode>) {
    let required_values = 2;
    if type_stack.len() < required_values {
        fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!(
                "The '{}' intrinsic requires {} values of type 'bool' but the stack only has {} values",
                op.token.value,
                required_values,
                type_stack.len()
            ),
        )
    }

    type_stack.pop_type("bool").unwrap();
    type_stack.pop_type("bool").unwrap();
    type_stack.push_type("bool", &op.token.location);
}

fn type_check_comparison_operator(op: &Op, type_stack: &mut Vec<TypeNode>) {
    let required_values = 2;
    if type_stack.len() < required_values {
        fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!(
                "The '{}' intrinsic requires {} values of type 'int' but the stack only has {} values",
                op.token.value,
                required_values,
                type_stack.len()
            ),
        )
    }

    type_stack.pop_type("int").unwrap();
    type_stack.pop_type("int").unwrap();
    type_stack.push_type("bool", &op.token.location);
}

fn type_check_drop(op: &Op, type_stack: &mut Vec<TypeNode>) {
    match type_stack.pop_stack() {
        Some(_) => {}
        None => fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            "Cannot drop value from empty stack",
        ),
    }
}

fn type_check_dup(op: &Op, type_stack: &mut Vec<TypeNode>) {
    if type_stack.is_empty() {
        fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            "Cannot duplicate value from empty stack",
        );
    }
    let node = type_stack.peek_stack().unwrap().clone();
    type_stack.push_type(&node.ty, &op.token.location);
}

fn type_check_load(op: &Op, type_stack: &mut Vec<TypeNode>) {
    match type_stack.pop_type("ptr") {
        Ok(_) => {}
        Err(PopError::EmptyStack) => fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!(
                "Cannot get 'ptr' for '{}' intrinsic from empty stack",
                op.token.value
            ),
        ),
        Err(PopError::WrongType(ty)) => fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!("Expected 'ptr' but got '{}'", ty),
        ),
    }
    type_stack.push_type("any", &op.token.location);
}

fn type_check_over(op: &Op, type_stack: &mut Vec<TypeNode>) {
    let required_values = 2;
    if type_stack.len() < required_values {
        fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!(
                "The '{}' intrinsic requires {} values but the stack only has {}",
                op.token.value,
                required_values,
                type_stack.len()
            ),
        )
    }

    let t1 = type_stack.pop_stack().unwrap();
    let t2 = type_stack.peek_stack().unwrap().clone();
    type_stack.push_node(&t1);
    type_stack.push_type(&t2.ty, &op.token.location);
}

fn type_check_rot(op: &Op, type_stack: &mut Vec<TypeNode>) {
    let required_values = 3;
    if type_stack.len() < required_values {
        fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!(
                "The '{}' intrinsic requires {} values but the stack only has {}",
                op.token.value,
                required_values,
                type_stack.len()
            ),
        )
    }

    let t1 = type_stack.pop_stack().unwrap();
    let t2 = type_stack.pop_stack().unwrap();
    let t3 = type_stack.pop_stack().unwrap();
    type_stack.push_node(&t2);
    type_stack.push_node(&t1);
    type_stack.push_node(&t3);
}

fn type_check_bitshift(op: &Op, type_stack: &mut Vec<TypeNode>) {
    let required_values = 2;
    if type_stack.len() < required_values {
        fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!(
                "The '{}' intrinsic requires {} values of type 'int' but the stack only has {} values",
                op.token.value,
                required_values,
                type_stack.len()
            ),
        )
    }

    type_stack.pop_type("int").unwrap();
    type_stack.pop_type("int").unwrap();
    type_stack.push_type("int", &op.token.location);
}

fn type_check_store(op: &Op, type_stack: &mut Vec<TypeNode>) {
    let required_values = 2;
    if type_stack.len() < required_values {
        fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!(
                "The '{}' intrinsic requires {} values but the stack only has {}",
                op.token.value,
                required_values,
                type_stack.len()
            ),
        )
    }

    match type_stack.pop_type("ptr") {
        Ok(_) => {}
        Err(PopError::EmptyStack) => unreachable!("The stack should have enough values"),
        Err(PopError::WrongType(ty)) => fatal_error(
            &op.token.location,
            CasaError::ValueError,
            &format!(
                "Expected 'ptr' but got '{}'

{}Hint{}: The first parameter of '{}' intrinsic should be a pointer to the memory location where a value will be stored",
                ty,
                Ansi::Blue,
                Ansi::Reset,
                op.token.value,
            ),
        ),
    }
    type_stack.pop_stack().unwrap();
}

fn type_check_swap(op: &Op, type_stack: &mut Vec<TypeNode>) {
    let required_values = 2;
    if type_stack.len() < required_values {
        fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!(
                "The '{}' intrinsic requires {} values but the stack only has {}",
                op.token.value,
                required_values,
                type_stack.len()
            ),
        )
    }

    let t1 = type_stack.pop_stack().unwrap();
    let t2 = type_stack.pop_stack().unwrap();
    type_stack.push_node(&t1);
    type_stack.push_node(&t2);
}

fn type_check_syscall(op: &Op, type_stack: &mut Vec<TypeNode>, argc: u8) {
    assert!(argc <= 6);
    let required_values = (argc + 1) as usize;
    if type_stack.len() < required_values {
        fatal_error(
            &op.token.location,
            CasaError::StackUnderflow,
            &format!(
                "The '{}' intrinsic requires {} values but the stack only has {}",
                op.token.value,
                required_values,
                type_stack.len()
            ),
        )
    }

    match type_stack.pop_type("int") {
        Ok(_) => {}
        Err(PopError::EmptyStack) => unreachable!("The stack should have enough values"),
        Err(PopError::WrongType(ty)) => fatal_error(
            &op.token.location,
            CasaError::ValueError,
            &format!(
                "Expected 'int' but got '{}'

{}Hint{}: The first parameter of '{}' intrinsic represents the syscall number",
                ty,
                Ansi::Blue,
                Ansi::Reset,
                op.token.value,
            ),
        ),
    }

    for _ in 0..argc {
        type_stack.pop_stack().unwrap();
    }

    // Return value of the syscall
    type_stack.push_type("int", &op.token.location);
}
