use strum_macros::Display;

use crate::common::{
    Function, GLOBAL_IDENTIFIERS, Identifier, Intrinsic, Literal, Op, OpType, Segment, TokenType,
    get_related_done_id, get_related_fi_id, get_related_while_id,
};

pub fn generate_assembly_code(segments: &[Segment]) -> String {
    [
        get_asm_bss_section().to_string(),
        get_asm_text_section(segments),
        get_asm_data_section(segments),
    ]
    .join("\n\n")
}

fn get_asm_bss_section() -> &'static str {
    ".section .bss
    args_ptr: .skip 8
    arena_allocator: .skip 8*3
    return_stack: .skip 1337*64"
}

fn get_asm_text_section(segments: &[Segment]) -> String {
    let mut asm_blocks = Vec::new();

    let header = ".section .text
.globl _start";
    asm_blocks.push(header.to_string());

    let entry_function = "_start:
movq %rsp, (args_ptr)
leaq return_stack(%rip), %r14
call main
popq %rdi
movq $60, %rax
syscall
ret";
    asm_blocks.push(entry_function.to_string());

    for segment in segments {
        match segment {
            Segment::Function(f) => asm_blocks.push(get_asm_for_function(f)),
        }
    }

    asm_blocks.join("\n\n")
}

fn get_asm_for_function(function: &Function) -> String {
    format!(
        "{}:
{}",
        function.name,
        get_asm_for_function_ops(function)
    )
}

fn get_asm_for_function_ops(function: &Function) -> String {
    let mut asm_blocks = Vec::new();
    for op in &function.ops {
        asm_blocks.push(get_asm_comment_for_op(op, function));
        asm_blocks.push(get_asm_code_for_op(op, function));
    }

    asm_blocks.join("\n")
}

fn get_asm_data_section(segments: &[Segment]) -> String {
    let mut asm_blocks = Vec::new();
    let header = ".section .data";
    asm_blocks.push(header.to_string());

    for segment in segments {
        match segment {
            Segment::Function(function) => {
                asm_blocks.push(get_asm_data_section_entries_function(function))
            }
        }
    }

    asm_blocks.join("\n")
}

fn get_asm_data_section_entries_function(function: &Function) -> String {
    let mut asm_blocks = Vec::new();
    for op in &function.ops {
        if op.ty == OpType::PushStr {
            let string_variable = generate_asm_string_variable(op, function);
            asm_blocks.push(string_variable);
        }
    }
    asm_blocks.join("\n")
}

fn get_asm_comment_for_op(op: &Op, function: &Function) -> String {
    let loc = &op.token.location;
    let file_name = loc.file.file_name().unwrap();
    format!(
        "# [{}] {:?} | File: {:?}, Row: {}, Column: {}",
        function.name, op.ty, file_name, loc.row, loc.col
    )
}

fn generate_asm_string_variable(op: &Op, function: &Function) -> String {
    format!(
        "{}:
    .asciz {}",
        get_asm_string_variable_name(op, function),
        op.token.value
    )
}

fn get_asm_string_variable_name(op: &Op, function: &Function) -> String {
    format!("{}_s{}", function.name, op.id)
}

fn get_asm_code_for_op(op: &Op, function: &Function) -> String {
    match &op.ty {
        OpType::Bind => "".to_string(),
        OpType::Break => get_asm_break(op, function),
        OpType::Continue => get_asm_continue(op, function),
        OpType::Do => get_asm_do(op, function),
        OpType::Done => get_asm_done(op, function),
        OpType::Fi => get_asm_fi(op, function),
        OpType::FunctionCall => get_asm_function_call(&op.token.value),
        OpType::FunctionEpilogue => match function.is_inline {
            true => "".to_string(),
            false => get_asm_function_epilogue(function),
        },
        OpType::FunctionPrologue => match function.is_inline {
            true => "".to_string(),
            false => get_asm_function_prologue(function),
        },
        OpType::If => "".to_string(),
        OpType::InlineFunctionCall => {
            let global_identifiers = GLOBAL_IDENTIFIERS.get().unwrap();
            let called_function = match global_identifiers.get(&op.token.value) {
                Some(Identifier::Function(f)) => f,
                None => {
                    eprintln!("Unknown function identifier {}", op.token.value);
                    panic!()
                }
            };
            get_asm_for_function_ops(called_function)
        }
        OpType::Intrinsic(intrinsic) => get_asm_intrinsic(intrinsic),
        OpType::Peek => get_asm_peek().to_string(),
        OpType::PeekBind => get_asm_peek_bind(op, function),
        OpType::PushBind => get_asm_push_bind(op, function),
        OpType::PushBool => get_asm_push_bool(op),
        OpType::PushInt => get_asm_push_int(op),
        OpType::PushStr => get_asm_push_str(op, function),
        OpType::Return => get_asm_return(function),
        OpType::Take => "".to_string(),
        OpType::TakeBind => get_asm_take_bind(op, function),
        OpType::Then => get_asm_then(op, function),
        OpType::While => get_asm_while(op, function),
        // All unknown ops should be resolved before assembly generation
        OpType::Unknown => {
            dbg!(op);
            todo!()
        }
    }
}

fn get_asm_function_epilogue(function: &Function) -> String {
    let mut epilogue = String::new();

    // Implicitly return 0 from the `main` function without return types
    if function.name == "main" && function.signature.return_types.is_empty() {
        epilogue.push_str("pushq $0\n");
    }

    epilogue.push_str(&format!(
        "pushq (%r14)
subq ${}, %r14
ret",
        function.variables.len() * 8 + 8
    ));
    epilogue
}

fn get_asm_function_prologue(function: &Function) -> String {
    format!(
        "addq ${}, %r14
popq (%r14)",
        function.variables.len() * 8 + 8
    )
}

fn get_asm_intrinsic(intrinsic: &Intrinsic) -> String {
    match intrinsic {
        Intrinsic::Add => get_asm_add().to_string(),
        Intrinsic::And => get_asm_and().to_string(),
        Intrinsic::Div => get_asm_div().to_string(),
        Intrinsic::Drop => get_asm_drop().to_string(),
        Intrinsic::Dup => get_asm_dup().to_string(),
        Intrinsic::Eq => get_asm_comparison(SetOperand::Sete),
        Intrinsic::Ge => get_asm_comparison(SetOperand::Setge),
        Intrinsic::Gt => get_asm_comparison(SetOperand::Setg),
        Intrinsic::Le => get_asm_comparison(SetOperand::Setle),
        Intrinsic::Lt => get_asm_comparison(SetOperand::Setl),
        Intrinsic::Mod => get_asm_mod().to_string(),
        Intrinsic::Mul => get_asm_mul().to_string(),
        Intrinsic::Ne => get_asm_comparison(SetOperand::Setne),
        Intrinsic::Or => get_asm_or().to_string(),
        Intrinsic::Over => get_asm_over().to_string(),
        Intrinsic::Rot => get_asm_rot().to_string(),
        Intrinsic::Shl => get_asm_shl().to_string(),
        Intrinsic::Shr => get_asm_shr().to_string(),
        Intrinsic::Sub => get_asm_sub().to_string(),
        Intrinsic::Swap => get_asm_swap().to_string(),
        Intrinsic::Syscall0 => get_asm_syscall(0),
        Intrinsic::Syscall1 => get_asm_syscall(1),
        Intrinsic::Syscall2 => get_asm_syscall(2),
        Intrinsic::Syscall3 => get_asm_syscall(3),
        Intrinsic::Syscall4 => get_asm_syscall(4),
        Intrinsic::Syscall5 => get_asm_syscall(5),
        Intrinsic::Syscall6 => get_asm_syscall(6),
        _ => todo!(),
    }
}

fn get_asm_push_bind(op: &Op, function: &Function) -> String {
    let variable_index = match function.variables.get_index_of(&op.token.value) {
        Some(index) => index,
        None => {
            eprintln!("Variable does not exist: {}", op.token.value);
            panic!()
        }
    };

    format!("pushq -{}(%r14)", variable_index * 8 + 8)
}

fn get_asm_push_bool(op: &Op) -> String {
    match op.token.ty {
        TokenType::Literal(Literal::Boolean(boolean)) => format!(
            "mov ${}, %rax
pushq %rax",
            boolean as u8
        ),
        _ => unreachable!("Expected a boolean literal"),
    }
}

fn get_asm_push_int(op: &Op) -> String {
    format!(
        "movabs ${}, %rax
pushq %rax",
        &op.token.value
    )
}

fn get_asm_push_str(op: &Op, function: &Function) -> String {
    format!(
        "leaq {}(%rip), %rsi
pushq %rsi",
        get_asm_string_variable_name(op, function)
    )
    .to_string()
}

fn get_asm_fi(op: &Op, function: &Function) -> String {
    format!("{}_fi{}:", function.name, op.id)
}

fn get_asm_then(op: &Op, function: &Function) -> String {
    let related_id = match get_related_fi_id(op, function) {
        Some(id) => id,
        None => panic!("Related `fi` was not found"),
    };

    format!(
        "popq %rax
testq %rax, %rax
jz {}_fi{}",
        function.name, related_id
    )
}

fn get_asm_while(op: &Op, function: &Function) -> String {
    format!("{}_while{}:", function.name, op.id)
}

fn get_asm_do(op: &Op, function: &Function) -> String {
    let related_id = match get_related_done_id(op, function) {
        Some(id) => id,
        None => panic!("Related `done` was not found"),
    };

    format!(
        "popq %rax
testq %rax, %rax
jz {}_done{}",
        function.name, related_id
    )
}

fn get_asm_done(op: &Op, function: &Function) -> String {
    let related_id = match get_related_while_id(op, function) {
        Some(id) => id,
        None => panic!("Related `while` was not found"),
    };

    format!(
        "jmp {}_while{}
{}_done{}:",
        function.name, related_id, function.name, op.id
    )
}

fn get_asm_break(op: &Op, function: &Function) -> String {
    let related_id = match get_related_done_id(op, function) {
        Some(id) => id,
        None => panic!("Related `done` was not found"),
    };

    format!("jmp {}_done{}", function.name, related_id)
}

fn get_asm_continue(op: &Op, function: &Function) -> String {
    let related_id = match get_related_while_id(op, function) {
        Some(id) => id,
        None => panic!("Related `while` was not found"),
    };

    format!("jmp {}_while{}", function.name, related_id)
}

fn get_asm_peek() -> &'static str {
    "movq %rsp, %r15"
}

fn get_asm_peek_bind(op: &Op, function: &Function) -> String {
    let variable_index = match function.variables.get_index_of(&op.token.value) {
        Some(index) => index,
        None => {
            eprintln!("Variable does not exist: {}", op.token.value);
            panic!()
        }
    };
    format!(
        "popq %rbx
movq %rbx, -{}(%r14)",
        variable_index * 8 + 8
    )
}

fn get_asm_take_bind(op: &Op, function: &Function) -> String {
    let variable_index = match function.variables.get_index_of(&op.token.value) {
        Some(index) => index,
        None => {
            eprintln!("Variable does not exist: {}", op.token.value);
            panic!()
        }
    };
    format!(
        "popq %rbx
movq %rbx, -{}(%r14)",
        variable_index * 8 + 8
    )
}

fn get_asm_return(function: &Function) -> String {
    get_asm_function_epilogue(function)
}

fn get_asm_function_call(function_name: &str) -> String {
    format!("call {}", function_name)
}

fn get_asm_add() -> &'static str {
    "popq %rax
addq %rax, (%rsp)"
}

fn get_asm_and() -> &'static str {
    "popq %rax
andq %rax, (%rsp)"
}

#[derive(Debug, Display)]
#[strum(serialize_all = "lowercase")]
enum SetOperand {
    Sete,
    Setg,
    Setge,
    Setl,
    Setle,
    Setne,
}

fn get_asm_comparison(set_operand: SetOperand) -> String {
    format!(
        "popq %rax
cmpq %rax, (%rsp)
{set_operand} %al
movzbq %al, %rax
movq %rax, (%rsp)"
    )
}

fn get_asm_div() -> &'static str {
    "xor %edx, %edx
popq %rbx
popq %rax
divq %rbx
pushq %rax"
}

fn get_asm_drop() -> &'static str {
    "popq %rax"
}

fn get_asm_dup() -> &'static str {
    "pushq (%rsp)"
}

fn get_asm_mod() -> &'static str {
    "xor %edx, %edx
popq %rbx
popq %rax
divq %rbx
pushq %rdx"
}

fn get_asm_mul() -> &'static str {
    "popq %rax
popq %rbx
mulq %rbx
pushq %rax"
}

fn get_asm_or() -> &'static str {
    "popq %rax
orq %rax, (%rsp)"
}

fn get_asm_over() -> &'static str {
    "pushq 8(%rsp)"
}

fn get_asm_rot() -> &'static str {
    "popq %rax
popq %rbx
popq %rcx
pushq %rbx
pushq %rax
pushq %rcx"
}

fn get_asm_shl() -> &'static str {
    "popq %rcx
shlq %cl, (%rsp)"
}

fn get_asm_shr() -> &'static str {
    "popq %rcx
shrq %cl, (%rsp)"
}

fn get_asm_sub() -> &'static str {
    "popq %rax
subq %rax, (%rsp)"
}

fn get_asm_swap() -> &'static str {
    "popq %rax
pushq (%rsp)
movq %rax, 8(%rsp)"
}

fn get_asm_syscall(argc: u8) -> String {
    let mut asm = "popq %rax".to_string();
    let argument_registers = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
    for (i, register) in argument_registers.iter().enumerate() {
        if usize::from(argc) <= i {
            break;
        }
        asm.push_str(&format!("\npopq %{}", register));
    }
    asm.push_str("\nsyscall");
    asm.push_str("\npushq %rax");
    asm
}
