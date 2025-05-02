use crate::defs::{Function, Intrinsic, Op, OpType, Segment};

pub fn generate_assembly_code(segments: &[Segment]) -> String {
    let mut asm_blocks = Vec::new();

    asm_blocks.push(get_asm_bss_section().to_string());
    asm_blocks.push(get_asm_text_section(segments));
    asm_blocks.push(get_asm_data_section(segments));

    let mut assembly_code = asm_blocks.join("\n\n");
    assembly_code.push('\n');
    assembly_code
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

    for segment in segments {
        match segment {
            Segment::Function(function) => asm_blocks.push(get_asm_for_function(function)),
        }
    }

    asm_blocks.join("\n\n")
}

fn get_asm_for_function(function: &Function) -> String {
    let mut asm_blocks = Vec::new();
    let label = get_asm_function_label(function);
    asm_blocks.push(format!("{}:", label));

    for op in &function.ops {
        asm_blocks.push(get_asm_comment_for_op(&op, function));
        asm_blocks.push(get_asm_code_for_op(&op, function));
    }

    asm_blocks.join("\n")
}

fn get_asm_function_label(function: &Function) -> String {
    if function.name == "main" {
        "_start".to_string()
    } else {
        function.name.clone()
    }
}

fn get_asm_data_section(segments: &[Segment]) -> String {
    let mut asm_blocks = Vec::new();
    let header = ".section .data";
    asm_blocks.push(header.to_string());

    for segment in segments {
        match segment {
            Segment::Function(function) => {
                asm_blocks.push(get_asm_data_section_entries_function(&function))
            }
        }
    }

    asm_blocks.join("\n")
}

fn get_asm_data_section_entries_function(function: &Function) -> String {
    let mut asm_blocks = Vec::new();
    for op in &function.ops {
        match op.ty {
            OpType::PushStr => {
                let string_variable = generate_asm_string_variable(&op, function);
                asm_blocks.push(string_variable);
            }
            _ => {}
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
        OpType::Do => get_asm_do(op, function),
        OpType::Done => get_asm_done(op, function),
        OpType::Fi => get_asm_fi(op, function),
        OpType::FunctionCall => get_asm_function_call(&op.token.value),
        OpType::FunctionEpilogue => get_asm_function_epilogue(function).to_string(),
        OpType::FunctionPrologue => get_asm_function_prologue(function).to_string(),
        OpType::If => "".to_string(),
        OpType::Intrinsic(intrinsic) => get_asm_intrinsic(intrinsic),
        OpType::PushInt => get_asm_push_int(op),
        OpType::PushStr => get_asm_push_str(op, function),
        OpType::Return => get_asm_return(function).to_string(),
        OpType::Then => get_asm_then(op, function),
        OpType::While => get_asm_while(op, function),
        // All unknown ops should be resolved before assembly generation
        OpType::Unknown => {
            dbg!(op);
            todo!()
        }
        _ => todo!(),
    }
}

fn get_asm_function_epilogue(function: &Function) -> &'static str {
    if function.name == "main" {
        "movq $0, %rdi
movq $60, %rax
syscall
ret"
    } else {
        // TODO: Deallocate variables from stack
        "pushq (%r14)
subq $8, %r14
ret"
    }
}

fn get_asm_function_prologue(function: &Function) -> &'static str {
    if function.name == "main" {
        "movq %rsp, (args_ptr)
leaq return_stack(%rip), %r14
addq $0, %r14"
    } else {
        // TODO: Allocate room for variables to the stack
        "addq $8, %r14
popq (%r14)"
    }
}

fn get_asm_intrinsic(intrinsic: &Intrinsic) -> String {
    match intrinsic {
        Intrinsic::Add => get_asm_add().to_string(),
        Intrinsic::And => get_asm_and().to_string(),
        Intrinsic::Div => get_asm_div().to_string(),
        Intrinsic::Drop => get_asm_drop().to_string(),
        Intrinsic::Dup => get_asm_dup().to_string(),
        Intrinsic::Sub => get_asm_sub().to_string(),
        Intrinsic::Mod => get_asm_mod().to_string(),
        Intrinsic::Mul => get_asm_mul().to_string(),
        Intrinsic::Or => get_asm_or().to_string(),
        Intrinsic::Over => get_asm_over().to_string(),
        Intrinsic::Rot => get_asm_rot().to_string(),
        Intrinsic::Shl => get_asm_shl().to_string(),
        Intrinsic::Shr => get_asm_shr().to_string(),
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

fn get_related_fi_id(op: &Op, function: &Function) -> Option<usize> {
    assert!(op.ty == OpType::Then);

    let mut nested_ifs = 0;
    for other_op in function.ops.iter().skip_while(|x| op.id >= x.id) {
        match other_op.ty {
            OpType::Fi if nested_ifs == 0 => return Some(other_op.id),
            OpType::Fi => nested_ifs -= 1,
            OpType::If => nested_ifs += 1,
            _ => {}
        }
    }
    None
}

fn get_related_done_id(op: &Op, function: &Function) -> Option<usize> {
    assert!(op.ty == OpType::Do);

    let mut nested_whiles = 0;
    for other_op in function.ops.iter().skip_while(|x| op.id >= x.id) {
        match other_op.ty {
            OpType::Done if nested_whiles == 0 => return Some(other_op.id),
            OpType::Done => nested_whiles -= 1,
            OpType::While => nested_whiles += 1,
            _ => {}
        }
    }
    None
}

fn get_related_while_id(op: &Op, function: &Function) -> Option<usize> {
    assert!(op.ty == OpType::Done);

    let mut nested_whiles = 0;
    for other_op in function.ops.iter().rev().skip_while(|x| op.id <= x.id) {
        match other_op.ty {
            OpType::While if nested_whiles == 0 => return Some(other_op.id),
            OpType::Done => nested_whiles += 1,
            OpType::While => nested_whiles -= 1,
            _ => {}
        }
    }
    None
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

fn get_asm_return(function: &Function) -> &'static str {
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
