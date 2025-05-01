use crate::defs::{Intrinsic, Op, OpType};

pub fn generate_assembly_code(ops: &[Op]) -> String {
    let mut asm_blocks = Vec::new();

    asm_blocks.push(get_asm_bss_section().to_string());
    asm_blocks.push(get_asm_text_section(ops));
    asm_blocks.push(get_asm_data_section(ops));

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

fn get_asm_text_section(ops: &[Op]) -> String {
    let mut asm_blocks = Vec::new();

    let header = ".section .text
.globl _start
_start:
movq %rsp, (args_ptr)
leaq return_stack(%rip), %r14
";
    asm_blocks.push(header.to_string());

    for op in ops {
        asm_blocks.push(get_asm_comment_for_op(op));
        asm_blocks.push(get_asm_code_for_op(op));
    }

    let exit_asm = "movq $60, %rax
popq %rdi
syscall
ret";
    asm_blocks.push(exit_asm.to_string());

    asm_blocks.join("\n")
}

fn get_asm_data_section(ops: &[Op]) -> String {
    let mut asm_blocks = Vec::new();

    let header = ".section .data";
    asm_blocks.push(header.to_string());

    for op in ops {
        match op.ty {
            OpType::PushStr => {
                let string_variable = get_asm_string_variable(op);
                asm_blocks.push(string_variable);
            }
            _ => {}
        }
    }

    asm_blocks.join("\n")
}

fn get_asm_comment_for_op(op: &Op) -> String {
    let loc = &op.token.location;
    let file_name = loc.file.file_name().unwrap();
    format!(
        "# {:?} | File: {:?}, Row: {}, Column: {}",
        op.ty, file_name, loc.row, loc.col
    )
}

fn get_asm_string_variable(op: &Op) -> String {
    format!(
        "s{}:
    .asciz {}",
        op.id, op.token.value
    )
}

fn get_asm_code_for_op(op: &Op) -> String {
    match &op.ty {
        OpType::Intrinsic(intrinsic) => get_asm_intrinsic(intrinsic),
        OpType::PushInt => get_asm_push_int(op),
        OpType::PushStr => get_asm_push_str(op),
    }
}

fn get_asm_intrinsic(intrinsic: &Intrinsic) -> String {
    match intrinsic {
        Intrinsic::Add => get_asm_add().to_string(),
        Intrinsic::Syscall3 => get_asm_syscall(3),
    }
}

fn get_asm_push_int(op: &Op) -> String {
    format!(
        "movabs ${}, %rax
pushq %rax",
        &op.token.value
    )
}

fn get_asm_push_str(op: &Op) -> String {
    format!(
        "leaq s{}(%rip), %rsi
pushq %rsi",
        op.id
    )
    .to_string()
}

fn get_asm_add() -> &'static str {
    "popq %rax
add %rax, (%rsp)"
}

fn get_asm_syscall(argc: usize) -> String {
    let mut asm = "popq %rax".to_string();
    let argument_registers = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
    for (i, register) in argument_registers.iter().enumerate() {
        if argc <= i {
            break;
        }
        asm.push_str(&format!("\npopq %{}", register));
    }
    asm.push_str("\nsyscall");
    asm
}
