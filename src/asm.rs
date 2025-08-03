use crate::common::{
    DataSize, Function, GLOBAL_IDENTIFIERS, Identifier, Intrinsic, Literal, Op, OpType, Segment,
    TokenType, get_related_done_id, get_related_elif_id, get_related_else_id, get_related_fi_id,
    get_related_while_id,
};
use crate::error::{CasaError, fatal_error};
use crate::lexer::{normalize_identifier, quote};
use base64::Engine;
use base64::prelude::BASE64_STANDARD;
use itertools::Itertools;
use std::collections::HashMap;
use strum_macros::Display;

pub fn generate_assembly_code(segments: &[Segment]) -> String {
    let file_numbers = get_asm_file_numbers(segments);
    let file_directives: String = file_numbers
        .iter()
        .map(|(file, number)| format!(".file {} \"{}\"", number, file))
        .join("\n");

    [
        file_directives,
        get_asm_bss_section().to_string(),
        get_asm_data_section(segments),
        get_asm_text_section(segments, &file_numbers),
    ]
    .join("\n\n")
}

fn get_asm_bss_section() -> &'static str {
    ".section .bss
args_ptr: .skip 8
arena_allocator: .skip 8*3
return_stack: .skip 1337*64"
}

fn get_asm_text_section(segments: &[Segment], file_numbers: &HashMap<String, usize>) -> String {
    let mut asm_blocks = Vec::new();

    let header = ".section .text
.globl _start";
    asm_blocks.push(header.to_string());

    let main_label = get_asm_label_for_gas("main");
    let entry_function = format!(
        "_start:
movq %rsp, (args_ptr)
leaq return_stack(%rip), %r14
call {}
popq %rdi
movq $60, %rax
syscall
ret",
        main_label
    );
    asm_blocks.push(entry_function.to_string());

    for segment in segments {
        match segment {
            Segment::Function(f) if *f.is_used.read().unwrap() => {
                asm_blocks.push(get_asm_for_function(f, &file_numbers))
            }
            _ => {}
        }
    }

    asm_blocks.join("\n\n")
}

fn get_asm_label_for_gas(name: &str) -> String {
    BASE64_STANDARD
        .encode(name)
        .replace('=', "_")
        .replace('+', ".")
}

fn get_asm_file_numbers(segments: &[Segment]) -> HashMap<String, usize> {
    let mut file_numbers: HashMap<String, usize> = HashMap::new();
    for segment in segments {
        match segment {
            Segment::Function(f) => {
                let file = f.location.file.to_string_lossy().to_string();
                if !file_numbers.contains_key(&file) {
                    file_numbers.insert(file, file_numbers.len());
                }
            }
            Segment::Constant(_) | Segment::Include(_) => {}
        }
    }
    file_numbers
}

fn get_asm_for_function(function: &Function, file_numbers: &HashMap<String, usize>) -> String {
    format!(
        "{}:
{}",
        get_asm_label_for_gas(&function.name),
        get_asm_for_function_ops(function, file_numbers)
    )
}

fn get_asm_for_function_ops(function: &Function, file_numbers: &HashMap<String, usize>) -> String {
    let mut asm_blocks = Vec::new();
    for op in &function.ops {
        asm_blocks.push(get_asm_location_for_op(op, file_numbers));
        if let Some(asm_code) = get_asm_code_for_op(op, function, file_numbers) {
            asm_blocks.push(asm_code);
        }
    }

    asm_blocks.join("\n")
}

fn get_asm_data_section(segments: &[Segment]) -> String {
    let mut asm_blocks = Vec::new();
    let header = ".section .data";
    asm_blocks.push(header.to_string());

    for segment in segments {
        match segment {
            Segment::Constant(constant) => {
                if let Literal::String(literal) = &constant.literal {
                    let variable_name = get_asm_string_variable_name("const", &constant.name);
                    let string_variable =
                        generate_asm_string_variable(&variable_name, &quote(&literal));
                    asm_blocks.push(string_variable);
                }
            }
            Segment::Function(function) if *function.is_used.read().unwrap() => {
                let function_entries = get_asm_data_section_entries_function(function);
                if !function_entries.is_empty() {
                    asm_blocks.push(function_entries);
                }
            }
            _ => {}
        }
    }

    asm_blocks.join("\n")
}

fn get_asm_data_section_entries_function(function: &Function) -> String {
    let mut asm_blocks = Vec::new();
    for op in &function.ops {
        if op.ty == OpType::PushStr {
            let variable_name = get_asm_string_variable_name(&function.name, &op.id.to_string());
            let string_variable = generate_asm_string_variable(&variable_name, &op.token.value);
            asm_blocks.push(string_variable);
        }
    }
    asm_blocks.join("\n")
}

fn get_asm_location_for_op(op: &Op, file_numbers: &HashMap<String, usize>) -> String {
    let location = &op.token.location;
    let file = &op.token.location.file.to_string_lossy().to_string();
    let file_number = file_numbers.get(file).expect("File number should be set");
    format!(".loc {} {} {}", file_number, location.row, location.col)
}

fn generate_asm_string_variable(variable_name: &str, value: &str) -> String {
    format!(
        "{}:
.asciz {}",
        variable_name, value,
    )
}

fn get_asm_string_variable_name(prefix: &str, name: &str) -> String {
    get_asm_label_for_gas(&format!("{}_{}", prefix, name))
}

fn get_asm_code_for_op(
    op: &Op,
    function: &Function,
    file_numbers: &HashMap<String, usize>,
) -> Option<String> {
    match &op.ty {
        OpType::Bind => None,
        OpType::Break => Some(get_asm_break(op, function)),
        OpType::Cast(_) => None,
        OpType::Continue => Some(get_asm_continue(op, function)),
        OpType::Do => Some(get_asm_do(op, function)),
        OpType::Done => Some(get_asm_done(op, function)),
        OpType::Elif => Some(get_asm_elif(op, function)),
        OpType::Else => Some(get_asm_else(op, function)),
        OpType::Fi => Some(get_asm_fi(op, function)),
        OpType::FunctionEpilogue => match function.is_inline {
            true => None,
            false => Some(get_asm_function_epilogue(function)),
        },
        OpType::FunctionPrologue => match function.is_inline {
            true => None,
            false => Some(get_asm_function_prologue(function)),
        },
        OpType::Identifier => {
            let identifier = normalize_identifier(&op.token.value);
            let global_identifiers = GLOBAL_IDENTIFIERS.get().unwrap();
            match global_identifiers.get(&identifier) {
                Some(Identifier::Constant(c)) => match &c.literal {
                    Literal::Boolean(b) => Some(get_asm_push_bool(*b)),
                    Literal::Integer(i) => Some(get_asm_push_int(*i)),
                    Literal::String(_) => {
                        let variable_name = get_asm_string_variable_name("const", &identifier);
                        Some(get_asm_push_str(&variable_name))
                    }
                },
                Some(Identifier::Function(f)) => match f.is_inline {
                    false => Some(get_asm_function_call(&f.name)),
                    true => {
                        let global_identifiers = GLOBAL_IDENTIFIERS.get().unwrap();
                        let called_function = match global_identifiers.get(&identifier) {
                            Some(Identifier::Function(f)) => f,
                            _ => fatal_error(
                                &op.token.location,
                                CasaError::InternalCompilerError,
                                &format!("Unknown function identifier `{}`", op.token.value),
                            ),
                        };
                        Some(get_asm_for_function_ops(called_function, file_numbers))
                    }
                },
                None => Some(get_asm_push_bind(op, function)),
            }
        }
        OpType::If => None,
        OpType::Intrinsic(intrinsic) => Some(get_asm_intrinsic(intrinsic)),
        OpType::MethodCall => {
            #[rustfmt::skip]
            let receiver = op.receiver.read().unwrap().clone().expect("Method receiver type");
            let function_name = format!("{}.{}", receiver, op.token.value);
            Some(get_asm_function_call(&function_name))
        }
        OpType::Peek => Some(get_asm_peek().to_string()),
        OpType::PeekBind => Some(get_asm_peek_bind(op, function)),
        OpType::PushBool => match op.token.ty {
            TokenType::Literal(Literal::Boolean(b)) => Some(get_asm_push_bool(b)),
            _ => unreachable!("Expected a boolean literal"),
        },
        OpType::PushInt => match op.token.ty {
            TokenType::Literal(Literal::Integer(i)) => Some(get_asm_push_int(i)),
            _ => unreachable!("Expected an integer literal"),
        },
        OpType::PushStr => {
            let variable_name = get_asm_string_variable_name(&function.name, &op.id.to_string());
            Some(get_asm_push_str(&variable_name))
        }
        OpType::Return => Some(get_asm_return(function)),
        OpType::Take => None,
        OpType::TakeBind => Some(get_asm_take_bind(op, function)),
        OpType::Then => Some(get_asm_then(op, function)),
        OpType::While => Some(get_asm_while(op, function)),
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
        Intrinsic::LoadByte => get_asm_load(DataSize::Byte),
        Intrinsic::LoadWord => get_asm_load(DataSize::Word),
        Intrinsic::LoadDword => get_asm_load(DataSize::Dword),
        Intrinsic::LoadQword => get_asm_load(DataSize::Qword),
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

fn get_asm_push_bool(value: bool) -> String {
    format!(
        "mov ${}, %rax
pushq %rax",
        value as u8
    )
}

fn get_asm_push_int(value: i32) -> String {
    format!(
        "movabs ${}, %rax
pushq %rax",
        value
    )
}

fn get_asm_push_str(asm_variable_name: &str) -> String {
    format!(
        "leaq {}(%rip), %rsi
pushq %rsi",
        asm_variable_name
    )
}

fn get_asm_fi(op: &Op, function: &Function) -> String {
    let function_label = get_asm_label_for_gas(&function.name);
    format!("{}_fi{}:", &function_label, op.id)
}

fn get_asm_then(op: &Op, function: &Function) -> String {
    let (label, related_id) = if let Some(id) = get_related_elif_id(op, function) {
        ("elif", id)
    } else if let Some(id) = get_related_else_id(op, function) {
        ("else", id)
    } else if let Some(id) = get_related_fi_id(op, function) {
        ("fi", id)
    } else {
        fatal_error(
            &op.token.location,
            CasaError::SyntaxError,
            "Related `elif`, `else` or `fi` was not found",
        )
    };

    let function_label = get_asm_label_for_gas(&function.name);
    format!(
        "popq %rax
testq %rax, %rax
jz {}_{}{}",
        &function_label, label, related_id
    )
}

fn get_asm_while(op: &Op, function: &Function) -> String {
    let function_label = get_asm_label_for_gas(&function.name);
    format!("{}_while{}:", &function_label, op.id)
}

fn get_asm_do(op: &Op, function: &Function) -> String {
    let related_id = match get_related_done_id(op, function) {
        Some(id) => id,
        None => fatal_error(
            &op.token.location,
            CasaError::SyntaxError,
            "Related `done` was not found",
        ),
    };

    let function_label = get_asm_label_for_gas(&function.name);
    format!(
        "popq %rax
testq %rax, %rax
jz {}_done{}",
        &function_label, related_id
    )
}

fn get_asm_done(op: &Op, function: &Function) -> String {
    let related_id = match get_related_while_id(op, function) {
        Some(id) => id,
        None => fatal_error(
            &op.token.location,
            CasaError::SyntaxError,
            "Related `while` was not found",
        ),
    };

    let function_label = get_asm_label_for_gas(&function.name);
    format!(
        "jmp {}_while{}
{}_done{}:",
        &function_label, related_id, &function_label, op.id
    )
}

fn get_asm_elif(op: &Op, function: &Function) -> String {
    let (label, related_id) = if let Some(id) = get_related_elif_id(op, function) {
        ("elif", id)
    } else if let Some(id) = get_related_else_id(op, function) {
        ("else", id)
    } else if let Some(id) = get_related_fi_id(op, function) {
        ("fi", id)
    } else {
        fatal_error(
            &op.token.location,
            CasaError::SyntaxError,
            "Related `elif`, `else` or `fi` was not found",
        )
    };

    let function_label = get_asm_label_for_gas(&function.name);
    format!(
        "jmp {}_{}{}
{}_elif{}:",
        &function_label, label, related_id, &function_label, op.id,
    )
}

fn get_asm_else(op: &Op, function: &Function) -> String {
    let related_id = match get_related_fi_id(op, function) {
        Some(id) => id,
        None => fatal_error(
            &op.token.location,
            CasaError::SyntaxError,
            "Related `fi` was not found",
        ),
    };

    let function_label = get_asm_label_for_gas(&function.name);
    format!(
        "jmp {}_fi{}
{}_else{}:",
        &function_label, related_id, &function_label, op.id
    )
}

fn get_asm_break(op: &Op, function: &Function) -> String {
    let related_id = match get_related_done_id(op, function) {
        Some(id) => id,
        None => fatal_error(
            &op.token.location,
            CasaError::SyntaxError,
            "Related `done` was not found",
        ),
    };

    let function_label = get_asm_label_for_gas(&function.name);
    format!("jmp {}_done{}", &function_label, related_id)
}

fn get_asm_continue(op: &Op, function: &Function) -> String {
    let related_id = match get_related_while_id(op, function) {
        Some(id) => id,
        None => fatal_error(
            &op.token.location,
            CasaError::SyntaxError,
            "Related `while` was not found",
        ),
    };

    let function_label = get_asm_label_for_gas(&function.name);
    format!("jmp {}_while{}", &function_label, related_id)
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

    // Push copy of the Nth value from the data stack
    // and store it to a variable in the return stack
    format!(
        "pushq (%r15)
addq $8, %r15
popq %rbx
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
    format!("call {}", get_asm_label_for_gas(function_name))
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

fn get_asm_load(size: DataSize) -> String {
    let load_asm = match size {
        DataSize::Byte => "movzbl (%rax), %eax",
        DataSize::Word => "movzwl (%rax), %eax",
        DataSize::Dword => "movl (%rax), %eax",
        DataSize::Qword => "movq (%rax), %rax",
    };

    format!(
        "popq %rax
{}
pushq %rax",
        load_asm
    )
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
