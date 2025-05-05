use indexmap::IndexSet;

use crate::common::{
    Counter, DELIMITERS, Function, Identifier, IdentifierTable, Intrinsic, Keyword, Literal,
    Location, Op, OpType, Parameter, Segment, Signature, Token, TokenType,
};
use crate::error::{CasaError, colored_error_tag, fatal_error};
use std::collections::HashMap;
use std::path::Path;

static OP_COUNTER: Counter = Counter::new();

pub fn parse_code_file(file: &Path) -> (Vec<Segment>, IdentifierTable) {
    let code = match std::fs::read_to_string(file) {
        Ok(code) => code,
        Err(err) => {
            eprintln!(
                "{} Cannot read file '{}': {}",
                colored_error_tag(CasaError::FileNotFound),
                file.display(),
                err
            );
            std::process::exit(1);
        }
    };

    let mut segments = Vec::new();
    let mut cursor = 0;
    while let Some(segment) = parse_next_segment(&code, &mut cursor, file) {
        segments.push(segment);
    }

    // Make sure the whole code was parsed
    if cursor < code.len() {
        let error_message = format!(
            "The lexer could not parse the whole file '{}'\n\nUnparsed code:\n\n{}",
            file.display(),
            &code[cursor..]
        );
        let location = &get_location(&code, cursor, file);
        fatal_error(location, CasaError::SyntaxError, &error_message);
    }

    // Identifier resolution was not performed during initial parsing
    let global_identifiers = get_global_identifiers(&segments);
    resolve_identifiers(&mut segments, &global_identifiers);

    (segments, global_identifiers)
}

fn parse_next_segment(code: &str, cursor: &mut usize, file: &Path) -> Option<Segment> {
    parse_over_whitespace(code, cursor);

    let keyword = peek_until_whitespace_or_delimiter(code, *cursor);
    match keyword {
        "fun" | "inline" => Some(Segment::Function(parse_function(code, cursor, file)?)),
        _ => None,
    }
}

fn get_global_identifiers(segments: &[Segment]) -> IdentifierTable {
    let mut global_identifiers: IdentifierTable = HashMap::new();
    for segment in segments {
        match segment {
            Segment::Function(f) => {
                global_identifiers.insert(f.name.clone(), Identifier::Function(f.clone()));
            }
        }
    }
    global_identifiers
}

fn resolve_identifiers(segments: &mut [Segment], global_identifiers: &IdentifierTable) {
    for segment in segments.iter_mut() {
        let Segment::Function(func) = segment;
        resolve_identifiers_for_function(func, global_identifiers);
    }
}

fn resolve_identifiers_for_function(function: &mut Function, global_identifiers: &IdentifierTable) {
    for op in &mut function.ops {
        if op.ty != OpType::Unknown {
            continue;
        }

        // Global identifiers
        match global_identifiers.get(&op.token.value) {
            Some(Identifier::Function(f)) => match f.is_inline {
                true => op.ty = OpType::InlineFunctionCall,
                false => op.ty = OpType::FunctionCall,
            },
            None => {}
        }

        // Variables
        if function.variables.get(&op.token.value).is_some() {
            op.ty = OpType::PushBind;
        }
    }
}

enum Binding {
    Take,
    Peek,
}

fn parse_function(code: &str, cursor: &mut usize, file: &Path) -> Option<Function> {
    let mut ops = Vec::new();

    // "inline"
    let is_inline = parse_over_word(code, cursor, "inline").is_some();
    parse_over_whitespace(code, cursor);

    // "fun"
    let location = get_location(code, *cursor, file);
    let function_token = get_next_token(code, cursor, file)?;
    assert!(function_token.value == "fun");
    // Add function prologue if the function is not inline
    if !is_inline {
        let id = OP_COUNTER.fetch_add();
        let prologue = Op::new(id, OpType::FunctionPrologue, &function_token);
        ops.push(prologue);
    }
    parse_over_whitespace(code, cursor);

    // Function name
    let name = parse_until_whitespace_or_delimiter(code, cursor).to_string();
    parse_over_whitespace(code, cursor);

    // Function signature
    let signature = parse_function_signature(code, cursor);
    validate_signature(&name, &signature);
    parse_over_word(code, cursor, ":");

    // Function tokens
    let mut variables = IndexSet::new();
    let mut binding: Option<Binding> = None;
    while let Some(token) = get_next_token(code, cursor, file) {
        match &token.ty {
            TokenType::Delimiter(_) => {}
            TokenType::Identifier => {
                ops.push(get_identifier_op(&token, &binding));
                if binding.is_some() {
                    variables.insert(token.value);
                }
            }
            TokenType::Intrinsic(v) => ops.push(get_intrinsic_op(v, &token)),
            TokenType::Literal(v) => ops.push(get_literal_op(v, &token)),
            TokenType::Keyword(Keyword::End) => {
                if !is_inline {
                    ops.push(get_keyword_op(&Keyword::End, &token)?);
                }
                break;
            }
            TokenType::Keyword(keyword) => {
                if let Some(op) = get_keyword_op(keyword, &token) {
                    ops.push(op);
                }
                match keyword {
                    Keyword::Bind => binding = None,
                    Keyword::Peek => binding = Some(Binding::Peek),
                    Keyword::Take => binding = Some(Binding::Take),
                    _ => {}
                }
            }
        }
    }

    Some(Function {
        name,
        signature,
        location,
        is_inline,
        ops,
        variables,
    })
}

fn validate_signature(function_name: &str, signature: &Signature) {
    // Only `main` function has exeptions
    if function_name != "main" {
        return;
    }

    if !signature.params.is_empty() {
        panic!("`main` function should not have parameters");
    }

    match signature.returns.as_slice() {
        [] => {}
        [ty] if ty == "int" => {}
        _ => panic!("`main` function should return int or nothing"),
    }
}

fn parse_function_signature(code: &str, cursor: &mut usize) -> Signature {
    let params = parse_function_params(code, cursor);
    let returns = match parse_over_word(code, cursor, "->") {
        Some(_) => parse_function_return_types(code, cursor),
        None => Vec::new(),
    };
    Signature { params, returns }
}

fn parse_function_params(code: &str, cursor: &mut usize) -> Vec<Parameter> {
    let mut params = Vec::new();
    parse_over_whitespace(code, cursor);
    while peek_word(code, *cursor, ":").is_none() && peek_word(code, *cursor, "->").is_none() {
        let name_or_type = parse_until_whitespace_or_delimiter(code, cursor).to_string();
        let param = match parse_over_word(code, cursor, ":") {
            None => Parameter {
                name: None,
                ty: name_or_type,
            },
            Some(_) => {
                let ty = parse_until_whitespace_or_delimiter(code, cursor);
                assert!(!ty.is_empty());
                Parameter {
                    name: Some(name_or_type),
                    ty: ty.to_string(),
                }
            }
        };
        params.push(param);
        parse_over_whitespace(code, cursor);
    }
    params
}

fn parse_function_return_types(code: &str, cursor: &mut usize) -> Vec<String> {
    let mut return_types = Vec::new();
    parse_over_whitespace(code, cursor);
    while peek_word(code, *cursor, ":").is_none() {
        let return_type = parse_until_whitespace_or_delimiter(code, cursor);
        return_types.push(return_type.to_string());
        parse_over_whitespace(code, cursor);
    }
    return_types
}

fn get_identifier_op(token: &Token, binding: &Option<Binding>) -> Op {
    let op_type = match binding {
        Some(Binding::Peek) => OpType::PeekBind,
        Some(Binding::Take) => OpType::TakeBind,
        None => OpType::Unknown, // Identifier resolution is performed later
    };
    Op::new(OP_COUNTER.fetch_add(), op_type, token)
}

fn get_intrinsic_op(intrinsic: &Intrinsic, token: &Token) -> Op {
    let id = OP_COUNTER.fetch_add();
    let op_type = OpType::Intrinsic(intrinsic.clone());
    Op::new(id, op_type, token)
}

fn get_literal_op(literal: &Literal, token: &Token) -> Op {
    let id = OP_COUNTER.fetch_add();
    match literal {
        Literal::Integer(_) => Op::new(id, OpType::PushInt, token),
        Literal::String(_) => Op::new(id, OpType::PushStr, token),
    }
}

fn get_keyword_op(keyword: &Keyword, token: &Token) -> Option<Op> {
    let id = OP_COUNTER.fetch_add();
    match keyword {
        Keyword::Bind => Some(Op::new(id, OpType::Bind, token)),
        Keyword::Break => Some(Op::new(id, OpType::Break, token)),
        Keyword::Continue => Some(Op::new(id, OpType::Continue, token)),
        Keyword::Do => Some(Op::new(id, OpType::Do, token)),
        Keyword::Done => Some(Op::new(id, OpType::Done, token)),
        Keyword::End => Some(Op::new(id, OpType::FunctionEpilogue, token)),
        Keyword::Fun => Some(Op::new(id, OpType::FunctionPrologue, token)),
        Keyword::Fi => Some(Op::new(id, OpType::Fi, token)),
        Keyword::If => Some(Op::new(id, OpType::If, token)),
        Keyword::Inline => None,
        Keyword::Peek => Some(Op::new(id, OpType::Peek, token)),
        Keyword::Return => Some(Op::new(id, OpType::Return, token)),
        Keyword::Take => Some(Op::new(id, OpType::Take, token)),
        Keyword::Then => Some(Op::new(id, OpType::Then, token)),
        Keyword::While => Some(Op::new(id, OpType::While, token)),
        _ => todo!(),
    }
}

fn get_unparsed(code: &str, cursor: usize) -> &str {
    &code[cursor..]
}

fn peek_word<'a>(code: &'a str, cursor: usize, word: &'a str) -> Option<&'a str> {
    let unparsed = get_unparsed(code, cursor);
    if unparsed.starts_with(word) {
        Some(word)
    } else {
        None
    }
}

fn parse_over_word<'a>(code: &'a str, cursor: &'a mut usize, word: &'a str) -> Option<&'a str> {
    match peek_word(code, *cursor, word) {
        Some(word) => {
            *cursor += word.len();
            Some(word)
        }
        None => None,
    }
}

fn get_next_token(code: &str, cursor: &mut usize, file: &Path) -> Option<Token> {
    parse_over_whitespace(code, cursor);

    let location = get_location(code, *cursor, file);
    let unparsed = get_unparsed(code, *cursor);
    let mut chars = unparsed.chars();
    let first = chars.next()?;
    assert!(!first.is_whitespace());

    // Handle tokens recognized by the first character
    match first {
        '"' => return parse_string_literal_token(code, cursor, file),
        c if let Some(delimiter) = DELIMITERS.get(&c) => {
            *cursor += 1;
            return Some(Token::new(
                &c.to_string(),
                TokenType::Delimiter(delimiter.clone()),
                location,
            ));
        }
        _ => {}
    }

    // Handle other tokens
    let value = parse_until_whitespace_or_delimiter(code, cursor);
    match value {
        v if let Ok(integer) = v.parse::<i32>() => Some(Token::new(
            value,
            TokenType::Literal(Literal::Integer(integer)),
            location,
        )),
        v if let Ok(intrinsic) = v.parse::<Intrinsic>() => {
            Some(Token::new(value, TokenType::Intrinsic(intrinsic), location))
        }
        v if let Ok(keyword) = v.parse::<Keyword>() => {
            Some(Token::new(value, TokenType::Keyword(keyword), location))
        }
        _ => Some(Token::new(value, TokenType::Identifier, location)),
    }
}

fn get_location(code: &str, cursor: usize, file: &Path) -> Location {
    let mut row = 1;
    let mut last_line_start_index = 0;

    for (i, char) in code.char_indices() {
        if i >= cursor {
            break;
        }
        if char == '\n' {
            row += 1;
            last_line_start_index = i + char.len_utf8();
        }
    }

    let col = code[last_line_start_index..cursor].chars().count() + 1;

    Location {
        file: file.to_path_buf(),
        row,
        col,
    }
}

fn parse_over_whitespace(code: &str, cursor: &mut usize) {
    let start = *cursor;
    for (i, char) in code[start..].char_indices() {
        if !char.is_whitespace() {
            *cursor = start + i;
            return;
        }
    }

    *cursor = code.len();
}

fn peek_until_whitespace_or_delimiter(code: &str, cursor: usize) -> &str {
    for (i, char) in code[cursor..].char_indices() {
        if char.is_whitespace() || DELIMITERS.get(&char).is_some() {
            return &code[cursor..cursor + i];
        }
    }

    &code[cursor..]
}

fn parse_until_whitespace_or_delimiter<'a>(code: &'a str, cursor: &'a mut usize) -> &'a str {
    let parsed = peek_until_whitespace_or_delimiter(code, *cursor);
    *cursor += parsed.len();
    parsed
}

fn parse_string_literal_token(code: &str, cursor: &mut usize, file: &Path) -> Option<Token> {
    let location = get_location(code, *cursor, file);
    let unparsed = get_unparsed(code, *cursor);
    let mut chars = unparsed.char_indices();
    let (_, first) = chars.next()?;

    if first != '"' {
        return None;
    }

    for (index, char) in chars {
        match char {
            '\n' => return None,
            '"' => {
                // Save literal value without the double quotes
                let value = &unparsed[1..index];
                *cursor += index + 1;
                return Some(Token::new(
                    &unparsed[..=index],
                    TokenType::Literal(Literal::String(value.to_string())),
                    location,
                ));
            }
            _ => {}
        }
    }

    None
}
