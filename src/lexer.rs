use crate::common::{
    Ansi, Counter, Function, Identifier, IdentifierTable, Intrinsic, Keyword, Literal, Location, Op, OpType, Parameter, Segment, Signature, Token, TokenType, DELIMITERS
};
use crate::error::{CasaError, colored_error_tag, fatal_error};
use indexmap::IndexSet;
use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};

static OP_COUNTER: Counter = Counter::new();

struct Parser {
    pub code: String,
    pub cursor: usize,
    pub file: PathBuf,
}

impl Parser {
    fn from_file(file: &Path) -> io::Result<Parser> {
        let code = std::fs::read_to_string(file)?;
        Ok(Parser {
            code,
            cursor: 0,
            file: file.to_path_buf(),
        })
    }

    /// Get unparsed code slice
    fn rest(&self) -> &str {
        &self.code[self.cursor..]
    }

    fn is_finished(&self) -> bool {
        self.cursor >= self.code.len()
    }

    fn get_location(&self) -> Location {
        let mut row = 1;
        let mut row_start_index = 0;

        for (i, char) in self.code.char_indices() {
            if i >= self.cursor {
                break;
            }
            if char == '\n' {
                row += 1;
                row_start_index = i + char.len_utf8();
            }
        }

        let col = self.code[row_start_index..self.cursor].chars().count() + 1;

        Location {
            file: self.file.to_path_buf(),
            row,
            col,
        }
    }

    fn skip_whitespace(&mut self) {
        let start = self.cursor;
        for (i, char) in self.code[start..].char_indices() {
            if !char.is_whitespace() {
                self.cursor = start + i;
                return;
            }
        }
        self.cursor = self.code.len();
    }

    /// Peeks what the next parsed word would be.
    /// Parsing stops on whitespace and delimiter characters.
    /// Returns an empty string only if the parsing is finished.
    fn peek_word(&self) -> &str {
        for (i, char) in self.code[self.cursor..].char_indices() {
            if char.is_whitespace() || DELIMITERS.get(&char).is_some() {
                // Parse at least one character
                let end_offset = if i > 0 { i } else { 1 };
                return &self.code[self.cursor..self.cursor + end_offset];
            }
        }
        &self.code[self.cursor..]
    }

    /// Parser the next word.
    /// Parsing stops on whitespace and delimiter characters.
    /// Returns an empty string only if the parsing is finished.
    fn parse_word(&mut self) -> String {
        let word = self.peek_word().to_string();
        self.cursor += word.len();
        word
    }

    /// Parse the next word if it is what is expected.
    fn expect_word(&mut self, expected: &str) -> Option<String> {
        let word = self.peek_word().to_string();
        if word == *expected {
            self.cursor += word.len();
            Some(word)
        } else {
            None
        }
    }

    fn peek_startswith(&self, expected: &str) -> bool {
        let rest = self.rest();
        rest.starts_with(expected)
    }

    /// Parse over the start of the unparsed code if it is what is expected.
    /// The `expected` can contain any characters.
    fn skip_if_startswith(&mut self, expected: &str) -> bool {
        if self.peek_startswith(expected) {
            self.cursor += expected.len();
            true
        } else {
            false
        }
    }
}

pub fn parse_code_file(file: &Path) -> (Vec<Segment>, IdentifierTable) {
    let mut parser = match Parser::from_file(file) {
        Ok(parser) => parser,
        Err(error) => {
            eprintln!(
                "{} Cannot read file '{}': {}",
                colored_error_tag(CasaError::FileNotFound),
                file.display(),
                error
            );
            std::process::exit(1);
        }
    };

    let mut segments = Vec::new();
    while let Some(segment) = parse_next_segment(&mut parser) {
        segments.push(segment);
    }

    // Make sure the whole code was parsed
    if !parser.is_finished() {
        let error_message = format!(
            "The lexer could not parse the whole file '{}'\n\nUnparsed code:\n\n{}",
            file.display(),
            parser.rest(),
        );
        let location = &parser.get_location();
        fatal_error(location, CasaError::SyntaxError, &error_message);
    }

    // Identifier resolution was not performed during initial parsing
    let global_identifiers = get_global_identifiers(&segments);
    resolve_identifiers(&mut segments, &global_identifiers);

    (segments, global_identifiers)
}

fn parse_next_segment(parser: &mut Parser) -> Option<Segment> {
    parser.skip_whitespace();
    let keyword = parser.peek_word();
    match keyword {
        "fun" | "inline" => Some(Segment::Function(parse_function(parser)?)),
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

fn parse_function(parser: &mut Parser) -> Option<Function> {
    let mut ops = Vec::new();
    let error_prefix = "Error occurred while parsing a function";

    // "inline"
    let is_inline = parser.expect_word("inline").is_some();
    parser.skip_whitespace();

    // "fun"
    let location = parser.get_location();
    let fun_token = parse_next_token(parser);
    if fun_token.value != "fun" {
        fatal_error(
            &location,
            CasaError::SyntaxError,
            &format!(
                "{}: Expected 'fun' but got '{}'",
                error_prefix, fun_token.value
            ),
        )
    }
    parser.skip_whitespace();

    // Add function prologue if the function is not inline
    if !is_inline {
        let id = OP_COUNTER.fetch_add();
        let prologue = Op::new(id, OpType::FunctionPrologue, &fun_token);
        ops.push(prologue);
    }

    // Function name
    let function_name = parser.parse_word();
    parser.skip_whitespace();

    // Function signature
    let signature = parse_function_signature(parser, &function_name);
    validate_signature(&function_name, &signature);
    parser.skip_whitespace();

    // "::"
    if !parser.skip_if_startswith("::") {
        let word = parser.peek_word();
        fatal_error(
            &parser.get_location(),
            CasaError::SyntaxError,
            &format!("{}: Expected '::' but got '{}'", error_prefix, word),
        )
    }

    // Function tokens
    let mut variables = IndexSet::new();
    let mut binding: Option<Binding> = None;
    loop {
        parser.skip_whitespace();
        let token = parse_next_token(parser);
        match &token.ty {
            TokenType::Delimiter(_) => {}
            TokenType::EndOfFile => {
                let error_message = format!(
                    "{}: The '{}' function is missing the 'end' token",
                    error_prefix, function_name
                );
                fatal_error(&token.location, CasaError::SyntaxError, &error_message);
            }
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
        name: function_name,
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

fn parse_token_value(parser: &mut Parser) -> String {
    let rest = parser.rest();
    let first = rest.chars().next();

    // Handle tokens recognized by the first character
    match first {
        None => return "".to_string(),
        Some('"') => {
            if let Some(string_literal) = parse_string_literal(parser) {
                return string_literal;
            }

            let location = parser.get_location();
            let value = parser.parse_word();
            let error_message = format!("Invalid string literal token: '{}'", value);
            fatal_error(&location, CasaError::SyntaxError, &error_message);
        }
        Some(_) => {} // Token was not recognized by the first character
    }

    // Handle other tokens
    parser.parse_word().to_string()
}

/// Returns None when the parsing is finished
fn parse_next_token(parser: &mut Parser) -> Token {
    let location = parser.get_location();
    let value = parse_token_value(parser);
    match value.as_ref() {
        "" => Token::new("", TokenType::EndOfFile, location),
        v if v.starts_with('"') && v.ends_with('"') => Token::new(
            v,
            // Save literal value without the double quotes
            TokenType::Literal(Literal::String(v[1..v.len() - 1].to_string())),
            location,
        ),
        v if let Ok(integer) = v.parse::<i32>() => {
            Token::new(v, TokenType::Literal(Literal::Integer(integer)), location)
        }
        v if let Ok(intrinsic) = v.parse::<Intrinsic>() => {
            Token::new(v, TokenType::Intrinsic(intrinsic), location)
        }
        v if let Ok(keyword) = v.parse::<Keyword>() => {
            Token::new(v, TokenType::Keyword(keyword), location)
        }
        v => Token::new(v, TokenType::Identifier, location),
    }
}

fn parse_function_signature(parser: &mut Parser, function_name: &str) -> Signature {
    let params = parse_function_params(parser, function_name);
    let returns = match parser.expect_word("->") {
        Some(_) => parse_function_return_types(parser, function_name),
        None => Vec::new(),
    };
    Signature { params, returns }
}

fn parse_function_params(parser: &mut Parser, function_name: &str) -> Vec<Parameter> {
    let mut params = Vec::new();
    loop {
        parser.skip_whitespace();
        if parser.peek_startswith("->") || parser.peek_startswith("::") {
            break;
        }

        if parser.is_finished() {
            fatal_error(
                &parser.get_location(),
                CasaError::SyntaxError,
                &format!(
                    "End of file while parsing parameters for '{}' function.
{}Hint{}: Did you forget to close the function signature with '::' token?",
                    function_name,
                    Ansi::Blue,
                    Ansi::Reset,
                ),
            )
        }

        let name_or_type = parser.parse_word();
        let param = match parser.expect_word(":") {
            None => Parameter {
                name: None,
                ty: name_or_type,
            },
            Some(_) => {
                let ty = parser.parse_word();
                assert!(!ty.is_empty());
                Parameter {
                    name: Some(name_or_type),
                    ty: ty.to_string(),
                }
            }
        };
        params.push(param);
    }
    params
}

fn parse_function_return_types(parser: &mut Parser, function_name: &str) -> Vec<String> {
    let mut return_types = Vec::new();

    parser.skip_whitespace();
    while !parser.peek_startswith("::") {
        if parser.is_finished() {
            fatal_error(
                &parser.get_location(),
                CasaError::SyntaxError,
                &format!(
                    "End of file while parsing return types for '{}' function.
{}Hint{}: Did you forget to close the function signature with '::' token?",
                    function_name,
                    Ansi::Blue,
                    Ansi::Reset,
                ),
            )
        }

        let return_type = parser.parse_word();
        return_types.push(return_type);
        parser.skip_whitespace();
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

fn parse_string_literal(parser: &mut Parser) -> Option<String> {
    let rest = parser.rest().to_string();
    let mut chars = rest.char_indices();
    let (_, first) = chars.next()?;

    if first != '"' {
        return None;
    }

    for (index, char) in chars {
        match char {
            '\n' => return None,
            '"' => {
                parser.cursor += index + 1;
                return Some(rest[..index + 1].to_string());
            }
            _ => {}
        }
    }

    None
}
