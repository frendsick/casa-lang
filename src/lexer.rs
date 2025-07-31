use crate::canonicalize_path_must_exist;
use crate::common::{
    Ansi, Constant, Counter, DELIMITERS, Function, GLOBAL_IDENTIFIERS, Identifier, IdentifierTable,
    Intrinsic, Keyword, Literal, Location, Op, OpType, Parameter, Segment, Signature, Token,
    TokenType,
};
use crate::error::{CasaError, fatal_error, fatal_error_short};
use indexmap::IndexSet;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::str::FromStr;

static OP_COUNTER: Counter = Counter::new();

struct Parser<'a> {
    pub code: &'a str,
    pub cursor: usize,
    pub file: &'a Path,
}

impl Parser<'_> {
    fn new<'a>(code: &'a str, file: &'a Path) -> Parser<'a> {
        Parser {
            code,
            cursor: 0,
            file,
        }
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

    fn skip_line(&mut self) {
        let start = self.cursor;
        for (i, char) in self.code[start..].char_indices() {
            if char == '\n' {
                self.cursor = start + i + 1;
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

fn parse_included_files(file: &Path) -> HashSet<PathBuf> {
    // Parse included files recursively
    let mut included_files = HashSet::new();
    do_parse_included_files(file, &mut included_files);

    fn do_parse_included_files(file: &Path, included_files: &mut HashSet<PathBuf>) {
        if !included_files.insert(file.to_path_buf()) {
            return;
        }

        let code = read_file(&file);
        let mut parser = Parser::new(&code, &file);

        // Example: `#include "lib/std.casa"`
        while !parser.is_finished() {
            parser.skip_whitespace();

            if parser.expect_word("#include").is_none() {
                parser.skip_line();
                continue;
            }
            parser.skip_whitespace();

            if let Some(include_path) = parse_include_path_as_absolute(&mut parser) {
                // Parse included files recursively
                do_parse_included_files(&include_path, included_files);
            } else {
                let word = parser.peek_word();
                let location = parser.get_location();
                let error_message = format!(
                    "Invalid string literal token for #include directive: '{}'

#include directive expects file path as string literal
Example: `#include \"lib/std.casa\"`",
                    word
                );
                fatal_error(&location, CasaError::SyntaxError, &error_message)
            }
        }
    }

    included_files
}

fn parse_include_path_as_absolute(parser: &mut Parser) -> Option<PathBuf> {
    let string_literal = parse_string_literal(parser)?;
    let include_file = unquote(&string_literal)?;
    let path = Path::new(include_file);
    let canonicalized_path = if path.is_absolute() {
        canonicalize_path_must_exist(path)
    } else {
        let parent = parser.file.parent().unwrap();
        let abs_path = PathBuf::from(format!("{}/{}", parent.display(), include_file));
        canonicalize_path_must_exist(&abs_path)
    };
    Some(canonicalized_path)
}

pub fn parse_segments_from_included_files(file: &Path) -> Vec<Segment> {
    let included_files = parse_included_files(file);

    // Parse segments
    let mut segments: Vec<Segment> = Vec::new();
    for file in included_files {
        let code = read_file(&file);
        let mut code_segments = parse_segments_from_code(&code, &file);
        segments.append(&mut code_segments);
    }

    // Resolve global identifiers
    resolve_global_identifiers(&mut segments);

    segments
}

fn parse_segments_from_code(code: &str, file: &Path) -> Vec<Segment> {
    let mut parser = Parser::new(&code, file);
    parse_code_segments(&mut parser)
}

pub fn quote(string: &str) -> String {
    format!("\"{}\"", string)
}

pub fn unquote(string: &str) -> Option<&str> {
    if string.len() >= 2 && string.starts_with('"') && string.ends_with('"') {
        Some(&string[1..string.len() - 1])
    } else {
        None
    }
}

fn read_file(file: &Path) -> String {
    match fs::read_to_string(file) {
        Ok(code) => code,
        Err(error) => fatal_error_short(
            CasaError::FileNotFound,
            &format!("Cannot read file '{}': {}", file.display(), error),
        ),
    }
}

fn parse_code_segments(parser: &mut Parser) -> Vec<Segment> {
    // Parse segments from code
    let segments: Vec<_> = std::iter::from_fn(|| parse_next_segment(parser)).collect();

    // Make sure the whole code was parsed
    if !parser.is_finished() {
        let error_message = format!(
            "The lexer could not parse the whole file '{}'\n\nUnparsed code:\n\n{}",
            parser.file.display(),
            parser.rest(),
        );
        let location = &parser.get_location();
        fatal_error(location, CasaError::SyntaxError, &error_message);
    }

    segments
}

fn parse_next_segment(parser: &mut Parser) -> Option<Segment> {
    parser.skip_whitespace();
    let keyword = parser.peek_word();
    match keyword {
        "const" => parse_const_segment(parser),
        "fun" | "inline" => Some(Segment::Function(parse_function(parser))),
        "#include" => parse_include_segment(parser),
        "" => None,
        _ => fatal_error(
            &parser.get_location(),
            CasaError::SyntaxError,
            &format!("Unknown segment keyword: '{}'", keyword),
        ),
    }
}

fn parse_const_segment(parser: &mut Parser) -> Option<Segment> {
    parser.expect_word("const")?;
    parser.skip_whitespace();

    let const_name_token = parse_next_token(parser);
    let name = const_name_token.value.clone();
    if is_reserved_word(&name) {
        fatal_error(
            &const_name_token.location,
            CasaError::InvalidIdentifier,
            &format!(
                "Cannot use reserved word as a constant name: `{}`",
                &const_name_token.value
            ),
        );
    }
    parser.skip_whitespace();

    let literal_token = parse_next_token(parser);
    let literal = match literal_token.ty {
        TokenType::Literal(literal) => literal,
        _ => fatal_error(
            &literal_token.location,
            CasaError::SyntaxError,
            &format!("Invalid constant literal: {}", literal_token.value),
        ),
    };
    parser.skip_whitespace();

    parser.expect_word("end");
    parser.skip_whitespace();

    let constant = Constant {
        name,
        literal,
        location: const_name_token.location,
    };
    Some(Segment::Constant(constant))
}

fn parse_include_segment(parser: &mut Parser) -> Option<Segment> {
    parser.expect_word("#include")?;
    parser.skip_whitespace();
    let include_path = parse_include_path_as_absolute(parser)?;
    Some(Segment::Include(include_path))
}

fn get_global_identifiers(segments: &[Segment]) -> IdentifierTable {
    let mut global_identifiers: IdentifierTable = HashMap::new();
    for segment in segments {
        match segment {
            Segment::Constant(c) => {
                if let Some(identifier) =
                    global_identifiers.insert(c.name.clone(), Identifier::Constant(c.clone()))
                {
                    duplicate_global_identifier_error(&c.name, identifier, &c.location)
                }
            }
            Segment::Function(f) => {
                if let Some(identifier) =
                    global_identifiers.insert(f.name.clone(), Identifier::Function(f.clone()))
                {
                    duplicate_global_identifier_error(&f.name, identifier, &f.location)
                }
            }
            Segment::Include(_) => {}
        }
    }
    global_identifiers
}

fn duplicate_global_identifier_error(
    identifier_name: &str,
    defined_identifier: Identifier,
    error_location: &Location,
) -> ! {
    let (identifier_type, identifier_location) = match defined_identifier {
        Identifier::Constant(c) => ("constant", c.location),
        Identifier::Function(f) => ("function", f.location),
    };
    let error_message = format!(
        "Global identifier with the name `{}` is already defined as a {} at {}",
        identifier_name, identifier_type, identifier_location,
    );
    fatal_error(
        &error_location,
        CasaError::InvalidIdentifier,
        &error_message,
    );
}

fn resolve_global_identifiers(segments: &mut [Segment]) {
    let global_identifiers = get_global_identifiers(segments);
    GLOBAL_IDENTIFIERS
        .set(global_identifiers)
        .expect("Global identifiers are only set here");
}

enum Binding {
    Take,
    Peek,
}

fn is_reserved_word(name: &str) -> bool {
    Intrinsic::from_str(name).is_ok() || Keyword::from_str(name).is_ok()
}

fn parse_function(parser: &mut Parser) -> Function {
    let mut ops = Vec::new();
    let error_prefix = "Error occurred while parsing a function";

    // "inline"
    let is_inline = parser.expect_word("inline").is_some();
    parser.skip_whitespace();

    // "fun"
    let fun_token = parse_next_token(parser);
    if fun_token.value != "fun" {
        fatal_error(
            &fun_token.location,
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
    let function_name_token = parse_next_token(parser);
    let function_name = function_name_token.value;
    if is_reserved_word(&function_name) {
        fatal_error(
            &function_name_token.location,
            CasaError::InvalidIdentifier,
            &format!(
                "Cannot use reserved word as a function name: `{}`",
                function_name
            ),
        )
    }
    parser.skip_whitespace();

    // Function signature
    let signature_location = parser.get_location();
    let signature = parse_function_signature(parser, &function_name);
    validate_signature(&function_name, &signature, &signature_location);
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
            TokenType::Keyword(Keyword::Bind) => binding = None,
            // Variable names cannot shadow reserved words
            _ if binding.is_some() && is_reserved_word(&token.value) => {
                fatal_error(
                    &token.location,
                    CasaError::InvalidIdentifier,
                    &format!(
                        "Cannot use reserved word as a variable name: `{}`",
                        token.value
                    ),
                );
            }
            TokenType::Intrinsic(v) => ops.push(get_intrinsic_op(v, &token)),
            TokenType::Literal(v) => ops.push(get_literal_op(v, &token)),
            TokenType::Keyword(Keyword::End) => {
                if !is_inline {
                    ops.push(get_keyword_op(&Keyword::End, &token).unwrap());
                }
                break;
            }
            TokenType::Keyword(keyword) => {
                if let Some(op) = get_keyword_op(keyword, &token) {
                    ops.push(op);
                }
                match keyword {
                    Keyword::Peek => binding = Some(Binding::Peek),
                    Keyword::Take => binding = Some(Binding::Take),
                    _ => {}
                }
            }
        }
    }

    Function {
        name: function_name,
        signature,
        location: function_name_token.location,
        is_inline,
        ops,
        variables,
    }
}

fn validate_signature(function_name: &str, signature: &Signature, location: &Location) {
    // Only `main` function has exeptions
    if function_name != "main" {
        return;
    }

    match (
        signature.params.is_empty(),
        signature.return_types.as_slice(),
    ) {
        (true, []) => {}
        (true, [ty]) if ty == "int" => {}
        _ => fatal_error(
            location,
            CasaError::SyntaxError,
            &format!(
                "Invalid signature for 'main' function:

    `fun main {}`

Expected one of the following:

    `fun main`
    `fun main -> int`",
                signature,
            ),
        ),
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
        v if let Ok(boolean) = v.parse::<bool>() => {
            Token::new(v, TokenType::Literal(Literal::Boolean(boolean)), location)
        }
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
    let return_types = match parser.expect_word("->") {
        Some(_) => parse_function_return_types(parser, function_name),
        None => Vec::new(),
    };
    Signature {
        params,
        return_types,
    }
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

{}Hint{}: Did you forget to start the function body with '::' token?",
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

{}Hint{}: Did you forget to start the function body with '::' token?",
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
        None => OpType::Identifier,
    };

    // Functions cannot shadow reserved words
    if (op_type == OpType::PeekBind || op_type == OpType::TakeBind)
        && is_reserved_word(&token.value)
    {
        fatal_error(
            &token.location,
            CasaError::InvalidIdentifier,
            &format!(
                "Cannot use reserved word as a function name: `{}`",
                token.value
            ),
        );
    }

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
        Literal::Boolean(_) => Op::new(id, OpType::PushBool, token),
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
        Keyword::Elif => Some(Op::new(id, OpType::Elif, token)),
        Keyword::Else => Some(Op::new(id, OpType::Else, token)),
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
