use crate::canonicalize_path_must_exist;
use crate::common::{
    Ansi, Constant, Counter, DELIMITERS, Function, GLOBAL_IDENTIFIERS, Identifier, IdentifierTable,
    Intrinsic, Keyword, Literal, Location, Op, OpType, Parameter, Segment, Signature, Token,
    TokenType, Type,
};
use crate::error::{CasaError, fatal_error, fatal_error_short};
use indexmap::IndexSet;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::{Arc, RwLock};

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

    fn peek_char(&self) -> Option<char> {
        self.rest().chars().next()
    }

    /// Peeks what the next parsed word would be.
    /// Parsing stops on whitespace and delimiter characters.
    /// Returns None only if the parsing is finished.
    fn peek_word(&self) -> Option<&str> {
        if self.is_finished() {
            return None;
        }

        for (i, char) in self.code[self.cursor..].char_indices() {
            if char.is_whitespace() || DELIMITERS.get(&char).is_some() {
                // Parse at least one character
                let end_offset = if i > 0 { i } else { 1 };
                return Some(&self.code[self.cursor..self.cursor + end_offset]);
            }
        }

        Some(&self.code[self.cursor..])
    }

    /// Parser the next word.
    /// Parsing stops on whitespace and delimiter characters.
    /// Returns an empty string only if the parsing is finished.
    fn parse_word(&mut self) -> Option<String> {
        let word = self.peek_word()?.to_string();
        self.cursor += word.len();
        Some(word)
    }

    fn expect_char(&mut self, expected: char) -> Option<char> {
        let first = self.code.chars().next();
        if let Some(c) = first {
            self.cursor += 1;
            Some(c)
        } else {
            None
        }
    }

    /// Parse the next word if it is what is expected.
    fn expect_word(&mut self, expected: &str) -> Option<String> {
        let word = self.peek_word()?.to_string();
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
                let location = parser.get_location();
                let Some(word) = parser.peek_word() else {
                    fatal_error(
                        &location,
                        CasaError::SyntaxError,
                        "End of file while parsing #include directive",
                    )
                };
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
    parse_code_segments(&mut parser).expect("Parsed segments")
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

fn parse_code_segments(parser: &mut Parser) -> Option<Vec<Segment>> {
    let mut segments = Vec::new();
    parser.skip_whitespace();

    while let Some(keyword) = parser.peek_word() {
        match keyword {
            "const" => segments.push(parse_const_segment(parser)?),
            "fun" | "inline" => segments.push(parse_function_segment(parser)?),
            "#include" => segments.push(parse_include_segment(parser)?),
            "impl" => {
                parser.expect_word("impl")?;
                parser.skip_whitespace();

                let impl_type = parser.parse_word()?;
                parser.skip_whitespace();

                while parser.expect_word("end").is_none() {
                    let function = parse_function(parser, Some(impl_type.clone()))?;
                    segments.push(Segment::Function(function));
                    parser.skip_whitespace();
                }
            }
            _ => fatal_error(
                &parser.get_location(),
                CasaError::SyntaxError,
                &format!("Unknown segment keyword: '{}'", keyword),
            ),
        }
        parser.skip_whitespace();
    }

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

    Some(segments)
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

    parser.expect_word("end")?;
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
        CasaError::DuplicateIdentifier,
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

fn parse_function_segment(parser: &mut Parser) -> Option<Segment> {
    let function = parse_function(parser, None)?;
    Some(Segment::Function(function))
}

fn parse_function(parser: &mut Parser, self_type: Option<Type>) -> Option<Function> {
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
    let function_name = match self_type {
        Some(ref ty) => format!("{}.{}", ty, function_name_token.value),
        None if function_name_token.value.contains('.') => fatal_error(
            &function_name_token.location,
            CasaError::InvalidIdentifier,
            &format!(
                "Invalid function name: {}

{}Hint{}: The '.' character is reserved for `impl` methods",
                function_name_token.value,
                Ansi::Blue,
                Ansi::Reset,
            ),
        ),
        None => function_name_token.value,
    };

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
    let mut variables = IndexSet::new();
    let signature_location = parser.get_location();
    let signature = parse_function_signature(
        parser,
        &function_name,
        &mut ops,
        &mut variables,
        self_type.clone(),
    );
    validate_signature(&function_name, &signature, &signature_location);
    parser.skip_whitespace();

    // "::"
    if !parser.skip_if_startswith("::") {
        let word = parser.peek_word()?;
        fatal_error(
            &parser.get_location(),
            CasaError::SyntaxError,
            &format!("{}: Expected '::' but got '{}'", error_prefix, word),
        )
    }

    // Function tokens
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
            TokenType::Keyword(Keyword::Cast) => {
                let open = parser.expect_char('(');
                let ty = parser.parse_word()?;
                let close = parser.expect_char(')');

                let invalid_syntax_message = &format!(
                    "Invalid syntax for `cast` keyword

{}Syntax{}: cast(<type>)",
                    Ansi::Blue,
                    Ansi::Reset
                );
                if open.is_none() || close.is_none() || ty.is_empty() {
                    fatal_error(
                        &token.location,
                        CasaError::SyntaxError,
                        &invalid_syntax_message,
                    )
                }

                match parser.peek_char() {
                    Some(c) if c.is_whitespace() => {
                        let cast_with_type = format!("{}({})", &token.value, &ty);
                        let id = OP_COUNTER.fetch_add();
                        let cast_token = Token::new(&cast_with_type, token.ty, token.location);
                        ops.push(Op::new(id, OpType::Cast(ty), &cast_token));
                    }
                    Some(c) => fatal_error(
                        &token.location,
                        CasaError::SyntaxError,
                        &invalid_syntax_message,
                    ),
                    None => fatal_error(
                        &token.location,
                        CasaError::SyntaxError,
                        &format!("End of file after parsing `cast({})` keyword", ty),
                    ),
                }
            }
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

    Some(Function {
        name: function_name.clone(),
        signature,
        location: function_name_token.location,
        is_inline,
        is_used: Arc::new(RwLock::new(function_name == "main")).into(),
        ops,
        variables,
    })
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

fn parse_token_value(parser: &mut Parser) -> Option<String> {
    let rest = parser.rest();
    let first = rest.chars().next()?;

    // Handle tokens recognized by the first character
    let token_value = match first {
        c if DELIMITERS.contains_key(&c) => {
            if parser.expect_char(c).is_none() {
                fatal_error(
                    &parser.get_location(),
                    CasaError::SyntaxError,
                    &format!("Token cannot start with delimiter: `{}`", c),
                )
            }
            c.to_string()
        }
        '"' => {
            if let Some(string_literal) = parse_string_literal(parser) {
                return Some(string_literal);
            }

            let location = parser.get_location();
            let value = parser.parse_word()?;
            let error_message = format!("Invalid string literal token: '{}'", value);
            fatal_error(&location, CasaError::SyntaxError, &error_message);
        }
        // Token was not recognized by the first character
        _ => parser.parse_word()?.to_string(),
    };

    Some(token_value)
}

fn parse_next_token(parser: &mut Parser) -> Token {
    let location = parser.get_location();
    let Some(value) = parse_token_value(parser) else {
        return Token::new("", TokenType::EndOfFile, location);
    };

    let token = match value {
        v if v.starts_with('"') && v.ends_with('"') => Token::new(
            &v,
            // Save literal value without the double quotes
            TokenType::Literal(Literal::String(v[1..v.len() - 1].to_string())),
            location,
        ),
        v if let Ok(boolean) = v.parse::<bool>() => {
            Token::new(&v, TokenType::Literal(Literal::Boolean(boolean)), location)
        }
        v if let Ok(integer) = v.parse::<i32>() => {
            Token::new(&v, TokenType::Literal(Literal::Integer(integer)), location)
        }
        v if let Ok(intrinsic) = v.parse::<Intrinsic>() => {
            Token::new(&v, TokenType::Intrinsic(intrinsic), location)
        }
        v if let Ok(keyword) = v.parse::<Keyword>() => {
            Token::new(&v, TokenType::Keyword(keyword), location)
        }
        v => Token::new(&v, TokenType::Identifier, location),
    };

    token
}

fn parse_function_signature(
    parser: &mut Parser,
    function_name: &str,
    ops: &mut Vec<Op>,
    variables: &mut IndexSet<String>,
    self_type: Option<Type>,
) -> Signature {
    let params = parse_function_params(parser, function_name, ops, variables, self_type).unwrap();
    let return_types = match parser.expect_word("->") {
        Some(_) => parse_function_return_types(parser, function_name),
        None => Vec::new(),
    };
    Signature {
        params,
        return_types,
    }
}

fn parse_function_params(
    parser: &mut Parser,
    function_name: &str,
    ops: &mut Vec<Op>,
    variables: &mut IndexSet<String>,
    self_type: Option<Type>,
) -> Option<Vec<Parameter>> {
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

        let name_or_type = parse_next_token(parser);
        let is_self = self_type.is_some() && name_or_type.value == "self";
        if is_self && let Some(ref ty) = self_type {
            if params.is_empty() && parser.peek_char() == Some(':') {
                fatal_error(
                    &name_or_type.location,
                    CasaError::SyntaxError,
                    &format!(
                        "Unexpected colon after `self` parameter.

{}Hint{}: The `self` parameter implicitly has the type of the related struct `{}`",
                        Ansi::Blue,
                        Ansi::Reset,
                        ty,
                    ),
                );
            } else if !params.is_empty() {
                fatal_error(
                    &name_or_type.location,
                    CasaError::InvalidIdentifier,
                    &format!(
                        "The `self` parameter should be the first parameter but is found at the {}. position of the `{}` function",
                        params.len() + 1,
                        function_name,
                    ),
                )
            }
        }

        let param = match parser.expect_word(":") {
            None if is_self => Parameter {
                name: Some("self".to_string()),
                ty: self_type.clone().expect("Type of `self` is defined"),
            },
            None => Parameter {
                name: None,
                ty: name_or_type.value.clone(),
            },
            Some(_) => Parameter {
                name: Some(name_or_type.value.clone()),
                ty: parser.parse_word()?,
            },
        };
        params.push(param.clone());

        // Add the named parameter to variables
        if let Some(name) = param.name {
            if !variables.insert(name.clone()) {
                fatal_error(
                    &name_or_type.location,
                    CasaError::DuplicateIdentifier,
                    &format!(
                        "Parameter with the name `{}` is already defined for function `{}`",
                        name, function_name,
                    ),
                )
            }
            ops.push(Op::new(
                OP_COUNTER.fetch_add(),
                OpType::TakeBind,
                &name_or_type,
            ));
        }
    }

    Some(params)
}

fn parse_function_return_types(parser: &mut Parser, function_name: &str) -> Vec<String> {
    let mut return_types = Vec::new();

    parser.skip_whitespace();
    while !parser.peek_startswith("::") {
        let Some(return_type) = parser.parse_word() else {
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
        };
        return_types.push(return_type);
        parser.skip_whitespace();
    }
    return_types
}

fn get_identifier_op(token: &Token, binding: &Option<Binding>) -> Op {
    let identifier = &token.value;

    let op_type = match binding {
        Some(Binding::Peek) => OpType::PeekBind,
        Some(Binding::Take) => OpType::TakeBind,
        None => OpType::Identifier,
    };

    if op_type == OpType::PeekBind || op_type == OpType::TakeBind {
        // Functions cannot shadow reserved words
        if is_reserved_word(&identifier) {
            fatal_error(
                &token.location,
                CasaError::InvalidIdentifier,
                &format!(
                    "Cannot use reserved word as a function name: `{}`",
                    &identifier
                ),
            );
        }

        if identifier.contains('.') {
            fatal_error(
                &token.location,
                CasaError::InvalidIdentifier,
                &format!(
                    "Invalid variable name: {}

{}Hint{}: The '.' character is reserved for `impl` methods",
                    identifier,
                    Ansi::Blue,
                    Ansi::Reset,
                ),
            );
        }
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
