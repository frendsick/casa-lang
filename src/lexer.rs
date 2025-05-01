use crate::defs::{Intrinsic, Literal, Token, TokenType};

const NEWLINE: char = 0xA as char;

pub fn parse_tokens(code: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut cursor = 0;
    while let Some(token) = get_next_token(code, &mut cursor) {
        tokens.push(token);
    }

    // Make sure the whole code was parsed
    assert!(cursor >= code.len());

    tokens
}

fn get_unparsed(code: &str, cursor: usize) -> &str {
    &code[cursor..]
}

fn get_next_token(code: &str, cursor: &mut usize) -> Option<Token> {
    parse_over_whitespace(code, cursor);

    let unparsed = get_unparsed(code, *cursor);
    let mut chars = unparsed.chars();
    let first = chars.next()?;
    assert!(!first.is_whitespace());

    // Handle tokens recognized by the first character
    match first {
        '"' => return parse_string_literal_token(code, cursor),
        _ => {}
    }

    // Handle other tokens
    let value = parse_until_whitespace(code, cursor);
    match value {
        v if let Ok(integer) = v.parse::<i32>() => Some(Token::new(
            value,
            TokenType::Literal(Literal::Integer(integer)),
        )),
        v if let Ok(intrinsic) = v.parse::<Intrinsic>() => {
            Some(Token::new(value, TokenType::Intrinsic(intrinsic)))
        }
        _ => todo!(),
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

fn parse_until_whitespace<'a>(code: &'a str, cursor: &'a mut usize) -> &'a str {
    let start = *cursor;
    for (i, char) in code[start..].char_indices() {
        if char.is_whitespace() {
            *cursor = start + i;
            return &code[start..start + i];
        }
    }

    *cursor = code.len();
    &code[start..]
}

fn parse_string_literal_token(code: &str, cursor: &mut usize) -> Option<Token> {
    let unparsed = get_unparsed(code, *cursor);
    let mut chars = unparsed.char_indices();
    let (_, first) = chars.next()?;

    if first != '"' {
        return None;
    }

    for (index, char) in chars {
        match char {
            NEWLINE => return None,
            '"' => {
                // Save literal value without the double quotes
                let value = &unparsed[1..index];
                *cursor += index + 1;
                return Some(Token::new(
                    &unparsed[..=index],
                    TokenType::Literal(Literal::String(value.to_string())),
                ));
            }
            _ => {}
        }
    }

    None
}
