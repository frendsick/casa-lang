use crate::defs::{DELIMITERS, Intrinsic, Literal, Location, Token, TokenType};
use std::io;
use std::path::PathBuf;

const NEWLINE: char = 0xA as char;

pub fn parse_tokens_from_file(file: PathBuf) -> io::Result<Vec<Token>> {
    let code = std::fs::read_to_string(&file)?;

    let mut tokens = Vec::new();
    let mut cursor = 0;
    while let Some(token) = get_next_token(&code, &mut cursor, &file) {
        tokens.push(token);
    }

    // Make sure the whole code was parsed
    assert!(cursor >= code.len());

    Ok(tokens)
}

fn get_unparsed(code: &str, cursor: usize) -> &str {
    &code[cursor..]
}

fn get_next_token(code: &str, cursor: &mut usize, file: &PathBuf) -> Option<Token> {
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
        _ => Some(Token::new(value, TokenType::Identifier, location)),
    }
}

fn get_location(code: &str, cursor: usize, file: &PathBuf) -> Location {
    let mut row = 1;
    let mut last_line_start_index = 0;

    for (i, char) in code.char_indices() {
        if i >= cursor {
            break;
        }
        if char == NEWLINE {
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

fn parse_until_whitespace_or_delimiter<'a>(code: &'a str, cursor: &'a mut usize) -> &'a str {
    let start = *cursor;
    for (i, char) in code[start..].char_indices() {
        if char.is_whitespace() || DELIMITERS.get(&char).is_some() {
            *cursor = start + i;
            return &code[start..start + i];
        }
    }

    *cursor = code.len();
    &code[start..]
}

fn parse_string_literal_token(code: &str, cursor: &mut usize, file: &PathBuf) -> Option<Token> {
    let location = get_location(code, *cursor, file);
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
                    location,
                ));
            }
            _ => {}
        }
    }

    None
}
