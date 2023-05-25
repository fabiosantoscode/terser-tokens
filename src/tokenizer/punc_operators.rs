use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    character::complete::{anychar, digit1},
    combinator::{map, not, verify},
    IResult,
};

use crate::tokenizer::tokens::JsToken;

pub(crate) fn handle_punc_and_operators(input: &str) -> IResult<&str, JsToken> {
    alt((
        handle_dot,
        handle_optional_dot,
        handle_nondot_punc,
        match_operator,
    ))(input)
}

pub(crate) fn handle_optional_dot(input: &str) -> IResult<&str, JsToken> {
    let (input, _) = tag("?")(input)?;
    let (input, _) = tag(".")(input)?;
    let (input, _) = not(digit1)(input)?;

    Ok((input, JsToken::Punc("?.".into())))
}

pub(crate) fn handle_dot(input: &str) -> IResult<&str, JsToken> {
    let (input, _) = tag(".")(input)?;
    let _ = not(digit1)(input)?;

    if let Ok((input, _)) = tag::<&str, &str, nom::error::Error<&str>>("..")(input) {
        return Ok((input, JsToken::Expand)); // "..." token
    }

    Ok((input, JsToken::Punc(".".into())))
}

fn handle_nondot_punc(input: &str) -> IResult<&str, JsToken> {
    map(
        verify(anychar, |c| {
            matches!(c, '[' | ']' | '{' | '}' | '(' | ')' | '|' | ';' | ':' | ',')
        }),
        |c: char| JsToken::Punc(String::from(c)),
    )(input)
}

macro_rules! tails {
    ($input: ident, $tail: literal, $($rest:literal),*) => {{
        let input: &str = $input;

        if input.starts_with($tail) {
            Some($tail.len())
        } else {
            tails!(input, $($rest),*)
        }
    }};
    ($input: ident, $tail: literal) => {{
        let input: &str = $input;

        if input.starts_with($tail) {
            Some($tail.len())
        } else {
            None
        }
    }}
}

fn match_operator(input: &str) -> IResult<&str, JsToken> {
    let chomp = match anychar(input)?.1 {
        // Math ops
        '+' => tails!(input, "+=", "++", "+"),
        '-' => tails!(input, "--", "-=", "-"),
        '*' => tails!(input, "**=", "**", "*=", "*"),
        '/' => tails!(input, "/=", "/"),
        '%' => tails!(input, "%=", "%"),
        // Comparisons
        '=' => tails!(input, "===", "==", "="),
        '!' => tails!(input, "!==", "!=", "!"),
        // Gt, Lt, Binary
        '>' => tails!(input, ">>>=", ">>>", ">>=", ">>", ">=", ">"),
        '<' => tails!(input, "<=", "<<=", "<<", "<"),
        '~' => tails!(input, "~"),
        // And, Or, Binary-and, Binary-or, Xor
        '|' => tails!(input, "||=", "||", "|=", "|"),
        '&' => tails!(input, "&&=", "&&", "&=", "&"),
        '^' => tails!(input, "^=", "^"),
        // ??, ?, ??=
        '?' => tails!(input, "??=", "??", "?"),
        _ => None,
    };

    if let Some(chomp) = chomp {
        let (input, operator) = take(chomp)(input)?;

        Ok((input, JsToken::Operator(operator.into())))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )))
    }
}

#[test]
fn test_handle_punc_and_operators() {
    assert_eq!(
        handle_punc_and_operators("?."),
        Ok(("", JsToken::Punc("?.".into())))
    );
    assert_eq!(
        handle_punc_and_operators("?.1"),
        Ok((".1", JsToken::Operator("?".into())))
    );
    assert_eq!(
        handle_punc_and_operators(">!"),
        Ok(("!", JsToken::Operator(">".into())))
    );
    assert_eq!(
        handle_punc_and_operators(">>>=!"),
        Ok(("!", JsToken::Operator(">>>=".into())))
    );
    assert_eq!(
        handle_punc_and_operators("!"),
        Ok(("", JsToken::Operator("!".into())))
    );
}

#[test]
fn test_dot_optional_dot() {
    assert_eq!(
        handle_optional_dot("?."),
        Ok(("", JsToken::Punc("?.".into())))
    );
    assert_eq!(handle_dot("."), Ok(("", JsToken::Punc(".".into()))));

    assert!(handle_optional_dot("?.0").is_err());
    assert!(handle_dot(".1").is_err());
    assert!(handle_optional_dot("? .").is_err());
    assert!(handle_optional_dot("?.1").is_err());
}
