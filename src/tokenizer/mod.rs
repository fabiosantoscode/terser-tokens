use escapes::read_escaped_char;
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag, take_while, take_while1, take_while_m_n},
    character::complete::{anychar, char},
    combinator::{cut, map, recognize},
    multi::fold_many0,
    sequence::{preceded, terminated},
    IResult,
};
use unicode_ident::{is_xid_continue, is_xid_start};

pub(crate) mod arrow;
pub(crate) mod comments;
pub(crate) mod escapes;
pub(crate) mod misc;
pub(crate) mod next_token;
pub(crate) mod number;
pub(crate) mod private;
pub(crate) mod punc_operators;
pub(crate) mod template_string;
pub(crate) mod tests;
pub(crate) mod tokenizer;
pub(crate) mod tokens;

use crate::tokenizer::tokens::JsToken;

fn string_contents(input: &str, quote: char) -> IResult<&str, String> {
    fold_many0(
        alt((
            |s| read_escaped_char(s, false),
            map(take_while1(|c| c != quote && c != '\\'), String::from), // Stops fold_many0 at the quote
        )),
        String::new,
        |mut s, c: String| {
            s.push_str(&c);
            s
        },
    )(input)
}

fn handle_string_inner(input: &str) -> IResult<&str, String> {
    alt((
        preceded(
            char('\"'),
            cut(terminated(|inp| string_contents(inp, '\"'), char('\"'))),
        ),
        preceded(
            char('\''),
            cut(terminated(|inp| string_contents(inp, '\''), char('\''))),
        ),
    ))(input)
}

fn handle_string(input: &str) -> IResult<&str, JsToken> {
    let (input, string) = handle_string_inner(input)?;

    Ok((input, JsToken::String(string)))
}

fn handle_ident_string(input: &str) -> IResult<&str, String> {
    let (input, first) = alt((
        map(
            take_while_m_n(1, 1, |c: char| is_xid_start(c) || c == '$' || c == '_'),
            |s: &str| s.to_string(),
        ),
        |input| read_escaped_char(input, false),
    ))(input)?;

    let (input, rest) = fold_many0(
        alt((
            map(
                take_while_m_n(1, 1, |c: char| is_xid_continue(c) || c == '$' || c == '_'),
                |s: &str| s.to_string(),
            ),
            |input| read_escaped_char(input, false),
        )),
        String::new,
        |mut s, c: String| {
            s.push_str(&c);
            s
        },
    )(input)?;

    Ok((input, format!("{}{}", first, rest)))
}

fn handle_ident(input: &str) -> IResult<&str, JsToken> {
    let (input, ident) = handle_ident_string(input)?;

    // actually an operator
    if let "in" | "instanceof" | "typeof" | "new" | "void" | "delete" = ident.as_str() {
        return Ok((input, JsToken::Operator(ident)));
    }

    // actually an atom
    if let "false" | "null" | "true" = ident.as_str() {
        return Ok((input, JsToken::Atom(ident)));
    }

    // actually a keyword
    if let "break" | "case" | "catch" | "class" | "const" | "continue" | "debugger" | "default"
    | "delete" | "do" | "else" | "export" | "extends" | "finally" | "for" | "function"
    | "if" | "in" | "instanceof" | "let" | "new" | "return" | "switch" | "throw" | "try"
    | "typeof" | "var" | "void" | "while" | "with" = ident.as_str()
    {
        return Ok((input, JsToken::Keyword(ident)));
    }

    // actually a name this time, I promise
    Ok((input, JsToken::Name(ident)))
}

fn handle_regex_brackets(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("[")(input)?;
    let (input, _) = take_while(|c: char| c != ']')(input)?;
    let (input, _) = tag("]")(input)?;

    Ok((input, input))
}

fn handle_regex_pattern(mut input: &str) -> IResult<&str, ()> {
    loop {
        if let Ok((_, _)) = tag::<&str, &str, nom::error::Error<&str>>("/")(input) {
            return Ok((input, ()));
        }

        if let Ok((inp, _)) = handle_regex_brackets(input) {
            input = inp;
            continue;
        }

        if let Ok((inp, _)) = tag::<&str, &str, nom::error::Error<&str>>("\\")(input) {
            input = inp;
            input = anychar(input)?.0;
            continue;
        }

        input = anychar(input)?.0;
    }
}

fn handle_regex(input: &str) -> IResult<&str, JsToken> {
    let (input, _) = tag("/")(input)?;
    let (input, pattern) = recognize(escaped(handle_regex_pattern, '\\', anychar))(input)?;
    let (input, _) = tag("/")(input)?;
    let (input, flags) = take_while(|c: char| c.is_alphabetic())(input)?;

    Ok((
        input,
        JsToken::Regexp(pattern.to_string(), flags.to_string()),
    ))
}

#[test]
fn test_main() {
    assert_eq!(
        handle_string_inner("\"abc\"").unwrap().1,
        ("abc".to_string())
    );
    assert_eq!(
        handle_string_inner("\'a\\\'bc\'").unwrap().1,
        ("a\'bc".to_string())
    );
    assert_eq!(
        handle_string_inner("\"a\\\"bc\"").unwrap().1,
        ("a\"bc".to_string())
    );
    assert_eq!(
        handle_string_inner("'he\\'\"llo'?.world").unwrap().1,
        ("he'\"llo".to_string())
    );
    assert_eq!(
        handle_string_inner("\"a\\bc\"").unwrap().1,
        ("a\u{8}c".to_string())
    );

    assert_eq!(
        handle_ident("$.").unwrap(),
        (".", JsToken::Name("$".to_string()))
    );
    assert_eq!(
        handle_ident("A1ಠ_ಠ23.").unwrap(),
        (".", JsToken::Name("A1ಠ_ಠ23".to_string()))
    );

    assert_eq!(
        handle_regex("/abc/.").unwrap(),
        (".", JsToken::Regexp("abc".to_string(), "".to_string()))
    );
    assert_eq!(
        handle_regex("/abc[lol]/lol.").unwrap(),
        (
            ".",
            JsToken::Regexp("abc[lol]".to_string(), "lol".to_string())
        )
    );
    assert_eq!(
        handle_regex("/ab\\/c[lol/]/lol.").unwrap(),
        (
            ".",
            JsToken::Regexp("ab\\/c[lol/]".to_string(), "lol".to_string())
        )
    );
    assert_eq!(
        handle_regex("/abc\\[lol/]/lol.").unwrap(),
        (
            "]/lol.",
            JsToken::Regexp("abc\\[lol".to_string(), "".to_string())
        )
    );
}

#[test]
fn test_nonterminated_strings_fail() {
    assert!(matches!(
        handle_string_inner("\"abc").unwrap_err(),
        nom::Err::Failure(_)
    ));
    assert!(matches!(
        handle_string_inner("'abc\n").unwrap_err(),
        nom::Err::Failure(_)
    ));
}
