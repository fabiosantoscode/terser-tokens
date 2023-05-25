/**
var read_template_characters = with_eof_error("Unterminated template", function(begin) {
    if (begin) {
        S.template_braces.push(S.brace_counter);
    }
    var content = "", ch, tok;
    next(true, true);
    while ((ch = next(true, true)) != "`") {
        if (ch == "$" && peek() == "{") {
            next(true, true);
            S.brace_counter++;
            tok = token(begin ? "template_head" : "template_substitution", content);
            tok.template_end = false;
            return tok;
        }

        if (ch == "\\") {
            var tmp = S.pos;
            var prev_is_tag = previous_token && (previous_token.type === "name" || previous_token.type === "punc" && (previous_token.value === ")" || previous_token.value === "]"));
            ch = read_escaped_char(true, !prev_is_tag, true);
        }

        content += ch;
    }
    S.template_braces.pop();
    tok = token(begin ? "template_head" : "template_substitution", content);
    tok.template_end = true;
    return tok;
});
*/
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::anychar,
    combinator::{cut, map, peek},
    multi::fold_many0,
    sequence::preceded,
    IResult,
};

use crate::tokenizer::{escapes::read_escaped_char, tokens::JsToken};

fn handle_dollar(input: &str) -> IResult<&str, String> {
    let (input, _) = tag("$")(input)?;
    let (input, next) = peek(anychar)(input)?;

    match next {
        // Break, such that we can handle the "${" case in read_template_string_inner
        '{' => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
        _ => Ok((input, "$".to_string())),
    }
}

pub(crate) fn read_template_string_inner(input: &str, is_begin: bool) -> IResult<&str, JsToken> {
    let (input, string_contents) = fold_many0(
        alt((
            map(
                take_while1(|c| !matches!(c, '`' | '$' | '\\')),
                |c: &str| c.to_string(),
            ),
            |input| read_escaped_char(input, false),
            handle_dollar,
        )),
        String::new,
        |mut s: String, c: String| {
            s.push_str(&c);
            s
        },
    )(input)?;

    let (input, is_end) = alt((map(tag("`"), |_| true), map(tag("${"), |_| false)))(input)?;

    Ok((
        input,
        JsToken::TemplateString(string_contents, is_begin, is_end),
    ))
}

// Called by next_token, may or may not be a template string depending on "`"
pub(crate) fn handle_template_string_begin(input: &str) -> IResult<&str, JsToken> {
    preceded(
        tag("`"),
        cut(|input| read_template_string_inner(input, true)),
    )(input)
}

// Called by tokenizer when it sees a "}" and knows it's for a template string
pub(crate) fn handle_template_string_continue(input: &str) -> IResult<&str, JsToken> {
    preceded(
        tag("}"),
        cut(|input| read_template_string_inner(input, false)),
    )(input)
}

#[test]
fn test_read_template_string_begin() {
    insta::assert_debug_snapshot!(handle_template_string_begin("`f`").unwrap(),
        @r###"
    (
        "",
        TemplateString(
            "f",
            true,
            true,
        ),
    )
    "###
    );
    insta::assert_debug_snapshot!(handle_template_string_begin("``").unwrap(),
        @r###"
    (
        "",
        TemplateString(
            "",
            true,
            true,
        ),
    )
    "###
    );
    insta::assert_debug_snapshot!(handle_template_string_begin("`$`").unwrap(),
        @r###"
    (
        "",
        TemplateString(
            "$",
            true,
            true,
        ),
    )
    "###
    );
    insta::assert_debug_snapshot!(handle_template_string_begin("`f${").unwrap(),
        @r###"
    (
        "",
        TemplateString(
            "f",
            true,
            false,
        ),
    )
    "###
    );
    insta::assert_debug_snapshot!(handle_template_string_begin("`$${").unwrap(),
        @r###"
    (
        "",
        TemplateString(
            "$",
            true,
            false,
        ),
    )
    "###
    );
}

#[test]
fn test_read_template_string_continue() {
    insta::assert_debug_snapshot!(handle_template_string_continue("}f`").unwrap(),
        @r###"
    (
        "",
        TemplateString(
            "f",
            false,
            true,
        ),
    )
    "###
    );
    insta::assert_debug_snapshot!(handle_template_string_continue("}}`").unwrap(),
        @r###"
    (
        "",
        TemplateString(
            "}",
            false,
            true,
        ),
    )
    "###
    );
    insta::assert_debug_snapshot!(handle_template_string_continue("}$`").unwrap(),
        @r###"
    (
        "",
        TemplateString(
            "$",
            false,
            true,
        ),
    )
    "###
    );
    insta::assert_debug_snapshot!(handle_template_string_continue("}f${").unwrap(),
        @r###"
    (
        "",
        TemplateString(
            "f",
            false,
            false,
        ),
    )
    "###
    );
    insta::assert_debug_snapshot!(handle_template_string_continue("}}${").unwrap(),
        @r###"
    (
        "",
        TemplateString(
            "}",
            false,
            false,
        ),
    )
    "###
    );
    insta::assert_debug_snapshot!(handle_template_string_continue("}$${").unwrap(),
        @r###"
    (
        "",
        TemplateString(
            "$",
            false,
            false,
        ),
    )
    "###
    );
}

#[test]
fn test_read_template_string_unexpected_end() {
    insta::assert_debug_snapshot!(handle_template_string_begin("`").unwrap_err(),
        @r###"
    Failure(
        Error {
            input: "",
            code: Tag,
        },
    )
    "###
    );
    insta::assert_debug_snapshot!(handle_template_string_begin("`$").unwrap_err(),
        @r###"
    Failure(
        Error {
            input: "$",
            code: Tag,
        },
    )
    "###
    );
}
