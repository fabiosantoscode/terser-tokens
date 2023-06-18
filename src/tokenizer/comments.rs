use crate::tokenizer::tokens::Comment;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    combinator::{cut, map},
    sequence::{delimited, preceded},
    IResult,
};

fn till_eol(input: &str) -> IResult<&str, &str> {
    take_while(|c: char| c != '\n')(input)
}

pub(crate) fn handle_comment(input: &str, html5_comments: bool) -> IResult<&str, Comment> {
    let oneline = map(preceded(tag("//"), till_eol), Comment::line_comment);
    let multiline = map(
        delimited(tag("/*"), cut(take_until("*/")), tag("*/")),
        Comment::multiline_comment,
    );

    if html5_comments {
        let html_start = map(preceded(tag("<!--"), till_eol), Comment::html_comment_start);
        let html_end = map(preceded(tag("-->"), till_eol), Comment::html_comment_end);
        alt((html_start, html_end, oneline, multiline))(input)
    } else {
        alt((oneline, multiline))(input)
    }
}

pub(crate) fn handle_shebang(input: &str) -> IResult<&str, Comment> {
    map(preceded(tag("#!"), till_eol), Comment::shebang_comment)(input)
}

#[test]
fn test_comment() {
    use crate::tokenizer::tokens::CommentKind;

    assert_eq!(
        handle_comment("// hello world", true).unwrap(),
        (
            "",
            Comment {
                kind: CommentKind::Comment1,
                value: " hello world".to_string()
            }
        )
    );
    assert_eq!(
        handle_comment("/* hello/* world*/", true).unwrap(),
        (
            "",
            Comment {
                kind: CommentKind::Comment2,
                value: " hello/* world".to_string()
            }
        )
    );
    assert_eq!(
        handle_comment("<!--comment\nxx", true).unwrap(),
        (
            "\nxx",
            Comment {
                kind: CommentKind::Comment3,
                value: "comment".to_string()
            }
        )
    );
    assert_eq!(
        handle_comment("-->comment\nxx", true).unwrap(),
        (
            "\nxx",
            Comment {
                kind: CommentKind::Comment4,
                value: "comment".to_string()
            }
        )
    );
}

#[test]
fn multiline_must_yield_failure() {
    assert!(matches!(
        handle_comment("/* hello ", true).unwrap_err(),
        nom::Err::Failure(_)
    ));
    assert!(matches!(
        handle_comment("notcomment", true).unwrap_err(),
        nom::Err::Error(_)
    ));
}
