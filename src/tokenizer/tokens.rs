use serde::{Deserialize, Serialize};
use wasm_bindgen::JsValue;

#[derive(Debug, PartialEq, Clone, Copy, Serialize, Deserialize)]
pub enum CommentKind {
    Comment1,
    Comment2,
    Comment3,
    Comment4,
    Comment5,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct Comment {
    pub kind: CommentKind,
    pub value: String,
}

impl Comment {
    pub fn line_comment(value: &str) -> Comment {
        Comment {
            kind: CommentKind::Comment1,
            value: value.to_string(),
        }
    }
    pub fn multiline_comment(value: &str) -> Comment {
        Comment {
            kind: CommentKind::Comment2,
            value: value.to_string(),
        }
    }
    pub fn html_comment_start(value: &str) -> Comment {
        Comment {
            kind: CommentKind::Comment3,
            value: value.to_string(),
        }
    }
    pub fn html_comment_end(value: &str) -> Comment {
        Comment {
            kind: CommentKind::Comment4,
            value: value.to_string(),
        }
    }
    pub fn shebang_comment(value: &str) -> Comment {
        Comment {
            kind: CommentKind::Comment5,
            value: value.to_string(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum JsToken {
    String(String),
    Num(f64),
    BigInt(String),
    Name(String),
    Keyword(String),
    Atom(String),
    Regexp(String, String),
    Punc(String),
    Operator(String),
    Expand,
    Comment(Comment),
    Arrow,
    Private(String),
    TemplateString(String, bool, bool),

    EOF,
}

impl JsToken {
    pub fn is_eof(&self) -> bool {
        match self {
            JsToken::EOF => true,
            _ => false,
        }
    }

    pub fn type_str(&self) -> &'static str {
        match self {
            JsToken::String(_) => "string",
            JsToken::Num(_) => "num",
            JsToken::BigInt(_) => "big_int",
            JsToken::Name(_) => "name",
            JsToken::Keyword(_) => "keyword",
            JsToken::Atom(_) => "atom",
            JsToken::Regexp(_, _) => "regexp",
            JsToken::Punc(_) => "punc",
            JsToken::Operator(_) => "operator",
            JsToken::Expand => "expand",
            JsToken::Comment(_) => "comment",
            JsToken::Arrow => "arrow",
            JsToken::Private(_) => "private",
            JsToken::TemplateString(_, is_begin, _) => {
                if *is_begin {
                    "template_head"
                } else {
                    "template_substitution"
                }
            }
            JsToken::EOF => "eof",
        }
    }

    pub fn to_wasm_bindgen_jsvalue(&self) -> JsValue {
        match self {
            /*JsToken::Punc(_) | JsToken::Operator(_)  | JsToken::Keyword(_)  | JsToken::Atom(_) | JsToken::Arrow | JsToken::Expand | JsToken::Name(_) => {
                serde_wasm_bindgen::to_value(&()).unwrap()
            },*/
            _ => return serde_wasm_bindgen::to_value(&self).unwrap(),
        }
    }
}
