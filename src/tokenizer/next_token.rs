use nom::{branch::alt, IResult};

use crate::tokenizer::{
    arrow::handle_arrow, handle_ident, handle_regex, handle_string, number::handle_number,
    private::handle_private, punc_operators::handle_punc_and_operators,
    template_string::handle_template_string_begin, tokens::JsToken,
};

pub fn next_token(input: &str) -> IResult<&str, JsToken> {
    alt((
        handle_string,
        handle_template_string_begin,
        handle_ident,
        handle_private,
        handle_arrow,
        handle_punc_and_operators,
        handle_regex,
        handle_number,
    ))(input)
}
