use nom::{bytes::complete::tag, IResult};

use crate::tokenizer::{handle_ident_string, tokens::JsToken};

pub(crate) fn handle_private(input: &str) -> IResult<&str, JsToken> {
    let (input, _) = tag("#")(input)?;
    let (input, ident) = handle_ident_string(input)?;

    Ok((input, JsToken::Private(ident)))
}
