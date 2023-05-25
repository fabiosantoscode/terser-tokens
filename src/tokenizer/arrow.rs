use nom::{bytes::complete::tag, IResult};

use crate::tokenizer::tokens::JsToken;

pub fn handle_arrow(input: &str) -> IResult<&str, JsToken> {
    let (input, _) = tag("=>")(input)?;
    Ok((input, JsToken::Arrow))
}
