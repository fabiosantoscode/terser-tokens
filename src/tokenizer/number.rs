use std::str::FromStr;

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::anychar,
    combinator::{map, opt, peek, verify},
    sequence::preceded,
    IResult,
};

use crate::tokenizer::tokens::JsToken;

fn zero_not_followed_by_digit(input: &str) -> IResult<&str, &str> {
    let (input, _) = tag("0")(input)?;
    let (input, _) = peek(verify(anychar, |c| !c.is_digit(10)))(input)?;

    Ok((input, "0"))
}

fn handle_number_base(input: &str) -> IResult<&str, (&str, u32)> {
    alt((
        map(tag("0x"), |s| (s, 16)),
        map(tag("0b"), |s| (s, 2)),
        map(tag("0o"), |s| (s, 8)),
        map(peek(zero_not_followed_by_digit), |s| (s, 10)),
        map(peek(tag("0")), |s| (s, 8)),
        map(peek(handle_number_digits10), |_| ("", 10)),
        map(tag("."), |_| ("", 0)),
    ))(input)
}

fn handle_number_digits<'a>(input: &str, base: u32) -> IResult<&str, String> {
    let (input, digits) = take_while1(|c: char| c.is_digit(base) || c == '_')(input)?;
    Ok((input, digits.replace("_", "")))
}

fn handle_number_digits10<'a>(input: &str) -> IResult<&str, String> {
    handle_number_digits(input, 10)
}

fn handle_exp(input: &str) -> IResult<&str, (bool, u32)> {
    let (input, _) = alt((tag("e"), tag("E")))(input)?;
    let (input, sign) = opt(alt((tag("+"), tag("-"))))(input)?;
    let (input, exp) = handle_number_digits10(input)?;

    let exp_f = if let Some("-") = sign {
        (true, u32::from_str(exp.as_str()).unwrap())
    } else {
        (false, u32::from_str(exp.as_str()).unwrap())
    };

    Ok((input, exp_f))
}

pub(crate) fn handle_number(input: &str) -> IResult<&str, JsToken> {
    let (input, (base_str, base)) = handle_number_base(input)?;

    if base == 0 {
        let (input, s) = handle_number_digits(input, 10)?;

        let order_of_magnitude = f64::powf(10.0, -(s.len() as f64));
        let n = f64::from_str(s.as_str()).expect("handle_number_digits is always a base10 num");
        let n = order_of_magnitude * n;

        return Ok((input, JsToken::Num(n)));
    }

    let (input, s) = handle_number_digits(input, base)?;

    let (input, decimal_part) =
        if let Ok((input, s)) = preceded(tag("."), handle_number_digits10)(input) {
            (input, Some(u64::from_str_radix(s.as_str(), 10).unwrap()))
        } else if let Ok((after_dot, _)) = tag::<&str, &str, nom::error::Error<&str>>(".")(input) {
            (if base == 10 { after_dot } else { input }, None)
        } else if let Ok((input, _)) = tag::<&str, &str, nom::error::Error<&str>>("n")(input) {
            return Ok((input, JsToken::BigInt(format!("{}{}", base_str, s))));
        } else {
            (input, None)
        };

    let (input, _) = if base == 10 && decimal_part.is_none() {
        opt(tag("0"))(input)?
    } else {
        (input, None)
    };

    if let Some(decimal_part) = decimal_part {
        let num = f64::from_str(format!("{}.{}", s.to_string(), decimal_part).as_str()).unwrap();

        Ok((input, JsToken::Num(num)))
    } else {
        let mut num = i64::from_str_radix(&s, base).unwrap() as f64;

        if let (input, Some((neg, exp))) = opt(handle_exp)(input)? {
            num = (num as f64) * f64::powf(10.0, if neg { -(exp as f64) } else { exp as f64 });

            Ok((input, JsToken::Num(num as f64)))
        } else {
            Ok((input, JsToken::Num(num as f64)))
        }
    }
}

#[test]
fn test_0x() {
    assert_eq!(handle_number("0x10\n").unwrap(), ("\n", JsToken::Num(16.0)));
    assert_eq!(
        handle_number("0x10.toString\n").unwrap(),
        (".toString\n", JsToken::Num(16.0))
    );
    assert_eq!(
        handle_number("0x1e1\n").unwrap(),
        ("\n", JsToken::Num(481.0))
    );
}

#[test]
fn test_bigint() {
    assert_eq!(
        handle_number("21nx").unwrap(),
        ("x", JsToken::BigInt("21".into()))
    );
    assert_eq!(
        handle_number("0xffn").unwrap(),
        ("", JsToken::BigInt("0xff".into()))
    );
    assert_eq!(
        handle_number("21nx").unwrap(),
        ("x", JsToken::BigInt("21".into()))
    );
    assert_eq!(
        handle_number("2_1nx").unwrap(),
        ("x", JsToken::BigInt("21".into()))
    );
}

#[test]
fn leading_trailing_dot() {
    assert_eq!(
        handle_number(".123\n").unwrap(),
        ("\n", JsToken::Num(0.123))
    );
    assert_eq!(
        handle_number("0.\n").unwrap(),
        ("\n", JsToken::Num(0.0))
    );
}

#[test]
fn test_e() {
    assert_eq!(handle_number("1e3").unwrap(), ("", JsToken::Num(1000.0)));
    assert_eq!(
        handle_number("1e-3\n").unwrap(),
        ("\n", JsToken::Num(0.001))
    );
    assert_eq!(
        handle_number("1e3.toString\n").unwrap(),
        (".toString\n", JsToken::Num(1000.0))
    );
}

#[test]
fn test_main() {
    assert_eq!(
        handle_number("0!").unwrap(),
        ("!", JsToken::Num(0.0))
    );
    assert_eq!(handle_number("0x10").unwrap(), ("", JsToken::Num(16.0)));
    assert_eq!(handle_number("010x").unwrap(), ("x", JsToken::Num(8.0)));
    assert_eq!(handle_number("10.35x").unwrap(), ("x", JsToken::Num(10.35)));
    assert_eq!(
        handle_number("10..toString").unwrap(),
        (".toString", JsToken::Num(10.0))
    );
    assert_eq!(
        handle_number("10.0..toString").unwrap(),
        ("..toString", JsToken::Num(10.0))
    );
}
