/*
function read_escaped_char(in_string, strict_hex, template_string) {
    var ch = next(true, in_string);
    switch (ch.charCodeAt(0)) {
      case 110 : return "\n";
      case 114 : return "\r";
      case 116 : return "\t";
      case 98  : return "\b";
      case 118 : return "\u000b"; // \v
      case 102 : return "\f";
      case 120 : return String.fromCharCode(hex_bytes(2, strict_hex)); // \x
      case 117 : // \u
        if (peek() == "{") {
            next(true);
            if (peek() === "}")
                handle_error("Expecting hex-character between {}");
            while (peek() == "0") next(true); // No significance
            var result, length = find("}", true) - S.pos;
            // Avoid 32 bit integer overflow (1 << 32 === 1)
            // We know first character isn't 0 and thus out of range anyway
            if (length > 6 || (result = hex_bytes(length, strict_hex)) > 0x10FFFF) {
                handle_error("Unicode reference out of bounds");
            }
            next(true);
            return from_char_code(result);
        }
        return String.fromCharCode(hex_bytes(4, strict_hex));
      case 10  : return ""; // newline
      case 13  :            // \r
        if (peek() == "\n") { // DOS newline
            next(true, in_string);
            return "";
        }
    }
    if (is_octal(ch)) {
        if (template_string && strict_hex) {
            const represents_null_character = ch === "0" && !is_octal(peek());
            if (!represents_null_character) {
                handle_error("Octal escape sequences are not allowed in template strings");
            }
        }
        return read_octal_escape_sequence(ch, strict_hex);
    }
    return ch;
}

function read_octal_escape_sequence(ch, strict_octal) {
    // Read
    var p = peek();
    if (p >= "0" && p <= "7") {
        ch += next(true);
        if (ch[0] <= "3" && (p = peek()) >= "0" && p <= "7")
            ch += next(true);
    }

    // Parse
    if (ch === "0") return "\0";
    if (ch.length > 0 && next_token.has_directive("use strict") && strict_octal)
        handle_error("Legacy octal escape sequences are not allowed in strict mode");
    return String.fromCharCode(parseInt(ch, 8));
}
*/

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while_m_n},
    character::complete::anychar,
    combinator::{map, recognize, verify},
    sequence::{delimited, tuple},
    IResult,
};

fn n_hex_digits<'a>(
    min: usize,
    max: usize,
    jump_zeroes: bool,
    input: &'a str,
) -> IResult<&'a str, Option<char>> {
    let (input, hex) = if jump_zeroes {
        verify(
            recognize(tuple((
                take_while(|c| c == '0'),
                take_while_m_n(0, max, is_hex),
            ))),
            |hex: &str| hex.len() > 0,
        )(input)?
    } else {
        take_while_m_n(min, max, is_hex)(input)?
    };

    let as_u32 = u32::from_str_radix(hex, 16).ok().and_then(char::from_u32);

    match as_u32 {
        Some(ch) => Ok((input, Some(ch))),
        None => Err(nom::Err::Error(nom::error::Error {
            input,
            code: nom::error::ErrorKind::HexDigit,
        })),
    }
}

pub fn read_escaped_char(input: &str, strict_hex: bool) -> IResult<&str, String> {
    let (input, ch) = read_escaped_char_inner(input, strict_hex)?;
    match ch {
        Some(ch) => Ok((input, ch.to_string())),
        None => Ok((input, String::new())),
    }
}

pub fn read_escaped_char_inner(input: &str, strict_hex: bool) -> IResult<&str, Option<char>> {
    let (input_before_ch, _) = tag("\\")(input)?;
    let (input, ch) = anychar(input_before_ch)?;

    match ch {
        'n' => Ok((input, Some('\n'))),
        'r' => Ok((input, Some('\r'))),
        't' => Ok((input, Some('\t'))),
        'b' => Ok((input, Some('\x08'))),
        'v' => Ok((input, Some('\x0b'))),
        'f' => Ok((input, Some('\x0c'))),
        'x' => n_hex_digits(2, 2, false, input),
        'u' => {
            alt((
                // \u{...}
                delimited(tag("{"), |inp| n_hex_digits(1, 6, true, inp), tag("}")),
                |inp| n_hex_digits(4, 4, false, inp),
            ))(input)
        }
        // newline after backslash
        '\n' => Ok((input, None)),
        // CRLF after backslash
        '\r' => {
            // \r
            let (input_peeked, ch) = anychar(input)?;
            if ch == '\n' {
                Ok((input_peeked, None))
            } else {
                Ok((input, Some('\r')))
            }
        }
        octal_ch if is_octal(octal_ch) => {
            let (input, oct) = read_octal_escape_sequence(input_before_ch, strict_hex)?;

            Ok((input, Some(oct)))
        }
        ch_itself => Ok((input, Some(ch_itself))),
    }
}

fn read_octal_escape_sequence(input: &str, strict_hex: bool) -> IResult<&str, char> {
    map(
        alt((
            // A digit below 4, followed by 2 digits (max 377, 255 in decimal)
            recognize(tuple((
                take_while_m_n(1, 1, is_octal_head_of_3_digit_byte),
                take_while_m_n(1, 2, is_octal),
            ))),
            // 1 or 2 octal digits
            recognize(take_while_m_n(1, 2, is_octal)),
        )),
        |octal_str: &str| {
            u8::from_str_radix(octal_str, 8).expect("can only parse up to 255") as u8 as char
        },
    )(input)
}

fn is_octal(ch: char) -> bool {
    matches!(ch, '0'..='7')
}

fn is_octal_head_of_3_digit_byte(ch: char) -> bool {
    matches!(ch, '0'..='3')
}

fn is_hex(ch: char) -> bool {
    matches!(ch, 'a'..='f' | 'A'..='F' | '0'..='9')
}

#[test]
fn test_escaped_char() {
    assert_eq!(
        read_escaped_char_inner("\\nx", false),
        Ok(("x", Some('\n')))
    );
    assert_eq!(read_escaped_char_inner("\\\nx", false), Ok(("x", None)));
    assert_eq!(
        read_escaped_char_inner("\\x7Fx", false),
        Ok(("x", Some('\x7F')))
    );
    assert_eq!(
        read_escaped_char_inner("\\uFFFFx", false),
        Ok(("x", Some('\u{ffff}')))
    );
    assert_eq!(
        read_escaped_char_inner("\\u{BeeF}x", false),
        Ok(("x", Some('\u{beef}')))
    );
    assert_eq!(
        read_escaped_char_inner("\\u{00000001}x", false),
        Ok(("x", Some('\u{1}')))
    );
    assert_eq!(
        read_escaped_char_inner("\\u{00}x", false),
        Ok(("x", Some('\u{0}')))
    );
    assert_eq!(
        read_escaped_char_inner("\\0x", false),
        Ok(("x", Some('\u{0}')))
    );
    assert_eq!(
        read_escaped_char_inner("\\'x", false),
        Ok(("x", Some('\'')))
    );
    assert_eq!(
        read_escaped_char_inner("\\\"x", false),
        Ok(("x", Some('"')))
    );
    assert!(read_escaped_char_inner("\\u{}x", false).is_err());
    assert!(read_escaped_char_inner("\\u{1234567}x", false).is_err());
    assert_eq!(read_escaped_char_inner("\\\r\nx", false), Ok(("x", None)));
}

#[test]
fn test_octal_escape_sequence() {
    assert_eq!(read_octal_escape_sequence("0n", false), Ok(("n", ('\0'))));
    assert_eq!(read_octal_escape_sequence("275x", false), Ok(("x", ('½'))));
    assert_eq!(
        read_octal_escape_sequence("475x", false),
        Ok(("5x", ('\'')))
    );
}
