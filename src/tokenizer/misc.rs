pub(crate) fn own_error(
    input: nom::Err<nom::error::Error<&str>>,
) -> nom::Err<nom::error::Error<String>> {
    match input {
        nom::Err::Error(e) => nom::Err::Error(nom::error::Error {
            input: e.input.to_string(),
            code: e.code,
        }),
        nom::Err::Failure(e) => nom::Err::Failure(nom::error::Error {
            input: e.input.to_string(),
            code: e.code,
        }),
        nom::Err::Incomplete(e) => {
            unreachable!("nom::Err::Incomplete({:?}) found", e)
        }
    }
}

pub(crate) fn is_ws(c: char) -> bool {
    matches!(
        c,
        ' ' | '\n'
            | '\r'
            | '\u{0009}'
            | '\u{000c}'
            | '\u{00a0}'
            | '\u{000b}'
            | '\u{200b}'
            | '\u{2000}'
            | '\u{2001}'
            | '\u{2002}'
            | '\u{2003}'
            | '\u{2004}'
            | '\u{2005}'
            | '\u{2006}'
            | '\u{2007}'
            | '\u{2008}'
            | '\u{2009}'
            | '\u{200a}'
            | '\u{2028}'
            | '\u{2029}'
            | '\u{202f}'
            | '\u{205f}'
            | '\u{3000}'
            | '\u{FEFF}'
    )
}
