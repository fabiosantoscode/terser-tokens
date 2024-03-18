use nom::IResult;

use crate::basic_blocks::{BreakableId, ExitType, StructuredFlow};

use super::parse_single_instruction;

pub fn parse_structured_flow(input: &str) -> StructuredFlow {
    use nom::branch::*;
    use nom::bytes::complete::*;
    use nom::character::complete::*;
    use nom::combinator::*;
    use nom::multi::*;
    use nom::sequence::*;

    macro_rules! whitespace {
        ($input:ident) => {
            multispace0($input)?.0
        };
    }

    fn parse_brk(input: &str) -> IResult<&str, BreakableId> {
        let input = whitespace!(input);
        let (input, n) = opt(delimited(tag("("), preceded(char('@'), digit1), tag(")")))(input)?;

        Ok((input, BreakableId(n.map(|n| n.parse().unwrap()))))
    }

    fn parse_ref(input: &str) -> IResult<&str, usize> {
        let (input, n) = preceded(char('$'), digit1)(input)?;

        Ok((input, n.parse().unwrap()))
    }

    fn parse_many_braced(input: &str) -> IResult<&str, Vec<StructuredFlow>> {
        let (input, _) = char('{')(input)?;
        let input = whitespace!(input);

        let (input, (the_many, _)) = many_till(cut(parse_structured_flow_root), char('}'))(input)?;

        Ok((input, the_many))
    }

    fn parse_braced_block(input: &str) -> IResult<&str, StructuredFlow> {
        let (input, brk) = parse_brk(input)?;
        let (input, contents) = parse_many_braced(input)?;

        Ok((input, StructuredFlow::Block(brk, contents)))
    }

    fn parse_instructions(input: &str) -> IResult<&str, StructuredFlow> {
        let (input, instructions) = many1(parse_single_instruction)(input)?;

        Ok((input, StructuredFlow::BasicBlock(instructions)))
    }

    fn parse_cond(input: &str) -> IResult<&str, StructuredFlow> {
        let (input, _) = tag("if")(input)?;
        let input = whitespace!(input);

        let (input, brk) = parse_brk(input)?;
        let input = whitespace!(input);

        let (input, _) = tag("(")(input)?;
        let (input, branch_var) = parse_ref(input)?;
        let (input, _) = tag(")")(input)?;
        let input = whitespace!(input);

        let (input, cons) = parse_many_braced(input)?;
        let input = whitespace!(input);

        let (input, _) = tag("else")(input)?;
        let input = whitespace!(input);
        let (input, alt) = parse_many_braced(input)?;
        let input = whitespace!(input);

        Ok((input, StructuredFlow::Cond(brk, branch_var, cons, alt)))
    }

    fn parse_loop(input: &str) -> IResult<&str, StructuredFlow> {
        let (input, _) = tag("loop")(input)?;
        let input = whitespace!(input);

        let (input, brk) = parse_brk(input)?;
        let input = whitespace!(input);

        let (input, body) = parse_many_braced(input)?;
        let input = whitespace!(input);

        Ok((input, StructuredFlow::Loop(brk, body)))
    }

    fn parse_try_catch(input: &str) -> IResult<&str, StructuredFlow> {
        let (input, _) = tag("try")(input)?;
        let input = whitespace!(input);

        let (input, brk) = parse_brk(input)?;
        let input = whitespace!(input);

        let (input, body) = parse_many_braced(input)?;
        let input = whitespace!(input);

        let (input, _) = tag("catch")(input)?;
        let input = whitespace!(input);
        let (input, catch) = parse_many_braced(input)?;
        let input = whitespace!(input);

        let (input, _) = tag("finally")(input)?;
        let input = whitespace!(input);
        let (input, fin) = parse_many_braced(input)?;
        let input = whitespace!(input);

        Ok((input, StructuredFlow::TryCatch(brk, body, catch, fin)))
    }

    fn spaced_tag<'a>(preceding_tag: &'a str) -> impl FnMut(&'a str) -> IResult<&'a str, ()> {
        preceded(
            tuple((tag(preceding_tag), multispace1)),
            nom::combinator::success(()),
        )
    }

    fn parse_jumps_exits(input: &str) -> IResult<&str, StructuredFlow> {
        alt((
            map(preceded(spaced_tag("Return"), parse_ref), |what| {
                StructuredFlow::Return(ExitType::Return, what)
            }),
            map(preceded(spaced_tag("Throw"), parse_ref), |what| {
                StructuredFlow::Return(ExitType::Throw, what)
            }),
            map(
                preceded(spaced_tag("Break"), parse_brk),
                StructuredFlow::Break,
            ),
            map(
                preceded(spaced_tag("Continue"), parse_brk),
                StructuredFlow::Continue,
            ),
        ))(input)
    }

    fn parse_structured_flow_root(input: &str) -> IResult<&str, StructuredFlow> {
        let input = whitespace!(input);

        let (input, out) = alt((
            parse_braced_block,
            parse_jumps_exits,
            parse_cond,
            parse_loop,
            parse_try_catch,
            parse_instructions,
        ))(input)?;

        let input = whitespace!(input);

        Ok((input, out))
    }

    let (input, complete) = parse_structured_flow_root(input).unwrap();

    assert!(input.is_empty());

    complete
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_structured_flow() {
        let parsed = parse_structured_flow(
            "{
                $0 = 123
                if ($0) {
                    $1 = 456
                } else {
                    $2 = 789
                }
                $3 = either($1, $2)
                $4 = undefined
                Return $4
            }",
        );

        insta::assert_debug_snapshot!(parsed, @"
        {
            $0 = 123
            if ($0) {
                $1 = 456
            } else {
                $2 = 789
            }
            $3 = either($1, $2)
            $4 = undefined
            Return $4
        }");

        let parsed = parse_structured_flow(
            "{
                $0 = 123
                loop (@2) {
                    $1 = 123
                    if ($1) {
                        $2 = 456
                        Continue (@2)
                    } else {
                        Break (@2)
                    }
                }
                $3 = undefined
                Return $3
            }",
        );

        insta::assert_debug_snapshot!(parsed, @"
        {
            $0 = 123
            loop (@2) {
                $1 = 123
                if ($1) {
                    $2 = 456
                    Continue (@2)
                } else {
                    Break (@2)
                }
            }
            $3 = undefined
            Return $3
        }");

        let parsed = parse_structured_flow(
            "{
                try (@2) {
                    $0 = 777
                } catch {
                    $1 = caught_error()
                    $2 = 888
                } finally {
                    $3 = 999
                }
                $4 = 111
                $5 = undefined
                Return $5
            }",
        );

        insta::assert_debug_snapshot!(parsed, @"
        {
            try (@2) {
                $0 = 777
            } catch {
                $1 = caught_error()
                $2 = 888
            } finally {
                $3 = 999
            }
            $4 = 111
            $5 = undefined
            Return $5
        }");
    }
}
