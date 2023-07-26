use crate::basic_blocks::{
    ArrayElement, BasicBlock, BasicBlockExit, BasicBlockGroup, BasicBlockInstruction, ExitType,
    TempExitType,
};

use nom::IResult;

pub fn parse_basic_blocks(input: &str) -> BasicBlockGroup {
    parse_basic_blocks_inner(input).unwrap().1
}

fn parse_basic_blocks_inner(input: &str) -> IResult<&str, BasicBlockGroup> {
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

    fn basic_block_instruction(input: &str) -> IResult<&str, (usize, BasicBlockInstruction)> {
        fn ins_litnumber(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // 10
            let (input, n) = digit1(input)?;
            let n_float: f64 = n.parse().unwrap();
            Ok((input, BasicBlockInstruction::LitNumber(n_float)))
        }

        fn ins_ref(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // $123
            let (input, _) = tag("$")(input)?;
            let (input, n) = digit1(input)?;
            let n_usize: usize = n.parse().unwrap();
            Ok((input, BasicBlockInstruction::Ref(n_usize)))
        }

        fn ins_binop(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // {ref} {operator} {ref}
            let (input, left) = parse_ref(input)?;
            let input = whitespace!(input);
            let (input, op) = one_of("+-*/")(input)?;
            let input = whitespace!(input);
            let (input, right) = parse_ref(input)?;
            Ok((
                input,
                BasicBlockInstruction::BinOp(String::from(op), left, right),
            ))
        }

        fn ins_undefined(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // undefined
            let (input, _) = tag("undefined")(input)?;
            Ok((input, BasicBlockInstruction::Undefined))
        }

        fn ins_this(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // this
            let (input, _) = tag("this")(input)?;
            Ok((input, BasicBlockInstruction::This))
        }

        fn ins_caught_error(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // caught_error
            let (input, _) = tag("caught_error")(input)?;
            Ok((input, BasicBlockInstruction::CaughtError))
        }

        fn ins_array(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // [ref, ref, ...]
            let (input, _) = tag("[")(input)?;
            let input = whitespace!(input);
            let (input, items) = separated_list0(
                tag(","),
                preceded(multispace0, parse_ref),
            )(input)?;
            let input = whitespace!(input);
            let (input, _) = tag("]")(input)?;
            Ok((
                input,
                BasicBlockInstruction::Array(
                    items
                        .into_iter()
                        .map(|r| ArrayElement::Item(r))
                        .collect::<Vec<_>>(),
                ),
            ))
        }

        fn ins_tempexit(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // YieldStar {ref}
            // Yield {ref}
            // Await {ref}
            let (input, exit_type) = alt((
                map(tag("YieldStar"), |_| TempExitType::YieldStar),
                map(tag("Yield"), |_| TempExitType::Yield),
                map(tag("Await"), |_| TempExitType::Await),
            ))(input)?;

            let input = whitespace!(input);
            let (input, ref_) = ins_ref(input)?;

            Ok((
                input,
                BasicBlockInstruction::TempExit(exit_type, ref_.unwrap_ref()),
            ))
        }

        fn ins_phi(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // either({ref}, {ref}, {ref}, ...)
            let (input, _) = tag("either")(input)?;
            let (input, _paren) = tag("(")(input)?;
            let input = whitespace!(input);
            let (input, items) = separated_list0(
                tag(","),
                preceded(multispace0, parse_ref),
            )(input)?;
            let (input, _paren) = tag(")")(input)?;

            Ok((input, BasicBlockInstruction::Phi(items)))
        }

        // $123 =
        let input = whitespace!(input);

        let (input, var_name) = parse_ref(input)?;
        let input = whitespace!(input);
        let (input, _) = tag("=")(input)?;
        let input = whitespace!(input);

        let (input, instruction) = cut(alt((
            ins_undefined,
            ins_this,
            ins_litnumber,
            ins_ref,
            ins_binop,
            ins_caught_error,
            ins_array,
            ins_tempexit,
            ins_phi,
        )))(input)?;

        let input = whitespace!(input);

        Ok((input, (var_name, instruction)))
    }

    fn parse_basic_block_exit(input: &str) -> IResult<&str, BasicBlockExit> {
        // exit = jump @123

        let input = whitespace!(input);
        let (input, _exit) = tag("exit")(input)?;
        let input = whitespace!(input);
        let (input, _exit) = tag("=")(input)?;
        let input = whitespace!(input);

        let (input, word) = alt((tag("jump"), tag("cond"), tag("return"), tag("try")))(input)?;

        let input = whitespace!(input);

        let (input, ret) = match word {
            "return" => {
                let (input, ref_) = parse_ref(input)?;
                (input, BasicBlockExit::ExitFn(ExitType::Return, ref_))
            }
            "jump" => {
                let (input, ref_) = parse_blockref(input)?;
                (input, BasicBlockExit::Jump(ref_))
            }
            "cond" => {
                // cond {ref} ? jump @123 : jump @456
                let (input, condition) = parse_ref(input)?;
                let input = whitespace!(input);
                let (input, _) = tag("?")(input)?;
                let input = whitespace!(input);
                let (input, _) = tag("jump")(input)?;
                let input = whitespace!(input);
                let (input, consequent) = parse_blockref(input)?;
                let input = whitespace!(input);
                let (input, _) = tag(":")(input)?;
                let input = whitespace!(input);
                let (input, _) = tag("jump")(input)?;
                let input = whitespace!(input);
                let (input, alternate) = parse_blockref(input)?;
                (
                    input,
                    BasicBlockExit::Cond(condition, consequent, alternate),
                )
            }
            "try" => {
                // try @123 catch @456 finally @789 after @101112
                let (input, try_block) = parse_blockref(input)?;
                let input = whitespace!(input);
                let (input, _) = tag("catch")(input)?;
                let input = whitespace!(input);
                let (input, catch_block) = parse_blockref(input)?;
                let input = whitespace!(input);
                let (input, _) = tag("finally")(input)?;
                let input = whitespace!(input);
                let (input, finally_block) = parse_blockref(input)?;
                let input = whitespace!(input);
                let (input, _) = tag("after")(input)?;
                let input = whitespace!(input);
                let (input, after_block) = parse_blockref(input)?;
                (
                    input,
                    BasicBlockExit::SetTryAndCatch(
                        try_block,
                        catch_block,
                        finally_block,
                        after_block,
                    ),
                )
            }
            _ => unimplemented!(),
        };

        let input = whitespace!(input);

        Ok((input, ret))
    }

    fn basic_block_header(input: &str) -> IResult<&str, usize> {
        // @0: { ...instructions, exit = {basic block exit} }

        let input = whitespace!(input);
        let (input, _) = tag("@")(input)?;
        let (input, index) = digit1(input)?;
        let (input, _) = tag(":")(input)?;

        Ok((input, index.parse().unwrap()))
    }

    fn basic_block(input: &str) -> IResult<&str, BasicBlock> {
        let input = whitespace!(input);
        // whitespace
        let input = whitespace!(input);
        let (input, _) = tag("{")(input)?;
        // whitespace
        let input = whitespace!(input);

        let (input, instructions) = cut(many0(basic_block_instruction))(input)?;

        let input = whitespace!(input);
        let (input, exit) = cut(parse_basic_block_exit)(input)?;

        let input = whitespace!(input);
        let (input, _) = tag("}")(input)?;

        let input = whitespace!(input);

        Ok((input, BasicBlock { instructions, exit }))
    }
    fn parse_ref(input: &str) -> IResult<&str, usize> {
        let (input, _) = tag("$")(input)?;
        let (input, n) = digit1(input)?;
        Ok((input, n.parse().unwrap()))
    }
    fn parse_blockref(input: &str) -> IResult<&str, usize> {
        let (input, _) = tag("@")(input)?;
        let (input, n) = digit1(input)?;
        Ok((input, n.parse().unwrap()))
    }

    let (input, blocks) = many0(preceded(basic_block_header, cut(basic_block)))(input)?;

    let (input, _) = eof(input)?;

    assert_eq!(input, "");

    Ok((input, BasicBlockGroup::from_asts(blocks)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic_blocks() {
        let blocks = parse_basic_blocks_inner(
            r###"
            @0: {
                $0 = 123
                exit = jump @1
            }
            "###,
        )
        .unwrap()
        .1;

        insta::assert_debug_snapshot!(blocks, @r###"
            @0: {
                $0 = 123
                exit = jump @1
            }
            "###
        );
    }

    #[test]
    fn test_parse_basic_blocks_2() {
        let blocks = parse_basic_blocks_inner(
            r###"
            @0: {
                $0 = 777
                exit = jump @1
            }
            @1: {
                exit = cond $0 ? jump @2 : jump @7
            }
            @2: {
                $1 = 888
                exit = jump @3
            }
            @3: {
                exit = cond $1 ? jump @4 : jump @5
            }
            @4: {
                exit = jump @7
            }
            @5: {
                exit = jump @6
            }
            @6: {
                exit = jump @0
            }
            @7: {
                $2 = 999
                $3 = undefined
                exit = return $3
            }
            "###,
        )
        .unwrap()
        .1;

        insta::assert_debug_snapshot!(blocks, @r###"
            @0: {
                $0 = 777
                exit = jump @1
            }
            @1: {
                exit = cond $0 ? jump @2 : jump @7
            }
            @2: {
                $1 = 888
                exit = jump @3
            }
            @3: {
                exit = cond $1 ? jump @4 : jump @5
            }
            @4: {
                exit = jump @7
            }
            @5: {
                exit = jump @6
            }
            @6: {
                exit = jump @0
            }
            @7: {
                $2 = 999
                $3 = undefined
                exit = return $3
            }
            "###
        );
    }
}
