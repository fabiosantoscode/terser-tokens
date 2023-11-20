use std::collections::BTreeMap;

use nom::IResult;

use crate::basic_blocks::{
    ArrayElement, BasicBlock, BasicBlockExit, BasicBlockGroup, BasicBlockInstruction,
    BasicBlockModule, ExitType, FunctionId, IncrDecr, NonLocalId, ObjectKey, TempExitType, LHS,
};

pub fn parse_instructions(input: &str) -> BasicBlockGroup {
    parse_instructions_inner(input).unwrap().1
}

pub fn parse_instructions_module(input: Vec<&str>) -> BasicBlockModule {
    let functions: BTreeMap<_, _> = input
        .iter()
        .enumerate()
        .map(|(i, s)| {
            (
                FunctionId(i),
                BasicBlockGroup {
                    id: FunctionId(i),
                    ..parse_instructions(s)
                },
            )
        })
        .collect();

    BasicBlockModule {
        functions,
        ..Default::default()
    }
}

fn parse_instructions_inner(input: &str) -> IResult<&str, BasicBlockGroup> {
    use nom::branch::*;
    use nom::bytes::complete::*;
    use nom::character::complete::*;
    use nom::combinator::*;
    use nom::multi::*;

    macro_rules! whitespace {
        ($input:ident) => {
            multispace0($input)?.0
        };
    }

    fn parse_blockref_range(input: &str) -> IResult<&str, (usize, usize)> {
        // @123..@456

        let input = whitespace!(input);
        let (input, start) = parse_blockref(input)?;
        let input = whitespace!(input);
        let (input, _) = tag("..")(input)?;
        let input = whitespace!(input);
        let (input, end) = parse_blockref(input)?;
        let input = whitespace!(input);

        Ok((input, (start, end)))
    }

    fn parse_basic_block_exit(input: &str) -> IResult<&str, BasicBlockExit> {
        // exit = jump @123

        let input = whitespace!(input);
        let (input, _exit) = tag("exit")(input)?;
        let input = whitespace!(input);
        let (input, _exit) = tag("=")(input)?;
        let input = whitespace!(input);

        let (input, word) = alt((
            tag("jump"),
            tag("break"),
            tag("continue"),
            tag("cond"),
            tag("loop"),
            tag("return"),
            tag("throw"),
            tag("try"),
            tag("error"),
            tag("finally"),
            tag("end finally after"),
        ))(input)?;

        let input = whitespace!(input);

        let (input, ret) = match word {
            "jump" => {
                let (input, ref_) = parse_blockref(input)?;
                (input, BasicBlockExit::Jump(ref_))
            }
            "break" => {
                let (input, ref_) = parse_blockref(input)?;
                (input, BasicBlockExit::Break(ref_))
            }
            "continue" => {
                let (input, ref_) = parse_blockref(input)?;
                (input, BasicBlockExit::Continue(ref_))
            }
            "cond" => {
                // cond {ref} ? @1..@2 : @3..@4
                let (input, condition) = parse_ref(input)?;
                let input = whitespace!(input);
                let (input, _) = tag("?")(input)?;
                let input = whitespace!(input);
                let (input, cons) = parse_blockref_range(input)?;
                let input = whitespace!(input);
                let (input, _) = tag(":")(input)?;
                let input = whitespace!(input);
                let (input, alt) = parse_blockref_range(input)?;
                let input = whitespace!(input);

                (
                    input,
                    BasicBlockExit::Cond(condition, cons.0, cons.1, alt.0, alt.1),
                )
            }
            "loop" => {
                // loop @123..@456
                let (input, (loop_start, loop_end)) = parse_blockref_range(input)?;
                let input = whitespace!(input);

                (input, BasicBlockExit::Loop(loop_start, loop_end))
            }
            "return" => {
                let (input, ref_) = parse_ref(input)?;
                (input, BasicBlockExit::ExitFn(ExitType::Return, ref_))
            }
            "throw" => {
                let (input, ref_) = parse_ref(input)?;
                (input, BasicBlockExit::ExitFn(ExitType::Throw, ref_))
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
            "error" => {
                // error ? jump @123 : jump @456
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

                (input, BasicBlockExit::PopCatch(consequent, alternate))
            }
            "finally" => {
                // finally @10 after @11
                let (input, finally_block) = parse_blockref(input)?;
                let input = whitespace!(input);
                let (input, _) = tag("after")(input)?;
                let input = whitespace!(input);
                let (input, after_block) = parse_blockref(input)?;

                (
                    input,
                    BasicBlockExit::PopFinally(finally_block, after_block),
                )
            }
            "end finally after" => {
                // end finally after @10
                let (input, after_block) = parse_blockref(input)?;

                (input, BasicBlockExit::EndFinally(after_block))
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

    fn basic_block(input: &str) -> IResult<&str, (usize, BasicBlock)> {
        let (input, index) = basic_block_header(input)?;

        let input = whitespace!(input);
        // whitespace
        let input = whitespace!(input);
        let (input, _) = tag("{")(input)?;
        // whitespace
        let input = whitespace!(input);

        let (input, instructions) =
            cut(many0(parse_single_instruction))(input).expect("bad block of instructions");

        let input = whitespace!(input);
        let (input, exit) = cut(parse_basic_block_exit)(input).expect("bad block exit");

        let input = whitespace!(input);
        let (input, _) = tag("}")(input)?;

        let input = whitespace!(input);

        Ok((input, (index, BasicBlock { instructions, exit })))
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

    let (input, blocks) = many0(basic_block)(input).expect("bad basic blocks");

    let (input, _) = eof(input)?;

    assert_eq!(input, "");

    Ok((
        input,
        BasicBlockGroup {
            blocks: blocks.into_iter().collect(),
            ..Default::default()
        },
    ))
}

/// Parse a single instruction
pub fn parse_single_instruction(input: &str) -> IResult<&str, (usize, BasicBlockInstruction)> {
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
        fn spaced_comma(input: &str) -> IResult<&str, ()> {
            let (input, _) = multispace0(input)?;
            let (input, _) = tag(",")(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, ()))
        }

        fn ins_litnumber(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // 10
            let (input, n) = digit1(input)?;
            let n_float: f64 = n.parse().unwrap();
            Ok((input, BasicBlockInstruction::LitNumber(n_float)))
        }

        fn ins_litbool(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // true | false
            let (input, bool) = alt((tag("true"), tag("false")))(input)?;
            let bool: bool = bool == "true";
            Ok((input, BasicBlockInstruction::LitBool(bool)))
        }

        fn ins_funcref(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // FunctionId(123)
            let (input, _) = tag("FunctionId(")(input)?;
            let (input, n) = digit1(input)?;
            let (input, _) = tag(")")(input)?;
            let n_usize: usize = n.parse().unwrap();
            Ok((input, BasicBlockInstruction::Function(FunctionId(n_usize))))
        }

        fn ins_argref(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // arguments[123]
            let (input, _) = tag("arguments[")(input)?;
            let (input, n) = digit1(input)?;
            let (input, rest) = opt(tag("..."))(input)?;
            let (input, _) = tag("]")(input)?;
            let n_usize: usize = n.parse().unwrap();
            if let Some(_) = rest {
                Ok((input, BasicBlockInstruction::ArgumentRest(n_usize)))
            } else {
                Ok((input, BasicBlockInstruction::ArgumentRead(n_usize)))
            }
        }

        fn ins_member_rw(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // {lhs}.{stringkey} | {lhs}[$123] | {lhs}.#{priv}
            // optionally, append = {rhs}

            let (input, incrdecr_prefix) = opt(alt((
                map(tag("++"), |_| IncrDecr::Incr),
                map(tag("--"), |_| IncrDecr::Decr),
            )))(input)?;

            let (input, lhs) = parse_lhs(input)?;

            let input = whitespace!(input);

            let (input, incrdecr_postfix) = opt(alt((
                map(tag("++"), |_| IncrDecr::Incr),
                map(tag("--"), |_| IncrDecr::Decr),
            )))(input)?;

            let (input, assignee) =
                opt(map(tuple((tag("="), space0, parse_ref)), |(_, _, r)| r))(input)?;

            match (incrdecr_prefix, incrdecr_postfix, assignee) {
                // {lhs}++ | {lhs}--
                (None, Some(op), None) => {
                    Ok((input, BasicBlockInstruction::IncrDecrPostfix(lhs, op)))
                }
                // ++{lhs} | --{lhs}
                (Some(op), None, None) => Ok((input, BasicBlockInstruction::IncrDecr(lhs, op))),
                // {lhs} = {rhs}
                (None, None, Some(rhs)) => Ok((input, BasicBlockInstruction::Write(lhs, rhs))),
                // ${ref}
                (None, None, None) => match lhs {
                    LHS::Local(n) => Ok((input, BasicBlockInstruction::Ref(n))),
                    lhs => Ok((input, BasicBlockInstruction::Read(lhs))),
                },
                invalid => panic!("invalid update expression: {:?}", invalid),
            }
        }

        fn ins_binop(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // {ref} {operator} {ref}
            let (input, left) = parse_ref(input)?;
            let input = whitespace!(input);

            use swc_ecma_ast::BinaryOp::*;
            let (input, op) = alt((
                map(tag("+"), |_| Add),
                map(tag("-"), |_| Sub),
                map(tag("*"), |_| Mul),
                map(tag("/"), |_| Div),
                map(tag("=="), |_| EqEq),
                map(tag("==="), |_| EqEqEq),
                map(tag("!="), |_| NotEq),
                map(tag("!=="), |_| NotEqEq),
                map(tag(">="), |_| GtEq),
                map(tag(">"), |_| Gt),
                map(tag("<="), |_| LtEq),
                map(tag("<"), |_| Lt),
                map(tag("<="), |_| LtEq),
            ))(input)?;

            let input = whitespace!(input);
            let (input, right) = parse_ref(input)?;
            Ok((input, BasicBlockInstruction::BinOp(op, left, right)))
        }

        fn ins_call(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // call {ref}({arg1}, {arg2}, ...)
            let (input, _) = tag("call")(input)?;
            let input = whitespace!(input);
            let (input, callee) = parse_ref(input)?;
            let input = whitespace!(input);
            let (input, _) = tag("(")(input)?;
            let input = whitespace!(input);
            let (input, args) = separated_list0(spaced_comma, parse_ref)(input)?;
            let (input, _) = tag(")")(input)?;
            Ok((input, BasicBlockInstruction::Call(callee, args)))
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
            let (input, _) = tag("caught_error()")(input)?;
            Ok((input, BasicBlockInstruction::CaughtError))
        }

        fn ins_array(input: &str) -> IResult<&str, BasicBlockInstruction> {
            fn array_elm(input: &str) -> IResult<&str, ArrayElement> {
                use ArrayElement::*;

                // In case we see ",]", skip the .expect() below
                not(tag("]"))(input)?;

                let r = cut(alt((
                    map(preceded(tag("..."), parse_ref), |r| Spread(r)),
                    map(parse_ref, |i| Item(i)),
                    map(peek(tag(",")), |_| Hole),
                )))(input);

                Ok(r.expect("bad array element"))
            }

            // [ref, ref, ...]
            let (input, _) = tag("[")(input)?;
            let input = whitespace!(input);
            let (input, items) =
                separated_list0(spaced_comma, preceded(multispace0, array_elm))(input)?;
            let input = whitespace!(input);
            let (input, _) = opt(spaced_comma)(input)?;
            let input = whitespace!(input);
            let (input, _) = tag("]")(input)?;
            Ok((input, BasicBlockInstruction::Array(items)))
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
            let (input, ref_) = parse_ref(input)?;

            Ok((input, BasicBlockInstruction::TempExit(exit_type, ref_)))
        }

        fn ins_phi(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // either({ref}, {ref}, {ref}, ...)
            let (input, _) = tag("either")(input)?;
            let (input, _paren) = tag("(")(input)?;
            let input = whitespace!(input);
            let (input, items) =
                separated_list0(spaced_comma, preceded(multispace0, parse_ref))(input)?;
            let (input, _paren) = tag(")")(input)?;

            Ok((input, BasicBlockInstruction::Phi(items)))
        }

        fn ins_read(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // read_non_local {ref}
            let (input, _) = tag("read_non_local")(input)?;
            let input = whitespace!(input);
            let (input, nonloc) = parse_lhs(input)?;

            Ok((input, BasicBlockInstruction::Read(nonloc)))
        }

        fn ins_write(input: &str) -> IResult<&str, BasicBlockInstruction> {
            // write_non_local {ref} {ref}
            let (input, _) = tag("write_non_local")(input)?;
            let input = whitespace!(input);
            let (input, nonloc) = parse_lhs(input)?;
            let input = whitespace!(input);
            let (input, value) = parse_ref(input)?;

            Ok((input, BasicBlockInstruction::Write(nonloc, value)))
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
            ins_litbool,
            ins_binop,
            ins_member_rw,
            ins_call,
            ins_funcref,
            ins_argref,
            ins_caught_error,
            ins_array,
            ins_tempexit,
            ins_phi,
            ins_read,
            ins_write,
        )))(input)
        .expect("bad instruction");

        let input = whitespace!(input);

        Ok((input, (var_name, instruction)))
    }

    fn parse_ref(input: &str) -> IResult<&str, usize> {
        let (input, _) = tag("$")(input)?;
        let (input, n) = digit1(input)?;
        Ok((input, n.parse().unwrap()))
    }
    fn parse_member(input: &str) -> IResult<&str, ObjectKey> {
        // .name | [$123] | .#private
        alt((
            map(preceded(tag("."), alphanumeric1), |name: &str| {
                ObjectKey::KeyValue(name.to_string())
            }),
            map(tuple((tag("["), parse_ref, tag("]"))), |(_, idx, _)| {
                ObjectKey::Computed(idx)
            }),
            map(preceded(tag(".#"), alphanumeric1), |name: &str| {
                ObjectKey::Private(name.to_string())
            }),
        ))(input)
    }
    fn parse_lhs(input: &str) -> IResult<&str, LHS> {
        let (input, lhs) = alt((
            map(parse_ref, |n| LHS::Local(n)),
            map(preceded(tag("$"), parse_ref), |n| {
                LHS::NonLocal(NonLocalId(n))
            }),
        ))(input)?;

        let (input, lhs) = fold_many0(
            parse_member,
            || lhs.clone(),
            |lhs, member| LHS::Member(Box::new(lhs), member),
        )(input)?;

        Ok((input, lhs))
    }

    let input = whitespace!(input);

    let (input, out) = basic_block_instruction(input)?;

    let input = whitespace!(input);

    Ok((input, out))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic_blocks() {
        let blocks = parse_instructions_inner(
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
        let blocks = parse_instructions_inner(
            r###"
            @0: {
                $0 = 777
                exit = jump @1
            }
            @1: {
                exit = cond $0 ? @2..@4 : @7..@8
            }
            @2: {
                $1 = 888
                exit = jump @3
            }
            @3: {
                exit = cond $1 ? @4..@4 : @5..@8
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
            @8: {
                $4 = read_non_local $$2
                $5 = write_non_local $$2 $4
                exit = jump @9
            }
            @9: {
                $6 = call $5($1, $2)
                $7 = call $6()
                $8 = arguments[0]
                $9 = arguments[1...]
                $10 = FunctionId(1)
                exit = return $8
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
            exit = cond $0 ? @2..@4 : @7..@8
        }
        @2: {
            $1 = 888
            exit = jump @3
        }
        @3: {
            exit = cond $1 ? @4..@4 : @5..@8
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
        @8: {
            $4 = read_non_local $$2
            $5 = write_non_local $$2 $4
            exit = jump @9
        }
        @9: {
            $6 = call $5($1, $2)
            $7 = call $6()
            $8 = arguments[0]
            $9 = arguments[1...]
            $10 = FunctionId(1)
            exit = return $8
        }
        "###
        );
    }
}
