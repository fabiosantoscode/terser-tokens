use nom::IResult;

use crate::basic_blocks::{
    ArrayElement, FunctionId, IncrDecr, Instruction, NonLocalId, ObjectKey, ObjectProperty,
    ObjectValue, TempExitType, LHS,
};

/// Parse a single instruction
pub fn parse_test_instruction(input: &str) -> IResult<&str, (usize, Instruction)> {
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

    fn basic_block_instruction(input: &str) -> IResult<&str, (usize, Instruction)> {
        fn spaced_comma(input: &str) -> IResult<&str, ()> {
            let (input, _) = multispace0(input)?;
            let (input, _) = tag(",")(input)?;
            let (input, _) = multispace0(input)?;
            Ok((input, ()))
        }

        fn ins_litnumber(input: &str) -> IResult<&str, Instruction> {
            // 10
            let (input, n) = digit1(input)?;
            let n_float: f64 = n.parse().unwrap();
            Ok((input, Instruction::LitNumber(n_float)))
        }

        fn ins_litbool(input: &str) -> IResult<&str, Instruction> {
            // true | false
            let (input, bool) = alt((tag("true"), tag("false")))(input)?;
            let bool: bool = bool == "true";
            Ok((input, Instruction::LitBool(bool)))
        }

        fn ins_funcref(input: &str) -> IResult<&str, Instruction> {
            // FunctionId(123)
            let (input, _) = tag("FunctionId(")(input)?;
            let (input, n) = digit1(input)?;
            let (input, _) = tag(")")(input)?;
            let n_usize: usize = n.parse().unwrap();
            Ok((input, Instruction::Function(FunctionId(n_usize))))
        }

        fn ins_argref(input: &str) -> IResult<&str, Instruction> {
            // arguments[123]
            let (input, _) = tag("arguments[")(input)?;
            let (input, n) = digit1(input)?;
            let (input, rest) = opt(tag("..."))(input)?;
            let (input, _) = tag("]")(input)?;
            let n_usize: usize = n.parse().unwrap();
            if let Some(_) = rest {
                Ok((input, Instruction::ArgumentRest(n_usize)))
            } else {
                Ok((input, Instruction::ArgumentRead(n_usize)))
            }
        }

        fn ins_member_rw(input: &str) -> IResult<&str, Instruction> {
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
                (None, Some(op), None) => Ok((input, Instruction::IncrDecrPostfix(lhs, op))),
                // ++{lhs} | --{lhs}
                (Some(op), None, None) => Ok((input, Instruction::IncrDecr(lhs, op))),
                // {lhs} = {rhs}
                (None, None, Some(rhs)) => Ok((input, Instruction::Write(lhs, rhs))),
                // ${ref}
                (None, None, None) => match lhs {
                    LHS::Local(n) => Ok((input, Instruction::Ref(n))),
                    lhs => Ok((input, Instruction::Read(lhs))),
                },
                invalid => panic!("invalid update expression: {:?}", invalid),
            }
        }

        fn ins_binop(input: &str) -> IResult<&str, Instruction> {
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
            Ok((input, Instruction::BinOp(op, left, right)))
        }

        fn ins_call(input: &str) -> IResult<&str, Instruction> {
            // call {ref}({arg1}, {arg2}, ...)
            let (input, _) = tag("call")(input)?;
            let input = whitespace!(input);
            let (input, callee) = parse_ref(input)?;
            let input = whitespace!(input);
            let (input, _) = tag("(")(input)?;
            let input = whitespace!(input);
            let (input, args) = separated_list0(spaced_comma, parse_ref)(input)?;
            let (input, _) = tag(")")(input)?;
            Ok((input, Instruction::Call(callee, args)))
        }

        fn ins_undefined(input: &str) -> IResult<&str, Instruction> {
            // undefined
            let (input, _) = tag("undefined")(input)?;
            Ok((input, Instruction::Undefined))
        }

        fn ins_this(input: &str) -> IResult<&str, Instruction> {
            // this
            let (input, _) = tag("this")(input)?;
            Ok((input, Instruction::This))
        }

        fn ins_caught_error(input: &str) -> IResult<&str, Instruction> {
            // caught_error
            let (input, _) = tag("caught_error()")(input)?;
            Ok((input, Instruction::CaughtError))
        }

        fn ins_array(input: &str) -> IResult<&str, Instruction> {
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
            Ok((input, Instruction::Array(items)))
        }

        fn ins_object(input: &str) -> IResult<&str, Instruction> {
            fn object_prop(input: &str) -> IResult<&str, ObjectProperty> {
                let input = whitespace!(input);
                not(tag("}"))(input)?; // In case we see ",}", skip the .expect() below

                let (input, opt_threedot) = opt(tag("..."))(input)?;
                if let Some(_) = opt_threedot {
                    let input = whitespace!(input);
                    let (input, r) = parse_ref(input)?;
                    return Ok((input, ObjectProperty::Spread(r)));
                }

                let (input, key) = cut(alt((
                    map(tuple((tag("["), parse_ref, tag("]"))), |(_, r, _)| {
                        ObjectKey::Computed(r)
                    }),
                    map(alphanumeric1, |s| ObjectKey::NormalKey(String::from(s))),
                )))(input)
                .expect("bad object key");

                let input = whitespace!(input);
                let (input, _) = tag(":")(input)?;
                let input = whitespace!(input);

                let (input, value) = parse_ref(input)?;

                Ok((
                    input,
                    ObjectProperty::KeyValue(key, ObjectValue::Property(value)),
                ))
            }

            // {key: ref, [$3]: ref, ...}
            let (input, _) = tag("{")(input)?;
            let (input, _) = opt(spaced_comma)(input)?;
            let input = whitespace!(input);
            let (input, props) =
                separated_list0(spaced_comma, preceded(multispace0, object_prop))(input)?;
            let input = whitespace!(input);
            let (input, _) = opt(spaced_comma)(input)?;
            let input = whitespace!(input);
            let (input, _) = tag("}")(input)?;
            Ok((input, Instruction::Object(None, props)))
        }

        fn ins_tempexit(input: &str) -> IResult<&str, Instruction> {
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

            Ok((input, Instruction::TempExit(exit_type, ref_)))
        }

        fn ins_phi(input: &str) -> IResult<&str, Instruction> {
            // either({ref}, {ref}, {ref}, ...)
            let (input, _) = tag("either")(input)?;
            let (input, _paren) = tag("(")(input)?;
            let input = whitespace!(input);
            let (input, items) =
                separated_list0(spaced_comma, preceded(multispace0, parse_ref))(input)?;
            let (input, _paren) = tag(")")(input)?;

            Ok((input, Instruction::Phi(items)))
        }

        fn ins_read(input: &str) -> IResult<&str, Instruction> {
            // read_non_local {ref}
            let as_nonlocal = |input| {
                let (input, _) = tag("read_non_local")(input)?;
                let input = whitespace!(input);
                let (input, nonloc) = parse_lhs(input)?;

                Ok((input, Instruction::Read(nonloc)))
            };
            let as_global = |input| {
                let (input, _) = tag("global")(input)?;
                let input = whitespace!(input);
                let (input, global) = parse_string(input)?;

                Ok((input, Instruction::Read(LHS::Global(global))))
            };

            alt((as_nonlocal, as_global))(input)
        }

        fn ins_write(input: &str) -> IResult<&str, Instruction> {
            // write_non_local {ref} {ref}
            let (input, _) = tag("write_non_local")(input)?;
            let input = whitespace!(input);
            let (input, nonloc) = parse_lhs(input)?;
            let input = whitespace!(input);
            let (input, value) = parse_ref(input)?;

            Ok((input, Instruction::Write(nonloc, value)))
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
            ins_object,
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
    fn parse_string(input: &str) -> IResult<&str, String> {
        let input = whitespace!(input);

        let (input, quote_style) = alt((tag("'"), tag("\"")))(input)?;

        let (input, contents) = escaped(none_of("\\\"'\n"), '\\', one_of(r#""'\nrt"#))(input)?;

        let contents = String::from(contents)
            .replace("\\\"", "\"")
            .replace("\\'", "'")
            .replace("\\n", "\n")
            .replace("\\r", "\r")
            .replace("\\t", "\t");

        let (input, _) = tag(quote_style)(input)?;

        Ok((input, contents))
    }
    fn parse_member(input: &str) -> IResult<&str, ObjectKey> {
        // .name | [$123] | .#private
        alt((
            map(preceded(tag("."), alphanumeric1), |name: &str| {
                ObjectKey::NormalKey(name.to_string())
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
mod tests {}
