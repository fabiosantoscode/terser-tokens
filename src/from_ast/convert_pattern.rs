use swc_ecma_ast::{
    AssignPat, BinaryOp, BindingIdent, Expr, MemberExpr, MemberProp, ObjectPatProp, Pat, PropName,
    RestPat, SuperPropExpr,
};

use crate::basic_blocks::{
    ArrayPatternPiece, BasicBlockInstruction, BreakableId, ObjectKey, ObjectPatternPiece,
    StructuredFlow, LHS,
};

use super::{expr_to_basic_blocks, to_basic_blocks_lhs, FromAstCtx};

#[derive(Clone, Copy, Debug)]
pub enum PatType {
    Assign,
    VarDecl,
    FunArg,
}

pub fn get_propname_normal_key(propname: &PropName) -> String {
    match propname {
        PropName::Ident(ref ident) => ident.sym.to_string(),
        PropName::BigInt(big_int) => big_int.value.to_string(),
        PropName::Str(str) => str.value.to_string(),
        PropName::Num(num) => {
            let mut buffer = ryu_js::Buffer::new();
            let printed = buffer.format(num.value);
            printed.to_string()
        }
        PropName::Computed(_) => unreachable!("handled by the caller"),
    }
}

pub fn pat_to_basic_blocks(
    ctx: &mut FromAstCtx,
    pat_type: PatType,
    pat: &Pat,
    input: usize,
) -> Result<(Vec<StructuredFlow>, usize), String> {
    match pat {
        Pat::Ident(ident) => ident_pat(ctx, pat_type, &ident.id.sym.to_string(), input),
        Pat::Assign(assign_pat) => {
            let mut assign_flow = vec![];

            let (flow, input) = default_assign_pat(ctx, input, &assign_pat.right)?;
            assign_flow.extend(flow);

            let (flow, input) = pat_to_basic_blocks(ctx, pat_type, &assign_pat.left, input)?;
            assign_flow.extend(flow);

            Ok((assign_flow, input))
        }
        Pat::Expr(expr) => pat_like_expr_to_basic_blocks(ctx, pat_type, expr, input),
        Pat::Array(array_pat) => {
            let mut array_pat_flow = vec![];

            assert!(!array_pat.optional);

            // First, take stock of what items exist and create an ArrayPattern
            let items = array_pat
                .elems
                .iter()
                .map(|elem| match elem {
                    Some(elem) => match elem {
                        Pat::Rest(_) => ArrayPatternPiece::Spread,
                        _ => ArrayPatternPiece::Item,
                    },
                    None => ArrayPatternPiece::Item, // array hole
                })
                .collect::<Vec<_>>();

            let (flow, pattern) =
                ctx.push_instruction(BasicBlockInstruction::ArrayPattern(input, items));
            array_pat_flow.extend(flow);

            // Then, we create an unpacker for each item
            let unpackers = array_pat
                .elems
                .iter()
                .enumerate()
                .map(|(i, elem)| {
                    let (flow, unpacker) =
                        ctx.push_instruction(BasicBlockInstruction::PatternUnpack(pattern, i));
                    array_pat_flow.extend(flow);

                    (elem, unpacker)
                })
                .collect::<Vec<_>>();

            // Finally, we recurse on each item
            for (elem, unpacked) in unpackers {
                // unpack pattern_unpacker[i]
                match elem {
                    Some(Pat::Rest(RestPat { arg: elem, .. })) => {
                        let (flow, _) = pat_to_basic_blocks(ctx, pat_type, elem, unpacked)?;
                        array_pat_flow.extend(flow);
                    }
                    Some(elem) => {
                        let (flow, _) = pat_to_basic_blocks(ctx, pat_type, elem, unpacked)?;
                        array_pat_flow.extend(flow);
                    }
                    None => {
                        // hole; this resulting instruction will be unused
                    }
                }
            }

            Ok((array_pat_flow, input))
        }
        Pat::Object(object_pat) => {
            let mut object_pat_flow = vec![];

            assert!(!object_pat.optional);

            // First, take stock of what items exist and create an ObjectPattern
            let items = object_pat
                .props
                .iter()
                .map(|prop| match prop {
                    ObjectPatProp::KeyValue(kv) => match &kv.key {
                        PropName::Computed(computed) => {
                            let (flow, key) = expr_to_basic_blocks(ctx, computed.expr.as_ref())?;
                            object_pat_flow.extend(flow);

                            Ok(ObjectPatternPiece::TakeComputedKey(key))
                        }
                        _ => Ok(ObjectPatternPiece::TakeKey(get_propname_normal_key(
                            &kv.key,
                        ))),
                    },
                    ObjectPatProp::Assign(a) => {
                        Ok(ObjectPatternPiece::TakeKey(a.key.sym.to_string()))
                    }
                    ObjectPatProp::Rest(_r) => Ok(ObjectPatternPiece::Spread),
                })
                .collect::<Result<Vec<_>, String>>()?;

            let (flow, pattern) =
                ctx.push_instruction(BasicBlockInstruction::ObjectPattern(input, items));
            object_pat_flow.extend(flow);

            // Then, we create an unpacker for each item
            let unpackers = object_pat
                .props
                .iter()
                .enumerate()
                .map(|(i, elem)| {
                    let (flow, unpacker) =
                        ctx.push_instruction(BasicBlockInstruction::PatternUnpack(pattern, i));
                    object_pat_flow.extend(flow);

                    (elem, unpacker)
                })
                .collect::<Vec<_>>();

            // Finally, we recurse on each item
            for (prop, unpacker) in unpackers {
                match prop {
                    ObjectPatProp::KeyValue(kv) => {
                        let (flow, _) = pat_to_basic_blocks(ctx, pat_type, &kv.value, unpacker)?;
                        object_pat_flow.extend(flow);
                    }
                    // AST representations of shorthand object keys are always weird.
                    // So we'll create a virtual Pat::Ident or a Pat::Assign to recurse.
                    ObjectPatProp::Assign(a) => {
                        let ident = Pat::Ident(BindingIdent {
                            id: a.key.clone(),
                            type_ann: None,
                        });

                        if let Some(default_value) = &a.value {
                            let ident = Pat::Assign(AssignPat {
                                span: Default::default(),
                                left: Box::new(ident),
                                right: default_value.clone(),
                            });
                            let (flow, _) = pat_to_basic_blocks(ctx, pat_type, &ident, unpacker)?;
                            object_pat_flow.extend(flow);
                        } else {
                            let (flow, _) = pat_to_basic_blocks(ctx, pat_type, &ident, unpacker)?;
                            object_pat_flow.extend(flow);
                        }
                    }
                    ObjectPatProp::Rest(rest) => {
                        let (flow, _) = pat_to_basic_blocks(ctx, pat_type, &rest.arg, unpacker)?;
                        object_pat_flow.extend(flow);
                    }
                }
            }

            return Ok((object_pat_flow, input));
        }
        Pat::Rest(_) => unreachable!("handled in array/funargs pattern"),
        Pat::Invalid(_) => unreachable!(),
    }
}

/// Member expressions and identifiers can be treated like patterns
pub fn pat_like_expr_to_basic_blocks(
    ctx: &mut FromAstCtx,
    pat_type: PatType,
    expr: &Expr,
    input: usize,
) -> Result<(Vec<StructuredFlow>, usize), String> {
    match expr {
        Expr::Ident(ident) => ident_pat(ctx, pat_type, &ident.sym.to_string(), input),
        Expr::SuperProp(SuperPropExpr { .. }) => todo!(),
        Expr::MetaProp(_) => todo!(),
        Expr::Member(MemberExpr { obj, prop, .. }) => {
            let mut ret_flow = Vec::new();

            let (flow, base) = to_basic_blocks_lhs(ctx, obj.as_ref())?;
            ret_flow.extend(flow);

            let prop = match &prop {
                MemberProp::Ident(ident) => ObjectKey::NormalKey(ident.sym.to_string()),
                MemberProp::PrivateName(pvt) => ObjectKey::Private(pvt.id.sym.to_string()),
                MemberProp::Computed(comp) => {
                    let (flow, comp) = expr_to_basic_blocks(ctx, comp.expr.as_ref())?;
                    ret_flow.extend(flow);

                    ObjectKey::Computed(comp)
                }
            };

            let memb = LHS::Member(Box::new(base), prop);

            let (flow, write) = ctx.push_instruction(BasicBlockInstruction::Write(memb, input));
            ret_flow.extend(flow);

            Ok((ret_flow, write))
        }
        _ => unreachable!(
            "pattern expression must be an identifier or member expression, got: {:?}",
            expr
        ),
    }
}

fn ident_pat(
    ctx: &mut FromAstCtx,
    pat_type: PatType,
    name: &str,
    input: usize,
) -> Result<(Vec<StructuredFlow>, usize), String> {
    use PatType::*;

    match pat_type {
        VarDecl | FunArg => {
            let (flow, _) = ctx.declare_name(name, input);

            Ok((flow, input))
        }
        Assign => {
            let mut ret_flow = Vec::new();

            let (flow, input) = ctx.assign_name(name, input);
            ret_flow.extend(flow);

            let (flow, input) = ctx.push_instruction(BasicBlockInstruction::Ref(input));
            ret_flow.extend(flow);

            Ok((ret_flow, input))
        }
    }
}

pub fn convert_object_propname(
    ctx: &mut FromAstCtx,
    propname: &PropName,
) -> Result<(Vec<StructuredFlow>, ObjectKey), String> {
    match propname {
        PropName::Computed(comp) => {
            let (flow, comp) = expr_to_basic_blocks(ctx, comp.expr.as_ref())?;
            Ok((flow, ObjectKey::Computed(comp)))
        }
        other => Ok((vec![], ObjectKey::NormalKey(get_propname_normal_key(other)))),
    }
}

fn default_assign_pat(
    ctx: &mut FromAstCtx,
    input: usize,
    by_default: &Expr,
) -> Result<(Vec<StructuredFlow>, usize), String> {
    let mut assign_flow = vec![];

    let (flow, undef) = ctx.push_instruction(BasicBlockInstruction::Undefined);
    assign_flow.extend(flow);
    let (flow, test) =
        ctx.push_instruction(BasicBlockInstruction::BinOp(BinaryOp::EqEqEq, input, undef));
    assign_flow.extend(flow);

    ctx.enter_conditional_branch();

    let (then_flow, default_value) = expr_to_basic_blocks(ctx, by_default)?;

    let (else_flow, input_value) = ctx.push_instruction(BasicBlockInstruction::Ref(input));

    assign_flow.push(StructuredFlow::Cond(
        BreakableId(None),
        test,
        then_flow,
        else_flow,
    ));

    let flow = ctx.leave_conditional_branch();
    assign_flow.extend(flow);

    let (flow, phi) =
        ctx.push_instruction(BasicBlockInstruction::Phi(vec![input_value, default_value]));
    assign_flow.extend(flow);

    Ok((assign_flow, phi))
}

#[cfg(test)]
mod tests {
    use crate::testutils::*;

    #[test]
    fn convert_object_patterns() {
        let s = test_basic_blocks(
            "var obj = {};
            var { a: { b: c = 4 } } = obj;
            c;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = {}
            $1 = $0
            $2 = pack $1 {a: _}
            $3 = unpack $2[0]
            $4 = pack $3 {b: _}
            $5 = unpack $4[0]
            $6 = undefined
            $7 = $5 === $6
            exit = cond $7 ? @1..@1 : @2..@2
        }
        @1: {
            $8 = 4
        }
        @2: {
            $9 = $5
        }
        @3: {
            $10 = either($8, $9)
            $11 = $10
        }
        "###);

        let s = test_basic_blocks(
            "var obj = { 'a': 1 };
            var { a: { b = 4 } } = obj;
            b;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            $1 = {a: $0}
            $2 = $1
            $3 = pack $2 {a: _}
            $4 = unpack $3[0]
            $5 = pack $4 {b: _}
            $6 = unpack $5[0]
            $7 = undefined
            $8 = $6 === $7
            exit = cond $8 ? @1..@1 : @2..@2
        }
        @1: {
            $9 = 4
        }
        @2: {
            $10 = $6
        }
        @3: {
            $11 = either($9, $10)
            $12 = $11
        }
        "###);
    }

    #[test]
    fn convert_array_patterns() {
        let s = test_basic_blocks(
            "var arr = [];
            var [a, ...b] = arr;
            a;
            b;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = []
            $1 = $0
            $2 = pack $1 [_, _...]
            $3 = unpack $2[0]
            $4 = unpack $2[1]
            $5 = $3
            $6 = $4
        }
        "###);

        let s = test_basic_blocks(
            "var arr = [];
            var [a, ...b] = arr;
            a;
            b;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = []
            $1 = $0
            $2 = pack $1 [_, _...]
            $3 = unpack $2[0]
            $4 = unpack $2[1]
            $5 = $3
            $6 = $4
        }
        "###);
    }

    #[test]
    fn convert_patterns_expr() {
        let s = test_basic_blocks(
            "var obj = {};
            var a
            ([a] = obj);
            ({ '0': a } = obj);",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = {}
            $1 = undefined
            $2 = $0
            $3 = pack $2 [_]
            $4 = unpack $3[0]
            $5 = $4
            $6 = $0
            $7 = pack $6 {0: _}
            $8 = unpack $7[0]
            $9 = $8
        }
        "###);
    }

    #[test]
    fn convert_spread() {
        let s = test_basic_blocks(
            "var obj = {};
            var {a, ...rest, b} = obj;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = {}
            $1 = $0
            $2 = pack $1 {a: _, ..._, b: _}
            $3 = unpack $2[0]
            $4 = unpack $2[1]
            $5 = unpack $2[2]
        }
        "###);

        let s = test_basic_blocks(
            "var arr = [];
            var [a, ...rest] = arr;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = []
            $1 = $0
            $2 = pack $1 [_, _...]
            $3 = unpack $2[0]
            $4 = unpack $2[1]
        }
        "###);
    }

    #[test]
    fn convert_computed() {
        let s = test_basic_blocks(
            "var obj = {};
            var {a, [1]: b} = obj;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = {}
            $1 = $0
            $2 = 1
            $3 = pack $1 {a: _, [$2]: _}
            $4 = unpack $3[0]
            $5 = unpack $3[1]
        }
        "###);
    }

    /* TODO: Order should be 100 200 300 400.
    #[test]
    fn convert_order() {
        let s = test_basic_blocks("var {a: {[300]: a} = (200, {}), [400]: b, x } = 100;");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 100
            $1 = 400
            $2 = pack $0 {a: _, [$1]: _, x: _}
            $3 = unpack $2[0]
            $4 = unpack $2[1]
            $5 = unpack $2[2]
            $6 = undefined
            $7 = $3 === $6
            exit = cond $7 ? @1..@1 : @2..@2
        }
        @1: {
            $8 = 200
            $9 = {}
            exit = jump @3
        }
        @2: {
            $10 = $3
            exit = jump @3
        }
        @3: {
            $11 = either($9, $10)
            $12 = 300
            $13 = pack $11 {[$12]: _}
            $14 = unpack $13[0]
            $15 = undefined
            exit = return $15
        }
        "###);
    } */

    #[test]
    fn convert_nested_obj_spread() {
        let s = test_basic_blocks(
            "var b, obj = { x: { a: b, c } };
            var { x: { a, c = b } = {}, ...rest } = obj;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = undefined
            $1 = $0
            $2 = undefined
            $3 = $2
            $4 = {a: $1, c: $3}
            $5 = {x: $4}
            $6 = $5
            $7 = pack $6 {x: _, ..._}
            $8 = unpack $7[0]
            $9 = unpack $7[1]
            $10 = undefined
            $11 = $8 === $10
            exit = cond $11 ? @1..@1 : @2..@2
        }
        @1: {
            $12 = {}
        }
        @2: {
            $13 = $8
        }
        @3: {
            $14 = either($12, $13)
            $15 = pack $14 {a: _, c: _}
            $16 = unpack $15[0]
            $17 = unpack $15[1]
            $18 = undefined
            $19 = $17 === $18
            exit = cond $19 ? @4..@4 : @5..@5
        }
        @4: {
            $20 = $0
        }
        @5: {
            $21 = $17
        }
        @6: {
            $22 = either($20, $21)
        }
        "###);

        let s = test_basic_blocks(
            "var arr = [];
            var [a, ...rest] = arr;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = []
            $1 = $0
            $2 = pack $1 [_, _...]
            $3 = unpack $2[0]
            $4 = unpack $2[1]
        }
        "###);
    }
}
