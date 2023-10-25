use swc_ecma_ast::{
    AssignPat, BinaryOp, BindingIdent, Expr, MemberExpr, MemberProp, ObjectPatProp, Pat, PropName,
    RestPat, SuperPropExpr,
};

use crate::{
    basic_blocks::{
        ArrayPatternPiece, BasicBlockExit, BasicBlockInstruction, ObjectMember, ObjectPatternPiece,
    },
    from_ast::expr_to_basic_blocks,
};

use super::FromAstCtx;

#[derive(Clone, Copy, Debug)]
pub enum PatType {
    Assign,
    VarDecl,
    FunArg,
}

pub fn pat_to_basic_blocks(
    ctx: &mut FromAstCtx,
    pat_type: PatType,
    pat: &Pat,
    input: usize,
) -> usize {
    match pat {
        Pat::Ident(ident) => ident_pat(ctx, pat_type, &ident.id.sym.to_string(), input),
        Pat::Assign(assign_pat) => {
            let input = default_assign_pat(ctx, input, &assign_pat.right);
            pat_to_basic_blocks(ctx, pat_type, &assign_pat.left, input)
        }
        Pat::Expr(expr) => pat_like_expr_to_basic_blocks(ctx, pat_type, expr, input),
        Pat::Array(array_pat) => {
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

            let pattern = ctx.push_instruction(BasicBlockInstruction::ArrayPattern(input, items));

            // Then, we create an unpacker for each item
            let unpackers = array_pat
                .elems
                .iter()
                .enumerate()
                .map(|(i, elem)| {
                    let unpacker =
                        ctx.push_instruction(BasicBlockInstruction::PatternUnpack(pattern, i));

                    (elem, unpacker)
                })
                .collect::<Vec<_>>();

            // Finally, we recurse on each item
            for (elem, unpacked) in unpackers {
                // unpack pattern_unpacker[i]
                match elem {
                    Some(Pat::Rest(RestPat { arg: elem, .. })) => {
                        pat_to_basic_blocks(ctx, pat_type, elem, unpacked);
                    }
                    Some(elem) => {
                        pat_to_basic_blocks(ctx, pat_type, elem, unpacked);
                    }
                    None => {
                        // hole; this resulting instruction will be unused
                    }
                }
            }

            return input;
        }
        Pat::Object(object_pat) => {
            assert!(!object_pat.optional);

            // First, take stock of what items exist and create an ObjectPattern
            let items = object_pat
                .props
                .iter()
                .map(|prop| match prop {
                    ObjectPatProp::KeyValue(kv) => match &kv.key {
                        PropName::Computed(computed) => {
                            let key = expr_to_basic_blocks(ctx, computed.expr.as_ref());
                            ObjectPatternPiece::TakeComputedKey(key)
                        }
                        _ => ObjectPatternPiece::TakeKey(object_propname_to_string(&kv.key)),
                    },
                    ObjectPatProp::Assign(a) => ObjectPatternPiece::TakeKey(a.key.sym.to_string()),
                    ObjectPatProp::Rest(_r) => ObjectPatternPiece::Spread,
                })
                .collect::<Vec<_>>();

            let pattern = ctx.push_instruction(BasicBlockInstruction::ObjectPattern(input, items));

            // Then, we create an unpacker for each item
            let unpackers = object_pat
                .props
                .iter()
                .enumerate()
                .map(|(i, elem)| {
                    let unpacker =
                        ctx.push_instruction(BasicBlockInstruction::PatternUnpack(pattern, i));

                    (elem, unpacker)
                })
                .collect::<Vec<_>>();

            // Finally, we recurse on each item
            for (prop, unpacker) in unpackers {
                match prop {
                    ObjectPatProp::KeyValue(kv) => {
                        pat_to_basic_blocks(ctx, pat_type, &kv.value, unpacker);
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
                            pat_to_basic_blocks(ctx, pat_type, &ident, unpacker);
                        } else {
                            pat_to_basic_blocks(ctx, pat_type, &ident, unpacker);
                        }
                    }
                    ObjectPatProp::Rest(rest) => {
                        pat_to_basic_blocks(ctx, pat_type, &rest.arg, unpacker);
                    }
                }
            }

            ctx.wrap_up_block();

            return input;
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
) -> usize {
    match expr {
        Expr::Ident(ident) => ident_pat(ctx, pat_type, &ident.sym.to_string(), input),
        Expr::SuperProp(SuperPropExpr { .. }) => todo!(),
        Expr::MetaProp(_) => todo!(),
        Expr::Member(MemberExpr { obj, prop, .. }) => {
            let base = expr_to_basic_blocks(ctx, obj.as_ref());
            let prop = match &prop {
                MemberProp::Ident(ident) => ObjectMember::KeyValue(ident.sym.to_string()),
                MemberProp::PrivateName(pvt) => ObjectMember::Private(pvt.id.sym.to_string()),
                MemberProp::Computed(comp) => {
                    let comp = expr_to_basic_blocks(ctx, comp.expr.as_ref());
                    ObjectMember::Computed(comp)
                }
            };

            return ctx.push_instruction(BasicBlockInstruction::MemberSet(base, prop, input));
        }
        _ => unreachable!(
            "pattern expression must be an identifier or member expression, got: {:?}",
            expr
        ),
    }
}

fn ident_pat(ctx: &mut FromAstCtx, pat_type: PatType, name: &str, input: usize) -> usize {
    use PatType::*;

    match pat_type {
        VarDecl | FunArg => {
            ctx.declare_name(name, input);

            input
        }
        Assign => {
            ctx.assign_name(name, input);

            ctx.push_instruction(BasicBlockInstruction::Ref(input))
        }
    }
}

pub fn object_propname_to_string(propname: &PropName) -> String {
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

fn default_assign_pat(ctx: &mut FromAstCtx, input: usize, by_default: &Expr) -> usize {
    let undef = ctx.push_instruction(BasicBlockInstruction::Undefined);
    let test = ctx.push_instruction(BasicBlockInstruction::BinOp(BinaryOp::EqEqEq, input, undef));
    let blockidx_before = ctx.wrap_up_block();
    ctx.wrap_up_block();

    ctx.enter_conditional_branch();

    let blockidx_consequent_before = ctx.current_block_index();
    let default_value = expr_to_basic_blocks(ctx, by_default);
    let blockidx_consequent_after = ctx.current_block_index();

    let blockidx_alternate_before = ctx.wrap_up_block();
    let input_value = ctx.push_instruction(BasicBlockInstruction::Ref(input));
    let blockidx_alternate_after = ctx.current_block_index();

    let blockidx_after = ctx.wrap_up_block();
    ctx.wrap_up_block();

    ctx.leave_conditional_branch();

    ctx.set_exit(
        blockidx_before,
        BasicBlockExit::Cond(
            test,
            blockidx_consequent_before,
            blockidx_consequent_after,
            blockidx_alternate_before,
            blockidx_alternate_after,
        ),
    );
    ctx.set_exit(
        blockidx_consequent_after,
        BasicBlockExit::Jump(blockidx_after),
    );
    ctx.set_exit(
        blockidx_alternate_after,
        BasicBlockExit::Jump(blockidx_after),
    );

    ctx.push_instruction(BasicBlockInstruction::Phi(vec![input_value, default_value]))
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
            exit = jump @3
        }
        @2: {
            $9 = $5
            exit = jump @3
        }
        @3: {
            $10 = either($8, $9)
            $11 = $10
            $12 = undefined
            exit = return $12
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
            exit = jump @3
        }
        @2: {
            $10 = $6
            exit = jump @3
        }
        @3: {
            $11 = either($9, $10)
            $12 = $11
            $13 = undefined
            exit = return $13
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
            $7 = undefined
            exit = return $7
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
            $7 = undefined
            exit = return $7
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
            $10 = undefined
            exit = return $10
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
            $6 = undefined
            exit = return $6
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
            $5 = undefined
            exit = return $5
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
            $6 = undefined
            exit = return $6
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
            exit = jump @3
        }
        @2: {
            $13 = $8
            exit = jump @3
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
            exit = jump @6
        }
        @5: {
            $21 = $17
            exit = jump @6
        }
        @6: {
            $22 = either($20, $21)
            $23 = undefined
            exit = return $23
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
            $5 = undefined
            exit = return $5
        }
        "###);
    }
}
