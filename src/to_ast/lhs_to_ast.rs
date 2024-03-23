use swc_ecma_ast::{ComputedPropName, Expr, Ident, MemberExpr, MemberProp, PrivateName};

use crate::basic_blocks::{Instruction, ObjectKey, LHS};

use super::{build_identifier, build_identifier_str, ref_or_inlined_expr, ToAstContext};

pub fn lhs_to_ast_expr(ctx: &mut ToAstContext, lhs: &LHS) -> Expr {
    lhs_to_ast_expr_inner(ctx, lhs, 0)
}

fn lhs_to_ast_expr_inner(ctx: &mut ToAstContext, lhs: &LHS, depth: usize) -> Expr {
    match lhs {
        LHS::Local(v) => {
            if depth == 0 {
                build_identifier(ctx.get_varname_for(*v))
            } else {
                ref_or_inlined_expr(ctx, *v)
            }
        }
        LHS::NonLocal(nonloc) => build_identifier(ctx.get_varname_for(nonloc.0)),
        LHS::Global(varname) => build_identifier_str(&varname),
        LHS::Member(lhs, member) => match lhs.as_ref() {
            LHS::Local(v)
                if matches!(ctx.peek_inlined_expression(*v), Some(Instruction::Super)) =>
            {
                // Super props are special!

                Expr::SuperProp(swc_ecma_ast::SuperPropExpr {
                    span: Default::default(),
                    obj: swc_ecma_ast::Super {
                        span: Default::default(),
                    },
                    prop: match member {
                        ObjectKey::NormalKey(member) => swc_ecma_ast::SuperProp::Ident(Ident::new(
                            member.as_str().into(),
                            Default::default(),
                        )),
                        ObjectKey::Private(_) => unreachable!(),
                        ObjectKey::Computed(member) => {
                            swc_ecma_ast::SuperProp::Computed(ComputedPropName {
                                span: Default::default(),
                                expr: Box::new(ref_or_inlined_expr(ctx, *member)),
                            })
                        }
                    },
                })
            }
            _ => {
                let base = lhs_to_ast_expr_inner(ctx, lhs, depth + 1);

                Expr::Member(MemberExpr {
                    span: Default::default(),
                    obj: Box::new(base),
                    prop: match member {
                        ObjectKey::NormalKey(member) => MemberProp::Ident(Ident::new(
                            member.as_str().into(),
                            Default::default(),
                        )),
                        ObjectKey::Private(member) => MemberProp::PrivateName(PrivateName {
                            span: Default::default(),
                            id: Ident::new(member.as_str().into(), Default::default()),
                        }),
                        ObjectKey::Computed(member) => MemberProp::Computed(ComputedPropName {
                            span: Default::default(),
                            expr: Box::new(ref_or_inlined_expr(ctx, *member)),
                        }),
                    },
                })
            }
        },
    }
}
