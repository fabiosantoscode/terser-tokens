use swc_ecma_ast::{Expr, MemberExpr, MemberProp};

use crate::basic_blocks::{ObjectMember, LHS};

use super::{expr_or_ref, FromAstCtx};

pub fn to_basic_blocks_lhs(ctx: &mut FromAstCtx, expr: &Expr) -> LHS {
    match expr {
        Expr::Ident(ident) => ctx.get_lhs_for_name(&ident.sym.to_string()),
        Expr::Member(MemberExpr { obj, prop, .. }) => {
            let obj = to_basic_blocks_lhs(ctx, &obj);
            let prop = match prop {
                MemberProp::Ident(ident) => ObjectMember::KeyValue(ident.sym.to_string()),
                MemberProp::PrivateName(pvt) => ObjectMember::Private(pvt.id.sym.to_string()),
                MemberProp::Computed(expr) => {
                    ObjectMember::Computed(expr_or_ref(ctx, &expr.expr, true))
                }
            };
            LHS::Member(Box::new(obj), prop)
        }
        expr => LHS::Local(expr_or_ref(ctx, expr, true)),
    }
}
