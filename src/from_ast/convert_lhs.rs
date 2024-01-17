use swc_ecma_ast::{Expr, MemberExpr, MemberProp};

use crate::basic_blocks::{ObjectKey, LHS};

use super::{expr_to_basic_blocks, FromAstCtx};

pub fn to_basic_blocks_lhs(ctx: &mut FromAstCtx, expr: &Expr) -> LHS {
    match expr {
        Expr::Ident(ident) => ctx.get_lhs_for_name(&ident.sym.to_string()),
        Expr::Member(MemberExpr { obj, prop, .. }) => {
            let obj = to_basic_blocks_lhs(ctx, &obj);
            let prop = match prop {
                MemberProp::Ident(ident) => ObjectKey::NormalKey(ident.sym.to_string()),
                MemberProp::PrivateName(pvt) => ObjectKey::Private(pvt.id.sym.to_string()),
                MemberProp::Computed(expr) => {
                    ObjectKey::Computed(expr_to_basic_blocks(ctx, &expr.expr))
                }
            };
            LHS::Member(Box::new(obj), prop)
        }
        expr => {
            let expr = expr_to_basic_blocks(ctx, expr);
            LHS::Local(expr)
        }
    }
}
