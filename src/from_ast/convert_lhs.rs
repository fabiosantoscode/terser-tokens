use swc_ecma_ast::{Expr, MemberExpr, MemberProp};

use crate::basic_blocks::{ObjectKey, StructuredFlow, LHS};

use super::{expr_to_basic_blocks, FromAstCtx};

pub fn to_basic_blocks_lhs(
    ctx: &mut FromAstCtx,
    expr: &Expr,
) -> Result<(Vec<StructuredFlow>, LHS), String> {
    match expr {
        Expr::Ident(ident) => ctx.get_lhs_for_name(&ident.sym.to_string()),
        Expr::Member(MemberExpr { obj, prop, .. }) => {
            let mut member_flow = Vec::new();

            let (flow, obj) = to_basic_blocks_lhs(ctx, &obj)?;
            member_flow.extend(flow);

            let prop = match prop {
                MemberProp::Ident(ident) => ObjectKey::NormalKey(ident.sym.to_string()),
                MemberProp::PrivateName(pvt) => ObjectKey::Private(pvt.id.sym.to_string()),
                MemberProp::Computed(expr) => {
                    let (flow, expr) = expr_to_basic_blocks(ctx, &expr.expr)?;
                    member_flow.extend(flow);

                    ObjectKey::Computed(expr)
                }
            };

            Ok((member_flow, LHS::Member(Box::new(obj), prop)))
        }
        expr => {
            let (flow, expr) = expr_to_basic_blocks(ctx, expr)?;

            Ok((flow, LHS::Local(expr)))
        }
    }
}
