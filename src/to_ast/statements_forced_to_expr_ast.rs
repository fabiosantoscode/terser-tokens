use super::{build_iife, ref_or_inlined_expr, to_statements, ToAstContext};
use crate::basic_blocks::StructuredFlow;
use swc_ecma_ast::{Expr, ParenExpr, SeqExpr, Stmt};

/// Convert a bunch of statements into an expression, even if we need to utilize an IIFE
pub fn statements_forced_to_expr_ast(
    ctx: &mut ToAstContext,
    block: &[StructuredFlow],
    varname: usize,
) -> Expr {
    let save_forbid = ctx.forbid_var_decls;
    ctx.forbid_var_decls = true;

    let all_stats = block
        .iter()
        .flat_map(|item| to_statements(ctx, item))
        .collect::<Vec<_>>();

    let all_inlined = all_stats.len() == 0;

    let ret_expr = ref_or_inlined_expr(ctx, varname);

    let expr = if all_inlined {
        ret_expr
    } else if let Some(exprs) = all_exprs(&all_stats[..]) {
        match exprs.len() {
            0 => ret_expr,
            _ => Expr::Paren(ParenExpr {
                span: Default::default(),
                expr: Box::new(Expr::Seq(SeqExpr {
                    span: Default::default(),
                    exprs: exprs
                        .into_iter()
                        .chain(std::iter::once(ret_expr))
                        .map(Box::new)
                        .collect(),
                })),
            }),
        }
    } else {
        build_iife(all_stats, ret_expr)
    };

    ctx.forbid_var_decls = save_forbid;

    expr
}

fn all_exprs(stats: &[Stmt]) -> Option<Vec<Expr>> {
    let mut out = vec![];

    for stat in stats {
        match stat {
            Stmt::Expr(expr) => out.push(*expr.expr.clone()),
            _ => return None,
        }
    }

    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutils::*;

    #[test]
    fn force_expr_simple() {
        let block_module = parse_test_module(vec![
            "{
                $0 = 1
                $1 = 3
                $2 = $0 + $1
            }",
        ]);

        let (mut ctx, root) = ToAstContext::new_for_test(block_module, 2);

        let expr = statements_forced_to_expr_ast(&mut ctx, &vec![root], 2);

        insta::assert_display_snapshot!(expr_to_string(&expr), @r###"
        (a = 1 + 3, a);
        "###);
    }

    #[test]
    fn force_expr_needs_stat() {
        let block_module = parse_test_module(vec![
            "{
                $0 = 1
                $1 = 3
                $2 = $0 + $1
                Throw $2
                $3 = $2
            }",
        ]);

        let (mut ctx, root) = ToAstContext::new_for_test(block_module, 2);

        let expr = statements_forced_to_expr_ast(&mut ctx, &vec![root], 2);
        insta::assert_display_snapshot!(expr_to_string(&expr), @r###"
        (()=>{
            a = 1 + 3;
            throw a;
            return a;
        })();
        "###);

        // $2 is reused
        insta::assert_debug_snapshot!(ctx.dequeue_enqueued_vars(), @"
            {
                2,
            }
        ");
    }

    #[test]
    fn force_expr_needs_cond() {
        let block_module = parse_test_module(vec![
            "{
                $0 = 1
                if ($0) {
                    $1 = 3
                } else {
                    $2 = 4
                }
                $3 = either($1, $2)
            }",
        ]);

        let (mut ctx, root) = ToAstContext::new_for_test(block_module, 1);

        let expr = statements_forced_to_expr_ast(&mut ctx, &vec![root], 1);

        insta::assert_display_snapshot!(expr_to_string(&expr), @r###"
        (()=>{
            if (1) {
                a = 3;
            } else {
                a = 4;
            }
            return a;
        })();
        "###);
        // $1 is NOT reused but we enqueue it for now
        insta::assert_debug_snapshot!(ctx.dequeue_enqueued_vars(), @r###"
        {
            1,
        }
        "###);
    }
}
