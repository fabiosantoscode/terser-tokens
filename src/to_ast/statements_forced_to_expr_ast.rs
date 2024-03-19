use super::{build_iife, ref_or_inlined_expr, to_statements, ToAstContext};
use crate::basic_blocks::StructuredFlow;
use swc_ecma_ast::{Expr, ParenExpr, Stmt};

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

    let expr = if all_inlined {
        ref_or_inlined_expr(ctx, varname)
    } else if let Some(expr) = stats_to_expr(&all_stats[..]) {
        Expr::Paren(ParenExpr {
            span: Default::default(),
            expr: Box::new(Expr::Seq(swc_ecma_ast::SeqExpr {
                span: Default::default(),
                exprs: vec![Box::new(expr), Box::new(ref_or_inlined_expr(ctx, varname))],
            })),
        })
    } else {
        let ret_expr = ref_or_inlined_expr(ctx, varname);

        build_iife(all_stats, ret_expr)
    };

    ctx.forbid_var_decls = save_forbid;

    expr
}

fn stats_to_expr(stats: &[Stmt]) -> Option<Expr> {
    match stats.len() {
        0 => None,
        1 => match &stats[0] {
            Stmt::Expr(expr) => Some(*expr.expr.clone()),
            _ => None,
        },
        _ => None, // We could use Seq
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutils::*;

    #[test]
    fn force_expr_simple() {
        let mut block_module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                $1 = 3
                $2 = $0 + $1
                exit = return $2
            }",
        ]);

        let (mut ctx, root) = ToAstContext::new(&mut block_module);

        let child = root.children()[0][0].clone();

        let expr = statements_forced_to_expr_ast(&mut ctx, vec![child].as_slice(), 2);

        insta::assert_display_snapshot!(expr_to_string(&expr), @r###"1 + 3;"###);
    }

    #[test]
    fn force_expr_needs_stat() {
        let mut block_module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                $1 = 3
                $2 = $0 + $1
                exit = throw $2
            }
            @1: {
                $3 = $2
            }",
        ]);

        let (mut ctx, root) = ToAstContext::new(&mut block_module);

        let child = root.children()[0]
            .iter()
            .cloned()
            .cloned()
            .collect::<Vec<_>>();

        let expr = statements_forced_to_expr_ast(&mut ctx, &child[..], 2);
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
        let mut block_module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                exit = cond $0 ? @1..@1 : @2..@2
            }
            @1: {
                $1 = 3
                exit = jump @3
            }
            @2: {
                $2 = 4
                exit = jump @3
            }
            @3: {
                $3 = either($1, $2)
                exit = return $3
            }",
        ]);

        let (mut ctx, mut root) = ToAstContext::new(&mut block_module);

        let children = vec![
            root.children()[0].clone()[0].clone(),
            root.children()[0].clone()[1].clone(),
            root.children()[0].clone()[2].clone(),
        ];

        let expr = statements_forced_to_expr_ast(&mut ctx, children.as_slice(), 1);

        insta::assert_display_snapshot!(expr_to_string(&expr), @r###"
        (()=>{
            if (1) {
                a = 3;
            } else {
                a = 4;
            }
            return a;
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
