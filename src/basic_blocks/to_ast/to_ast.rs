use swc_ecma_ast::{
    AwaitExpr, BindingIdent, Decl, Expr, ExprOrSpread, Ident, Module, ModuleItem, ReturnStmt, Stmt,
    ThrowStmt, YieldExpr,
};

use crate::basic_blocks::{
    basic_block::{ArrayElement, BasicBlockInstruction, ExitType, TempExitType},
    basic_block_group::BasicBlockGroup,
    to_basic_blocks::convert_module::BasicBlockModule,
};

use super::{
    remove_phi::remove_phi,
    to_structured_flow::{do_tree, StructuredFlow},
};

pub fn module_to_ast(block_module: &BasicBlockModule) -> Module {
    Module {
        span: Default::default(),
        body: to_ast(&block_module.top_level_stats)
            .into_iter()
            .map(|stat| ModuleItem::Stmt(stat))
            .collect::<Vec<_>>(),
        shebang: None,
    }
}

pub fn to_ast(block_group: &BasicBlockGroup) -> Vec<Stmt> {
    let mut block_group: BasicBlockGroup = block_group.clone();

    remove_phi(&mut block_group);

    let tree = do_tree(&block_group);

    to_stat_ast(&mut Default::default(), &tree, &block_group)
}

#[derive(Default)]
struct ToAstContext {
    caught_error: Option<String>,
    error_counter: usize,
}

impl ToAstContext {
    pub fn get_caught_error(&mut self) -> String {
        self.caught_error
            .take()
            .expect("reference to caught error must be inside catch block")
    }

    pub fn set_caught_error(&mut self) -> String {
        self.error_counter += 1;
        let varname = format!("$error{}", self.error_counter);
        self.caught_error = Some(varname.clone());
        varname
    }
}

fn get_variable(var_idx: usize) -> String {
    format!("${}", var_idx)
}

fn get_identifier(i: String) -> Expr {
    Expr::Ident(Ident::new(i.into(), Default::default()))
}

fn var_decl(name: &str, value: Expr) -> Stmt {
    let varname = Ident::new(name.into(), Default::default());
    let vardecl = swc_ecma_ast::VarDecl {
        span: Default::default(),
        kind: swc_ecma_ast::VarDeclKind::Var,
        declare: false,
        decls: vec![swc_ecma_ast::VarDeclarator {
            span: Default::default(),
            name: swc_ecma_ast::Pat::Ident(BindingIdent {
                id: varname,
                type_ann: None,
            }),
            init: Some(Box::new(value)),
            definite: false,
        }],
    };
    let vardecl = Stmt::Decl(Decl::Var(Box::new(vardecl)));

    vardecl
}

fn get_block(stats: Vec<Stmt>) -> Stmt {
    Stmt::Block(swc_ecma_ast::BlockStmt {
        span: Default::default(),
        stmts: stats,
    })
}

fn to_stat_ast(
    ctx: &mut ToAstContext,
    node: &StructuredFlow,
    block_group: &BasicBlockGroup,
) -> Vec<Stmt> {
    let to_stat_vec = |ctx: &mut ToAstContext, stats: &Vec<StructuredFlow>| -> Vec<Stmt> {
        stats
            .iter()
            .flat_map(|stat| to_stat_ast(ctx, stat, block_group))
            .collect()
    };

    match node {
        StructuredFlow::Block(_, stats) => to_stat_vec(ctx, stats),
        StructuredFlow::BasicBlock(block_idx) => {
            let stats = &block_group.blocks[*block_idx].1.instructions;

            stats
                .iter()
                .map(|(variable, instruction)| {
                    let expression: Expr = to_expr_ast(ctx, instruction);

                    var_decl(&format!("${}", variable), expression)
                })
                .collect::<Vec<_>>()
        }
        StructuredFlow::Return(ExitType::Return, Some(var_idx)) => {
            let varname = Ident::new(format!("${}", var_idx).into(), Default::default());

            let return_stmt = Stmt::Return(ReturnStmt {
                span: Default::default(),
                arg: Some(Box::new(Expr::Ident(Ident {
                    span: Default::default(),
                    sym: varname.sym.clone(),
                    optional: false,
                }))),
            });

            vec![return_stmt]
        }
        StructuredFlow::Return(ExitType::Throw, Some(var_idx)) => {
            let varname = Ident::new(format!("${}", var_idx).into(), Default::default());

            let throw_stmt = Stmt::Throw(ThrowStmt {
                span: Default::default(),
                arg: (Box::new(Expr::Ident(Ident {
                    span: Default::default(),
                    sym: varname.sym.clone(),
                    optional: false,
                }))),
            });

            vec![throw_stmt]
        }
        StructuredFlow::Branch(_, branch_expr, cons, alt) => {
            let branch_expr = get_identifier(get_variable(*branch_expr));
            let cons = to_stat_vec(ctx, cons);
            let alt = to_stat_vec(ctx, alt);

            let if_stmt = Stmt::If(swc_ecma_ast::IfStmt {
                span: Default::default(),
                test: Box::new(branch_expr),
                cons: Box::new(get_block(cons)),
                alt: Some(Box::new(get_block(alt))),
            });

            vec![if_stmt]
        }
        StructuredFlow::Break(to_id) => {
            let break_stmt = Stmt::Break(swc_ecma_ast::BreakStmt {
                span: Default::default(),
                label: None, /* TODO */
            });

            vec![break_stmt]
        }
        StructuredFlow::Continue(to_id) => {
            let break_stmt = Stmt::Continue(swc_ecma_ast::ContinueStmt {
                span: Default::default(),
                label: None, /* TODO */
            });

            vec![break_stmt]
        }
        StructuredFlow::Loop(id, body) => {
            let body = to_stat_vec(ctx, body);

            let while_stmt = Stmt::While(swc_ecma_ast::WhileStmt {
                span: Default::default(),
                test: Box::new(Expr::Lit(true.into())),
                body: Box::new(get_block(body)),
            });

            vec![while_stmt]
        }
        StructuredFlow::TryCatch(_id, try_block, catch_block, finally_block, after_block) => {
            let try_block = to_stat_vec(ctx, try_block);

            let catch_handler = ctx.set_caught_error();
            let catch_block = to_stat_vec(ctx, catch_block);

            let finally_block = to_stat_vec(ctx, finally_block);
            let after_block = to_stat_vec(ctx, after_block);

            let try_catch_stmt = Stmt::Try(Box::new(swc_ecma_ast::TryStmt {
                span: Default::default(),
                block: swc_ecma_ast::BlockStmt {
                    span: Default::default(),
                    stmts: try_block,
                },
                handler: Some(swc_ecma_ast::CatchClause {
                    span: Default::default(),
                    param: Some(swc_ecma_ast::Pat::Ident(BindingIdent {
                        id: swc_ecma_ast::Ident {
                            span: Default::default(),
                            sym: catch_handler.into(),
                            optional: false,
                        },
                        type_ann: None,
                    })),
                    body: swc_ecma_ast::BlockStmt {
                        span: Default::default(),
                        stmts: catch_block,
                    },
                }),
                finalizer: match finally_block.len() {
                    0 => None,
                    _ => Some(swc_ecma_ast::BlockStmt {
                        span: Default::default(),
                        stmts: finally_block,
                    }),
                },
            }));

            vec![vec![try_catch_stmt], after_block].concat()
        }
        _ => {
            todo!("to_stat: {:?}", node)
        }
    }
}

fn to_expr_ast(ctx: &mut ToAstContext, expr: &BasicBlockInstruction) -> Expr {
    match expr {
        BasicBlockInstruction::LitNumber(num) => (*num).into(),
        BasicBlockInstruction::Undefined => {
            Expr::Ident(Ident::new("undefined".into(), Default::default()))
        }
        BasicBlockInstruction::BinOp(name, left, right) => {
            let left = get_identifier(get_variable(*left));
            let right = get_identifier(get_variable(*right));

            Expr::Bin(swc_ecma_ast::BinExpr {
                span: Default::default(),
                op: swc_ecma_ast::BinaryOp::Add, /* major TODO lol */
                left: Box::new(left),
                right: Box::new(right),
            })
        }
        BasicBlockInstruction::Ref(var_idx) => Expr::Ident(Ident::new(
            get_variable(*var_idx).into(),
            Default::default(),
        )),
        BasicBlockInstruction::This => Expr::Ident(Ident::new("this".into(), Default::default())),
        BasicBlockInstruction::Array(items) => {
            let items = items
                .iter()
                .map(|item| match item {
                    ArrayElement::Item(var_idx) => Some(ExprOrSpread {
                        spread: None,
                        expr: Box::new(get_identifier(get_variable(*var_idx))),
                    }),
                    ArrayElement::Spread(var_idx) => Some(ExprOrSpread {
                        spread: Some(Default::default()),
                        expr: Box::new(get_identifier(get_variable(*var_idx))),
                    }),
                    ArrayElement::Hole => None,
                })
                .collect();

            Expr::Array(swc_ecma_ast::ArrayLit {
                span: Default::default(),
                elems: items,
            })
        }

        BasicBlockInstruction::TempExit(typ, arg) => match typ {
            TempExitType::Yield => Expr::Yield(YieldExpr {
                span: Default::default(),
                delegate: false,
                arg: Some(Box::new(get_identifier(get_variable(*arg)))),
            }),
            TempExitType::YieldStar => Expr::Yield(YieldExpr {
                span: Default::default(),
                delegate: true,
                arg: Some(Box::new(get_identifier(get_variable(*arg)))),
            }),
            TempExitType::Await => Expr::Await(AwaitExpr {
                span: Default::default(),
                arg: Box::new(get_identifier(get_variable(*arg))),
            }),
        },

        BasicBlockInstruction::CaughtError => Expr::Ident(Ident::new(
            ctx.get_caught_error().into(),
            Default::default(),
        )),

        BasicBlockInstruction::Phi(_) => unreachable!("phi should be removed by remove_phi()"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_blocks::testutils::{stats_to_string, test_basic_blocks};

    #[test]
    fn to_tree() {
        let block_group = test_basic_blocks("1 + 2 + 3");

        let tree = to_ast(&block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var $0 = 1;
        var $1 = 2;
        var $2 = $0 + $1;
        var $3 = 3;
        var $4 = $2 + $3;
        var $5 = undefined;
        return $5;
        "###);
    }

    #[test]
    fn to_tree_cond() {
        let block_group = test_basic_blocks("1 ? 2 : 3");

        let tree = to_ast(&block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var $0 = 1;
        if ($0) {
            var $3 = 2;
        } else {
            var $3 = 3;
        }
        var $4 = undefined;
        return $4;
        "###);
    }

    #[test]
    fn to_loop() {
        let block_group = test_basic_blocks("while (123) { if (456) { break; } }");

        let tree = to_ast(&block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        while(true){
            var $0 = 123;
            if ($0) {
                var $1 = 456;
                if ($1) {} else {
                    continue;
                }
            } else {}
            var $2 = undefined;
            return $2;
        }
        "###);
    }

    #[test]
    fn to_conditional_var() {
        let block_group = test_basic_blocks(
            "var x = 10;
            if (123) { x = 456; } else { x = 789; }
            x + 1",
        );

        let tree = to_ast(&block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var $0 = 10;
        var $1 = 123;
        if ($1) {
            var $4 = 456;
        } else {
            var $4 = 789;
        }
        var $5 = $4;
        var $6 = 1;
        var $7 = $5 + $6;
        var $8 = undefined;
        return $8;
        "###);
    }

    #[test]
    fn to_trycatch() {
        let block_group = test_basic_blocks(
            "var x = 10;
            try {
                if (x > 10) {
                    throw 123;
                }
            } catch (e) {
                x = 456;
            }
            return x;",
        );

        let tree = to_ast(&block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var $0 = 10;
        try {
            var $1 = $0;
            var $2 = 10;
            var $3 = $1 + $2;
            if ($3) {
                var $4 = 123;
                throw $4;
            } else {}
        } catch ($error1) {
            var $5 = $error1;
            var $6 = 456;
        }
        var $7 = $6;
        return $7;
        "###);
    }

    #[test]
    fn to_trycatch_nested() {
        let block_group = test_basic_blocks(
            "var x = 10;
            try {
                if (x > 10) {
                    throw 123;
                }
            } catch (e) {
                try {
                    123
                } catch (e2) {
                    return e + e2;
                }
            }
            return x;",
        );

        let tree = to_ast(&block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var $0 = 10;
        try {
            var $1 = $0;
            var $2 = 10;
            var $3 = $1 + $2;
            if ($3) {
                var $4 = 123;
                throw $4;
            } else {}
        } catch ($error1) {
            var $5 = $error1;
            try {
                var $6 = 123;
            } catch ($error2) {
                var $7 = $error2;
                var $8 = $5;
                var $9 = $7;
                var $10 = $8 + $9;
                return $10;
            }
        }
        var $11 = $0;
        return $11;
        "###);
    }
}
