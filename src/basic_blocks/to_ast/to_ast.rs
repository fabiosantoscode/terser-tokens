use swc_ecma_ast::{BindingIdent, Decl, Expr, Ident, ReturnStmt, Stmt};

use crate::basic_blocks::{
    basic_block::{BasicBlockInstruction, ExitType},
    basic_block_group::BasicBlockGroup,
};

use super::to_structured_flow::{do_tree, StructuredFlow};

fn to_ast(block_group: &BasicBlockGroup) -> Vec<Stmt> {
    let tree = do_tree(block_group);

    to_ast_inner(&tree, &block_group)
}

fn to_ast_inner(tree: &StructuredFlow, block_group: &BasicBlockGroup) -> Vec<Stmt> {
    let get_variable = |var_idx: usize| format!("${}", var_idx);
    let get_identifier =
        |i: String| swc_ecma_ast::Expr::Ident(Ident::new(i.into(), Default::default()));
    let var_decl = |name: &String, value: Expr| {
        let varname = swc_ecma_ast::Ident::new(name.clone().into(), Default::default());
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
        let vardecl = swc_ecma_ast::Stmt::Decl(Decl::Var(Box::new(vardecl)));

        vardecl
    };
    let block = |stats: &Vec<Stmt>| {
        swc_ecma_ast::Stmt::Block(swc_ecma_ast::BlockStmt {
            span: Default::default(),
            stmts: stats.clone(),
        })
    };

    let to_expr = fix_fn::fix_fn!(|to_expr, expr: &BasicBlockInstruction| -> Expr {
        match expr {
            BasicBlockInstruction::LitNumber(num) => (*num).into(),
            BasicBlockInstruction::Undefined => {
                swc_ecma_ast::Expr::Ident(Ident::new("undefined".into(), Default::default()))
            }
            BasicBlockInstruction::BinOp(name, left, right) => {
                let left = get_identifier(get_variable(*left));
                let right = get_identifier(get_variable(*right));

                swc_ecma_ast::Expr::Bin(swc_ecma_ast::BinExpr {
                    span: Default::default(),
                    op: swc_ecma_ast::BinaryOp::Add, /* major TODO lol */
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }
            _ => todo!("to_expr: {:?}", expr),
        }
    });

    let to_stat = fix_fn::fix_fn!(|to_stat, node: &StructuredFlow| -> Vec<Stmt> {
        let to_stat_vec = |stats: &Vec<StructuredFlow>| -> Vec<Stmt> {
            stats.iter().flat_map(|stat| to_stat(stat)).collect()
        };

        match node {
            StructuredFlow::Block(stats) => to_stat_vec(stats),
            StructuredFlow::BasicBlock(block_idx) => {
                let stats = &block_group.asts[*block_idx].body;

                stats
                    .iter()
                    .map(|(variable, instruction)| {
                        let expression: swc_ecma_ast::Expr = to_expr(instruction);

                        var_decl(&format!("${}", variable), expression)
                    })
                    .collect::<Vec<_>>()
            }
            StructuredFlow::Return(ExitType::Return, Some(var_idx)) => {
                let varname =
                    swc_ecma_ast::Ident::new(format!("${}", var_idx).into(), Default::default());

                let return_stmt = swc_ecma_ast::Stmt::Return(ReturnStmt {
                    span: Default::default(),
                    arg: Some(Box::new(swc_ecma_ast::Expr::Ident(Ident {
                        span: Default::default(),
                        sym: varname.sym.clone(),
                        optional: false,
                    }))),
                });

                vec![return_stmt]
            }
            StructuredFlow::Branch(branch_expr, cons, alt) => {
                let branch_expr = get_identifier(get_variable(*branch_expr));
                let cons = to_stat_vec(cons);
                let alt = to_stat_vec(alt);

                let if_stmt = swc_ecma_ast::Stmt::If(swc_ecma_ast::IfStmt {
                    span: Default::default(),
                    test: Box::new(branch_expr),
                    cons: Box::new(block( &cons)),
                    alt: Some( Box::new(block( &alt))),
                });

                vec![if_stmt]
            }
            StructuredFlow::Break(nest_count) => {
                let break_stmt = swc_ecma_ast::Stmt::Break(swc_ecma_ast::BreakStmt {
                    span: Default::default(),
                    label: None /* TODO */,
                });

                vec![break_stmt]
            }
            StructuredFlow::Continue(nest_count) => {
                let break_stmt = swc_ecma_ast::Stmt::Continue(swc_ecma_ast::ContinueStmt {
                    span: Default::default(),
                    label: None /* TODO */,
                });

                vec![break_stmt]
            }
            StructuredFlow::Loop(body) => {
                let body = to_stat_vec(body);

                let while_stmt = swc_ecma_ast::Stmt::While(swc_ecma_ast::WhileStmt {
                    span: Default::default(),
                    test: Box::new(swc_ecma_ast::Expr::Lit(true.into())),
                    body: Box::new(block( &body)),
                });

                vec![while_stmt]
            }
            _ => {
                todo!("to_stat: {:?}", node)
            }
        }
    });

    to_stat(tree)
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
            var $1 = 2;
            break;
        } else {
            var $2 = 3;
            break;
        }
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
                if ($1) {
                    break;
                } else {
                    continue;
                }
            } else {
                break;
            }
        }
        "###);
    }
}
