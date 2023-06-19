use swc_ecma_ast::{BindingIdent, Decl, Expr, ExprOrSpread, Ident, ReturnStmt, Stmt};

use crate::basic_blocks::{
    basic_block::{ArrayElement, BasicBlockInstruction, ExitType},
    basic_block_group::BasicBlockGroup,
};

use super::{
    remove_phi::remove_phi,
    to_structured_flow::{do_tree, StructuredFlow},
};

fn to_ast(block_group: &BasicBlockGroup) -> Vec<Stmt> {
    let mut block_group: BasicBlockGroup = block_group.clone();

    remove_phi(&mut block_group);

    let tree = do_tree(&block_group);

    to_ast_inner(&tree, &block_group)
}

fn to_ast_inner(tree: &StructuredFlow, block_group: &BasicBlockGroup) -> Vec<Stmt> {
    let get_variable = |var_idx: usize| format!("${}", var_idx);
    let get_identifier = |i: String| Expr::Ident(Ident::new(i.into(), Default::default()));
    let var_decl = |name: &String, value: Expr| {
        let varname = Ident::new(name.clone().into(), Default::default());
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
    };
    let block = |stats: &Vec<Stmt>| {
        Stmt::Block(swc_ecma_ast::BlockStmt {
            span: Default::default(),
            stmts: stats.clone(),
        })
    };

    let to_expr = fix_fn::fix_fn!(|to_expr, expr: &BasicBlockInstruction| -> Expr {
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
            BasicBlockInstruction::Phi(_) => unreachable!("phi should be removed by remove_phi()"),
            BasicBlockInstruction::This => {
                Expr::Ident(Ident::new("this".into(), Default::default()))
            }
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
        }
    });

    let to_stat = fix_fn::fix_fn!(|to_stat, node: &StructuredFlow| -> Vec<Stmt> {
        let to_stat_vec = |stats: &Vec<StructuredFlow>| -> Vec<Stmt> {
            stats.iter().flat_map(|stat| to_stat(stat)).collect()
        };

        match node {
            StructuredFlow::Block(stats) => to_stat_vec(stats),
            StructuredFlow::BasicBlock(block_idx) => {
                let stats = &block_group.blocks[*block_idx].instructions;

                stats
                    .iter()
                    .map(|(variable, instruction)| {
                        let expression: Expr = to_expr(instruction);

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
            StructuredFlow::Branch(branch_expr, cons, alt) => {
                let branch_expr = get_identifier(get_variable(*branch_expr));
                let cons = to_stat_vec(cons);
                let alt = to_stat_vec(alt);

                let if_stmt = Stmt::If(swc_ecma_ast::IfStmt {
                    span: Default::default(),
                    test: Box::new(branch_expr),
                    cons: Box::new(block(&cons)),
                    alt: Some(Box::new(block(&alt))),
                });

                vec![if_stmt]
            }
            StructuredFlow::Break(nest_count) => {
                let break_stmt = Stmt::Break(swc_ecma_ast::BreakStmt {
                    span: Default::default(),
                    label: None, /* TODO */
                });

                vec![break_stmt]
            }
            StructuredFlow::Continue(nest_count) => {
                let break_stmt = Stmt::Continue(swc_ecma_ast::ContinueStmt {
                    span: Default::default(),
                    label: None, /* TODO */
                });

                vec![break_stmt]
            }
            StructuredFlow::Loop(body) => {
                let body = to_stat_vec(body);

                let while_stmt = Stmt::While(swc_ecma_ast::WhileStmt {
                    span: Default::default(),
                    test: Box::new(Expr::Lit(true.into())),
                    body: Box::new(block(&body)),
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
            var $3 = 2;
            break;
        } else {
            var $3 = 3;
            break;
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
                if ($1) {
                    break;
                } else {
                    continue;
                }
            } else {
                break;
            }
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
            break;
        } else {
            var $4 = 789;
            break;
        }
        var $5 = $4;
        var $6 = 1;
        var $7 = $5 + $6;
        var $8 = undefined;
        return $8;
        "###);
    }
}
