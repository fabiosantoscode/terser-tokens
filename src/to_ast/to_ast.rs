use std::collections::BTreeMap;

use swc_ecma_ast::{
    ArrayLit, AssignExpr, AssignOp, AwaitExpr, BindingIdent, BlockStmt, CallExpr, Callee,
    ComputedPropName, ContinueStmt, Decl, Expr, ExprOrSpread, ExprStmt, FnExpr, Function, Ident,
    Lit, MemberExpr, MemberProp, Module, ModuleItem, Pat, PatOrExpr, ReturnStmt, Stmt, ThrowStmt,
    TryStmt, WhileStmt, YieldExpr,
};

use crate::{
    analyze::count_variable_uses,
    basic_blocks::{
        remove_phi, ArrayElement, BasicBlockGroup, BasicBlockInstruction, BasicBlockModule,
        ExitType, TempExitType,
    },
};

use super::{do_tree, get_inlined_variables, Base54, StructuredFlow, ToAstContext};

pub fn module_to_ast(block_module: BasicBlockModule) -> Module {
    Module {
        span: Default::default(),
        body: to_ast_inner(block_module)
            .into_iter()
            .map(|stat| ModuleItem::Stmt(stat))
            .collect::<Vec<_>>(),
        shebang: None,
    }
}

fn to_ast_inner(mut block_module: BasicBlockModule) -> Vec<Stmt> {
    let phied = block_module
        .iter_all_instructions()
        .flat_map(|(_, _, varname, ins)| match ins {
            BasicBlockInstruction::Phi(vars) => vec![&vec![varname], vars]
                .into_iter()
                .flatten()
                .copied()
                .collect(),
            _ => vec![],
        })
        .collect();

    for (_, block_group) in block_module.iter_mut() {
        remove_phi(block_group);
    }

    let variable_use_count = count_variable_uses(&block_module);
    let inlined_variables = get_inlined_variables(&block_module, &variable_use_count, phied);

    let mut ctx = ToAstContext {
        caught_error: None,
        module: &block_module,
        inlined_variables,
        variable_use_count,
        emitted_vars: BTreeMap::new(),
        gen_var_index: Base54::new(0),
        breakable_stack: vec![],
        gen_label_index: Base54::new(0),
    };

    let tree = do_tree(&block_module.top_level_stats());

    to_statements(&mut ctx, &tree, &block_module.top_level_stats())
}

fn to_statements(
    ctx: &mut ToAstContext,
    node: &StructuredFlow,
    block_group: &BasicBlockGroup,
) -> Vec<Stmt> {
    let to_stat_vec = |ctx: &mut ToAstContext, stats: &Vec<StructuredFlow>| -> Vec<Stmt> {
        stats
            .iter()
            .flat_map(|stat| to_statements(ctx, stat, block_group))
            .collect()
    };

    match node {
        StructuredFlow::Block(stats) => to_stat_vec(ctx, stats),
        StructuredFlow::BasicBlock(block_idx) => block_group.blocks[block_idx]
            .iter()
            .flat_map(|(varname, ins)| instruction_to_statement(ctx, varname, ins))
            .collect::<Vec<_>>(),
        StructuredFlow::Return(ExitType::Return, Some(var_idx)) => {
            let return_stmt = Stmt::Return(ReturnStmt {
                span: Default::default(),
                arg: Some(Box::new(ref_or_inlined_expr(ctx, *var_idx))),
            });

            vec![return_stmt]
        }
        StructuredFlow::Return(ExitType::Throw, Some(var_idx)) => {
            let throw_stmt = Stmt::Throw(ThrowStmt {
                span: Default::default(),
                arg: (Box::new(ref_or_inlined_expr(ctx, *var_idx))),
            });

            vec![throw_stmt]
        }
        StructuredFlow::Branch(brk_id, branch_expr, cons, alt) => {
            let if_stmt = ctx.enter_breakable(brk_id, false, |ctx| {
                let branch_expr = ref_or_inlined_expr(ctx, *branch_expr);
                let cons = to_stat_vec(ctx, cons);
                let alt = to_stat_vec(ctx, alt);

                Stmt::If(swc_ecma_ast::IfStmt {
                    span: Default::default(),
                    test: Box::new(branch_expr),
                    cons: Box::new(get_block(cons)),
                    alt: Some(Box::new(get_block(alt))),
                })
            });

            vec![if_stmt]
        }
        StructuredFlow::Break(brk_id) => {
            let break_stmt = Stmt::Break(swc_ecma_ast::BreakStmt {
                span: Default::default(),
                label: ctx
                    .break_label_for(brk_id)
                    .map(|l| Ident::new(l.to_string().into(), Default::default())),
            });

            vec![break_stmt]
        }
        StructuredFlow::Continue(brk_id) => {
            let break_stmt = Stmt::Continue(ContinueStmt {
                span: Default::default(),
                label: ctx
                    .break_label_for(brk_id)
                    .map(|l| Ident::new(l.to_string().into(), Default::default())),
            });

            vec![break_stmt]
        }
        StructuredFlow::Loop(brk_id, body) => {
            let while_stmt = ctx.enter_breakable(brk_id, true, |ctx| {
                let body = to_stat_vec(ctx, body);

                Stmt::While(WhileStmt {
                    span: Default::default(),
                    test: Box::new(Expr::Lit(true.into())),
                    body: Box::new(get_block(body)),
                })
            });

            vec![while_stmt]
        }
        StructuredFlow::TryCatch(brk_id, try_block, catch_block, finally_block) => {
            let try_stmt = ctx.enter_breakable(brk_id, true, |ctx| {
                let try_block = to_stat_vec(ctx, try_block);

                let catch_handler = ctx.set_caught_error();
                let catch_block = to_stat_vec(ctx, catch_block);

                let finally_block = to_stat_vec(ctx, finally_block);

                Stmt::Try(Box::new(TryStmt {
                    span: Default::default(),
                    block: BlockStmt {
                        span: Default::default(),
                        stmts: try_block,
                    },
                    handler: Some(swc_ecma_ast::CatchClause {
                        span: Default::default(),
                        param: Some(get_binding_identifier(&catch_handler)),
                        body: BlockStmt {
                            span: Default::default(),
                            stmts: catch_block,
                        },
                    }),
                    finalizer: match finally_block.len() {
                        0 => None,
                        _ => Some(BlockStmt {
                            span: Default::default(),
                            stmts: finally_block,
                        }),
                    },
                }))
            });

            vec![try_stmt]
        }
        _ => {
            todo!("to_stat: {:?}", node)
        }
    }
}

/// Emit a single instruction. If it will be inlined somewhere else, emit nothing.
/// If a name is necessary, we emit a variable declaration or assignment.
fn instruction_to_statement(
    ctx: &mut ToAstContext<'_>,
    variable: usize,
    instruction: &BasicBlockInstruction,
) -> Vec<Stmt> {
    if ctx.will_be_inlined(variable) {
        // This will come up as a parameter to something else later.
        vec![]
    } else if !ctx.variable_has_uses(variable) && !instruction.may_have_side_effects() {
        // This is a dead instruction, but its contents, when inlined, may have side effects
        instruction
            .used_vars()
            .into_iter()
            .flat_map(|var_idx| {
                if let Some(instruction) = ctx.get_inlined_expression(var_idx) {
                    instruction_to_statement(ctx, var_idx, &instruction)
                } else {
                    vec![]
                }
            })
            .collect()
    } else {
        let (expression, variable) =
            if let BasicBlockInstruction::WriteNonLocal(id, value_of) = instruction {
                let expression = ref_or_inlined_expr(ctx, *value_of);
                (expression, id.0)
            } else {
                let expression = to_expression(ctx, instruction);
                (expression, variable)
            };

        if ctx.variable_has_uses(variable) {
            if ctx.emitted_vars.contains_key(&variable) {
                vec![write_name(ctx, variable, expression)]
            } else {
                vec![define_name(ctx, variable, expression)]
            }
        } else {
            vec![Stmt::Expr(ExprStmt {
                span: Default::default(),
                expr: Box::new(expression),
            })]
        }
    }
}

fn ref_or_inlined_expr(ctx: &mut ToAstContext, var_idx: usize) -> Expr {
    if let Some(ins) = ctx.get_inlined_expression(var_idx) {
        to_expression(ctx, &ins)
    } else {
        get_identifier(ctx.get_varname_for(var_idx))
    }
}

fn to_expression(ctx: &mut ToAstContext, expr: &BasicBlockInstruction) -> Expr {
    match expr {
        BasicBlockInstruction::LitNumber(num) => (*num).into(),
        BasicBlockInstruction::LitBool(s) => (*s).into(),
        BasicBlockInstruction::Undefined => {
            Expr::Ident(Ident::new("undefined".into(), Default::default()))
        }
        BasicBlockInstruction::BinOp(op, left, right) => {
            let left = ref_or_inlined_expr(ctx, *left);
            let right = ref_or_inlined_expr(ctx, *right);

            Expr::Bin(swc_ecma_ast::BinExpr {
                span: Default::default(),
                op: op.clone(),
                left: Box::new(left),
                right: Box::new(right),
            })
        }
        BasicBlockInstruction::Ref(var_idx) => ref_or_inlined_expr(ctx, *var_idx),
        BasicBlockInstruction::This => Expr::Ident(Ident::new("this".into(), Default::default())),
        BasicBlockInstruction::Array(items) => {
            let items = items
                .iter()
                .map(|item| match item {
                    ArrayElement::Item(var_idx) => Some(ExprOrSpread {
                        spread: None,
                        expr: Box::new(ref_or_inlined_expr(ctx, *var_idx)),
                    }),
                    ArrayElement::Spread(var_idx) => Some(ExprOrSpread {
                        spread: Some(Default::default()),
                        expr: Box::new(ref_or_inlined_expr(ctx, *var_idx)),
                    }),
                    ArrayElement::Hole => None,
                })
                .collect();

            Expr::Array(swc_ecma_ast::ArrayLit {
                span: Default::default(),
                elems: items,
            })
        }
        BasicBlockInstruction::Function(id) => {
            let func = ctx.module.get_function(*id).unwrap();

            let stmts = to_statements(ctx, &do_tree(&func), func);

            Expr::Fn(FnExpr {
                ident: None,
                function: Box::new(Function {
                    span: Default::default(),
                    decorators: Default::default(),
                    params: Default::default(),
                    body: Some(BlockStmt {
                        span: Default::default(),
                        stmts,
                    }),
                    is_generator: false,
                    is_async: false,
                    type_params: None,
                    return_type: None,
                }),
            })
        }
        BasicBlockInstruction::Call(func_idx, args) => {
            let args = args
                .iter()
                .map(|arg| ExprOrSpread::from(ref_or_inlined_expr(ctx, *arg)))
                .collect();

            Expr::Call(CallExpr {
                span: Default::default(),
                callee: Callee::Expr(Box::new(ref_or_inlined_expr(ctx, *func_idx))),
                args,
                type_args: None,
            })
        }

        BasicBlockInstruction::ArgumentRead(idx) => Expr::Member(MemberExpr {
            span: Default::default(),
            obj: Box::new(Expr::Ident(Ident::new(
                "arguments".into(),
                Default::default(),
            ))),
            prop: MemberProp::Computed(ComputedPropName {
                span: Default::default(),
                expr: Box::new(Expr::Lit(Lit::Num((*idx).into()))),
            }),
        }),
        BasicBlockInstruction::ArgumentRest(from_idx) => {
            // [...arguments].slice(from_idx)
            Expr::Call(CallExpr {
                span: Default::default(),
                callee: Callee::Expr(Box::new(Expr::Member(MemberExpr {
                    span: Default::default(),
                    obj: Box::new(Expr::Array(ArrayLit {
                        span: Default::default(),
                        elems: vec![Some(ExprOrSpread {
                            spread: Some(Default::default()),
                            expr: Box::new(Expr::Ident(Ident::new(
                                "arguments".into(),
                                Default::default(),
                            ))),
                        })],
                    })),
                    prop: MemberProp::Ident(Ident::new("slice".into(), Default::default())),
                }))),
                args: vec![ExprOrSpread {
                    spread: None,
                    expr: Box::new(Expr::Lit(Lit::Num((*from_idx).into()))),
                }],
                type_args: None,
            })
        }

        BasicBlockInstruction::ReadNonLocal(id) => Expr::Ident(Ident::new(
            ctx.get_varname_for(id.0).into(),
            Default::default(),
        )),
        BasicBlockInstruction::WriteNonLocal(_, _) => {
            unreachable!("handled in instruction_to_statement")
        }

        BasicBlockInstruction::TempExit(typ, arg) => match typ {
            TempExitType::Yield => Expr::Yield(YieldExpr {
                span: Default::default(),
                delegate: false,
                arg: Some(Box::new(ref_or_inlined_expr(ctx, *arg))),
            }),
            TempExitType::YieldStar => Expr::Yield(YieldExpr {
                span: Default::default(),
                delegate: true,
                arg: Some(Box::new(ref_or_inlined_expr(ctx, *arg))),
            }),
            TempExitType::Await => Expr::Await(AwaitExpr {
                span: Default::default(),
                arg: Box::new(ref_or_inlined_expr(ctx, *arg)),
            }),
        },

        BasicBlockInstruction::CaughtError => Expr::Ident(Ident::new(
            ctx.get_caught_error().into(),
            Default::default(),
        )),
        BasicBlockInstruction::Phi(_) => unreachable!("phi should be removed by remove_phi()"),
    }
}

fn get_identifier(i: String) -> Expr {
    Expr::Ident(Ident::new(i.into(), Default::default()))
}

fn get_binding_identifier(i: &str) -> Pat {
    Pat::Ident(BindingIdent {
        id: Ident::new(i.into(), Default::default()),
        type_ann: None,
    })
}

fn write_name(ctx: &mut ToAstContext, variable: usize, value: Expr) -> Stmt {
    let varname = ctx.get_varname_for(variable);

    Stmt::Expr(ExprStmt {
        span: Default::default(),
        expr: Box::new(Expr::Assign(AssignExpr {
            span: Default::default(),
            op: AssignOp::Assign,
            left: PatOrExpr::Pat(Box::new(get_binding_identifier(&varname))),
            right: Box::new(value),
        })),
    })
}

fn define_name(ctx: &mut ToAstContext, variable: usize, value: Expr) -> Stmt {
    let varname = ctx.create_varname_for(variable);

    let vardecl = swc_ecma_ast::VarDecl {
        span: Default::default(),
        kind: swc_ecma_ast::VarDeclKind::Var,
        declare: false,
        decls: vec![swc_ecma_ast::VarDeclarator {
            span: Default::default(),
            name: get_binding_identifier(&varname),
            init: Some(Box::new(value)),
            definite: false,
        }],
    };
    Stmt::Decl(Decl::Var(Box::new(vardecl)))
}

fn get_block(stats: Vec<Stmt>) -> Stmt {
    Stmt::Block(swc_ecma_ast::BlockStmt {
        span: Default::default(),
        stmts: stats,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutils::*;

    #[test]
    fn to_tree() {
        let block_group = test_basic_blocks_module("1 + 2 + 3");

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        1 + 2 + 3;
        return undefined;
        "###);
    }

    #[test]
    fn to_tree_cond_1() {
        let block_group = test_basic_blocks_module("return (1 ? 2 : 3)");

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        if (1) {
            var a = 2;
        } else {
            a = 3;
        }
        return a;
        "###);
    }

    #[test]
    fn to_tree_cond_2() {
        let block_group = test_basic_blocks_module(
            "let x = 0;
            1 ? x = 2 : 3;
            return x",
        );

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var a = 0;
        if (1) {
            a = 2;
        } else {}
        return a;
        "###);
    }

    #[test]
    fn to_functions() {
        let block_group = test_basic_blocks_module(
            "var foo = function foo() {
                var foo_inner = function foo_inner(arg) {
                    return arg;
                }
                return foo_inner(123);
            }
            var bar = function bar() { return 456; }
            foo() + bar()",
        );

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var a = undefined;
        var d = function() {
            var b = undefined;
            var c = function() {
                return arguments[0];
            };
            b = c;
            return c(123);
        };
        a = d;
        var e = undefined;
        var f = function() {
            return 456;
        };
        e = f;
        d() + f();
        return undefined;
        "###);
    }

    #[test]
    fn removes_unused() {
        let block_group = test_basic_blocks_module(
            "var a = 1
            return 2",
        );

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        return 2;
        "###);
    }

    #[test]
    fn to_scopes() {
        let block_group = test_basic_blocks_module(
            "var outer = 1
            return function bar() { return outer; }",
        );

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var a = undefined;
        a = 1;
        var b = undefined;
        var c = function() {
            return a;
        };
        b = c;
        return c;
        "###);
    }

    #[test]
    fn to_scopes_rw() {
        let block_group = test_basic_blocks_module(
            "var outer = 1
            return function bar() { outer = outer + 1 }",
        );

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var a = undefined;
        a = 1;
        var b = undefined;
        var d = function() {
            var c = a + 1;
            a = c;
            return undefined;
        };
        b = d;
        return d;
        "###);
    }

    #[test]
    fn to_loop() {
        let block_group = test_basic_blocks_module("while (123) { if (456) { break; } }");

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        while(true){
            if (123) {
                if (456) {
                    break;
                } else {}
                continue;
            } else {
                break;
            }
        }
        return undefined;
        "###);
    }

    #[test]
    fn to_loop_nested() {
        let block_group = test_basic_blocks_module(
            "while (123) {
                if (456) { break; }
                while (789) { if (1234) { break; } }
            }",
        );

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        while(true){
            if (123) {
                if (456) {
                    break;
                } else {}
                while(true){
                    if (789) {
                        if (1234) {
                            break;
                        } else {}
                        continue;
                    } else {
                        break;
                    }
                }
                continue;
            } else {
                break;
            }
        }
        return undefined;
        "###);
    }

    #[test]
    fn to_loop_nested_labelled() {
        let block_group = test_basic_blocks_module(
            "a: while (123) {
                if (456) { break a; }
                b: while (789) {
                    if (1234) { break a; } else { break b; }
                }
            }",
        );

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        a: while(true){
            if (123) {
                if (456) {
                    break;
                } else {}
                while(true){
                    if (789) {
                        if (1234) {
                            break a;
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                continue;
            } else {
                break;
            }
        }
        return undefined;
        "###);
    }

    #[test]
    fn to_conditional_var() {
        let block_group = test_basic_blocks_module(
            "var x = 10;
            if (123) { x = 456; } else { x = 789; }
            return x + 1",
        );

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var a = 10;
        if (123) {
            a = 456;
        } else {
            a = 789;
        }
        return a + 1;
        "###);
    }

    #[test]
    fn to_trycatch() {
        let block_group = test_basic_blocks_module(
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

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var a = 10;
        try {
            if (a > 10) {
                throw 123;
            } else {}
        } catch (b) {
            a = 456;
        }
        return a;
        "###);
    }

    #[test]
    fn to_trycatch_nested() {
        let block_group = test_basic_blocks_module(
            "var x = 10;
            try {
                if (x > 10) {
                    throw 123;
                }
            } catch (e) {
                try {
                    return 123
                } catch (e2) {
                    return e + e2;
                }
            }
            return x;",
        );

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var a = 10;
        try {
            if (a > 10) {
                throw 123;
            } else {}
        } catch (b) {
            var c = b;
            try {
                return 123;
            } catch (d) {
                return c + d;
            }
        }
        return a;
        "###);
    }
}
