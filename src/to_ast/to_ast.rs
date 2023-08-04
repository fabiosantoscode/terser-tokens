use std::collections::{BTreeMap, BTreeSet};

use swc_ecma_ast::{
    ArrayLit, AssignExpr, AssignOp, AwaitExpr, BindingIdent, BlockStmt, CallExpr, Callee,
    ComputedPropName, Decl, Expr, ExprOrSpread, ExprStmt, FnExpr, Function, Ident, Lit, MemberExpr,
    MemberProp, Module, ModuleItem, Pat, PatOrExpr, ReturnStmt, Stmt, ThrowStmt, YieldExpr,
};

use crate::{
    analyze::count_variable_uses,
    basic_blocks::{
        ArrayElement, BasicBlockGroup, BasicBlockInstruction, BasicBlockModule, ExitType,
        TempExitType,
    },
};

use super::{do_tree, get_inlined_variables, remove_phi, StructuredFlow};

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
    let variable_use_count = count_variable_uses(&block_module);
    let inlined_variables = get_inlined_variables(&block_module, &variable_use_count);

    block_module.mutate_all_block_groups(&mut |block_group| {
        remove_phi(block_group);
    });

    let mut ctx = ToAstContext {
        caught_error: None,
        error_counter: 0,
        module: &block_module,
        inlined_variables,
        variable_use_count,
        emitted_vars: BTreeSet::new(),
    };

    let tree = do_tree(&block_module.top_level_stats());

    to_stat_ast(&mut ctx, &tree, &block_module.top_level_stats())
}

struct ToAstContext<'a> {
    pub caught_error: Option<String>,
    pub error_counter: usize,
    pub module: &'a BasicBlockModule,
    pub inlined_variables: BTreeMap<usize, BasicBlockInstruction>,
    pub variable_use_count: BTreeMap<usize, u32>,
    pub emitted_vars: BTreeSet<usize>,
}

impl ToAstContext<'_> {
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

    fn will_be_inlined(&self, variable: usize) -> bool {
        self.inlined_variables.contains_key(&variable)
    }
    fn get_inlined_expression(&self, var_idx: usize) -> Option<&BasicBlockInstruction> {
        self.inlined_variables.get(&var_idx)
    }

    fn variable_has_uses(&self, variable: usize) -> bool {
        self.variable_use_count.get(&variable).unwrap_or(&0) > &0
    }
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
                .flat_map(|(variable, instruction)| {
                    if ctx.will_be_inlined(*variable) {
                        None
                    } else {
                        let expression: Expr = to_expr_ast(ctx, instruction);

                        // only used vars get "var X = ..."
                        if ctx.variable_has_uses(*variable) {
                            Some(var_decl(ctx, *variable, expression))
                        } else {
                            Some(Stmt::Expr(ExprStmt {
                                span: Default::default(),
                                expr: Box::new(expression),
                            }))
                        }
                    }
                })
                .collect::<Vec<_>>()
        }
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
        StructuredFlow::Branch(_, branch_expr, cons, alt) => {
            let branch_expr = ref_or_inlined_expr(ctx, *branch_expr);
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
        StructuredFlow::Break(_to_id) => {
            let break_stmt = Stmt::Break(swc_ecma_ast::BreakStmt {
                span: Default::default(),
                label: None, /* TODO */
            });

            vec![break_stmt]
        }
        StructuredFlow::Continue(_to_id) => {
            let break_stmt = Stmt::Continue(swc_ecma_ast::ContinueStmt {
                span: Default::default(),
                label: None, /* TODO */
            });

            vec![break_stmt]
        }
        StructuredFlow::Loop(_id, body) => {
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
            }));

            vec![vec![try_catch_stmt], after_block].concat()
        }
        _ => {
            todo!("to_stat: {:?}", node)
        }
    }
}

fn ref_or_inlined_expr(ctx: &mut ToAstContext, var_idx: usize) -> Expr {
    if let Some(varname) = ctx.get_inlined_expression(var_idx) {
        to_expr_ast(ctx, &varname.clone())
    } else {
        get_identifier(get_variable(var_idx))
    }
}

fn to_expr_ast(ctx: &mut ToAstContext, expr: &BasicBlockInstruction) -> Expr {
    match expr {
        BasicBlockInstruction::LitNumber(num) => (*num).into(),
        BasicBlockInstruction::Undefined => {
            Expr::Ident(Ident::new("undefined".into(), Default::default()))
        }
        BasicBlockInstruction::BinOp(_name, left, right) => {
            let left = ref_or_inlined_expr(ctx, *left);
            let right = ref_or_inlined_expr(ctx, *right);

            Expr::Bin(swc_ecma_ast::BinExpr {
                span: Default::default(),
                op: swc_ecma_ast::BinaryOp::Add, /* major TODO lol */
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

            let stmts = to_stat_ast(ctx, &do_tree(&func), func);

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

        BasicBlockInstruction::ReadNonLocal(id) => {
            Expr::Ident(Ident::new(get_variable(id.0).into(), Default::default()))
        }
        BasicBlockInstruction::WriteNonLocal(id, value) => Expr::Assign(AssignExpr {
            op: AssignOp::Assign,
            span: Default::default(),
            left: PatOrExpr::Pat(Box::new(get_binding_identifier(&get_variable(id.0)))),
            right: Box::new(ref_or_inlined_expr(ctx, *value)),
        }),

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

fn get_variable(var_idx: usize) -> String {
    format!("${}", var_idx)
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

fn var_decl(ctx: &mut ToAstContext, variable: usize, value: Expr) -> Stmt {
    let varname = get_variable(variable);

    let var_or_assign = if ctx.emitted_vars.contains(&variable) {
        Stmt::Expr(ExprStmt {
            span: Default::default(),
            expr: Box::new(Expr::Assign(AssignExpr {
                span: Default::default(),
                op: AssignOp::Assign,
                left: PatOrExpr::Pat(Box::new(get_binding_identifier(&varname))),
                right: Box::new(value),
            })),
        })
    } else {
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
    };

    ctx.emitted_vars.insert(variable);

    var_or_assign
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
    fn to_tree_cond() {
        let block_group = test_basic_blocks_module("1 ? 2 : 3");

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        if (1) {
            2;
        } else {
            3;
        }
        return undefined;
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
        var $11 = function() {
            return 456;
        };
        var $12 = function() {
            var $4 = function() {
                return arguments[0];
            };
            var $5 = 123;
            var $6 = $4($5);
            return $6;
        };
        var $13 = $12();
        var $14 = $11;
        var $15 = $14();
        $13 + $15;
        return undefined;
        "###);
    }

    #[test]
    fn to_scopes() {
        let block_group = test_basic_blocks_module(
            "var outer = 1
            var bar = function bar() { return outer; }",
        );

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var $0 = undefined;
        $1 = $0;
        var $2 = 1;
        $1 = $2;
        function() {
            var $4 = $1;
            return $4;
        };
        return undefined;
        "###);
    }

    #[test]
    fn to_scopes_rw() {
        let block_group = test_basic_blocks_module(
            "var outer = 1
            var bar = function bar() { outer = 9 }",
        );

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var $0 = undefined;
        $1 = $0;
        var $2 = 1;
        $1 = $2;
        function() {
            $1;
            var $5 = 9;
            $1 = $5;
            return undefined;
        };
        return undefined;
        "###);
    }

    #[test]
    fn to_loop() {
        let block_group = test_basic_blocks_module("while (123) { if (456) { break; } }");

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        while(true){
            if (123) {
                if (456) {} else {
                    continue;
                }
            } else {}
            return undefined;
        }
        "###);
    }

    #[test]
    fn to_conditional_var() {
        let block_group = test_basic_blocks_module(
            "var x = 10;
            if (123) { x = 456; } else { x = 789; }
            x + 1",
        );

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        10;
        if (123) {
            var $4 = 456;
        } else {
            $4 = 789;
        }
        $4 + 1;
        return undefined;
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
        try {
            if (10 + 10) {
                throw 123;
            } else {}
        } catch ($error1) {
            $error1;
        }
        return 456;
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
                    123
                } catch (e2) {
                    return e + e2;
                }
            }
            return x;",
        );

        let tree = to_ast_inner(block_group);
        insta::assert_snapshot!(stats_to_string(tree), @r###"
        var $0 = 10;
        try {
            if ($0 + 10) {
                throw 123;
            } else {}
        } catch ($error1) {
            var $5 = $error1;
            try {
                123;
            } catch ($error2) {
                var $7 = $error2;
                return $5 + $7;
            }
        }
        return $0;
        "###);
    }
}
