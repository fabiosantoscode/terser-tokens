use std::collections::BTreeMap;

use swc_ecma_ast::{
    ArrayLit, AssignExpr, AssignOp, AwaitExpr, BlockStmt, CallExpr, Callee, ComputedPropName,
    ContinueStmt, Expr, ExprOrSpread, ExprStmt, FnExpr, Function, Ident, KeyValueProp, Lit,
    MemberExpr, MemberProp, Module, ModuleItem, ObjectLit, PatOrExpr, PrivateName, Prop, PropName,
    PropOrSpread, ReturnStmt, SpreadElement, Stmt, Str, ThrowStmt, TryStmt, WhileStmt, YieldExpr,
};

use crate::{
    analyze::count_variable_uses,
    basic_blocks::{
        ArrayElement, BasicBlockInstruction, BasicBlockModule, ExitType, ObjectMember, ObjectProp,
        StructuredFlow, TempExitType,
    },
    block_ops::{block_group_to_structured_flow, remove_phi},
    to_ast::{build_block, build_var_assign, build_var_decl},
};

use super::{
    build_binding_identifier, build_identifier, get_inlined_variables, pattern_to_statement,
    Base54, ToAstContext,
};

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

pub fn to_ast_inner(mut block_module: BasicBlockModule) -> Vec<Stmt> {
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

    let tree = block_group_to_structured_flow(block_module.take_top_level_stats().blocks);

    let mut ctx = ToAstContext {
        caught_error: None,
        module: &mut block_module,
        inlined_variables,
        variable_use_count,
        emitted_vars: BTreeMap::new(),
        gen_var_index: Base54::new(0),
        breakable_stack: vec![],
        gen_label_index: Base54::new(0),
        destructuring_patterns: BTreeMap::new(),
    };

    to_statements(&mut ctx, &tree)
}

fn to_statements(ctx: &mut ToAstContext, node: &StructuredFlow) -> Vec<Stmt> {
    let to_stat_vec = |ctx: &mut ToAstContext, stats: &Vec<StructuredFlow>| -> Vec<Stmt> {
        stats
            .iter()
            .flat_map(|stat| to_statements(ctx, stat))
            .collect()
    };

    match node {
        StructuredFlow::Block(stats) => to_stat_vec(ctx, stats),
        StructuredFlow::BasicBlock(ins) => ins
            .iter()
            .flat_map(|(varname, ins)| instruction_to_statement(ctx, *varname, ins))
            .collect(),
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
                    cons: Box::new(build_block(cons)),
                    alt: Some(Box::new(build_block(alt))),
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
                    body: Box::new(build_block(body)),
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
                        param: Some(build_binding_identifier(&catch_handler)),
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
    if let Some(instruction_as_stat) = pattern_to_statement(ctx, instruction, variable) {
        vec![instruction_as_stat]
    } else if ctx.will_be_inlined(variable) {
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
                let variable = ctx.get_varname_for(variable);

                vec![build_var_assign(&variable, expression)]
            } else {
                let varname = ctx.create_varname_for(variable);

                vec![build_var_decl(
                    build_binding_identifier(&varname),
                    expression,
                )]
            }
        } else {
            vec![Stmt::Expr(ExprStmt {
                span: Default::default(),
                expr: Box::new(expression),
            })]
        }
    }
}

pub fn ref_or_inlined_expr(ctx: &mut ToAstContext, var_idx: usize) -> Expr {
    if let Some(ins) = ctx.get_inlined_expression(var_idx) {
        to_expression(ctx, &ins)
    } else {
        build_identifier(ctx.get_varname_for(var_idx))
    }
}

fn to_expression(ctx: &mut ToAstContext, expr: &BasicBlockInstruction) -> Expr {
    match expr {
        BasicBlockInstruction::LitNumber(num) => (*num).into(),
        BasicBlockInstruction::LitBool(s) => (*s).into(),
        BasicBlockInstruction::LitString(s) => Expr::Lit(Lit::Str(Str::from(&s[..]))),
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
        BasicBlockInstruction::Object(proto, props) => Expr::Object(ObjectLit {
            span: Default::default(),
            props: proto
                .map(|proto| {
                    PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                        key: PropName::Ident(Ident::new("__proto__".into(), Default::default())),
                        value: Box::new(ref_or_inlined_expr(ctx, proto)),
                    })))
                })
                .into_iter()
                .chain(props.iter().map(|prop| match prop {
                    ObjectProp::Spread(spread_obj) => PropOrSpread::Spread(SpreadElement {
                        dot3_token: Default::default(),
                        expr: Box::new(ref_or_inlined_expr(ctx, *spread_obj)),
                    }),
                    ObjectProp::KeyValue(key, value) => {
                        if key.chars().all(|c| c.is_ascii_alphabetic()) {
                            PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                                key: PropName::Ident(Ident::new(
                                    key.as_str().into(),
                                    Default::default(),
                                )),
                                value: Box::new(ref_or_inlined_expr(ctx, *value)),
                            })))
                        } else {
                            todo!("object keys that aren't just identifiers, also shorthands")
                        }
                    }
                    ObjectProp::Computed(key, value) => {
                        PropOrSpread::Prop(Box::new(Prop::KeyValue(KeyValueProp {
                            key: PropName::Computed(ComputedPropName {
                                span: Default::default(),
                                expr: Box::new(ref_or_inlined_expr(ctx, *key)),
                            }),
                            value: Box::new(ref_or_inlined_expr(ctx, *value)),
                        })))
                    }
                }))
                .collect::<Vec<PropOrSpread>>(),
        }),
        BasicBlockInstruction::Member(base, member) => {
            let base = ref_or_inlined_expr(ctx, *base);

            Expr::Member(MemberExpr {
                span: Default::default(),
                obj: Box::new(base),
                prop: match member {
                    ObjectMember::KeyValue(member) => {
                        MemberProp::Ident(Ident::new(member.as_str().into(), Default::default()))
                    }
                    ObjectMember::Private(member) => MemberProp::PrivateName(PrivateName {
                        span: Default::default(),
                        id: Ident::new(member.as_str().into(), Default::default()),
                    }),
                    ObjectMember::Computed(member) => MemberProp::Computed(ComputedPropName {
                        span: Default::default(),
                        expr: Box::new(ref_or_inlined_expr(ctx, *member)),
                    }),
                },
            })
        }
        BasicBlockInstruction::MemberSet(base, member, value) => {
            let value = ref_or_inlined_expr(ctx, *value);

            Expr::Assign(AssignExpr {
                span: Default::default(),
                left: PatOrExpr::Expr(Box::new(to_expression(
                    ctx,
                    &BasicBlockInstruction::Member(*base, member.clone()),
                ))),
                op: AssignOp::Assign,
                right: Box::new(value),
            })
        }
        BasicBlockInstruction::ArrayPattern(_, _) => unreachable!(),
        BasicBlockInstruction::ObjectPattern(_, _) => unreachable!(),
        BasicBlockInstruction::PatternUnpack(pat_var, idx) => {
            build_identifier(ctx.get_varname_for_pattern(*pat_var, *idx))
        }
        BasicBlockInstruction::Function(id) => {
            let func = ctx.module.take_function(*id).unwrap().blocks;

            let stmts = to_statements(ctx, &block_group_to_structured_flow(func));

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
