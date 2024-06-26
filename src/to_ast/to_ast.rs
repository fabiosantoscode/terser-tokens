use swc_ecma_ast::{
    op, AssignExpr, AssignOp, AwaitExpr, BinExpr, BinaryOp, BlockStmt, CallExpr, Callee,
    ComputedPropName, ContinueStmt, DebuggerStmt, Expr, ExprOrSpread, ExprStmt, ForHead, ForInStmt,
    ForOfStmt, Ident, KeyValueProp, Lit, Module, ModuleItem, NewExpr, Null, ObjectLit, PatOrExpr,
    PrivateName, Prop, PropName, PropOrSpread, Regex, ReturnStmt, SpreadElement, Stmt, Str, Super,
    ThrowStmt, TplElement, TryStmt, UnaryExpr, UnaryOp, UpdateExpr, UpdateOp, WhileStmt, YieldExpr,
};

use crate::{
    basic_blocks::{
        identifier_needs_quotes, ArrayElement, ExitType, ForInOfKind, IncrDecr, Instruction,
        LogicalCondKind, MethodKind, ObjectProperty, StructuredFlow, StructuredModule,
        TempExitType, LHS,
    },
    to_ast::{
        build_block, build_empty_var_decl, build_multivar_decl, build_var_assign, build_var_decl,
        statements_forced_to_expr_ast,
    },
};

use super::{
    build_binding_identifier, build_identifier, build_identifier_str, build_parens, class_to_ast,
    class_to_ast_prepare, function_expr_to_ast, function_to_ast, lhs_to_ast_expr,
    pattern_to_statement, should_add_parens, ToAstContext,
};

pub fn module_to_ast(block_module: StructuredModule) -> Module {
    let (mut ctx, tree) = ToAstContext::new(block_module);

    Module {
        span: Default::default(),
        body: to_blockgroup_statements(&mut ctx, &tree)
            .into_iter()
            .map(|stat| ModuleItem::Stmt(stat))
            .collect::<Vec<_>>(),
        shebang: None,
    }
}

/// to_statements but it declares any leftover `var` at the end
pub fn to_blockgroup_statements(ctx: &mut ToAstContext, node: &StructuredFlow) -> Vec<Stmt> {
    let mut stats = to_statements(ctx, node);

    if let Some(multivar_decl) = build_multivar_decl(
        ctx.dequeue_enqueued_vars()
            .into_iter()
            .map(|var| ctx.get_varname_for(var))
            .collect(),
    ) {
        stats.push(multivar_decl);
    }

    stats
}

/// Convert a structured flow into a list of statements
pub fn to_statements(ctx: &mut ToAstContext, node: &StructuredFlow) -> Vec<Stmt> {
    let to_stat_vec = |ctx: &mut ToAstContext, stats: &Vec<StructuredFlow>| -> Vec<Stmt> {
        stats
            .iter()
            .flat_map(|stat| to_statements(ctx, stat))
            .collect()
    };

    match node {
        StructuredFlow::Block(brk, stats) => {
            if brk.0.is_some() {
                let stmt = ctx.enter_breakable(brk, true, |ctx| {
                    Stmt::Block(BlockStmt {
                        span: Default::default(),
                        stmts: to_stat_vec(ctx, stats),
                    })
                });

                vec![stmt]
            } else {
                to_stat_vec(ctx, stats)
            }
        }
        StructuredFlow::Instruction(varname, ins) => match ins {
            Instruction::CreateClass(extends) => {
                class_to_ast_prepare(ctx, *varname, extends.clone());
                vec![]
            }
            _ => instruction_to_statement(ctx, *varname, ins),
        },
        StructuredFlow::Return(ExitType::Return, var_idx) => {
            let return_stmt = Stmt::Return(ReturnStmt {
                span: Default::default(),
                arg: Some(Box::new(ref_or_inlined_expr(ctx, *var_idx))),
            });

            vec![return_stmt]
        }
        StructuredFlow::Return(ExitType::Throw, var_idx) => {
            let throw_stmt = Stmt::Throw(ThrowStmt {
                span: Default::default(),
                arg: (Box::new(ref_or_inlined_expr(ctx, *var_idx))),
            });

            vec![throw_stmt]
        }
        StructuredFlow::Cond(brk_id, branch_expr, cons, alt) => {
            let if_stmt = ctx.enter_breakable(brk_id, false, |ctx| {
                let branch_expr = ref_or_inlined_expr(ctx, *branch_expr);
                let cons = to_stat_vec(ctx, cons);
                let alt = to_stat_vec(ctx, alt);

                Stmt::If(swc_ecma_ast::IfStmt {
                    span: Default::default(),
                    test: Box::new(branch_expr),
                    cons: Box::new(build_block(cons)),
                    alt: if alt.is_empty() {
                        None
                    } else {
                        Some(Box::new(build_block(alt)))
                    },
                })
            });

            vec![if_stmt]
        }
        StructuredFlow::LogicalCond(kind, left, cond_on, right, then_take) => {
            let left = statements_forced_to_expr_ast(ctx, &left, *cond_on);
            let right = statements_forced_to_expr_ast(ctx, &right, *then_take);

            vec![Stmt::Expr(ExprStmt {
                span: Default::default(),
                expr: Box::new(Expr::Bin(BinExpr {
                    span: Default::default(),
                    op: match kind {
                        LogicalCondKind::And => op!("&&"),
                        LogicalCondKind::Or => op!("||"),
                        LogicalCondKind::NullishCoalescing => op!("??"),
                    },
                    left: Box::new(left),
                    right: Box::new(right),
                })),
            })]
        }
        StructuredFlow::Switch(brk_id, test, cases) => {
            let switch_stmt = ctx.enter_breakable(brk_id, false, |ctx| {
                let test = ref_or_inlined_expr(ctx, *test);
                let cases = cases
                    .iter()
                    .map(|case| {
                        let test = case.condition.as_ref().map(|(insx, varname)| {
                            Box::new(statements_forced_to_expr_ast(ctx, &insx, *varname))
                        });
                        let cons = to_stat_vec(ctx, &case.body);

                        swc_ecma_ast::SwitchCase {
                            span: Default::default(),
                            test,
                            cons,
                        }
                    })
                    .collect();

                Stmt::Switch(swc_ecma_ast::SwitchStmt {
                    span: Default::default(),
                    discriminant: Box::new(test),
                    cases,
                })
            });

            vec![switch_stmt]
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
        StructuredFlow::ForInOfLoop(brk_id, looped_var, kind, body) => {
            let for_in_of_stmt = ctx.enter_breakable(brk_id, true, |ctx| {
                let left = ctx.set_for_in_of_value();

                let looped_var = ref_or_inlined_expr(ctx, *looped_var);

                let body = to_stat_vec(ctx, body);

                match kind {
                    ForInOfKind::ForIn => Stmt::ForIn(ForInStmt {
                        body: Box::new(build_block(body)),
                        left: ForHead::VarDecl(Box::new(build_empty_var_decl(
                            build_binding_identifier(&left),
                        ))),
                        right: Box::new(looped_var),
                        span: Default::default(),
                    }),
                    _ => Stmt::ForOf(ForOfStmt {
                        body: Box::new(build_block(body)),
                        is_await: *kind == ForInOfKind::ForAwaitOf,
                        left: ForHead::VarDecl(Box::new(build_empty_var_decl(
                            build_binding_identifier(&left),
                        ))),
                        right: Box::new(looped_var),
                        span: Default::default(),
                    }),
                }
            });

            vec![for_in_of_stmt]
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
        StructuredFlow::Class(class_var, members) => class_to_ast(ctx, *class_var, members),
        StructuredFlow::Debugger => vec![Stmt::Debugger(DebuggerStmt {
            span: Default::default(),
        })],
    }
}

/// Emit a single instruction. If it will be inlined somewhere else, emit nothing.
/// If a name is necessary, we emit a variable declaration or assignment.
fn instruction_to_statement(
    ctx: &mut ToAstContext,
    variable: usize,
    instruction: &Instruction,
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
            if let Instruction::Write(LHS::NonLocal(id), value_of) = instruction {
                let expression = ref_or_inlined_expr(ctx, *value_of);
                (expression, id.0)
            } else {
                let expression = to_expression(ctx, instruction);
                (expression, variable)
            };

        if ctx.variable_has_uses(variable) {
            let (should_declare, varname) = ctx.create_or_assign_to_var(variable);

            vec![if should_declare {
                build_var_decl(build_binding_identifier(&varname), expression)
            } else {
                build_var_assign(&varname, expression)
            }]
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

fn to_expression(ctx: &mut ToAstContext, expr: &Instruction) -> Expr {
    match expr {
        Instruction::LitNumber(num) => (*num).into(),
        Instruction::LitBigInt(num) => num.clone().into(),
        Instruction::LitBool(s) => (*s).into(),
        Instruction::LitString(s) => Expr::Lit(Lit::Str(Str::from(&s[..]))),
        Instruction::LitRegExp(re, flags) => Expr::Lit(Lit::Regex(Regex {
            span: Default::default(),
            flags: flags.clone().into(),
            exp: re.clone().into(),
        })),
        Instruction::TemplateString(tag, strings_exps) => {
            let tag = tag.map(|tag| ref_or_inlined_expr(ctx, tag));

            let mut quasis = vec![];
            let mut exprs = vec![];

            for (string, exp) in strings_exps.iter() {
                quasis.push(TplElement {
                    span: Default::default(),
                    cooked: None, // unused by SWC
                    tail: exp.is_none(),
                    raw: string.clone().into(),
                });

                if let Some(exp) = exp {
                    let exp = ref_or_inlined_expr(ctx, *exp);
                    exprs.push(Box::new(exp));
                } else {
                    break; // exp is None at the end
                }
            }

            let tpl = swc_ecma_ast::Tpl {
                span: Default::default(),
                exprs,
                quasis,
            };

            match tag {
                Some(tag) => Expr::TaggedTpl(swc_ecma_ast::TaggedTpl {
                    span: Default::default(),
                    tag: Box::new(tag),
                    tpl: Box::new(tpl),
                    type_params: None,
                }),
                None => Expr::Tpl(tpl),
            }
        }
        Instruction::Undefined => Expr::Ident(Ident::new("undefined".into(), Default::default())),
        Instruction::Null => Expr::Lit(Lit::Null(Null {
            span: Default::default(),
        })),
        Instruction::UnaryOp(op, operand) => {
            let operand = ref_or_inlined_expr(ctx, *operand);

            Expr::Unary(UnaryExpr {
                span: Default::default(),
                op: op.clone(),
                arg: Box::new(operand),
            })
        }
        Instruction::BinOp(op, left, right) => {
            let left = ref_or_inlined_expr(ctx, *left);
            let right = ref_or_inlined_expr(ctx, *right);

            let left = if should_add_parens(&op, &left, false) {
                build_parens(left)
            } else {
                left
            };
            let right = if should_add_parens(&op, &right, true) {
                build_parens(right)
            } else {
                right
            };

            Expr::Bin(swc_ecma_ast::BinExpr {
                span: Default::default(),
                op: op.clone(),
                left: Box::new(left),
                right: Box::new(right),
            })
        }
        Instruction::PrivateIn(priv_name, right) => {
            let right = ref_or_inlined_expr(ctx, *right);

            let right = if should_add_parens(&BinaryOp::In, &right, true) {
                build_parens(right)
            } else {
                right
            };

            Expr::Bin(swc_ecma_ast::BinExpr {
                span: Default::default(),
                op: BinaryOp::In,
                left: Box::new(Expr::PrivateName(PrivateName {
                    span: Default::default(),
                    id: Ident::new(priv_name.clone().into(), Default::default()),
                })),
                right: Box::new(right),
            })
        }
        Instruction::IncrDecr(lhs, is_incr) | Instruction::IncrDecrPostfix(lhs, is_incr) => {
            let op = match is_incr {
                IncrDecr::Incr => UpdateOp::PlusPlus,
                IncrDecr::Decr => UpdateOp::MinusMinus,
            };

            Expr::Update(UpdateExpr {
                op,
                arg: Box::new(lhs_to_ast_expr(ctx, lhs)),
                prefix: matches!(expr, Instruction::IncrDecr(..)),
                span: Default::default(),
            })
        }
        Instruction::Ref(var_idx) => ref_or_inlined_expr(ctx, *var_idx),
        Instruction::This => Expr::Ident(Ident::new("this".into(), Default::default())),
        Instruction::TypeOf(var_idx) => {
            let var = ref_or_inlined_expr(ctx, *var_idx);

            Expr::Unary(UnaryExpr {
                span: Default::default(),
                op: UnaryOp::TypeOf,
                arg: Box::new(var),
            })
        }
        Instruction::TypeOfGlobal(varname) => Expr::Unary(UnaryExpr {
            span: Default::default(),
            op: UnaryOp::TypeOf,
            arg: Box::new(build_identifier_str(varname.as_str())),
        }),
        Instruction::Array(items) => {
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
        Instruction::Object(proto, props) => Expr::Object(ObjectLit {
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
                    ObjectProperty::Spread(spread_obj) => PropOrSpread::Spread(SpreadElement {
                        dot3_token: Default::default(),
                        expr: Box::new(ref_or_inlined_expr(ctx, *spread_obj)),
                    }),
                    ObjectProperty::KeyValue(obj_key, obj_val) => {
                        let key = match obj_key {
                            crate::basic_blocks::ObjectKey::NormalKey(k) => {
                                if identifier_needs_quotes(&k) {
                                    PropName::Str(k.as_str().into())
                                } else {
                                    PropName::Ident(Ident::new(
                                        k.as_str().into(),
                                        Default::default(),
                                    ))
                                }
                            }
                            crate::basic_blocks::ObjectKey::Private(_) => {
                                unreachable!("objects cannot have private props")
                            }
                            crate::basic_blocks::ObjectKey::Computed(expr) => {
                                PropName::Computed(ComputedPropName {
                                    span: Default::default(),
                                    expr: Box::new(ref_or_inlined_expr(ctx, *expr)),
                                })
                            }
                        };

                        PropOrSpread::Prop(Box::new(match obj_val {
                            crate::basic_blocks::ObjectValue::Property(prop) => {
                                Prop::KeyValue(KeyValueProp {
                                    key,
                                    value: Box::new(ref_or_inlined_expr(ctx, *prop)),
                                })
                            }
                            crate::basic_blocks::ObjectValue::Method(kind, fn_id) => {
                                let function = ctx.take_function(*fn_id);
                                let function = function_to_ast(ctx, function);

                                match kind {
                                    MethodKind::Method => {
                                        swc_ecma_ast::Prop::Method(swc_ecma_ast::MethodProp {
                                            key,
                                            function: Box::new(function),
                                        })
                                    }
                                    MethodKind::Getter => {
                                        swc_ecma_ast::Prop::Getter(swc_ecma_ast::GetterProp {
                                            span: Default::default(),
                                            key,
                                            type_ann: None,
                                            body: function.body,
                                        })
                                    }
                                    MethodKind::Setter => {
                                        swc_ecma_ast::Prop::Setter(swc_ecma_ast::SetterProp {
                                            span: Default::default(),
                                            key,
                                            param: Box::new(
                                                function.params.into_iter().next().unwrap().pat,
                                            ),
                                            body: function.body,
                                        })
                                    }
                                }
                            }
                        }))
                    }
                }))
                .collect::<Vec<PropOrSpread>>(),
        }),
        Instruction::Super => {
            // Cannot appear on its own, but an optimizer may lead us here.
            Expr::Lit(Lit::Null(Null {
                span: Default::default(),
            }))
        }
        Instruction::ArrayPattern(_, _) => unreachable!(),
        Instruction::ObjectPattern(_, _) => unreachable!(),
        Instruction::PatternUnpack(pat_var, idx) => {
            build_identifier(ctx.get_varname_for_pattern(*pat_var, *idx))
        }
        Instruction::Function(id) => {
            let function = ctx.take_function(*id);
            function_expr_to_ast(ctx, function)
        }
        Instruction::Call(func_idx, args) => {
            let callee = match ctx.peek_inlined_expression(*func_idx) {
                Some(Instruction::Super) => {
                    // Super call!
                    Callee::Super(Super {
                        span: Default::default(),
                    })
                }
                _ => Callee::Expr(Box::new(ref_or_inlined_expr(ctx, *func_idx))),
            };
            let args = args
                .iter()
                .map(|arg| ExprOrSpread::from(ref_or_inlined_expr(ctx, *arg)))
                .collect();

            Expr::Call(CallExpr {
                span: Default::default(),
                callee,
                args,
                type_args: None,
            })
        }
        Instruction::New(func_idx, args) => {
            let callee = ref_or_inlined_expr(ctx, *func_idx);
            let args: Vec<ExprOrSpread> = args
                .iter()
                .map(|arg| ExprOrSpread::from(ref_or_inlined_expr(ctx, *arg)))
                .collect();

            Expr::New(NewExpr {
                span: Default::default(),
                callee: Box::new(callee),
                args: Some(args), // TODO SWC can't dynamically add parens when needed here, maybe
                // upstream a fix so we can ellide arguments by passing None
                type_args: None,
            })
        }

        Instruction::ArgumentRead(_) => unreachable!("handled in function_to_ast"),
        Instruction::ArgumentRest(_) => unreachable!("handled in function_to_ast"),

        Instruction::TempExit(typ, arg) => match typ {
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

        Instruction::CaughtError => Expr::Ident(Ident::new(
            ctx.get_caught_error().into(),
            Default::default(),
        )),
        Instruction::ForInOfValue => Expr::Ident(Ident::new(
            ctx.get_for_in_of_value().into(),
            Default::default(),
        )),
        Instruction::Phi(_) => unreachable!("phi should be removed by remove_phi()"),
        Instruction::Read(lhs) => lhs_to_ast_expr(ctx, lhs),
        Instruction::Write(lhs, assignee) => Expr::Assign(AssignExpr {
            span: Default::default(),
            op: AssignOp::Assign,
            left: PatOrExpr::Expr(Box::new(lhs_to_ast_expr(ctx, lhs))),
            right: Box::new(ref_or_inlined_expr(ctx, *assignee)),
        }),
        Instruction::Delete(lhs) => Expr::Unary(UnaryExpr {
            span: Default::default(),
            op: UnaryOp::Delete,
            arg: Box::new(lhs_to_ast_expr(ctx, lhs)),
        }),

        Instruction::CreateClass(_optional_extends) => {
            unreachable!("handled in function_to_ast")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutils::*;

    #[test]
    fn to_tree() {
        let block_group = test_basic_blocks_module("1 + 2 + 3");

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        1 + 2 + 3;
        "###);
    }

    #[test]
    fn to_ast_tpl_string() {
        let block_group = test_basic_blocks_module("tplString`a${1 + 2}`");

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        tplString`a${1 + 2}`;
        "###);
    }

    #[test]
    fn logical_ops() {
        let block_group = test_basic_blocks_module("((X ?? Y) && Z) || W");
        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        (((a = X, a) ?? (a = Y, a), a) && (a = Z, a), a) || (a = W, a);
        var a;
        "###);

        let block_group = test_basic_blocks_module("X ?? (Y && (Z || W))");
        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        (a = X, a) ?? ((a = Y, a) && ((a = Z, a) || (a = W, a), a), a);
        var a;
        "###);

        let block_group = test_basic_blocks_module("return ((X ?? Y) && Z) || W");
        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        (((a = X, a) ?? (a = Y, a), a) && (a = Z, a), a) || (a = W, a);
        return a;
        var a;
        "###);

        let block_group = test_basic_blocks_module("return X ?? (Y && (Z || W))");
        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        (a = X, a) ?? ((a = Y, a) && ((a = Z, a) || (a = W, a), a), a);
        return a;
        var a;
        "###);
    }

    #[test]
    fn to_incr_decr() {
        let block_group = test_basic_blocks_module(
            "var a = 100;
            use(a++);
            var b = 200;
            use(--b);
            ++globalVar;
            globalVar.foo--;
            a.decrProp--;
            ++a.incrProp;",
        );

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        var a = 100;
        use(a++);
        var b = 200;
        use(--b);
        ++globalVar;
        globalVar.foo--;
        a.decrProp--;
        ++a.incrProp;
        "###);

        let block_group = test_basic_blocks_module(
            "var a = 100;
            use(a++);
            use(a);
            var b = 200;
            use(--b);
            use(b);
            use(++globalVar);
            use(globalVar--);
            use(++globalVar.prop);
            use(globalVar.prop--);",
        );

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        var a = 100;
        use(a++);
        use(a);
        var b = 200;
        use(--b);
        use(b);
        use(++globalVar);
        use(globalVar--);
        use(++globalVar.prop);
        use(globalVar.prop--);
        "###);
    }

    #[test]
    fn to_tree_cond_1() {
        let block_group = test_basic_blocks_module("return (1 ? 2 : 3)");

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
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

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        var a = 0;
        if (1) {
            a = 2;
        }
        return a;
        "###);
    }

    #[test]
    fn removes_unused() {
        let block_group = test_basic_blocks_module(
            "var a = 1
            return 2",
        );

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        return 2;
        "###);
    }

    #[test]
    fn to_scopes() {
        let block_group = test_basic_blocks_module(
            "var outer = 1
            return function bar() { return outer; }",
        );

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        var a = undefined;
        a = 1;
        var b = undefined;
        var c = (function() {
            return a;
        });
        b = c;
        return c;
        "###);
    }

    #[test]
    fn to_gen_scopes() {
        let block_group = test_basic_blocks_module("(function* bar() { yield 1; })");

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        var a = undefined;
        a = (function*() {
            yield 1;
        });
        "###);
    }

    #[test]
    fn to_scopes_rw() {
        let block_group = test_basic_blocks_module(
            "var outer = 1
            return function bar() { outer = outer + 1 }",
        );

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        var a = undefined;
        a = 1;
        var b = undefined;
        var d = (function() {
            var c = a + 1;
            a = c;
        });
        b = d;
        return d;
        "###);
    }

    #[test]
    fn to_loop() {
        let block_group = test_basic_blocks_module("while (123) { if (456) { break; } }");

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        while(true){
            if (123) {
                if (456) {
                    break;
                }
            } else {
                break;
            }
        }
        "###);
    }

    #[test]
    fn to_loop_forin() {
        let block_group = test_basic_blocks_module(
            "for (var x in {}) {
                x()
            }",
        );

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        for(var a in {}){
            var b = a;
            b();
        }
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

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        while(true){
            if (123) {
                if (456) {
                    break;
                }
                while(true){
                    if (789) {
                        if (1234) {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            } else {
                break;
            }
        }
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

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        a: while(true){
            if (123) {
                if (456) {
                    break;
                }
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
            } else {
                break;
            }
        }
        "###);
    }

    #[test]
    fn to_conditional_var() {
        let block_group = test_basic_blocks_module(
            "var x = 10;
            if (123) { x = 456; } else { x = 789; }
            return x + 1",
        );

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
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

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        var a = 10;
        try {
            if (a > 10) {
                throw 123;
            }
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

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        var a = 10;
        try {
            if (a > 10) {
                throw 123;
            }
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
