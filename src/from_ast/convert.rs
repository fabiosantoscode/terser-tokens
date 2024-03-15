use swc_ecma_ast::{
    AwaitExpr, BinaryOp, Decl, Expr, ExprOrSpread, ForHead, ForInStmt, ForOfStmt, GetterProp,
    IfStmt, LabeledStmt, Lit, MethodProp, ObjectLit, PatOrExpr, Prop, PropName, PropOrSpread,
    SetterProp, Stmt, ThrowStmt, UnaryOp, UpdateExpr, UpdateOp, VarDecl, VarDeclKind,
    VarDeclOrExpr, YieldExpr,
};

use crate::basic_blocks::{
    ArrayElement, BasicBlockExit, BasicBlockInstruction, ExitType, ForInOfKind, IncrDecr,
    MethodKind, ObjectKey, ObjectProperty, ObjectValue, TempExitType, LHS,
};

use super::{
    block_to_basic_blocks, class_to_basic_blocks, convert_object_propname,
    function_to_basic_blocks, get_propname_normal_key, pat_to_basic_blocks, to_basic_blocks_lhs,
    FromAstCtx, FunctionLike, NestedIntoStatement, PatType,
};

/// Turn a statement into basic blocks.
/// wraps `stat_to_basic_blocks_inner` while passing it the label, if what we got was a labeled statement
pub fn stat_to_basic_blocks(ctx: &mut FromAstCtx, stat: &Stmt) {
    let is_loop = |stat: &Stmt| match stat {
        Stmt::While(_) | Stmt::DoWhile(_) | Stmt::For(_) | Stmt::ForIn(_) | Stmt::ForOf(_) => true,
        _ => false,
    };

    let (pushed, is_labelled_block) = if let Stmt::Labeled(LabeledStmt { label, body, .. }) = stat {
        ctx.push_label(NestedIntoStatement::Labelled(label.sym.to_string()));
        stat_to_basic_blocks_inner(ctx, body);
        (true, !is_loop(body))
    } else if is_loop(stat) {
        ctx.push_label(NestedIntoStatement::Unlabelled);
        stat_to_basic_blocks_inner(ctx, stat);
        (true, false)
    } else {
        stat_to_basic_blocks_inner(ctx, stat);
        (false, false)
    };

    if pushed {
        let jumpers_towards_me = ctx.pop_label();
        if jumpers_towards_me.len() > 0 {
            for jumper in jumpers_towards_me {
                if is_labelled_block {
                    ctx.set_exit(jumper, BasicBlockExit::Jump(ctx.current_block_index()))
                } else {
                    ctx.set_exit(jumper, BasicBlockExit::Break(ctx.current_block_index()))
                }
            }
        }
    }
}

/// Turn a statement into basic blocks. Wrapped by `stat_to_basic_blocks` to handle labels.
fn stat_to_basic_blocks_inner(ctx: &mut FromAstCtx, stat: &Stmt) {
    match stat {
        Stmt::Expr(expr) => {
            let _exprid = expr_to_basic_blocks(ctx, &expr.expr);

            ctx.wrap_up_block();
        }
        Stmt::Decl(Decl::Var(var)) => {
            var_decl_to_basic_blocks(ctx, var);
        }
        Stmt::Decl(Decl::Fn(_)) => {
            unreachable!("function declarations should be handled by block_to_basic_blocks")
        }
        Stmt::Decl(Decl::Class(class)) => {
            class_to_basic_blocks(ctx, class.class.as_ref(), Some(class.ident.sym.to_string()))
                .expect("error in class_to_basic_blocks");
        }
        // https://262.ecma-international.org/#sec-runtime-semantics-forin-div-ofbodyevaluation-lhs-stmt-iterator-lhskind-labelset
        Stmt::ForOf(ForOfStmt {
            left, right, body, ..
        })
        | Stmt::ForIn(ForInStmt {
            left, right, body, ..
        }) => {
            ctx.wrap_up_block();

            // ForInOfLoop($right, {kind}, 1, 3)
            // 1: (take and assign the ForInOfValue)
            // 2: $body
            // 3: Jump(1)
            // 4: (outside now)

            let right = expr_to_basic_blocks(ctx, &right);

            let blockidx_loop_head = ctx.wrap_up_block(); // '0 ForInOfLoop(1..3)

            ctx.enter_conditional_branch();

            let blockidx_bind_start = ctx.wrap_up_block();

            let loop_value = ctx.push_instruction(BasicBlockInstruction::ForInOfValue);
            match left {
                ForHead::VarDecl(var_decl) => {
                    assert_eq!(
                        var_decl.decls.len(),
                        1,
                        "for-in/of var decls should have exactly one binding"
                    );
                    let only_decl = &var_decl.decls[0];

                    match var_decl.kind {
                        VarDeclKind::Var => {
                            pat_to_basic_blocks(ctx, PatType::VarDecl, &only_decl.name, loop_value)
                        }
                        VarDeclKind::Let => todo!(),
                        VarDeclKind::Const => todo!(),
                    }
                }
                ForHead::Pat(ref pat) => pat_to_basic_blocks(ctx, PatType::Assign, pat, loop_value),
                ForHead::UsingDecl(_) => todo!(),
            };

            let kind = match stat {
                Stmt::ForIn(_) => ForInOfKind::ForIn,
                Stmt::ForOf(st) if !st.is_await => ForInOfKind::ForOf,
                Stmt::ForOf(_) => ForInOfKind::ForAwaitOf,
                _ => unreachable!(),
            };

            stat_to_basic_blocks(ctx, body);

            ctx.leave_conditional_branch();

            let blockidx_jump_back = ctx.wrap_up_block();

            ctx.set_exit(
                blockidx_loop_head,
                BasicBlockExit::ForInOfLoop(right, kind, blockidx_bind_start, blockidx_jump_back),
            );

            ctx.set_exit(
                blockidx_jump_back,
                BasicBlockExit::Continue(blockidx_bind_start),
            );
        }
        Stmt::For(for_loop) => {
            // -1: $init
            // 0: Loop(1, 4)
            // 1: Cond($test, 2, 4)
            // 2:   then: $body, $update
            // 3:         Continue(1)
            // 4:   else: Break(5)
            // 5: (outside now)

            match &for_loop.init {
                Some(VarDeclOrExpr::VarDecl(decl)) => {
                    var_decl_to_basic_blocks(ctx, decl);
                }
                Some(VarDeclOrExpr::Expr(expr)) => {
                    expr_to_basic_blocks(ctx, &expr);
                }
                None => {}
            };

            let blockidx_loop = ctx.wrap_up_block(); // '0 Loop(1..4)

            ctx.enter_conditional_branch();

            let blockidx_cond = ctx.wrap_up_block(); // '1 Cond(2..3, 4..4)

            let test = if let Some(test) = &for_loop.test {
                expr_to_basic_blocks(ctx, test)
            } else {
                ctx.push_instruction(BasicBlockInstruction::LitBool(true))
            };

            let blockidx_body_start = ctx.wrap_up_block(); // '2 $body, $update

            stat_to_basic_blocks(ctx, &for_loop.body);
            if let Some(update) = &for_loop.update {
                expr_to_basic_blocks(ctx, update);
            }

            let blockidx_loop_back = ctx.wrap_up_block(); // '3

            let blockidx_cond_else = ctx.wrap_up_block(); // '4

            let blockidx_outside = ctx.wrap_up_block(); // '5

            ctx.leave_conditional_branch(); // insert phi nodes in '5

            ctx.set_exit(
                blockidx_loop,
                BasicBlockExit::Loop(blockidx_cond, blockidx_cond_else),
            );
            ctx.set_exit(
                blockidx_cond,
                BasicBlockExit::Cond(
                    test,
                    blockidx_body_start,
                    blockidx_loop_back,
                    blockidx_cond_else,
                    blockidx_cond_else,
                ),
            );
            ctx.set_exit(blockidx_loop_back, BasicBlockExit::Continue(blockidx_cond));
            ctx.set_exit(blockidx_cond_else, BasicBlockExit::Break(blockidx_outside));
        }
        Stmt::DoWhile(dowhil) => {
            // Loop(1, 4)
            // 1: $body
            // 2: Cond($test, 3, 4)
            // 3:   then: Continue(1)
            // 4:   else: Break(5)
            // 5: (outside now)

            let blockidx_loop = ctx.wrap_up_block(); // '0 Loop(1..4)

            let blockidx_body_start = ctx.wrap_up_block(); // '1 $body

            stat_to_basic_blocks(ctx, &dowhil.body);

            ctx.enter_conditional_branch();

            let blockidx_cond = ctx.wrap_up_block(); // '2 Cond(3..3, 4..4)

            let test = expr_to_basic_blocks(ctx, &dowhil.test);

            let blockidx_loop_back = ctx.wrap_up_block(); // '3

            let blockidx_cond_else = ctx.wrap_up_block(); // '4

            let blockidx_outside = ctx.wrap_up_block(); // '5

            ctx.leave_conditional_branch(); // insert phi nodes in '5

            ctx.set_exit(
                blockidx_loop,
                BasicBlockExit::Loop(blockidx_body_start, blockidx_cond_else),
            );
            ctx.set_exit(
                blockidx_cond,
                BasicBlockExit::Cond(
                    test,
                    blockidx_loop_back,
                    blockidx_loop_back,
                    blockidx_cond_else,
                    blockidx_cond_else,
                ),
            );
            ctx.set_exit(
                blockidx_loop_back,
                BasicBlockExit::Continue(blockidx_body_start),
            );
            ctx.set_exit(blockidx_cond_else, BasicBlockExit::Break(blockidx_outside));
        }
        Stmt::While(whil) => {
            // Loop(1, 4)
            // 1: Cond($test, 2, 4)
            // 2:   then: $body
            // 3:         Continue(1)
            // 4:   else: Break(5)
            // 5: (outside now)

            let blockidx_loop = ctx.wrap_up_block(); // '0 Loop(1..4)

            ctx.enter_conditional_branch();

            let blockidx_cond = ctx.wrap_up_block(); // '1 Cond(2..3, 4..4)

            let test = expr_to_basic_blocks(ctx, &whil.test);

            let blockidx_body_start = ctx.wrap_up_block(); // '2 $body

            stat_to_basic_blocks(ctx, &whil.body);

            let blockidx_loop_back = ctx.wrap_up_block(); // '3

            let blockidx_cond_else = ctx.wrap_up_block(); // '4

            let blockidx_outside = ctx.wrap_up_block(); // '5

            ctx.leave_conditional_branch(); // insert phi nodes in '5

            ctx.set_exit(
                blockidx_loop,
                BasicBlockExit::Loop(blockidx_cond, blockidx_cond_else),
            );
            ctx.set_exit(
                blockidx_cond,
                BasicBlockExit::Cond(
                    test,
                    blockidx_body_start,
                    blockidx_loop_back,
                    blockidx_cond_else,
                    blockidx_cond_else,
                ),
            );
            ctx.set_exit(blockidx_loop_back, BasicBlockExit::Continue(blockidx_cond));
            ctx.set_exit(blockidx_cond_else, BasicBlockExit::Break(blockidx_outside));
        }
        Stmt::If(IfStmt {
            test, cons, alt, ..
        }) => {
            ctx.wrap_up_block();

            // IF($test)
            let test = expr_to_basic_blocks(ctx, &test);
            let blockidx_before = ctx.wrap_up_block();

            ctx.enter_conditional_branch();

            // THEN
            let blockidx_consequent_before = ctx.wrap_up_block();
            stat_to_basic_blocks(ctx, &cons);
            let blockidx_consequent_after = ctx.current_block_index();

            // ELSE
            let blockidx_alternate_before = ctx.wrap_up_block();
            if let Some(alt) = alt {
                stat_to_basic_blocks(ctx, &alt);
            }
            let blockidx_alternate_after = ctx.current_block_index();

            let after = ctx.wrap_up_block();
            ctx.leave_conditional_branch();

            ctx.set_exit(
                blockidx_before,
                BasicBlockExit::Cond(
                    test,
                    blockidx_consequent_before,
                    blockidx_consequent_after,
                    blockidx_alternate_before,
                    blockidx_alternate_after,
                ),
            );
            ctx.set_exit(blockidx_consequent_after, BasicBlockExit::Jump(after)); // TODO should be Break
            ctx.set_exit(blockidx_alternate_after, BasicBlockExit::Jump(after));
        }
        Stmt::Block(block) => {
            block_to_basic_blocks(ctx, &block.stmts).expect("todo error handling")
        }
        Stmt::Break(br) => {
            ctx.register_break(&br.label);

            ctx.wrap_up_block();
        }
        Stmt::Continue(_cont) => todo!("ctx.register_continue(cont.label)"),
        Stmt::Labeled(_) => unreachable!("label is handled in stat_to_basic_blocks"),
        Stmt::Debugger(_) => {
            let index = ctx.wrap_up_block();
            let after_index = ctx.wrap_up_block();

            ctx.set_exit(index, BasicBlockExit::Debugger(after_index));
        }
        Stmt::With(_) => todo!(),
        Stmt::Switch(_) => todo!(),
        Stmt::Throw(ThrowStmt { arg, .. }) => {
            ctx.wrap_up_block();
            let arg = expr_to_basic_blocks(ctx, arg);

            let throw_from = ctx.wrap_up_block();
            ctx.set_exit(throw_from, BasicBlockExit::ExitFn(ExitType::Throw, arg));

            ctx.wrap_up_block();
        }
        Stmt::Return(ret) => {
            ctx.wrap_up_block();
            let expr = expr_to_basic_blocks(ctx, ret.arg.as_ref().unwrap());

            let return_from = ctx.wrap_up_block();
            ctx.set_exit(return_from, BasicBlockExit::ExitFn(ExitType::Return, expr));

            ctx.wrap_up_block();
        }
        Stmt::Try(ref stmt) => {
            let catch_pusher_idx = ctx.wrap_up_block();
            let try_idx = ctx.wrap_up_block();

            ctx.enter_conditional_branch();

            block_to_basic_blocks(ctx, &stmt.block.stmts).expect("todo error handling");

            let before_catch_idx = ctx.wrap_up_block();
            let catch_idx = ctx.wrap_up_block();
            ctx.enter_conditional_branch();

            if let Some(ref handler) = stmt.handler {
                if let Some(p) = &handler.param {
                    let catcherr = ctx.push_instruction(BasicBlockInstruction::CaughtError);
                    pat_to_basic_blocks(ctx, PatType::VarDecl, p, catcherr);
                }
                block_to_basic_blocks(ctx, &handler.body.stmts).expect("todo error handling");
            }

            let after_catch_idx = ctx.wrap_up_block();
            let finally_idx = ctx.wrap_up_block();

            ctx.leave_conditional_branch();

            if let Some(ref finalizer) = stmt.finalizer {
                block_to_basic_blocks(ctx, &finalizer.stmts).expect("todo error handling");
            }

            let after_finally_idx = ctx.wrap_up_block();

            ctx.leave_conditional_branch();

            let done_and_dusted = ctx.wrap_up_block();

            // declare the trycatch
            ctx.set_exit(
                catch_pusher_idx,
                BasicBlockExit::SetTryAndCatch(try_idx, catch_idx, finally_idx, after_finally_idx),
            );
            // catch the error
            ctx.set_exit(
                before_catch_idx,
                BasicBlockExit::PopCatch(catch_idx, after_catch_idx),
            );
            // finally
            ctx.set_exit(
                after_catch_idx,
                BasicBlockExit::PopFinally(finally_idx, after_finally_idx),
            );
            // end
            ctx.set_exit(
                after_finally_idx,
                BasicBlockExit::EndFinally(done_and_dusted),
            );
        }
        Stmt::Empty(_) => {
            ctx.wrap_up_block();
        }
        Stmt::Decl(Decl::Using(_)) => unreachable!("using decl"),
        Stmt::Decl(
            Decl::TsInterface(_) | Decl::TsTypeAlias(_) | Decl::TsEnum(_) | Decl::TsModule(_),
        ) => unreachable!("typescript features"),
    }
}

fn var_decl_to_basic_blocks(ctx: &mut FromAstCtx, var: &VarDecl) {
    for decl in &var.decls {
        let init = match decl.init.as_ref() {
            Some(init) => expr_to_basic_blocks(ctx, &init.as_ref()),
            None => ctx.push_instruction(BasicBlockInstruction::Undefined),
        };
        pat_to_basic_blocks(ctx, PatType::VarDecl, &decl.name, init);
    }
}

pub fn expr_to_basic_blocks(ctx: &mut FromAstCtx, exp: &Expr) -> usize {
    match exp {
        Expr::Lit(lit) => {
            let lit = match lit {
                Lit::Null(_) => BasicBlockInstruction::Null,
                Lit::Bool(b) => BasicBlockInstruction::LitBool(b.value),
                Lit::Num(num) => BasicBlockInstruction::LitNumber(num.value),
                Lit::BigInt(_) => todo!(),
                Lit::Str(s) => BasicBlockInstruction::LitString(s.value.to_string()),
                Lit::Regex(_) => todo!(),
                Lit::JSXText(_) => todo!(),
            };
            return ctx.push_instruction(lit);
        }
        Expr::Bin(bin) => {
            let l = expr_to_basic_blocks(ctx, &bin.left);

            match &bin.op {
                BinaryOp::LogicalAnd | BinaryOp::LogicalOr | BinaryOp::NullishCoalescing => {
                    let condition = match bin.op {
                        BinaryOp::LogicalAnd => l,
                        BinaryOp::LogicalOr => {
                            ctx.push_instruction(BasicBlockInstruction::UnaryOp(UnaryOp::Bang, l))
                        }
                        BinaryOp::NullishCoalescing => {
                            // eqnull
                            todo!("nullish coalescing")
                        }
                        _ => unreachable!(),
                    };

                    let after_condition = ctx.wrap_up_block();
                    ctx.enter_conditional_branch();

                    let l = ctx.push_instruction(BasicBlockInstruction::Ref(l));

                    let before_else = ctx.wrap_up_block();

                    let r = expr_to_basic_blocks(ctx, &bin.right);

                    ctx.leave_conditional_branch();

                    let after_else = ctx.wrap_up_block();

                    let after = ctx.wrap_up_block();

                    ctx.set_exit(
                        after_condition,
                        BasicBlockExit::Cond(
                            condition,
                            after_condition + 1,
                            before_else,
                            before_else + 1,
                            after_else,
                        ),
                    );
                    ctx.set_exit(after_else, BasicBlockExit::Jump(after));
                    ctx.set_exit(before_else, BasicBlockExit::Jump(after));

                    return ctx.push_instruction(BasicBlockInstruction::Phi(vec![l, r]));
                }
                BinaryOp::In | BinaryOp::InstanceOf => todo!("in/instanceof"),
                _ => {
                    let r = expr_to_basic_blocks(ctx, &bin.right);
                    return ctx.push_instruction(BasicBlockInstruction::BinOp(
                        bin.op.clone(),
                        l,
                        r,
                    ));
                }
            }
        }
        Expr::Assign(assign) => match &assign.left {
            PatOrExpr::Pat(pat) => {
                let init = expr_to_basic_blocks(ctx, &assign.right);
                return pat_to_basic_blocks(ctx, PatType::Assign, pat, init);
            }
            _ => todo!(),
        },
        Expr::Paren(paren) => return expr_to_basic_blocks(ctx, &paren.expr),
        Expr::Seq(seq) => {
            let mut last = None;
            for expr in &seq.exprs {
                last = Some(expr_to_basic_blocks(ctx, expr));
            }
            return last.expect("Seq must have 1+ exprs");
        }
        Expr::Cond(cond_expr) => {
            let test = expr_to_basic_blocks(ctx, &cond_expr.test);
            let blockidx_before = ctx.current_block_index();
            ctx.wrap_up_block();

            ctx.enter_conditional_branch();

            let blockidx_consequent_before = ctx.current_block_index();
            let cons = expr_to_basic_blocks(ctx, &cond_expr.cons);
            let blockidx_consequent_after = ctx.current_block_index();

            let blockidx_alternate_before = ctx.wrap_up_block();
            let alt = expr_to_basic_blocks(ctx, &cond_expr.alt);
            let blockidx_alternate_after = ctx.current_block_index();
            let blockidx_after = ctx.wrap_up_block();

            ctx.wrap_up_block();

            ctx.leave_conditional_branch();

            // block before gets a Cond node added
            ctx.set_exit(
                blockidx_before,
                BasicBlockExit::Cond(
                    test,
                    blockidx_consequent_before,
                    blockidx_consequent_after,
                    blockidx_alternate_before,
                    blockidx_alternate_after,
                ),
            );

            // block starting with cons gets a Jump node added, to the current block
            ctx.set_exit(
                blockidx_consequent_after,
                BasicBlockExit::Jump(blockidx_after),
            );
            ctx.set_exit(
                blockidx_alternate_after,
                BasicBlockExit::Jump(blockidx_after),
            );

            // the retval of our ternary is a phi node
            return ctx.push_instruction(BasicBlockInstruction::Phi(vec![cons, alt]));
        }
        Expr::Ident(ident) => {
            let ident = ident.sym.to_string();
            let instruction = match ident.as_str() {
                "undefined" => BasicBlockInstruction::Undefined,
                "Infinity" => BasicBlockInstruction::LitNumber(f64::INFINITY),
                ident => {
                    if ctx.is_global_name(ident) {
                        BasicBlockInstruction::Read(LHS::Global(ident.to_string()))
                    } else if let Some(nonloc) = ctx.is_nonlocal(ident) {
                        BasicBlockInstruction::Read(LHS::NonLocal(nonloc))
                    } else {
                        BasicBlockInstruction::Ref(ctx.read_name(ident))
                    }
                }
            };

            return ctx.push_instruction(instruction);
        }
        Expr::This(_) => return ctx.push_instruction(BasicBlockInstruction::This),
        Expr::Array(array_lit) => {
            let mut elements = Vec::with_capacity(array_lit.elems.len());

            for elem in &array_lit.elems {
                let elem = match elem {
                    Some(ExprOrSpread { spread, expr }) => match spread {
                        None => ArrayElement::Item(expr_to_basic_blocks(ctx, expr)),
                        Some(_) => ArrayElement::Spread(expr_to_basic_blocks(ctx, expr)),
                    },
                    None => ArrayElement::Hole,
                };

                elements.push(elem);
            }

            return ctx.push_instruction(BasicBlockInstruction::Array(elements));
        }
        Expr::Object(ObjectLit { props, .. }) => {
            let mut kvs = vec![];
            let mut proto = None;
            for prop in props {
                match prop {
                    PropOrSpread::Spread(spread) => kvs.push(ObjectProperty::Spread(
                        expr_to_basic_blocks(ctx, &spread.expr),
                    )),
                    PropOrSpread::Prop(prop) => match prop.as_ref() {
                        Prop::Shorthand(ident) => {
                            let value = expr_to_basic_blocks(ctx, &Expr::Ident(ident.clone()));
                            kvs.push(ObjectProperty::KeyValue(
                                ObjectKey::NormalKey(ident.sym.to_string()),
                                ObjectValue::Property(value),
                            ));
                        }
                        Prop::KeyValue(kv) => match &kv.key {
                            PropName::Computed(expr) => kvs.push(ObjectProperty::KeyValue(
                                ObjectKey::Computed(expr_to_basic_blocks(ctx, &expr.expr)),
                                ObjectValue::Property(expr_to_basic_blocks(ctx, &kv.value)),
                            )),
                            _ => {
                                let prop_name = get_propname_normal_key(&kv.key);
                                if &prop_name == "__proto__" {
                                    proto = Some(expr_to_basic_blocks(ctx, &kv.value));
                                } else {
                                    kvs.push(ObjectProperty::KeyValue(
                                        ObjectKey::NormalKey(prop_name),
                                        ObjectValue::Property(expr_to_basic_blocks(ctx, &kv.value)),
                                    ));
                                }
                            }
                        },
                        Prop::Getter(GetterProp { key, .. })
                        | Prop::Setter(SetterProp { key, .. })
                        | Prop::Method(MethodProp { key, .. }) => {
                            let key = convert_object_propname(ctx, key);
                            let (method_kind, func) = match prop.as_ref() {
                                Prop::Getter(getter) => {
                                    (MethodKind::Getter, FunctionLike::ObjectGetter(&getter))
                                }
                                Prop::Setter(setter) => {
                                    (MethodKind::Setter, FunctionLike::ObjectSetter(&setter))
                                }
                                Prop::Method(meth) => {
                                    (MethodKind::Method, FunctionLike::ObjectMethod(&meth))
                                }
                                _ => unreachable!(),
                            };

                            let (_varname, fn_id) = function_to_basic_blocks(ctx, func, None)
                                .expect("todo error handling");

                            kvs.push(ObjectProperty::KeyValue(
                                key,
                                ObjectValue::Method(method_kind, fn_id),
                            ));
                        }
                        Prop::Assign(_) => unreachable!(),
                    },
                }
            }

            return ctx.push_instruction(BasicBlockInstruction::Object(proto, kvs));
        }
        Expr::Unary(unary_expr) => match unary_expr.op {
            UnaryOp::TypeOf => {
                if let Expr::Ident(global_ident) = &unary_expr.arg.as_ref() {
                    let s = global_ident.sym.to_string();
                    if ctx.is_global_name(&s) {
                        return ctx.push_instruction(BasicBlockInstruction::TypeOfGlobal(s));
                    }
                }
                let expr = expr_to_basic_blocks(ctx, &unary_expr.arg);
                return ctx.push_instruction(BasicBlockInstruction::TypeOf(expr));
            }
            UnaryOp::Delete => {
                let lhs = to_basic_blocks_lhs(ctx, &unary_expr.arg);
                return ctx.push_instruction(BasicBlockInstruction::Delete(lhs));
            }
            UnaryOp::Void => {
                expr_to_basic_blocks(ctx, &unary_expr.arg);
                return ctx.push_instruction(BasicBlockInstruction::Undefined);
            }
            UnaryOp::Minus | UnaryOp::Plus | UnaryOp::Bang | UnaryOp::Tilde => {
                let expr = expr_to_basic_blocks(ctx, &unary_expr.arg);
                return ctx.push_instruction(BasicBlockInstruction::UnaryOp(unary_expr.op, expr));
            }
        },
        Expr::Update(UpdateExpr {
            op, prefix, arg, ..
        }) => {
            let lhs = to_basic_blocks_lhs(ctx, arg);
            let op = match op {
                UpdateOp::PlusPlus => IncrDecr::Incr,
                UpdateOp::MinusMinus => IncrDecr::Decr,
            };
            let ins = if *prefix {
                BasicBlockInstruction::IncrDecr(lhs, op)
            } else {
                BasicBlockInstruction::IncrDecrPostfix(lhs, op)
            };
            return ctx.push_instruction(ins);
        }
        Expr::Member(_) => {
            let lhs = to_basic_blocks_lhs(ctx, exp);

            return ctx.push_instruction(BasicBlockInstruction::Read(lhs));
        }
        Expr::SuperProp(sp) => {
            let sup = ctx.push_instruction(BasicBlockInstruction::Super);

            let lhs = match &sp.prop {
                swc_ecma_ast::SuperProp::Ident(ident) => {
                    let prop = ident.sym.to_string();
                    LHS::Member(Box::new(LHS::Local(sup)), ObjectKey::NormalKey(prop))
                }
                swc_ecma_ast::SuperProp::Computed(computed) => {
                    let expr = expr_to_basic_blocks(ctx, &computed.expr);
                    LHS::Member(Box::new(LHS::Local(expr)), ObjectKey::Computed(expr))
                }
            };

            return ctx.push_instruction(BasicBlockInstruction::Read(lhs));
        }
        Expr::Arrow(arrow_expr) => {
            let (varname, _fn_id) =
                function_to_basic_blocks(ctx, FunctionLike::ArrowExpr(arrow_expr), None)
                    .expect("todo error handling");
            return varname;
        }
        Expr::Fn(fn_expr) => {
            let (varname, _fn_id) =
                function_to_basic_blocks(ctx, FunctionLike::FnExpr(fn_expr), None)
                    .expect("todo error handling");

            return varname;
        }
        Expr::Call(call) => {
            // TODO non-expr callees (super, import)
            let callee = match &call.callee {
                swc_ecma_ast::Callee::Super(_) => {
                    ctx.push_instruction(BasicBlockInstruction::Super)
                }
                swc_ecma_ast::Callee::Import(_) => todo!("import()"),
                swc_ecma_ast::Callee::Expr(expr) => expr_to_basic_blocks(ctx, &expr),
            };

            let mut args = Vec::with_capacity(call.args.len());
            for arg in &call.args {
                match arg.spread {
                    Some(_) => todo!("spread args"),
                    None => args.push(expr_to_basic_blocks(ctx, arg.expr.as_ref())),
                }
            }

            return ctx.push_instruction(BasicBlockInstruction::Call(callee, args));
        }
        Expr::New(new_expr) => {
            let callee = expr_to_basic_blocks(ctx, &new_expr.callee);

            let mut out_args = vec![];
            if let Some(args) = &new_expr.args {
                for arg in args {
                    match arg.spread {
                        Some(_) => todo!("spread args"),
                        None => out_args.push(expr_to_basic_blocks(ctx, arg.expr.as_ref())),
                    }
                }
            }

            return ctx.push_instruction(BasicBlockInstruction::New(callee, out_args));
        }
        Expr::Tpl(_) => todo!(),
        Expr::TaggedTpl(_) => todo!(),
        Expr::Class(class) => {
            let class_name = class.ident.as_ref().map(|id| id.sym.to_string());
            return class_to_basic_blocks(ctx, class.class.as_ref(), class_name).unwrap();
        }
        Expr::MetaProp(_) => todo!(),
        Expr::Yield(YieldExpr { arg, delegate, .. }) => {
            let typ = if *delegate {
                TempExitType::YieldStar
            } else {
                TempExitType::Yield
            };

            let arg = match arg {
                Some(arg) => expr_to_basic_blocks(ctx, arg),
                None => ctx.current_block_index(),
            };

            return ctx.push_instruction(BasicBlockInstruction::TempExit(typ, arg));
        }
        Expr::Await(AwaitExpr { arg, .. }) => {
            let arg = expr_to_basic_blocks(ctx, arg);

            return ctx.push_instruction(BasicBlockInstruction::TempExit(TempExitType::Await, arg));
        }
        Expr::OptChain(_) => todo!(),
        Expr::PrivateName(_) => todo!("handle this in the binary op and member op"),
        Expr::Invalid(_) => unreachable!("Expr::Invalid from SWC should be impossible"),
        Expr::JSXMember(_)
        | Expr::JSXNamespacedName(_)
        | Expr::JSXEmpty(_)
        | Expr::JSXElement(_)
        | Expr::JSXFragment(_) => unreachable!("Expr::JSX from SWC should be impossible"),
        Expr::TsTypeAssertion(_)
        | Expr::TsConstAssertion(_)
        | Expr::TsNonNull(_)
        | Expr::TsAs(_)
        | Expr::TsInstantiation(_)
        | Expr::TsSatisfies(_) => unreachable!("Expr::Ts from SWC should be impossible"),
    };
}

#[cfg(test)]
mod tests {
    use crate::testutils::*;

    #[test]
    fn simple_add() {
        let s = test_basic_blocks_expr("10 + 20 + 30;");
        insta::assert_debug_snapshot!(s, @r###"
        function():
        @0: {
            $0 = 10
            $1 = 20
            $2 = $0 + $1
            $3 = 30
            $4 = $2 + $3
            $5 = undefined
            exit = return $5
        }
        "###);
    }

    #[test]
    fn an_array() {
        let s = test_basic_blocks("var x = [1, 2, , ...3];");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            $1 = 2
            $2 = 3
            $3 = [$0, $1, , ...$2]
            $4 = undefined
            exit = return $4
        }
        "###);
    }

    /* TODO
    #[test]
    fn yield_await() {
        let s = test_basic_blocks("yield (await (yield* 1))");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            $1 = YieldStar $0
            $2 = Await $1
            $3 = Yield $2
            $4 = undefined
            exit = return $4
        }
        "###);
    }
    */

    #[test]
    fn simple_await() {
        let s = test_basic_blocks("await (await 1)");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            $1 = Await $0
            $2 = Await $1
            $3 = undefined
            exit = return $3
        }
        "###);
    }

    #[test]
    fn simple_add_2() {
        let s = test_basic_blocks(
            "
        var a = 10;
        var b = 20;
        var c = a + b + 30;
        ",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 10
            $1 = 20
            $2 = $0
            $3 = $1
            $4 = $2 + $3
            $5 = 30
            $6 = $4 + $5
            $7 = undefined
            exit = return $7
        }
        "###);
    }

    #[test]
    fn convert_simple_unary() {
        let s = test_basic_blocks(
            "var a = 10;
            typeof globalVar;
            typeof a;
            void a;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 10
            $1 = typeof global "globalVar"
            $2 = $0
            $3 = typeof $2
            $4 = $0
            $5 = undefined
            $6 = undefined
            exit = return $6
        }
        "###);
    }

    #[test]
    fn convert_global() {
        let s = test_basic_blocks(
            "readGlobal;
            writeGlobal = 1;
            readGlobalProp.prop;
            writeGlobalProp.prop = 1;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = global "readGlobal"
            $1 = 1
            $2 = global "writeGlobal" = $1
            $3 = $1
            $4 = globalThis.readGlobalProp.prop
            $5 = 1
            $6 = globalThis.writeGlobalProp.prop = $5
            $7 = undefined
            exit = return $7
        }
        "###);
    }

    #[test]
    fn convert_incr_decr() {
        let s = test_basic_blocks(
            "var a = 100;
            a++;
            use(a);
            var b = 200;
            --b;
            use(b);",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 100
            $1 = $0++
            $2 = global "use"
            $3 = $0
            $4 = call $2($3)
            $5 = 200
            $6 = --$5
            $7 = global "use"
            $8 = $5
            $9 = call $7($8)
            $10 = undefined
            exit = return $10
        }
        "###);

        let s = test_basic_blocks(
            "var a = 100;
            use(a++);
            use(--a);",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 100
            $1 = global "use"
            $2 = $0++
            $3 = call $1($2)
            $4 = global "use"
            $5 = --$0
            $6 = call $4($5)
            $7 = undefined
            exit = return $7
        }
        "###);
    }

    #[test]
    fn convert_incr_decr_conditional() {
        let s = test_basic_blocks(
            "var a = 100;
            let b = cond ? a++ : (--a, --a);",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 100
            $1 = global "cond"
            exit = cond $1 ? @1..@1 : @2..@2
        }
        @1: {
            $2 = $0++
            exit = jump @3
        }
        @2: {
            $3 = --$0
            $4 = --$0
            exit = jump @3
        }
        @3: {
            $5 = either($2, $4)
            $6 = undefined
            exit = return $6
        }
        "###);
    }

    #[test]
    fn simple_cond() {
        let s = test_basic_blocks_expr("1 ? 10 : 20;");
        insta::assert_debug_snapshot!(s, @r###"
        function():
        @0: {
            $0 = 1
            exit = cond $0 ? @1..@1 : @2..@2
        }
        @1: {
            $1 = 10
            exit = jump @3
        }
        @2: {
            $2 = 20
            exit = jump @3
        }
        @3: {
            $3 = either($1, $2)
            $4 = undefined
            exit = return $4
        }
        "###);
    }

    #[test]
    fn cond_assign() {
        let s = test_basic_blocks(
            "let x = 999;
            1 ? (x = 2) : 3;
            return x;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 999
            $1 = 1
            exit = cond $1 ? @1..@1 : @2..@2
        }
        @1: {
            $2 = 2
            $3 = $2
            exit = jump @3
        }
        @2: {
            $4 = 3
            exit = jump @3
        }
        @3: {
            $5 = either($0, $2)
            $6 = either($3, $4)
            $7 = $5
            exit = return $7
        }
        "###);
    }

    #[test]
    fn cond_nested() {
        let s = test_basic_blocks_expr("1 ? (2 ? 10 : 15) : 20;");
        insta::assert_debug_snapshot!(s, @r###"
        function():
        @0: {
            $0 = 1
            exit = cond $0 ? @1..@4 : @5..@5
        }
        @1: {
            $1 = 2
            exit = cond $1 ? @2..@2 : @3..@3
        }
        @2: {
            $2 = 10
            exit = jump @4
        }
        @3: {
            $3 = 15
            exit = jump @4
        }
        @4: {
            $4 = either($2, $3)
            exit = jump @6
        }
        @5: {
            $5 = 20
            exit = jump @6
        }
        @6: {
            $6 = either($4, $5)
            $7 = undefined
            exit = return $7
        }
        "###);
    }

    #[test]
    fn simple_vars() {
        let s = test_basic_blocks("var x = 1; x + 2");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            $1 = $0
            $2 = 2
            $3 = $1 + $2
            $4 = undefined
            exit = return $4
        }
        "###);
    }

    #[test]
    fn cond_nested_reassign() {
        let s = test_basic_blocks("var x = 1; 123 ? (x = 2, 1) : x = 3; x + 2");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            $1 = 123
            exit = cond $1 ? @1..@1 : @2..@2
        }
        @1: {
            $2 = 2
            $3 = $2
            $4 = 1
            exit = jump @3
        }
        @2: {
            $5 = 3
            $6 = $5
            exit = jump @3
        }
        @3: {
            $7 = either($0, $2, $5)
            $8 = either($4, $6)
            $9 = $7
            $10 = 2
            $11 = $9 + $10
            $12 = undefined
            exit = return $12
        }
        "###);
    }

    #[test]
    fn cond_nested_reassign_2() {
        let s = test_basic_blocks(
            "var x = 1;
            123 ? ((x = 1234) ? (x = 567) : 890, 1) : x = 3;
            x + 2",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            $1 = 123
            exit = cond $1 ? @1..@4 : @5..@5
        }
        @1: {
            $2 = 1234
            $3 = $2
            exit = cond $3 ? @2..@2 : @3..@3
        }
        @2: {
            $4 = 567
            $5 = $4
            exit = jump @4
        }
        @3: {
            $6 = 890
            exit = jump @4
        }
        @4: {
            $7 = either($5, $6)
            $8 = either($0, $2, $4)
            $9 = 1
            exit = jump @6
        }
        @5: {
            $10 = 3
            $11 = $10
            exit = jump @6
        }
        @6: {
            $12 = either($0, $2, $8, $10)
            $13 = either($9, $11)
            $14 = $12
            $15 = 2
            $16 = $14 + $15
            $17 = undefined
            exit = return $17
        }
        "###);
    }

    #[test]
    fn a_loop() {
        let s = test_basic_blocks("123; while (123) { 456; }");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 123
            exit = loop @1..@3
        }
        @1: {
            $1 = 123
            exit = cond $1 ? @2..@2 : @3..@3
        }
        @2: {
            $2 = 456
            exit = continue @1
        }
        @3: {
            exit = break @4
        }
        @4: {
            $3 = undefined
            exit = return $3
        }
        "###);
    }

    #[test]
    fn a_loop_break() {
        let s = test_basic_blocks("while (123) { break }");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            exit = loop @1..@3
        }
        @1: {
            $0 = 123
            exit = cond $0 ? @2..@2 : @3..@3
        }
        @2: {
            exit = break @4
        }
        @3: {
            exit = break @4
        }
        @4: {
            $1 = undefined
            exit = return $1
        }
        "###);
    }

    #[test]
    fn a_loop_nested() {
        let s = test_basic_blocks(
            "var x = 1;
            outer: while (111) {
                x = x + 1000;
                while (222) {
                    x = x + 2000;
                    break outer;
                }
            }
            return x;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            exit = loop @1..@9
        }
        @1: {
            $1 = either($0, $15)
            $2 = 111
            exit = cond $2 ? @2..@7 : @8..@8
        }
        @2: {
            $3 = $1
            $4 = 1000
            $5 = $3 + $4
            $6 = $5
            exit = loop @3..@6
        }
        @3: {
            $7 = either($5, $13)
            $8 = 222
            exit = cond $8 ? @4..@4 : @5..@5
        }
        @4: {
            $9 = $7
            $10 = 2000
            $11 = $9 + $10
            $12 = $11
            exit = break @10
        }
        @5: {
            exit = break @7
        }
        @6: {
            $13 = either($0, $1, $5, $7, $11)
            exit = jump @7
        }
        @7: {
            $14 = either($0, $1, $5, $7, $13)
            exit = continue @1
        }
        @8: {
            exit = break @10
        }
        @9: {
            $15 = either($0, $1, $5, $14)
            exit = jump @10
        }
        @10: {
            $16 = either($0, $1, $15)
            $17 = $16
            exit = return $17
        }
        "###);
    }

    #[test]
    fn a_dw_loop() {
        let s = test_basic_blocks(
            r###"do {
                123;
            } while (456);"###,
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            exit = loop @1..@3
        }
        @1: {
            $0 = 123
            $1 = 456
            exit = cond $1 ? @2..@2 : @3..@3
        }
        @2: {
            exit = continue @1
        }
        @3: {
            exit = break @4
        }
        @4: {
            $2 = undefined
            exit = return $2
        }
        "###);
    }

    #[test]
    fn a_for_loop() {
        let s = test_basic_blocks(
            r###"
            for (var i = 123; i < 456; i = i + 1) {
                789;
            }
            "###,
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 123
            exit = loop @1..@4
        }
        @1: {
            $1 = either($0, $10)
            $2 = $1
            $3 = 456
            $4 = $2 < $3
            exit = cond $4 ? @2..@2 : @3..@3
        }
        @2: {
            $5 = 789
            $6 = $1
            $7 = 1
            $8 = $6 + $7
            $9 = $8
            exit = continue @1
        }
        @3: {
            exit = break @5
        }
        @4: {
            $10 = either($0, $1, $8)
            exit = jump @5
        }
        @5: {
            $11 = either($0, $1, $10)
            $12 = undefined
            exit = return $12
        }
        "###);
    }

    #[test]
    fn a_if_nested() {
        let s = test_basic_blocks(
            "var x = 1;
            if (111) {
                x = x + 1000;
                if (222) {
                    x = x + 2000;
                }
            }
            return x;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            $1 = 111
            exit = cond $1 ? @1..@4 : @5..@5
        }
        @1: {
            $2 = $0
            $3 = 1000
            $4 = $2 + $3
            $5 = $4
            $6 = 222
            exit = cond $6 ? @2..@2 : @3..@3
        }
        @2: {
            $7 = $4
            $8 = 2000
            $9 = $7 + $8
            $10 = $9
            exit = jump @4
        }
        @3: {
            exit = jump @4
        }
        @4: {
            $11 = either($0, $4, $9)
            exit = jump @6
        }
        @5: {
            exit = jump @6
        }
        @6: {
            $12 = either($0, $4, $11)
            $13 = $12
            exit = return $13
        }
        "###);
    }

    #[test]
    fn a_if_nested_2() {
        let s = test_basic_blocks(
            "let x = 1;
            if (x == 1) {
                if (x == 1) {
                    x = x + 2000;
                } else {
                    x = 3;
                }
                x = x + 1000;
            } else {
                x = 3;
            }
            return x;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            $1 = $0
            $2 = 1
            $3 = $1 == $2
            exit = cond $3 ? @1..@4 : @5..@5
        }
        @1: {
            $4 = $0
            $5 = 1
            $6 = $4 == $5
            exit = cond $6 ? @2..@2 : @3..@3
        }
        @2: {
            $7 = $0
            $8 = 2000
            $9 = $7 + $8
            $10 = $9
            exit = jump @4
        }
        @3: {
            $11 = 3
            $12 = $11
            exit = jump @4
        }
        @4: {
            $13 = either($0, $9, $11)
            $14 = $13
            $15 = 1000
            $16 = $14 + $15
            $17 = $16
            exit = jump @6
        }
        @5: {
            $18 = 3
            $19 = $18
            exit = jump @6
        }
        @6: {
            $20 = either($0, $13, $16, $18)
            $21 = $20
            exit = return $21
        }
        "###);
    }

    #[test]
    fn a_logical_operator() {
        let s = test_basic_blocks("var x = 1 && 2; return x;");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            $1 = $0
            exit = cond $0 ? @1..@1 : @2..@2
        }
        @1: {
            $2 = 2
            exit = jump @3
        }
        @2: {
            exit = jump @3
        }
        @3: {
            $3 = either($1, $2)
            $4 = $3
            exit = return $4
        }
        "###);

        let s = test_basic_blocks("var x = y() || z(); return x;");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = global "y"
            $1 = call $0()
            $2 = !$1
            $3 = $1
            exit = cond $2 ? @1..@1 : @2..@2
        }
        @1: {
            $4 = global "z"
            $5 = call $4()
            exit = jump @3
        }
        @2: {
            exit = jump @3
        }
        @3: {
            $6 = either($3, $5)
            $7 = $6
            exit = return $7
        }
        "###);
    }

    #[test]
    fn a_labelled_break() {
        let s = test_basic_blocks("outer: while (123) { while (456) { break outer } }");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            exit = loop @1..@7
        }
        @1: {
            $0 = 123
            exit = cond $0 ? @2..@6 : @7..@7
        }
        @2: {
            exit = loop @3..@5
        }
        @3: {
            $1 = 456
            exit = cond $1 ? @4..@4 : @5..@5
        }
        @4: {
            exit = break @8
        }
        @5: {
            exit = break @6
        }
        @6: {
            exit = continue @1
        }
        @7: {
            exit = break @8
        }
        @8: {
            $2 = undefined
            exit = return $2
        }
        "###);
    }

    #[test]
    fn convert_bail_break() {
        let s = test_basic_blocks(
            "lab: {
                123;
                break lab;
                456;
            }",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 123
            $1 = undefined
            exit = return $1
        }
        "###);
    }

    #[test]
    fn convert_bail_break_2() {
        let s = test_basic_blocks(
            "lab: if (123) {
                456;
                break lab;
                789;
            }",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 123
            exit = cond $0 ? @1..@1 : @2..@2
        }
        @1: {
            $1 = 456
            exit = jump @3
        }
        @2: {
            exit = jump @3
        }
        @3: {
            $2 = undefined
            exit = return $2
        }
        "###);
    }

    #[test]
    fn an_if() {
        let s = test_basic_blocks(
            "if (123) {
                456;
            } else {
                789;
            }",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 123
            exit = cond $0 ? @1..@1 : @2..@2
        }
        @1: {
            $1 = 456
            exit = jump @3
        }
        @2: {
            $2 = 789
            exit = jump @3
        }
        @3: {
            $3 = undefined
            exit = return $3
        }
        "###);
    }

    #[test]
    fn an_if_2() {
        let s = test_basic_blocks(
            "if (123) {
                if (456) {
                    789;
                }
            } else {
                999;
            }
        ",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 123
            exit = cond $0 ? @1..@4 : @5..@5
        }
        @1: {
            $1 = 456
            exit = cond $1 ? @2..@2 : @3..@3
        }
        @2: {
            $2 = 789
            exit = jump @4
        }
        @3: {
            exit = jump @4
        }
        @4: {
            exit = jump @6
        }
        @5: {
            $3 = 999
            exit = jump @6
        }
        @6: {
            $4 = undefined
            exit = return $4
        }
        "###);
    }

    #[test]
    fn a_try_catch() {
        let s = test_basic_blocks(
            "try {
                777
            } catch {
                888
            }",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            exit = try @1 catch @2 finally @3 after @3
        }
        @1: {
            $0 = 777
            exit = error ? jump @2 : jump @3
        }
        @2: {
            $1 = 888
            exit = finally @3 after @3
        }
        @3: {
            exit = end finally after @4
        }
        @4: {
            $2 = undefined
            exit = return $2
        }
        "###);
    }

    #[test]
    fn convert_pattern_catch() {
        let s = test_basic_blocks(
            "try {
                777
            } catch ({ message }) {
                return message
            }",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            exit = try @1 catch @2 finally @4 after @4
        }
        @1: {
            $0 = 777
            exit = error ? jump @2 : jump @4
        }
        @2: {
            $1 = caught_error()
            $2 = pack $1 {message: _}
            $3 = unpack $2[0]
            $4 = $3
            exit = return $4
        }
        @3: {
            exit = finally @4 after @4
        }
        @4: {
            exit = end finally after @5
        }
        @5: {
            $5 = undefined
            exit = return $5
        }
        "###);
    }

    #[test]
    fn try_catch_phi() {
        let s = test_basic_blocks(
            "try {
                var a = 777
            } catch {
                var a = 888
            }
            return a",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            exit = try @1 catch @2 finally @3 after @3
        }
        @1: {
            $0 = 777
            exit = error ? jump @2 : jump @3
        }
        @2: {
            $1 = 888
            exit = finally @3 after @3
        }
        @3: {
            $2 = either($0, $1)
            exit = end finally after @4
        }
        @4: {
            $3 = $2
            exit = return $3
        }
        "###);
    }

    #[test]
    fn a_trycatchfinally() {
        let s = test_basic_blocks(
            "try {
                777
            } catch {
                888
            } finally {
                999
            }",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            exit = try @1 catch @2 finally @3 after @3
        }
        @1: {
            $0 = 777
            exit = error ? jump @2 : jump @3
        }
        @2: {
            $1 = 888
            exit = finally @3 after @3
        }
        @3: {
            $2 = 999
            exit = end finally after @4
        }
        @4: {
            $3 = undefined
            exit = return $3
        }
        "###);
    }

    #[test]
    fn a_nested_trycatchfinally() {
        let s = test_basic_blocks(
            "var x = 10;
            try {
                if (x > 10) {
                    throw 123;
                }
            } catch (e) {
                try {
                    return 456;
                } catch (e2) {
                    return 789;
                }
            }
            return x;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 10
            exit = try @1 catch @5 finally @12 after @12
        }
        @1: {
            $1 = $0
            $2 = 10
            $3 = $1 > $2
            exit = cond $3 ? @2..@2 : @3..@3
        }
        @2: {
            $4 = 123
            exit = throw $4
        }
        @3: {
            exit = jump @4
        }
        @4: {
            exit = error ? jump @5 : jump @12
        }
        @5: {
            $5 = caught_error()
            exit = try @6 catch @8 finally @10 after @10
        }
        @6: {
            $6 = 456
            exit = return $6
        }
        @7: {
            exit = error ? jump @8 : jump @10
        }
        @8: {
            $7 = caught_error()
            $8 = 789
            exit = return $8
        }
        @9: {
            exit = finally @10 after @10
        }
        @10: {
            exit = end finally after @11
        }
        @11: {
            exit = finally @12 after @12
        }
        @12: {
            exit = end finally after @13
        }
        @13: {
            $9 = $0
            exit = return $9
        }
        "###);
    }

    #[test]
    fn convert_for_in() {
        let s = test_basic_blocks(
            "for (var x in {}) {
                x();
            }",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = {}
            exit = for in $0 @1..@1
        }
        @1: {
            $1 = for_in_of_value()
            $2 = $1
            $3 = call $2()
            exit = continue @1
        }
        @2: {
            $4 = undefined
            exit = return $4
        }
        "###);
    }

    #[test]
    fn an_object() {
        let s = test_basic_blocks(
            "var other = {}
            var obj = {
                key: 'val',
                ...other,
                other,
                [1000 + 2000]: 'computed',
                // 1000000000000000000000000000000: 'num',
                1000000000000000000000000000000n: 'bignum',
            }",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = {}
            $1 = "val"
            $2 = $0
            $3 = $0
            $4 = 1000
            $5 = 2000
            $6 = $4 + $5
            $7 = "computed"
            $8 = "bignum"
            $9 = {key: $1, ...$2, other: $3, [$6]: $7, 1000000000000000000000000000000: $8}
            $10 = undefined
            exit = return $10
        }
        "###);
    }

    #[test]
    fn convert_object_props() {
        let s = test_basic_blocks(
            "var obj = { '1': 1 }
            obj.prop = 2
            obj.prop
            obj[1]
            obj['prop']",
        );
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            $1 = {1: $0}
            $2 = 2
            $3 = $1.prop = $2
            $4 = $1.prop
            $5 = 1
            $6 = $1[$5]
            $7 = "prop"
            $8 = $1[$7]
            $9 = undefined
            exit = return $9
        }
        "###);
    }
}
