use swc_ecma_ast::{
    AwaitExpr, BinaryOp, Decl, Expr, ExprOrSpread, ForHead, ForInStmt, ForOfStmt, GetterProp,
    IfStmt, LabeledStmt, Lit, MethodProp, ObjectLit, PatOrExpr, Prop, PropName, PropOrSpread,
    SetterProp, Stmt, ThrowStmt, UnaryOp, UpdateExpr, UpdateOp, VarDecl, VarDeclKind,
    VarDeclOrExpr, YieldExpr,
};

use crate::basic_blocks::{
    ArrayElement, BreakableId, ExitType, ForInOfKind, IncrDecr, Instruction, LogicalCondKind,
    MethodKind, ObjectKey, ObjectProperty, ObjectValue, StructuredFlow, StructuredSwitchCase,
    TempExitType, LHS,
};

use super::{
    block_to_basic_blocks, class_to_basic_blocks, convert_object_propname,
    function_to_basic_blocks, get_propname_normal_key, pat_to_basic_blocks, to_basic_blocks_lhs,
    FromAstCtx, FunctionLike, NestedIntoStatement, PatType,
};

/// Turn a statement into basic blocks.
/// wraps `stat_to_basic_blocks_inner` while passing it the label, if what we got was a labeled statement
pub fn stat_to_basic_blocks(
    ctx: &mut FromAstCtx,
    stat: &Stmt,
) -> Result<Vec<StructuredFlow>, String> {
    let can_break_without_label = |stat: &Stmt| {
        matches!(
            stat,
            Stmt::While(_)
                | Stmt::DoWhile(_)
                | Stmt::For(_)
                | Stmt::ForIn(_)
                | Stmt::ForOf(_)
                | Stmt::Switch(_)
        )
    };

    let ret = if let Stmt::Labeled(LabeledStmt { label, body, .. }) = stat {
        let brk = ctx.push_label(NestedIntoStatement::Labelled(label.sym.to_string()));
        stat_to_basic_blocks_inner(ctx, brk, body)
    } else if can_break_without_label(stat) {
        let brk = ctx.push_label(NestedIntoStatement::Unlabelled);
        stat_to_basic_blocks_inner(ctx, brk, stat)
    } else {
        let ret = stat_to_basic_blocks_inner(ctx, BreakableId(None), stat);
        return ret; // not breakable
    };

    ctx.pop_label();

    ret
}

/// Turn a statement into basic blocks. Wrapped by `stat_to_basic_blocks` to handle labels.
fn stat_to_basic_blocks_inner(
    ctx: &mut FromAstCtx,
    brk_id: BreakableId,
    stat: &Stmt,
) -> Result<Vec<StructuredFlow>, String> {
    match stat {
        Stmt::Expr(expr) => {
            let (flow, _expr) = expr_to_basic_blocks(ctx, &expr.expr)?;
            return Ok(flow);
        }
        Stmt::Decl(Decl::Var(var)) => {
            return var_decl_to_basic_blocks(ctx, var);
        }
        Stmt::Decl(Decl::Fn(_)) => {
            unreachable!("function declarations should be handled by block_to_basic_blocks")
        }
        Stmt::Decl(Decl::Class(class)) => {
            let (structured_class, _var) = class_to_basic_blocks(
                ctx,
                class.class.as_ref(),
                Some(class.ident.sym.to_string()),
            )?;
            return Ok(structured_class);
        }
        // https://262.ecma-international.org/#sec-runtime-semantics-forin-div-ofbodyevaluation-lhs-stmt-iterator-lhskind-labelset
        Stmt::ForOf(ForOfStmt {
            left, right, body, ..
        })
        | Stmt::ForIn(ForInStmt {
            left, right, body, ..
        }) => {
            let (before_loop, looped_value) = expr_to_basic_blocks(ctx, &right)?;

            ctx.enter_conditional_branch();

            let mut loop_body = vec![];

            let (loop_value_flow, loop_value) = ctx.push_instruction(Instruction::ForInOfValue);
            loop_body.extend(loop_value_flow);

            match left {
                ForHead::VarDecl(var_decl) => {
                    assert_eq!(
                        var_decl.decls.len(),
                        1,
                        "for-in/of var decl have exactly one binding"
                    );
                    let only_decl = &var_decl.decls[0];

                    let kind = match var_decl.kind {
                        VarDeclKind::Var => PatType::VarDecl,
                        VarDeclKind::Let | VarDeclKind::Const => PatType::LetDecl,
                    };
                    let (flow, _pat_var) =
                        pat_to_basic_blocks(ctx, kind, &only_decl.name, loop_value)?;
                    loop_body.extend(flow);
                }
                ForHead::Pat(ref pat) => {
                    let (flow, _pat_var) =
                        pat_to_basic_blocks(ctx, PatType::Assign, pat, loop_value)?;
                    loop_body.extend(flow);
                }
                ForHead::UsingDecl(_) => todo!(),
            };

            let kind = match stat {
                Stmt::ForIn(_) => ForInOfKind::ForIn,
                Stmt::ForOf(st) if !st.is_await => ForInOfKind::ForOf,
                Stmt::ForOf(_) => ForInOfKind::ForAwaitOf,
                _ => unreachable!(),
            };

            let body = stat_to_basic_blocks_inner(ctx, BreakableId(None), body)?;
            loop_body.extend(body);

            let after_loop = ctx.leave_conditional_branch();

            return Ok(vec![
                StructuredFlow::Block(BreakableId(None), before_loop),
                StructuredFlow::ForInOfLoop(brk_id, looped_value, kind, loop_body),
                StructuredFlow::Block(BreakableId(None), after_loop),
            ]);
        }
        Stmt::For(for_loop) => {
            let before_loop = match &for_loop.init {
                Some(VarDeclOrExpr::VarDecl(decl)) => var_decl_to_basic_blocks(ctx, decl)?,
                Some(VarDeclOrExpr::Expr(expr)) => expr_to_basic_blocks(ctx, &expr)?.0,
                None => vec![],
            };

            ctx.enter_conditional_branch();

            let mut loop_body = vec![];

            let (test_flow, test) = if let Some(test) = &for_loop.test {
                expr_to_basic_blocks(ctx, test)?
            } else {
                ctx.push_instruction(Instruction::LitBool(true))
            };
            loop_body.extend(test_flow);

            let mut loop_inner_body = vec![];

            let flow = stat_to_basic_blocks(ctx, &for_loop.body)?;
            loop_inner_body.extend(flow);

            if let Some(update) = &for_loop.update {
                loop_inner_body.extend(expr_to_basic_blocks(ctx, update)?.0);
            }

            loop_body.push(StructuredFlow::Cond(
                BreakableId(None),
                test,
                loop_inner_body,
                vec![StructuredFlow::Break(brk_id)],
            ));

            let after_loop = ctx.leave_conditional_branch();

            return Ok(vec![
                StructuredFlow::from_vec(before_loop),
                StructuredFlow::Loop(brk_id, loop_body),
                StructuredFlow::from_vec(after_loop),
            ]);
        }
        Stmt::DoWhile(dowhil) => {
            ctx.enter_conditional_branch();

            let mut loop_body = vec![];

            let body = stat_to_basic_blocks(ctx, &dowhil.body)?;
            loop_body.extend(body);

            let (test_flow, test) = expr_to_basic_blocks(ctx, &dowhil.test)?;
            loop_body.extend(test_flow);

            loop_body.push(StructuredFlow::Cond(
                BreakableId(None),
                test,
                vec![],
                vec![StructuredFlow::Break(brk_id)],
            ));

            let after = ctx.leave_conditional_branch();

            return Ok(vec![
                StructuredFlow::Loop(brk_id, loop_body),
                StructuredFlow::from_vec(after),
            ]);
        }
        Stmt::While(whil) => {
            ctx.enter_conditional_branch();

            let mut loop_body = vec![];

            let (test_flow, test) = expr_to_basic_blocks(ctx, &whil.test)?;
            loop_body.extend(test_flow);

            let mut loop_inner_body = vec![];
            loop_inner_body.extend(stat_to_basic_blocks(ctx, &whil.body)?);

            loop_body.push(StructuredFlow::Cond(
                BreakableId(None),
                test,
                loop_inner_body,
                vec![StructuredFlow::Break(brk_id)],
            ));

            let outside = ctx.leave_conditional_branch();

            return Ok(vec![
                StructuredFlow::Loop(brk_id, loop_body),
                StructuredFlow::from_vec(outside),
            ]);
        }
        Stmt::If(IfStmt {
            test, cons, alt, ..
        }) => {
            // IF($test)
            let (test_flow, test) = expr_to_basic_blocks(ctx, &test)?;

            ctx.enter_conditional_branch();

            // THEN
            let then_flow = stat_to_basic_blocks(ctx, &cons)?;

            // ELSE
            let else_flow = if let Some(alt) = alt {
                stat_to_basic_blocks(ctx, &alt)?
            } else {
                vec![Default::default()]
            };

            let after_flow = ctx.leave_conditional_branch();

            let cond = StructuredFlow::Cond(brk_id, test, then_flow, else_flow);

            return Ok(vec![
                StructuredFlow::from_vec(test_flow),
                cond,
                StructuredFlow::from_vec(after_flow),
            ]);
        }
        Stmt::Block(block) => {
            let contents = block_to_basic_blocks(ctx, &block.stmts)?;
            return Ok(vec![StructuredFlow::Block(brk_id, contents)]);
        }
        Stmt::Break(br) => {
            if brk_id != BreakableId(None) {
                todo!(
                    "illegal statement with label {:?} in non-breakable context",
                    brk_id
                )
            }

            let brk_id = ctx.label_to_break_id(&br.label);
            return Ok(vec![StructuredFlow::Break(brk_id)]);
        }
        Stmt::Continue(cont) => {
            let cont_id = ctx.label_to_break_id(&cont.label);
            return Ok(vec![StructuredFlow::Continue(cont_id)]);
        }
        Stmt::Labeled(_) => unreachable!("label is handled in stat_to_basic_blocks"),
        Stmt::Debugger(_) => Ok(vec![StructuredFlow::Debugger]),
        Stmt::With(_) => todo!("with statement is unsupported"),
        Stmt::Switch(switch) => {
            let (before_switch, switch_exp) = expr_to_basic_blocks(ctx, &switch.discriminant)?;

            ctx.enter_conditional_branch();

            let mut switch_cases = vec![];
            for case in switch.cases.iter() {
                let condition = if let Some(test) = &case.test {
                    let (cond_flow, cond) = expr_to_basic_blocks(ctx, test)?;

                    Some((cond_flow, cond))
                } else {
                    None
                };

                let body = block_to_basic_blocks(ctx, &case.cons)?;

                switch_cases.push(StructuredSwitchCase { condition, body });
            }

            let after_switch = ctx.leave_conditional_branch();

            return Ok(vec![
                StructuredFlow::from_vec(before_switch),
                StructuredFlow::Switch(brk_id, switch_exp, switch_cases),
                StructuredFlow::from_vec(after_switch),
            ]);
        }
        Stmt::Throw(ThrowStmt { arg, .. }) => {
            let (throw_flow, throw_val) = expr_to_basic_blocks(ctx, arg)?;

            return Ok(vec![
                StructuredFlow::Block(BreakableId(None), throw_flow),
                StructuredFlow::Return(ExitType::Throw, throw_val),
            ]);
        }
        Stmt::Return(ret) => {
            let (expr_flow, expr) = expr_to_basic_blocks(ctx, ret.arg.as_ref().unwrap())?;

            return Ok(vec![
                StructuredFlow::from_vec(expr_flow),
                StructuredFlow::Return(ExitType::Return, expr),
            ]);
        }
        Stmt::Try(ref stmt) => {
            ctx.enter_conditional_branch();

            let try_flow = block_to_basic_blocks(ctx, &stmt.block.stmts)?;

            let mut catch_flow = vec![];

            if let Some(ref handler) = stmt.handler {
                if let Some(p) = &handler.param {
                    let (err_flow, catcherr) = ctx.push_instruction(Instruction::CaughtError);
                    catch_flow.extend(err_flow);

                    let (pat_flow, _pat) = pat_to_basic_blocks(ctx, PatType::VarDecl, p, catcherr)?;
                    catch_flow.extend(pat_flow);
                }

                catch_flow.extend(block_to_basic_blocks(ctx, &handler.body.stmts)?);
            }

            let mut finally_flow = vec![];
            finally_flow.extend(ctx.leave_conditional_branch());

            if let Some(ref finalizer) = stmt.finalizer {
                finally_flow.extend(block_to_basic_blocks(ctx, &finalizer.stmts)?);
            };

            return Ok(vec![StructuredFlow::TryCatch(
                brk_id,
                try_flow,
                catch_flow,
                finally_flow,
            )]);
        }
        Stmt::Empty(_) => Ok(vec![]),
        Stmt::Decl(Decl::Using(_)) => unreachable!("using decl"),
        Stmt::Decl(
            Decl::TsInterface(_) | Decl::TsTypeAlias(_) | Decl::TsEnum(_) | Decl::TsModule(_),
        ) => unreachable!("typescript features"),
    }
}

fn var_decl_to_basic_blocks(
    ctx: &mut FromAstCtx,
    var: &VarDecl,
) -> Result<Vec<StructuredFlow>, String> {
    let mut var_flow = vec![];

    for decl in &var.decls {
        let (flow, init) = match decl.init.as_ref() {
            Some(init) => expr_to_basic_blocks(ctx, &init.as_ref())?,
            None => ctx.push_instruction(Instruction::Undefined),
        };
        var_flow.extend(flow);
        let kind = match var.kind {
            VarDeclKind::Const | VarDeclKind::Let => PatType::LetDecl,
            VarDeclKind::Var => PatType::VarDecl,
        };
        let (flow, _) = pat_to_basic_blocks(ctx, kind, &decl.name, init)?;
        var_flow.extend(flow);
    }

    Ok(var_flow)
}

pub fn expr_to_basic_blocks(
    ctx: &mut FromAstCtx,
    exp: &Expr,
) -> Result<(Vec<StructuredFlow>, usize), String> {
    match exp {
        Expr::Lit(lit) => {
            let lit = match lit {
                Lit::Null(_) => Instruction::Null,
                Lit::Bool(b) => Instruction::LitBool(b.value),
                Lit::Num(num) => Instruction::LitNumber(num.value),
                Lit::BigInt(num) => Instruction::LitBigInt(num.value.as_ref().clone()),
                Lit::Str(s) => Instruction::LitString(s.value.to_string()),
                Lit::Regex(re) => Instruction::LitRegExp(re.exp.to_string(), re.flags.to_string()),
                Lit::JSXText(_) => todo!(),
            };
            Ok(ctx.push_instruction(lit))
        }
        Expr::Bin(bin) => {
            let mut bin_flow = vec![];

            match (&bin.left.as_ref(), &bin.op) {
                (_, BinaryOp::LogicalAnd | BinaryOp::LogicalOr | BinaryOp::NullishCoalescing) => {
                    let kind = match bin.op {
                        BinaryOp::LogicalAnd => LogicalCondKind::And,
                        BinaryOp::LogicalOr => LogicalCondKind::Or,
                        BinaryOp::NullishCoalescing => LogicalCondKind::NullishCoalescing,
                        _ => unreachable!(),
                    };

                    let (left, l) = expr_to_basic_blocks(ctx, &bin.left)?;

                    ctx.enter_conditional_branch();

                    let (right, r) = expr_to_basic_blocks(ctx, &bin.right)?;

                    bin_flow.push(StructuredFlow::LogicalCond(kind, left, l, right, r));

                    let (flow, phi) = ctx.push_instruction(Instruction::Phi(vec![l, r]));
                    bin_flow.extend(flow);

                    let after_cond = ctx.leave_conditional_branch();
                    bin_flow.extend(after_cond);

                    Ok((bin_flow, phi))
                }
                (Expr::PrivateName(priv_name), BinaryOp::In) => {
                    // #private in x

                    let (flow, r) = expr_to_basic_blocks(ctx, &bin.right)?;
                    bin_flow.extend(flow);

                    let (flow, private_in) = ctx
                        .push_instruction(Instruction::PrivateIn(priv_name.id.sym.to_string(), r));
                    bin_flow.extend(flow);

                    Ok((bin_flow, private_in))
                }
                (_, BinaryOp::In | BinaryOp::InstanceOf) => {
                    todo!("in/instanceof")
                }
                _ => {
                    let (left_flow, l) = expr_to_basic_blocks(ctx, &bin.left)?;
                    bin_flow.extend(left_flow);

                    let (flow, r) = expr_to_basic_blocks(ctx, &bin.right)?;
                    bin_flow.extend(flow);

                    let (flow, op) = ctx.push_instruction(Instruction::BinOp(bin.op.clone(), l, r));
                    bin_flow.extend(flow);

                    Ok((bin_flow, op))
                }
            }
        }
        Expr::Assign(assign) => match &assign.left {
            PatOrExpr::Pat(pat) => {
                let mut ret_flow = vec![];

                let (flow, init) = expr_to_basic_blocks(ctx, &assign.right)?;
                ret_flow.extend(flow);

                let (flow, ret) = pat_to_basic_blocks(ctx, PatType::Assign, pat, init)?;
                ret_flow.extend(flow);

                Ok((ret_flow, ret))
            }
            _ => todo!(),
        },
        Expr::Paren(paren) => expr_to_basic_blocks(ctx, &paren.expr),
        Expr::Seq(seq) => {
            let mut seq_flow = vec![];

            let mut last = None;
            for expr in &seq.exprs {
                let (flow, exp) = expr_to_basic_blocks(ctx, expr)?;
                seq_flow.extend(flow);

                last = Some(exp);
            }

            Ok((seq_flow, last.expect("Seq must have 1+ exprs")))
        }
        Expr::Cond(cond_expr) => {
            let mut cond_flow = vec![];

            let (flow, test) = expr_to_basic_blocks(ctx, &cond_expr.test)?;
            cond_flow.extend(flow);

            ctx.enter_conditional_branch();

            let (cons_flow, cons) = expr_to_basic_blocks(ctx, &cond_expr.cons)?;
            let (alt_flow, alt) = expr_to_basic_blocks(ctx, &cond_expr.alt)?;

            cond_flow.push(StructuredFlow::Cond(
                BreakableId(None),
                test,
                cons_flow,
                alt_flow,
            ));

            let after_flow = ctx.leave_conditional_branch();
            cond_flow.extend(after_flow);

            // the retval of our ternary is a phi node
            let (flow, phi) = ctx.push_instruction(Instruction::Phi(vec![cons, alt]));
            cond_flow.extend(flow);

            Ok((cond_flow, phi))
        }
        Expr::Ident(ident) => {
            let mut ident_flow = vec![];

            let ident = ident.sym.to_string();
            let (flow, instruction) = match ident.as_str() {
                "undefined" => (vec![], Instruction::Undefined),
                "Infinity" => (vec![], Instruction::LitNumber(f64::INFINITY)),
                ident => {
                    if ctx.is_global_name(ident) {
                        (vec![], Instruction::Read(LHS::Global(ident.to_string())))
                    } else if let Some(nonloc) = ctx.is_nonlocal(ident) {
                        (vec![], Instruction::Read(LHS::NonLocal(nonloc)))
                    } else {
                        let (flow, ident) = ctx.read_name(ident);

                        (flow, Instruction::Ref(ident))
                    }
                }
            };
            ident_flow.extend(flow);

            // todo!("can I make the code coming out shorter?");

            let (flow, ident_var) = ctx.push_instruction(instruction);
            ident_flow.extend(flow);

            Ok((ident_flow, ident_var))
        }
        Expr::This(_) => Ok(ctx.push_instruction(Instruction::This)),
        Expr::Array(array_lit) => {
            let mut array_flow = vec![];
            let mut elements = Vec::with_capacity(array_lit.elems.len());

            for elem in &array_lit.elems {
                let elem = match elem {
                    Some(ExprOrSpread { spread, expr }) => {
                        let (flow, expr) = expr_to_basic_blocks(ctx, expr)?;
                        array_flow.extend(flow);

                        match spread {
                            None => ArrayElement::Item(expr),
                            Some(_) => ArrayElement::Spread(expr),
                        }
                    }
                    None => ArrayElement::Hole,
                };

                elements.push(elem);
            }

            let (flow, array_var) = ctx.push_instruction(Instruction::Array(elements));
            array_flow.extend(flow);

            Ok((array_flow, array_var))
        }
        Expr::Object(ObjectLit { props, .. }) => {
            let mut object_flow = vec![];

            let mut kvs = vec![];
            let mut proto = None;
            for prop in props {
                match prop {
                    PropOrSpread::Spread(spread) => {
                        let (flow, spread) = expr_to_basic_blocks(ctx, &spread.expr)?;
                        object_flow.extend(flow);

                        kvs.push(ObjectProperty::Spread(spread))
                    }
                    PropOrSpread::Prop(prop) => match prop.as_ref() {
                        Prop::Shorthand(ident) => {
                            let (flow, value) =
                                expr_to_basic_blocks(ctx, &Expr::Ident(ident.clone()))?;
                            object_flow.extend(flow);

                            kvs.push(ObjectProperty::KeyValue(
                                ObjectKey::NormalKey(ident.sym.to_string()),
                                ObjectValue::Property(value),
                            ));
                        }
                        Prop::KeyValue(kv) => match &kv.key {
                            PropName::Computed(expr) => {
                                let (flow, expr) = expr_to_basic_blocks(ctx, &expr.expr)?;
                                object_flow.extend(flow);

                                let (flow, value) = expr_to_basic_blocks(ctx, &kv.value)?;
                                object_flow.extend(flow);

                                kvs.push(ObjectProperty::KeyValue(
                                    ObjectKey::Computed(expr),
                                    ObjectValue::Property(value),
                                ))
                            }
                            _ => {
                                let prop_name = get_propname_normal_key(&kv.key);
                                if &prop_name == "__proto__" {
                                    let (flow, proto_val) = expr_to_basic_blocks(ctx, &kv.value)?;
                                    object_flow.extend(flow);

                                    proto = Some(proto_val);
                                } else {
                                    let (flow, value) = expr_to_basic_blocks(ctx, &kv.value)?;
                                    object_flow.extend(flow);

                                    kvs.push(ObjectProperty::KeyValue(
                                        ObjectKey::NormalKey(prop_name),
                                        ObjectValue::Property(value),
                                    ));
                                }
                            }
                        },
                        Prop::Getter(GetterProp { key, .. })
                        | Prop::Setter(SetterProp { key, .. })
                        | Prop::Method(MethodProp { key, .. }) => {
                            let (flow, key) = convert_object_propname(ctx, key)?;
                            object_flow.extend(flow);

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

                            let (flow, _, fn_id) = function_to_basic_blocks(ctx, func, None)?;
                            object_flow.extend(flow);

                            kvs.push(ObjectProperty::KeyValue(
                                key,
                                ObjectValue::Method(method_kind, fn_id),
                            ));
                        }
                        Prop::Assign(_) => unreachable!(),
                    },
                }
            }

            let (flow, object_var) = ctx.push_instruction(Instruction::Object(proto, kvs));
            object_flow.extend(flow);

            Ok((object_flow, object_var))
        }
        Expr::Unary(unary_expr) => match unary_expr.op {
            UnaryOp::TypeOf => {
                let mut typeof_flow = vec![];

                if let Expr::Ident(global_ident) = &unary_expr.arg.as_ref() {
                    let s = global_ident.sym.to_string();
                    if ctx.is_global_name(&s) {
                        return Ok(ctx.push_instruction(Instruction::TypeOfGlobal(s)));
                    }
                }
                let (flow, expr) = expr_to_basic_blocks(ctx, &unary_expr.arg)?;
                typeof_flow.extend(flow);

                let (flow, type_of) = ctx.push_instruction(Instruction::TypeOf(expr));
                typeof_flow.extend(flow);

                Ok((typeof_flow, type_of))
            }
            UnaryOp::Delete => {
                let mut delete_flow = vec![];

                let (flow, lhs) = to_basic_blocks_lhs(ctx, &unary_expr.arg)?;
                delete_flow.extend(flow);

                let (flow, del) = ctx.push_instruction(Instruction::Delete(lhs));
                delete_flow.extend(flow);

                Ok((delete_flow, del))
            }
            UnaryOp::Void => {
                let mut void_flow = vec![];

                let (flow, _) = expr_to_basic_blocks(ctx, &unary_expr.arg)?;
                void_flow.extend(flow);

                let (flow, undef) = ctx.push_instruction(Instruction::Undefined);
                void_flow.extend(flow);

                Ok((void_flow, undef))
            }
            UnaryOp::Minus | UnaryOp::Plus | UnaryOp::Bang | UnaryOp::Tilde => {
                let mut unary_flow = vec![];

                let (flow, expr) = expr_to_basic_blocks(ctx, &unary_expr.arg)?;
                unary_flow.extend(flow);

                let (flow, res) = ctx.push_instruction(Instruction::UnaryOp(unary_expr.op, expr));
                unary_flow.extend(flow);

                Ok((unary_flow, res))
            }
        },
        Expr::Update(UpdateExpr {
            op, prefix, arg, ..
        }) => {
            let mut update_flow = vec![];

            let (flow, lhs) = to_basic_blocks_lhs(ctx, arg)?;
            update_flow.extend(flow);

            let op = match op {
                UpdateOp::PlusPlus => IncrDecr::Incr,
                UpdateOp::MinusMinus => IncrDecr::Decr,
            };
            let ins = match *prefix {
                true => Instruction::IncrDecr(lhs, op),
                false => Instruction::IncrDecrPostfix(lhs, op),
            };

            let (flow, ins) = ctx.push_instruction(ins);
            update_flow.extend(flow);

            Ok((update_flow, ins))
        }
        Expr::Member(_) => {
            let mut member_flow = vec![];

            let (flow, lhs) = to_basic_blocks_lhs(ctx, exp)?;
            member_flow.extend(flow);

            let (flow, read) = ctx.push_instruction(Instruction::Read(lhs));
            member_flow.extend(flow);

            Ok((member_flow, read))
        }
        Expr::SuperProp(sp) => {
            let mut super_prop_flow = vec![];

            let (flow, sup) = ctx.push_instruction(Instruction::Super);
            super_prop_flow.extend(flow);

            let lhs = match &sp.prop {
                swc_ecma_ast::SuperProp::Ident(ident) => {
                    let prop = ident.sym.to_string();
                    LHS::Member(Box::new(LHS::Local(sup)), ObjectKey::NormalKey(prop))
                }
                swc_ecma_ast::SuperProp::Computed(computed) => {
                    let (flow, expr) = expr_to_basic_blocks(ctx, &computed.expr)?;
                    super_prop_flow.extend(flow);

                    LHS::Member(Box::new(LHS::Local(expr)), ObjectKey::Computed(expr))
                }
            };

            let (flow, read) = ctx.push_instruction(Instruction::Read(lhs));
            super_prop_flow.extend(flow);

            Ok((super_prop_flow, read))
        }
        Expr::Arrow(arrow_expr) => {
            let (flow, varname, _fn_id) =
                function_to_basic_blocks(ctx, FunctionLike::ArrowExpr(arrow_expr), None)
                    .expect("todo error handling");
            Ok((flow, varname))
        }
        Expr::Fn(fn_expr) => {
            let (flow, varname, _fn_id) =
                function_to_basic_blocks(ctx, FunctionLike::FnExpr(fn_expr), None)
                    .expect("todo error handling");

            Ok((flow, varname))
        }
        Expr::Call(call) => {
            let mut call_flow = vec![];

            // TODO non-expr callees (super, import)
            let callee = match &call.callee {
                swc_ecma_ast::Callee::Super(_) => {
                    let (flow, callee) = ctx.push_instruction(Instruction::Super);
                    call_flow.extend(flow);

                    callee
                }
                swc_ecma_ast::Callee::Import(_) => todo!("import()"),
                swc_ecma_ast::Callee::Expr(expr) => {
                    let (flow, callee) = expr_to_basic_blocks(ctx, &expr)?;
                    call_flow.extend(flow);

                    callee
                }
            };

            let mut args = Vec::with_capacity(call.args.len());
            for arg in &call.args {
                match arg.spread {
                    Some(_) => todo!("spread args"),
                    None => {
                        let (arg_flow, arg_expr) = expr_to_basic_blocks(ctx, &arg.expr)?;
                        call_flow.extend(arg_flow);

                        args.push(arg_expr)
                    }
                }
            }

            let (flow, call) = ctx.push_instruction(Instruction::Call(callee, args));
            call_flow.extend(flow);

            return Ok((call_flow, call));
        }
        Expr::New(new_expr) => {
            let mut new_flow = vec![];

            let (flow, callee) = expr_to_basic_blocks(ctx, &new_expr.callee)?;
            new_flow.extend(flow);

            let mut out_args = vec![];
            if let Some(args) = &new_expr.args {
                for arg in args {
                    match arg.spread {
                        Some(_) => todo!("spread args"),
                        None => {
                            let (flow, callee) = expr_to_basic_blocks(ctx, arg.expr.as_ref())?;
                            new_flow.extend(flow);

                            out_args.push(callee);
                        }
                    }
                }
            }

            let (flow, new) = ctx.push_instruction(Instruction::New(callee, out_args));
            new_flow.extend(flow);

            Ok((new_flow, new))
        }
        Expr::Tpl(_) => todo!(),
        Expr::TaggedTpl(_) => todo!(),
        Expr::Class(class) => {
            let class_name = class.ident.as_ref().map(|id| id.sym.to_string());
            class_to_basic_blocks(ctx, class.class.as_ref(), class_name)
        }
        Expr::MetaProp(_) => todo!(),
        Expr::Yield(YieldExpr { arg, delegate, .. }) => {
            let mut yield_flow = vec![];

            let typ = match *delegate {
                true => TempExitType::YieldStar,
                false => TempExitType::Yield,
            };

            let (flow, arg) = match arg {
                Some(arg) => expr_to_basic_blocks(ctx, arg)?,
                None => ctx.push_instruction(Instruction::Undefined),
            };
            yield_flow.extend(flow);

            let (flow, exit) = ctx.push_instruction(Instruction::TempExit(typ, arg));
            yield_flow.extend(flow);

            Ok((yield_flow, exit))
        }
        Expr::Await(AwaitExpr { arg, .. }) => {
            let mut await_flow = vec![];

            let (flow, arg) = expr_to_basic_blocks(ctx, arg)?;
            await_flow.extend(flow);

            let (flow, exit) =
                ctx.push_instruction(Instruction::TempExit(TempExitType::Await, arg));
            await_flow.extend(flow);

            Ok((await_flow, exit))
        }
        Expr::OptChain(_) => todo!(),
        Expr::PrivateName(_) => {
            unreachable!("Expr::PrivateName is only valid in `#private in` expression")
        }
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
    }
}

#[cfg(test)]
mod tests {
    use crate::testutils::*;

    #[test]
    fn simple_add() {
        let s = test_basic_blocks_expr("10 + 20 + 30;");
        insta::assert_debug_snapshot!(s, @r###"
        function():
        $0 = 10
        $1 = 20
        $2 = $0 + $1
        $3 = 30
        $4 = $2 + $3
        "###);
    }

    #[test]
    fn an_array() {
        let s = test_basic_blocks("var x = [1, 2, , ...3];");
        insta::assert_debug_snapshot!(s, @r###"
        {
            $0 = 1
            $1 = 2
            $2 = 3
            $3 = [$0, $1, , ...$2]
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
            Return $4
        }
        "###);
    }
    */

    #[test]
    fn simple_await() {
        let s = test_basic_blocks("await (await 1)");
        insta::assert_debug_snapshot!(s, @r###"
        {
            $0 = 1
            $1 = Await $0
            $2 = Await $1
        }
        "###);
    }

    #[test]
    fn simple_add_2() {
        let s = test_basic_blocks(
            "var a = 10;
            var b = 20;
            var c = a + b + 30;",
        );
        insta::assert_debug_snapshot!(s, @r###"
        {
            $0 = 10
            $1 = 20
            $2 = $0
            $3 = $1
            $4 = $2 + $3
            $5 = 30
            $6 = $4 + $5
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
        {
            $0 = 10
            $1 = typeof global "globalVar"
            $2 = $0
            $3 = typeof $2
            $4 = $0
            $5 = undefined
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
        {
            $0 = global "readGlobal"
            $1 = 1
            $2 = global "writeGlobal" = $1
            $3 = $1
            $4 = globalThis.readGlobalProp.prop
            $5 = 1
            $6 = globalThis.writeGlobalProp.prop = $5
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
        {
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
        }
        "###);

        let s = test_basic_blocks(
            "var a = 100;
            use(a++);
            use(--a);",
        );
        insta::assert_debug_snapshot!(s, @r###"
        {
            $0 = 100
            $1 = global "use"
            $2 = $0++
            $3 = call $1($2)
            $4 = global "use"
            $5 = --$0
            $6 = call $4($5)
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
        {
            $0 = 100
            $1 = global "cond"
            if ($1) {
                $2 = $0++
            } else {
                $3 = --$0
                $4 = --$0
            }
            $5 = either($2, $4)
        }
        "###);
    }

    #[test]
    fn simple_cond() {
        let s = test_basic_blocks_expr("1 ? 10 : 20;");
        insta::assert_debug_snapshot!(s, @r###"
        function():
        $0 = 1
        if ($0) {
            $1 = 10
        } else {
            $2 = 20
        }
        $3 = either($1, $2)
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
        {
            $0 = 999
            $1 = 1
            if ($1) {
                $2 = 2
                $3 = $2
            } else {
                $4 = 3
            }
            $5 = either($0, $2)
            $6 = either($3, $4)
            $7 = $5
            Return $7
        }
        "###);
    }

    #[test]
    fn cond_nested() {
        let s = test_basic_blocks_expr("1 ? (2 ? 10 : 15) : 20;");
        insta::assert_debug_snapshot!(s, @r###"
        function():
        $0 = 1
        if ($0) {
            $1 = 2
            if ($1) {
                $2 = 10
            } else {
                $3 = 15
            }
            $4 = either($2, $3)
        } else {
            $5 = 20
        }
        $6 = either($4, $5)
        "###);
    }

    #[test]
    fn simple_vars() {
        let s = test_basic_blocks("var x = 1; x + 2");
        insta::assert_debug_snapshot!(s, @r###"
        {
            $0 = 1
            $1 = $0
            $2 = 2
            $3 = $1 + $2
        }
        "###);
    }

    #[test]
    fn cond_nested_reassign() {
        let s = test_basic_blocks("var x = 1; 123 ? (x = 2, 1) : x = 3; x + 2");
        insta::assert_debug_snapshot!(s, @r###"
        {
            $0 = 1
            $1 = 123
            if ($1) {
                $2 = 2
                $3 = $2
                $4 = 1
            } else {
                $5 = 3
                $6 = $5
            }
            $7 = either($0, $2, $5)
            $8 = either($4, $6)
            $9 = $7
            $10 = 2
            $11 = $9 + $10
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
        {
            $0 = 1
            $1 = 123
            if ($1) {
                $2 = 1234
                $3 = $2
                if ($3) {
                    $4 = 567
                    $5 = $4
                } else {
                    $6 = 890
                }
                $7 = either($0, $2, $4)
                $8 = either($5, $6)
                $9 = 1
            } else {
                $10 = 3
                $11 = $10
            }
            $12 = either($0, $2, $7, $10)
            $13 = either($9, $11)
            $14 = $12
            $15 = 2
            $16 = $14 + $15
        }
        "###);
    }

    #[test]
    fn a_switch() {
        let s = test_basic_blocks("switch (10) { case 20: 30; default: 40; break; case 50: 60; }");
        insta::assert_debug_snapshot!(s, @r###"
        {
            $0 = 10
            switch (@0) ($0) {
            {
                $1 = 20
            }
            case ($1) 
            {
                $2 = 30
            }
            default: 
            {
                $3 = 40
                Break (@0)
            }
            {
                $4 = 50
            }
            case ($4) 
            {
                $5 = 60
            }
            }
        }
        "###);
    }

    #[test]
    fn a_loop() {
        let s = test_basic_blocks("123; while (123) { 456; }");
        insta::assert_debug_snapshot!(s, @r###"
        {
            $0 = 123
            loop (@0) {
                $1 = 123
                if ($1) {
                    $2 = 456
                } else {
                    Break (@0)
                }
            }
        }
        "###);
    }

    #[test]
    fn a_loop_break() {
        let s = test_basic_blocks("while (123) { break }");
        insta::assert_debug_snapshot!(s, @r###"
        loop (@0) {
            $0 = 123
            {
                Break (@0)
            }
        }
        "###);
    }

    #[test]
    fn a_loop_continue() {
        let s = test_basic_blocks("while (123) { if (456) continue; }");
        insta::assert_debug_snapshot!(s, @r###"
        loop (@0) {
            $0 = 123
            if ($0) {
                $1 = 456
                if ($1) {
                    Continue (@0)
                } else {
                }
            } else {
                Break (@0)
            }
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
        {
            $0 = 1
            loop (@0) {
                $1 = either($0, $15)
                $2 = 111
                if ($2) {
                    $3 = $1
                    $4 = 1000
                    $5 = $3 + $4
                    $6 = $5
                    loop (@1) {
                        $7 = either($5, $13)
                        $8 = 222
                        if ($8) {
                            $9 = $7
                            $10 = 2000
                            $11 = $9 + $10
                            $12 = $11
                            Break (@0)
                        } else {
                            Break (@1)
                        }
                        $13 = either($0, $1, $5, $7, $11)
                    }
                    $14 = either($0, $1, $5, $7, $13)
                } else {
                    Break (@0)
                }
                $15 = either($0, $1, $5, $14)
            }
            $16 = either($0, $1, $15)
            $17 = $16
            Return $17
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
        loop (@0) {
            $0 = 123
            $1 = 456
            if ($1) {
            } else {
                Break (@0)
            }
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
        {
            $0 = 123
            loop (@0) {
                $1 = either($0, $10)
                $2 = $1
                $3 = 456
                $4 = $2 < $3
                if ($4) {
                    $5 = 789
                    $6 = $1
                    $7 = 1
                    $8 = $6 + $7
                    $9 = $8
                } else {
                    Break (@0)
                }
                $10 = either($0, $1, $8)
            }
            $11 = either($0, $1, $10)
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
        {
            $0 = 1
            $1 = 111
            if ($1) {
                $2 = $0
                $3 = 1000
                $4 = $2 + $3
                $5 = $4
                $6 = 222
                if ($6) {
                    $7 = $4
                    $8 = 2000
                    $9 = $7 + $8
                    $10 = $9
                } else {
                }
                $11 = either($0, $4, $9)
            } else {
            }
            $12 = either($0, $4, $11)
            $13 = $12
            Return $13
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
        {
            $0 = 1
            $1 = $0
            $2 = 1
            $3 = $1 == $2
            if ($3) {
                $4 = $0
                $5 = 1
                $6 = $4 == $5
                if ($6) {
                    $7 = $0
                    $8 = 2000
                    $9 = $7 + $8
                    $10 = $9
                } else {
                    $11 = 3
                    $12 = $11
                }
                $13 = either($0, $9, $11)
                $14 = $13
                $15 = 1000
                $16 = $14 + $15
                $17 = $16
            } else {
                $18 = 3
                $19 = $18
            }
            $20 = either($0, $13, $16, $18)
            $21 = $20
            Return $21
        }
        "###);
    }

    #[test]
    fn a_logical_operator() {
        let s = test_basic_blocks("var x = 1 && 2; return x;");
        insta::assert_debug_snapshot!(s, @r###"
        {
            ({
                $0 = 1
            }, $0) && ({
                $1 = 2
            }, $1)
            $2 = either($0, $1)
            $3 = $2
            Return $3
        }
        "###);

        let s = test_basic_blocks("var x = y() || z(); return x;");
        insta::assert_debug_snapshot!(s, @r###"
        {
            ({
                $0 = global "y"
                $1 = call $0()
            }, $1) || ({
                $2 = global "z"
                $3 = call $2()
            }, $3)
            $4 = either($1, $3)
            $5 = $4
            Return $5
        }
        "###);
    }

    #[test]
    fn a_labelled_break() {
        let s = test_basic_blocks("outer: while (123) { while (456) { break outer } }");
        insta::assert_debug_snapshot!(s, @r###"
        loop (@0) {
            $0 = 123
            if ($0) {
                loop (@1) {
                    $1 = 456
                    if ($1) {
                        Break (@0)
                    } else {
                        Break (@1)
                    }
                }
            } else {
                Break (@0)
            }
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
        {
            $0 = 123
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
        {
            $0 = 123
            if ($0) {
                $1 = 456
            } else {
            }
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
        {
            $0 = 123
            if ($0) {
                $1 = 456
            } else {
                $2 = 789
            }
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
        {
            $0 = 123
            if ($0) {
                $1 = 456
                if ($1) {
                    $2 = 789
                } else {
                }
            } else {
                $3 = 999
            }
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
        try {
            $0 = 777
        } catch {
            $1 = 888
        } finally {
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
        try {
            $0 = 777
        } catch {
            $1 = caught_error()
            $2 = pack $1 {message: _}
            $3 = unpack $2[0]
            $4 = $3
            Return $4
        } finally {
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
        {
            try {
                $0 = 777
            } catch {
                $1 = 888
            } finally {
                $2 = either($0, $1)
            }
            $3 = $2
            Return $3
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
        try {
            $0 = 777
        } catch {
            $1 = 888
        } finally {
            $2 = 999
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
        {
            $0 = 10
            try {
                $1 = $0
                $2 = 10
                $3 = $1 > $2
                if ($3) {
                    $4 = 123
                    Throw $4
                } else {
                }
            } catch {
                $5 = caught_error()
                try {
                    $6 = 456
                    Return $6
                } catch {
                    $7 = caught_error()
                    $8 = 789
                    Return $8
                } finally {
                }
            } finally {
            }
            $9 = $0
            Return $9
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
        {
            $0 = {}
            for in ($0) {
                $1 = for_in_of_value()
                {
                    $2 = $1
                    $3 = call $2()
                }
            }
        }
        "###);
    }

    #[test]
    fn convert_for_of() {
        let s = test_basic_blocks(
            "for (const x of [1]) {
                x();
            }",
        );
        insta::assert_debug_snapshot!(s, @r###"
        {
            $0 = 1
            $1 = [$0]
            for of ($1) {
                $2 = for_in_of_value()
                {
                    $3 = $2
                    $4 = call $3()
                }
            }
        }
        "###);
    }

    #[test]
    fn convert_for_of_cursed() {
        let s = test_basic_blocks(
            "var myVar
            for (myVar of [1]) {
                x();
            }",
        );
        insta::assert_debug_snapshot!(s, @r###"
        {
            $0 = undefined
            $1 = 1
            $2 = [$1]
            for of ($2) {
                $3 = either($0, $4)
                $4 = for_in_of_value()
                $5 = $4
                {
                    $6 = global "x"
                    $7 = call $6()
                }
            }
            $8 = either($0, $3, $4)
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
        {
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
        {
            $0 = 1
            $1 = {1: $0}
            $2 = 2
            $3 = $1.prop = $2
            $4 = $1.prop
            $5 = 1
            $6 = $1[$5]
            $7 = "prop"
            $8 = $1[$7]
        }
        "###);
    }
}
