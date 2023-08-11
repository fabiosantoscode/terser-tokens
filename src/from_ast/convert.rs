use std::borrow::Borrow;

use swc_ecma_ast::{
    AwaitExpr, Decl, Expr, ExprOrSpread, IfStmt, LabeledStmt, Lit, Pat, PatOrExpr, Stmt, ThrowStmt,
    YieldExpr,
};

use crate::basic_blocks::{
    ArrayElement, BasicBlockExit, BasicBlockInstruction, ExitType, TempExitType,
};

use super::{function_to_basic_blocks, FromAstCtx, FunctionLike, NestedIntoStatement};

/// Turn a statement into basic blocks.
/// wraps `stat_to_basic_blocks_inner` while passing it the label, if what we got was a labeled statement
fn stat_to_basic_blocks(ctx: &mut FromAstCtx, stat: &Stmt) {
    let is_loop = |stat: &Stmt| match stat {
        Stmt::While(_) | Stmt::DoWhile(_) | Stmt::For(_) | Stmt::ForIn(_) | Stmt::ForOf(_) => true,
        _ => false,
    };

    let might_break = if let Stmt::Labeled(LabeledStmt { label, body, .. }) = stat {
        if is_loop(body) {
            ctx.push_label(NestedIntoStatement::Labelled(label.sym.to_string()));
            stat_to_basic_blocks_inner(ctx, body);
            true
        } else {
            todo!()
        }
    } else if is_loop(stat) {
        ctx.push_label(NestedIntoStatement::Unlabelled);
        stat_to_basic_blocks_inner(ctx, stat);
        true
    } else {
        stat_to_basic_blocks_inner(ctx, stat);
        false
    };

    ctx.wrap_up_block();

    if might_break {
        let jumpers_towards_me = ctx.pop_label();
        if jumpers_towards_me.len() > 0 {
            for jumper in jumpers_towards_me {
                ctx.set_exit(jumper, BasicBlockExit::Jump(ctx.current_block_index()))
            }
        }
    }
}

fn block_scoped_to_basic_blocks(ctx: &mut FromAstCtx, stats: &[Stmt]) {
    // TODO actually implement block scope
    for stat in stats {
        stat_to_basic_blocks(ctx, stat);
    }
    ctx.wrap_up_block();
}

/// Turn a statement into basic blocks. Wrapped by `stat_to_basic_blocks` to handle labels.
fn stat_to_basic_blocks_inner(ctx: &mut FromAstCtx, stat: &Stmt) {
    match stat {
        Stmt::Expr(expr) => {
            let _exprid = expr_to_basic_blocks(ctx, &expr.expr);
        }
        Stmt::Decl(Decl::Var(var)) => {
            for decl in &var.decls {
                let Pat::Ident(ident) = &decl.name else {todo!()};
                let expr = expr_to_basic_blocks(ctx, decl.init.as_ref().unwrap().borrow());
                ctx.assign_name(&ident.sym.to_string(), expr);
            }
        }
        Stmt::DoWhile(_) => todo!(),
        Stmt::For(_) => todo!(),
        Stmt::ForIn(_) => todo!(),
        Stmt::ForOf(_) => todo!(),
        Stmt::While(whil) => {
            let blockidx_start = ctx.wrap_up_block();

            let test = expr_to_basic_blocks(ctx, &whil.test);

            let test_after_idx = ctx.wrap_up_block();

            let blockidx_before_body = ctx.wrap_up_block();
            stat_to_basic_blocks(ctx, &whil.body);

            // loop back to start
            ctx.set_exit(
                ctx.current_block_index(),
                BasicBlockExit::Jump(blockidx_start),
            );

            let blockidx_after_body = ctx.wrap_up_block();

            ctx.set_exit(
                test_after_idx,
                BasicBlockExit::Cond(test, blockidx_before_body, blockidx_after_body),
            );

            ctx.wrap_up_block();
        }
        Stmt::If(IfStmt {
            test, cons, alt, ..
        }) => {
            ctx.wrap_up_block();
            let test = expr_to_basic_blocks(ctx, &test);
            ctx.wrap_up_block();
            let blockidx_before = ctx.current_block_index();
            ctx.wrap_up_block();

            ctx.enter_conditional_branch();

            let blockidx_consequent_before = ctx.current_block_index();
            stat_to_basic_blocks(ctx, &cons);
            let blockidx_consequent_after = ctx.wrap_up_block();

            let alt = if let Some(alt) = alt {
                ctx.wrap_up_block();
                let blockidx_alternate_before = ctx.current_block_index();
                stat_to_basic_blocks(ctx, &alt);
                ctx.wrap_up_block();
                let blockidx_alternate_after = ctx.current_block_index();
                Some((blockidx_alternate_before, blockidx_alternate_after))
            } else {
                None
            };

            ctx.leave_conditional_branch();
            ctx.wrap_up_block();

            if let Some((blockidx_alternate_before, blockidx_alternate_after)) = alt {
                ctx.set_exit(
                    blockidx_before,
                    BasicBlockExit::Cond(
                        test,
                        blockidx_consequent_before,
                        blockidx_alternate_before,
                    ),
                );
                ctx.set_exit(
                    blockidx_consequent_after,
                    BasicBlockExit::Jump(blockidx_alternate_after),
                );
            } else {
                ctx.set_exit(
                    blockidx_before,
                    BasicBlockExit::Cond(
                        test,
                        blockidx_consequent_before,
                        blockidx_consequent_after,
                    ),
                );
            }
        }
        Stmt::Block(block) => block_scoped_to_basic_blocks(ctx, &block.stmts),
        Stmt::Break(br) => ctx.register_break(&br.label),
        Stmt::Continue(_cont) => todo!("ctx.register_continue(cont.label)"),
        Stmt::Labeled(_) => unreachable!("label is handled in stat_to_basic_blocks"),
        Stmt::Debugger(_) => todo!(),
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

            block_scoped_to_basic_blocks(ctx, &stmt.block.stmts);

            let before_catch_idx = ctx.wrap_up_block();
            let catch_idx = ctx.wrap_up_block();

            if let Some(ref handler) = stmt.handler {
                if let Some(p) = &handler.param {
                    let sym = p.clone().ident().unwrap(/* TODO */);
                    let catcherr = ctx.push_instruction(BasicBlockInstruction::CaughtError);
                    ctx.assign_name(&sym.sym, catcherr);
                }
                block_scoped_to_basic_blocks(ctx, &handler.body.stmts);
            }

            let after_catch_idx = ctx.wrap_up_block();
            let finally_idx = ctx.wrap_up_block();

            if let Some(ref finalizer) = stmt.finalizer {
                block_scoped_to_basic_blocks(ctx, &finalizer.stmts);
            }

            let after_finally_idx = ctx.wrap_up_block();

            ctx.leave_conditional_branch();

            let done_and_dusted = ctx.wrap_up_block();

            // declare the trycatch
            ctx.set_exit(
                catch_pusher_idx,
                BasicBlockExit::SetTryAndCatch(try_idx, catch_idx, finally_idx, done_and_dusted),
            );
            // catch the error
            ctx.set_exit(
                before_catch_idx,
                BasicBlockExit::PopCatch(catch_idx, finally_idx),
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
        Stmt::Empty(_) => {}
        _ => {
            todo!("statements_to_ssa: stat_to_ssa: {:?} not implemented", stat)
        }
    }
}

pub fn statements_to_basic_blocks(ctx: &mut FromAstCtx, statements: &[&Stmt]) {
    for stat in statements {
        stat_to_basic_blocks(ctx, stat);
        ctx.wrap_up_block();
    }
}

fn expr_to_basic_blocks(ctx: &mut FromAstCtx, exp: &Expr) -> usize {
    let node = match exp {
        Expr::Lit(Lit::Num(num)) => BasicBlockInstruction::LitNumber(num.value),
        Expr::Bin(bin) => {
            let l = expr_to_basic_blocks(ctx, &bin.left);
            let r = expr_to_basic_blocks(ctx, &bin.right);

            BasicBlockInstruction::BinOp("+".into(), l, r)
        }
        Expr::Assign(assign) => match &assign.left {
            PatOrExpr::Pat(e) => match e.borrow() {
                Pat::Ident(ident) => {
                    let sym = ident.sym.to_string();
                    let Some(_old_idx) = ctx.read_name(&sym) else {todo!()};

                    let expr_idx = expr_to_basic_blocks(ctx, &assign.right);
                    ctx.assign_name(&sym, expr_idx);

                    return expr_idx;
                }
                _ => todo!(),
            },
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
            ctx.wrap_up_block();

            let blockidx_alternate_before = ctx.current_block_index();
            let alt = expr_to_basic_blocks(ctx, &cond_expr.alt);
            ctx.wrap_up_block();
            let blockidx_after = ctx.current_block_index();

            // block before gets a Cond node added
            ctx.set_exit(
                blockidx_before,
                BasicBlockExit::Cond(test, blockidx_consequent_before, blockidx_alternate_before),
            );

            // block starting with cons gets a Jump node added, to the current block
            ctx.set_exit(
                blockidx_consequent_after,
                BasicBlockExit::Jump(blockidx_after),
            );

            ctx.leave_conditional_branch();
            ctx.wrap_up_block();

            // the retval of our ternary is a phi node
            BasicBlockInstruction::Phi(vec![cons, alt])
        }
        Expr::Ident(ident) => {
            let Some(var_idx) = ctx.read_name(&ident.sym.to_string()) else {
                todo!("{} not found in scope", ident.sym.to_string())
            };

            BasicBlockInstruction::Ref(var_idx)
        }
        Expr::This(_) => BasicBlockInstruction::This,
        Expr::Array(array_lit) => {
            let mut elements = vec![];

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

            BasicBlockInstruction::Array(elements)
        }
        Expr::Object(_) => todo!(),
        Expr::Unary(_) => todo!(),
        Expr::Update(_) => todo!(),
        Expr::Member(_) => todo!(),
        Expr::SuperProp(_) => todo!(),
        Expr::Fn(function) => {
            let func = function_to_basic_blocks(ctx, FunctionLike::FnExpr(function))
                .expect("todo error handling");
            BasicBlockInstruction::Function(func.id)
        }
        Expr::Call(call) => {
            // TODO non-expr callees (super, import)
            let callee = expr_to_basic_blocks(ctx, &call.callee.clone().expect_expr());

            let mut args = vec![];
            for arg in &call.args {
                match arg.spread {
                    Some(_) => todo!("spread args"),
                    None => args.push(expr_to_basic_blocks(ctx, arg.expr.as_ref())),
                }
            }

            BasicBlockInstruction::Call(callee, args)
        }
        Expr::New(_) => todo!(),
        Expr::Tpl(_) => todo!(),
        Expr::TaggedTpl(_) => todo!(),
        Expr::Arrow(_) => todo!(),
        Expr::Class(_) => todo!(),
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

            BasicBlockInstruction::TempExit(typ, arg)
        }
        Expr::Await(AwaitExpr { arg, .. }) => {
            let arg = expr_to_basic_blocks(ctx, arg);

            BasicBlockInstruction::TempExit(TempExitType::Await, arg)
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
        _ => {
            todo!("statements_to_ssa: expr_to_ssa: {:?} not implemented", exp)
        }
    };

    ctx.push_instruction(node)
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
            $3 = [$0, $1, , ...$2,]
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
    fn simple_cond() {
        let s = test_basic_blocks_expr("1 ? 10 : 20;");
        insta::assert_debug_snapshot!(s, @r###"
        function():
        @0: {
            $0 = 1
            exit = cond $0 ? jump @1 : jump @2
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
    fn cond_nested() {
        let s = test_basic_blocks_expr("1 ? (2 ? 10 : 15) : 20;");
        insta::assert_debug_snapshot!(s, @r###"
        function():
        @0: {
            $0 = 1
            exit = cond $0 ? jump @1 : jump @6
        }
        @1: {
            $1 = 2
            exit = cond $1 ? jump @2 : jump @3
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
            exit = jump @5
        }
        @5: {
            $4 = either($2, $3)
            exit = jump @7
        }
        @6: {
            $5 = 20
            exit = jump @7
        }
        @7: {
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
            exit = jump @1
        }
        @1: {
            $1 = 123
            exit = cond $1 ? jump @2 : jump @3
        }
        @2: {
            $2 = 2
            $3 = 1
            exit = jump @4
        }
        @3: {
            $4 = 3
            exit = jump @4
        }
        @4: {
            $5 = either($2, $4)
            $6 = either($3, $4)
            $7 = $5
            $8 = 2
            $9 = $7 + $8
            $10 = undefined
            exit = return $10
        }
        "###);
    }

    #[test]
    fn cond_nested_reassign_2() {
        let s =
            test_basic_blocks("var x = 1; 123 ? ((x = 1234) ? (x = 567) : 890, 1) : x = 3; x + 2");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            exit = jump @1
        }
        @1: {
            $1 = 123
            exit = cond $1 ? jump @2 : jump @7
        }
        @2: {
            $2 = 1234
            exit = cond $2 ? jump @3 : jump @4
        }
        @3: {
            $3 = 567
            exit = jump @5
        }
        @4: {
            $4 = 890
            exit = jump @5
        }
        @5: {
            exit = jump @6
        }
        @6: {
            $5 = either($3, $4)
            $6 = 1
            exit = jump @8
        }
        @7: {
            $7 = 3
            exit = jump @8
        }
        @8: {
            $8 = either($2, $7)
            $9 = either($6, $7)
            $10 = $8
            $11 = 2
            $12 = $10 + $11
            $13 = undefined
            exit = return $13
        }
        "###);
    }

    #[test]
    fn a_loop() {
        let s = test_basic_blocks("123; while (123) { 456; }");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 123
            exit = jump @1
        }
        @1: {
            exit = jump @2
        }
        @2: {
            $1 = 123
            exit = jump @3
        }
        @3: {
            exit = cond $1 ? jump @4 : jump @6
        }
        @4: {
            $2 = 456
            exit = jump @5
        }
        @5: {
            exit = jump @2
        }
        @6: {
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
            exit = jump @1
        }
        @1: {
            $0 = 123
            exit = jump @2
        }
        @2: {
            exit = cond $0 ? jump @3 : jump @4
        }
        @3: {
            exit = jump @6
        }
        @4: {
            exit = jump @5
        }
        @5: {
            exit = jump @6
        }
        @6: {
            $1 = undefined
            exit = return $1
        }
        "###);
    }

    #[test]
    fn a_labelled_break() {
        let s = test_basic_blocks("outer: while (123) { while (456) { break outer } }");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            exit = jump @1
        }
        @1: {
            $0 = 123
            exit = jump @2
        }
        @2: {
            exit = cond $0 ? jump @3 : jump @9
        }
        @3: {
            exit = jump @4
        }
        @4: {
            $1 = 456
            exit = jump @5
        }
        @5: {
            exit = cond $1 ? jump @6 : jump @7
        }
        @6: {
            exit = jump @11
        }
        @7: {
            exit = jump @8
        }
        @8: {
            exit = jump @1
        }
        @9: {
            exit = jump @10
        }
        @10: {
            exit = jump @11
        }
        @11: {
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
            exit = jump @1
        }
        @1: {
            exit = cond $0 ? jump @2 : jump @4
        }
        @2: {
            $1 = 456
            exit = jump @3
        }
        @3: {
            exit = jump @6
        }
        @4: {
            $2 = 789
            exit = jump @5
        }
        @5: {
            exit = jump @6
        }
        @6: {
            $3 = undefined
            exit = return $3
        }
        "###);
    }

    #[test]
    fn an_if_2() {
        let s = test_basic_blocks(
            "
            if (123) {
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
            exit = jump @1
        }
        @1: {
            exit = cond $0 ? jump @2 : jump @8
        }
        @2: {
            $1 = 456
            exit = jump @3
        }
        @3: {
            exit = cond $1 ? jump @4 : jump @6
        }
        @4: {
            $2 = 789
            exit = jump @5
        }
        @5: {
            exit = jump @6
        }
        @6: {
            exit = jump @7
        }
        @7: {
            exit = jump @10
        }
        @8: {
            $3 = 999
            exit = jump @9
        }
        @9: {
            exit = jump @10
        }
        @10: {
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
            exit = jump @1
        }
        @1: {
            exit = try @2 catch @4 finally @6 after @8
        }
        @2: {
            $0 = 777
            exit = jump @3
        }
        @3: {
            exit = error ? jump @4 : jump @6
        }
        @4: {
            $1 = 888
            exit = jump @5
        }
        @5: {
            exit = finally @6 after @7
        }
        @6: {
            exit = jump @7
        }
        @7: {
            exit = end finally after @8
        }
        @8: {
            $2 = undefined
            exit = return $2
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
            exit = jump @1
        }
        @1: {
            exit = try @2 catch @4 finally @6 after @8
        }
        @2: {
            $0 = 777
            exit = jump @3
        }
        @3: {
            exit = error ? jump @4 : jump @6
        }
        @4: {
            $1 = 888
            exit = jump @5
        }
        @5: {
            exit = finally @6 after @7
        }
        @6: {
            exit = jump @7
        }
        @7: {
            $2 = either($0, $1)
            exit = end finally after @8
        }
        @8: {
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
            exit = jump @1
        }
        @1: {
            exit = try @2 catch @4 finally @6 after @8
        }
        @2: {
            $0 = 777
            exit = jump @3
        }
        @3: {
            exit = error ? jump @4 : jump @6
        }
        @4: {
            $1 = 888
            exit = jump @5
        }
        @5: {
            exit = finally @6 after @7
        }
        @6: {
            $2 = 999
            exit = jump @7
        }
        @7: {
            exit = end finally after @8
        }
        @8: {
            $3 = undefined
            exit = return $3
        }
        "###);
    }
}
