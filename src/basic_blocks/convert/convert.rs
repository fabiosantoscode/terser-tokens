#[macro_use]
use fix_fn::fix_fn;
use std::{borrow::Borrow, cell::RefCell, collections::HashMap};

use crate::basic_blocks::basic_block::{ArrayElement, BasicBlockExit, BasicBlockInstruction};
use crate::scope::scope::Scope;
use swc_ecma_ast::{CondExpr, Decl, Expr, ExprOrSpread, IfStmt, Lit, Pat, PatOrExpr, Stmt};

use super::super::basic_block::ExitType;
use super::super::normalize::normalize_basic_blocks;
use super::super::{basic_block::BasicBlock, basic_block_group::BasicBlockGroup};

enum NestedIntoStatement {
    Labelled(String),
    Unlabelled,
}

pub fn statements_to_basic_blocks(statements: &[&Stmt]) -> BasicBlockGroup {
    let basic_blocks = RefCell::new(vec![vec![]]);
    let exits = RefCell::new(vec![None]);
    // We'll be incrementing these unique varnames as we go
    let var_index = RefCell::new(0_usize);
    let conditionals: RefCell<Option<HashMap<String, Vec<usize>>>> = RefCell::new(None);
    let scope: RefCell<Scope> = RefCell::new(Scope::new(false));
    let push_instruction = |node: BasicBlockInstruction| {
        let id = var_index.borrow().clone();
        *var_index.borrow_mut() += 1;
        basic_blocks
            .borrow_mut()
            .last_mut()
            .unwrap()
            .push((id, node));
        id
    };
    let push_instruction_to_nth_block = |node: BasicBlockInstruction, n: usize| {
        let id = var_index.borrow().clone();
        *var_index.borrow_mut() += 1;
        basic_blocks.borrow_mut()[n].push((id, node));
        id
    };
    let current_block_index = || basic_blocks.borrow().len() - 1;
    let assign_maybe_conditionally = |name: &str, value: usize| {
        let mut conditionals = conditionals.borrow_mut();
        println!("assign_maybe_conditionally: {} = {}", name, value);
        println!("conditionals: {:?}", conditionals);
        match *conditionals {
            Some(ref mut conditionals) => {
                if let Some(conditional) = conditionals.get_mut(name) {
                    conditional.push(value);
                } else {
                    conditionals.insert(name.to_string(), vec![value]);
                }
            }
            None => {}
        }

        let mut scope = scope.borrow_mut();
        scope.insert(name.into(), value);
    };
    let push_phi_assignments = |conditionally_assigned: Option<HashMap<String, Vec<usize>>>| {
        // phi nodes for conditionally assigned variables
        let to_phi = conditionally_assigned.unwrap();
        let mut to_phi = to_phi
            .iter()
            .filter(|(_name, phies)| phies.len() > 1)
            .collect::<Vec<_>>();
        if to_phi.len() > 0 {
            to_phi.sort_by_key(|(name, _)| *name);
            for (varname, phies) in to_phi {
                let phi = BasicBlockInstruction::Phi(phies.clone());
                let phi_idx = push_instruction(phi);
                scope.borrow_mut().insert(varname.clone(), phi_idx);
            }
        }
    };

    let wrap_up_block = || {
        basic_blocks.borrow_mut().push(vec![]);
        exits.borrow_mut().push(None);
        current_block_index()
    };

    let expr_to_basic_blocks = fix_fn!(|expr_to_basic_blocks, exp: &Expr| -> usize {
        let create_gapped_block = |expr: &Expr| {
            let before = current_block_index();
            let expr = expr_to_basic_blocks(&expr);
            let after = current_block_index();
            wrap_up_block();

            (before, expr, after)
        };

        let node = match exp {
            Expr::Lit(Lit::Num(num)) => BasicBlockInstruction::LitNumber(num.value),
            Expr::Bin(bin) => {
                let l = expr_to_basic_blocks(&bin.left);
                let r = expr_to_basic_blocks(&bin.right);

                BasicBlockInstruction::BinOp("+".into(), l, r)
            }
            Expr::Assign(assign) => match &assign.left {
                PatOrExpr::Pat(e) => match e.borrow() {
                    Pat::Ident(ident) => {
                        let sym = ident.sym.to_string();
                        let Some(_old_idx) = scope.borrow().get(&sym) else {todo!()};

                        let expr_idx = expr_to_basic_blocks(&assign.right);
                        assign_maybe_conditionally(&sym, expr_idx);

                        return expr_idx;
                    }
                    _ => todo!(),
                },
                _ => todo!(),
            },
            Expr::Paren(paren) => return expr_to_basic_blocks(&paren.expr),
            Expr::Seq(seq) => {
                let mut last = 0;
                for expr in &seq.exprs {
                    last = expr_to_basic_blocks(expr);
                }
                return last;
            }
            Expr::Cond(CondExpr {
                test, cons, alt, ..
            }) => {
                let (_, test, blockidx_before) = create_gapped_block(&test);

                let old_conditionals = conditionals.replace(Some(HashMap::new()));

                let (blockidx_consequent_before, cons, blockidx_consequent_after) =
                    create_gapped_block(&cons);

                let blockidx_alternate_before = current_block_index();
                let alt = expr_to_basic_blocks(&alt);
                wrap_up_block();
                let blockidx_after = current_block_index();

                // block before gets a Cond node added
                exits.borrow_mut()[blockidx_before] = Some(BasicBlockExit::Cond(
                    test,
                    blockidx_consequent_before,
                    blockidx_alternate_before,
                ));

                // block starting with cons gets a Jump node added, to the current block
                exits.borrow_mut()[blockidx_consequent_after] =
                    Some(BasicBlockExit::Jump(blockidx_after));

                let conditionally_assigned = conditionals.replace(old_conditionals);

                push_phi_assignments(conditionally_assigned);
                wrap_up_block();

                // the retval of our ternary is a phi node
                BasicBlockInstruction::Phi(vec![cons, alt])
            }
            Expr::Ident(ident) => {
                let Some(var_idx) = scope.borrow().get(&ident.sym.to_string()) else {todo!()};

                BasicBlockInstruction::Ref(var_idx)
            }
            Expr::This(_) => BasicBlockInstruction::This,
            Expr::Array(array_lit) => {
                let mut elements = vec![];

                for elem in &array_lit.elems {
                    let elem = match elem {
                        Some(ExprOrSpread { spread, expr }) => {
                            if spread.is_none() {
                                ArrayElement::Item(expr_to_basic_blocks(expr))
                            } else {
                                ArrayElement::Spread(expr_to_basic_blocks(expr))
                            }
                        }
                        None => ArrayElement::Hole,
                    };

                    elements.push(elem);
                }

                BasicBlockInstruction::Array(elements)
            }
            Expr::Object(_) => todo!(),
            Expr::Fn(_) => todo!(),
            Expr::Unary(_) => todo!(),
            Expr::Update(_) => todo!(),
            Expr::Member(_) => todo!(),
            Expr::SuperProp(_) => todo!(),
            Expr::Call(_) => todo!(),
            Expr::New(_) => todo!(),
            Expr::Tpl(_) => todo!(),
            Expr::TaggedTpl(_) => todo!(),
            Expr::Arrow(_) => todo!(),
            Expr::Class(_) => todo!(),
            Expr::Yield(_) => todo!(),
            Expr::MetaProp(_) => todo!(),
            Expr::Await(_) => todo!(),
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

        push_instruction(node)
    });

    // First item is downward propagated (contains where we are), the second is upward propagated (the jump target of a break)
    let label_tracking: RefCell<Vec<(NestedIntoStatement, Vec<usize>)>> = Default::default();
    let push_label = |label: NestedIntoStatement| {
        let mut label_tracking = label_tracking.borrow_mut();
        label_tracking.push((label, vec![]));
    };
    let pop_label = || {
        let (_, target) = label_tracking.borrow_mut().pop().unwrap();
        target
    };

    let stat_to_basic_blocks = fix_fn!(|stat_to_basic_blocks, stat: &Stmt| -> () {
        match stat {
            Stmt::Expr(expr) => {
                let _exprid = expr_to_basic_blocks(&expr.expr);
            }
            Stmt::Decl(Decl::Var(var)) => {
                for decl in &var.decls {
                    let Pat::Ident(ident) = &decl.name else {todo!()};
                    let expr = expr_to_basic_blocks(decl.init.as_ref().unwrap().borrow());
                    assign_maybe_conditionally(&ident.sym.to_string(), expr);
                }
            }
            Stmt::Return(ret) => {
                let expr = expr_to_basic_blocks(ret.arg.as_ref().unwrap());
                {
                    let mut exits = exits.borrow_mut();
                    *exits.last_mut().unwrap() =
                        Some(BasicBlockExit::ExitFn(ExitType::Return, expr));
                }

                wrap_up_block();
            }
            Stmt::DoWhile(_) => todo!(),
            Stmt::For(_) => todo!(),
            Stmt::ForIn(_) => todo!(),
            Stmt::ForOf(_) => todo!(),
            Stmt::While(whil) => {
                let blockidx_start = current_block_index();
                push_label(NestedIntoStatement::Unlabelled);

                let test = expr_to_basic_blocks(&whil.test);

                let test_after_idx = wrap_up_block();

                let blockidx_before_body = wrap_up_block();
                stat_to_basic_blocks(&whil.body);

                // loop back to start
                {
                    let mut exits = exits.borrow_mut();
                    *exits.last_mut().unwrap() = Some(BasicBlockExit::Jump(blockidx_start));
                }

                let blockidx_after_body = wrap_up_block();

                {
                    let mut exits = exits.borrow_mut();
                    exits[test_after_idx] = Some(BasicBlockExit::Cond(
                        test,
                        blockidx_before_body,
                        blockidx_after_body,
                    ));
                }

                let jumpers_towards_me = pop_label();
                if jumpers_towards_me.len() > 0 {
                    for jumper in jumpers_towards_me {
                        let mut exits = exits.borrow_mut();
                        exits[jumper] = Some(BasicBlockExit::Jump(current_block_index()));
                    }
                }

                wrap_up_block();
            }
            Stmt::If(IfStmt {
                test, cons, alt, ..
            }) => {
                wrap_up_block();
                let test = expr_to_basic_blocks(&test);
                wrap_up_block();
                let blockidx_before = current_block_index();
                wrap_up_block();

                let old_conditionals = conditionals.replace(Some(HashMap::new()));

                let blockidx_consequent_before = current_block_index();
                stat_to_basic_blocks(&cons);
                let blockidx_consequent_after = wrap_up_block();

                let blockidx_alternate = if let Some(alt) = alt {
                    wrap_up_block();
                    let blockidx_alternate_before = current_block_index();
                    stat_to_basic_blocks(&alt);
                    wrap_up_block();
                    let blockidx_alternate_after = current_block_index();
                    Some((blockidx_alternate_before, blockidx_alternate_after))
                } else {
                    None
                };

                let conditionally_assigned = conditionals.replace(old_conditionals);

                push_phi_assignments(conditionally_assigned);
                wrap_up_block();

                if let Some((blockidx_alternate_before, blockidx_alternate_after)) =
                    blockidx_alternate
                {
                    exits.borrow_mut()[blockidx_before] = Some(BasicBlockExit::Cond(
                        test,
                        blockidx_consequent_before,
                        blockidx_alternate_before,
                    ));
                    exits.borrow_mut()[blockidx_consequent_after] =
                        Some(BasicBlockExit::Jump(blockidx_alternate_after));
                } else {
                    exits.borrow_mut()[blockidx_before] = Some(BasicBlockExit::Cond(
                        test,
                        blockidx_consequent_before,
                        blockidx_consequent_after,
                    ));
                }
            }
            Stmt::Block(block) => {
                for stat in &block.stmts {
                    stat_to_basic_blocks(stat);
                    wrap_up_block();
                }
            }
            Stmt::Break(br) => match br.label {
                Some(_) => {
                    todo!("statements_to_ssa: stat_to_ssa: break with label not implemented")
                }
                None => {
                    if let Some(last) = label_tracking.borrow_mut().last_mut() {
                        let this_block = wrap_up_block();
                        last.1.push(this_block);
                    } else {
                        unreachable!()
                    }
                }
            },
            Stmt::Empty(_) => todo!(),
            Stmt::Debugger(_) => todo!(),
            Stmt::With(_) => todo!(),
            Stmt::Labeled(_) => todo!(),
            Stmt::Continue(_) => todo!(),
            Stmt::Switch(_) => todo!(),
            Stmt::Throw(_) => todo!(),
            Stmt::Try(_) => todo!(),
            _ => {
                todo!("statements_to_ssa: stat_to_ssa: {:?} not implemented", stat)
            }
        }
    });

    for stat in statements {
        stat_to_basic_blocks(stat);
        wrap_up_block();
    }

    let exit_count = exits.borrow().len();
    let exits = exits
        .borrow()
        .iter()
        .enumerate()
        .map(|(i, e)| match e {
            Some(exit) => exit.clone(),
            None => {
                if i + 1 >= exit_count {
                    let undef_ret =
                        push_instruction_to_nth_block(BasicBlockInstruction::Undefined, i);
                    BasicBlockExit::ExitFn(ExitType::Return, undef_ret)
                } else {
                    BasicBlockExit::Jump(i + 1)
                }
            }
        })
        .collect::<Vec<_>>();

    let (exits, basic_blocks) = normalize_basic_blocks(&exits, &basic_blocks.borrow());

    let asts = exits
        .iter()
        .zip(basic_blocks.iter())
        .map(|(exit, block)| BasicBlock::new(block.clone(), exit.clone()))
        .collect::<Vec<_>>();

    BasicBlockGroup::from_asts(asts)
}

#[cfg(test)]
mod tests {
    use crate::basic_blocks::testutils::{test_basic_blocks, test_basic_blocks_expr};

    #[test]
    fn simple_add() {
        let s = test_basic_blocks_expr("10 + 20 + 30;");
        insta::assert_debug_snapshot!(s, @r###"
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
            $1 = 123
            exit = jump @2
        }
        @2: {
            exit = cond $1 ? jump @3 : jump @5
        }
        @3: {
            $2 = 456
            exit = jump @4
        }
        @4: {
            exit = jump @1
        }
        @5: {
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
            $0 = 123
            exit = jump @1
        }
        @1: {
            exit = cond $0 ? jump @2 : jump @3
        }
        @2: {
            exit = jump @3
        }
        @3: {
            $1 = undefined
            exit = return $1
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
}
