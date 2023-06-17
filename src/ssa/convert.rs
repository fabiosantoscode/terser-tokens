#[macro_use]
use fix_fn::fix_fn;
use std::{borrow::Borrow, cell::RefCell, collections::HashMap};

use crate::scope::scope::Scope;
use crate::ssa::ssa_ast::{SsaAstNode, SsaExit};
use swc_ecma_ast::{CondExpr, Decl, Expr, Lit, Module, ModuleItem, Pat, PatOrExpr, Stmt};

use super::ssa_ast::ExitType;
use super::{ssa_ast::SsaAst, ssa_fn::SsaFn};

pub fn module_to_ssa(m: &Module) -> SsaFn {
    let mut statements = Vec::new();

    for stmt in &m.body {
        match &stmt {
            ModuleItem::ModuleDecl(_) => {
                todo!("module_to_ssa: module decls not implemented");
            }
            ModuleItem::Stmt(stmt) => statements.push(stmt),
        }
    }

    statements_to_ssa(&statements)
}

pub fn statements_to_ssa(statements: &[&Stmt]) -> SsaFn {
    let basic_blocks = RefCell::new(vec![vec![]]);
    let exits = RefCell::new(vec![None]);
    // We'll be incrementing these unique varnames as we go
    let var_index = RefCell::new(0_usize);
    let conditionals: RefCell<Option<HashMap<String, Vec<usize>>>> = RefCell::new(None);
    let scope: RefCell<Scope> = RefCell::new(Scope::new(false));
    let current_ssa_push = |node: SsaAstNode| {
        let id = var_index.borrow().clone();
        *var_index.borrow_mut() += 1;
        basic_blocks
            .borrow_mut()
            .last_mut()
            .unwrap()
            .push((id, node));
        id
    };
    let nth_ssa_push = |node: SsaAstNode, n: usize| {
        let id = var_index.borrow().clone();
        *var_index.borrow_mut() += 1;
        basic_blocks.borrow_mut()[n].push((id, node));
        id
    };
    let current_ssa_index = || basic_blocks.borrow().len() - 1;
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

    let wrap_up_block = || {
        basic_blocks.borrow_mut().push(vec![]);
        exits.borrow_mut().push(None);
        basic_blocks.borrow().len() - 1
    };

    let expr_to_ssa = fix_fn!(|expr_to_ssa, exp: &Expr| -> usize {
        let node = match exp {
            Expr::Lit(Lit::Num(num)) => SsaAstNode::LitNumber(num.value),
            Expr::Bin(bin) => {
                let l = expr_to_ssa(&bin.left);
                let r = expr_to_ssa(&bin.right);

                SsaAstNode::BinOp("+".into(), l, r)
            }
            Expr::Ident(ident) => {
                let Some(var_idx) = scope.borrow().get(&ident.sym.to_string()) else {todo!()};

                SsaAstNode::Ref(var_idx)
            }
            Expr::Assign(assign) => match &assign.left {
                PatOrExpr::Pat(e) => match e.borrow() {
                    Pat::Ident(ident) => {
                        let sym = ident.sym.to_string();
                        let Some(_old_idx) = scope.borrow().get(&sym) else {todo!()};

                        let expr_idx = expr_to_ssa(&assign.right);
                        assign_maybe_conditionally(&sym, expr_idx);

                        return expr_idx;
                    }
                    _ => todo!(),
                },
                _ => todo!(),
            },
            Expr::Paren(paren) => return expr_to_ssa(&paren.expr),
            Expr::Seq(seq) => {
                let mut last = 0;
                for expr in &seq.exprs {
                    last = expr_to_ssa(expr);
                }
                return last;
            }
            Expr::Cond(CondExpr {
                test, cons, alt, ..
            }) => {
                let test = expr_to_ssa(&test);
                let blockidx_before = basic_blocks.borrow().len() - 1;
                wrap_up_block();

                let old_conditionals = conditionals.replace(Some(HashMap::new()));

                let blockidx_consequent_before = basic_blocks.borrow().len() - 1;
                let cons = expr_to_ssa(&cons);
                let blockidx_consequent_after = basic_blocks.borrow().len() - 1;
                wrap_up_block();

                let blockidx_alternate_before = basic_blocks.borrow().len() - 1;
                let alt = expr_to_ssa(&alt);
                let blockidx_after = basic_blocks.borrow().len() - 1;
                wrap_up_block();

                // block before gets a Cond node added
                exits.borrow_mut()[blockidx_before] = Some(SsaExit::Cond(
                    test,
                    blockidx_consequent_before,
                    blockidx_alternate_before,
                ));

                // block starting with cons gets a Jump node added, to the current block
                exits.borrow_mut()[blockidx_consequent_after] = Some(SsaExit::Jump(blockidx_after));

                let conditionally_assigned = conditionals.replace(old_conditionals);

                // phi nodes for conditionally assigned variables
                let to_phi = conditionally_assigned.unwrap();
                let mut to_phi = to_phi
                    .iter()
                    .filter(|(_name, phies)| phies.len() > 1)
                    .collect::<Vec<_>>();
                if to_phi.len() > 0 {
                    to_phi.sort_by_key(|(name, _)| *name);
                    for (varname, phies) in to_phi {
                        let phi = SsaAstNode::Phi(phies.clone());
                        let phi_idx = current_ssa_push(phi);
                        scope.borrow_mut().insert(varname.clone(), phi_idx);
                    }
                }
                wrap_up_block();

                // the retval of our ternary is a phi node
                SsaAstNode::Phi(vec![cons, alt])
            }
            _ => {
                todo!("statements_to_ssa: expr_to_ssa: {:?} not implemented", exp)
            }
        };

        current_ssa_push(node)
    });
    let stat_to_ssa = fix_fn!(|stat_to_ssa, stat: &Stmt| -> () {
        match stat {
            Stmt::Expr(expr) => {
                let _exprid = expr_to_ssa(&expr.expr);
            }
            Stmt::Decl(Decl::Var(var)) => {
                for decl in &var.decls {
                    let Pat::Ident(ident) = &decl.name else {todo!()};
                    let expr = expr_to_ssa(decl.init.as_ref().unwrap().borrow());
                    assign_maybe_conditionally(&ident.sym.to_string(), expr);
                }
            }
            Stmt::Return(ret) => {
                let expr = expr_to_ssa(ret.arg.as_ref().unwrap());
                {
                    let mut exits = exits.borrow_mut();
                    *exits.last_mut().unwrap() = Some(SsaExit::ExitFn(ExitType::Return, expr));
                }

                wrap_up_block();
            }
            Stmt::While(whil) => {
                let blockidx_start = basic_blocks.borrow().len() - 1;

                let test = expr_to_ssa(&whil.test);

                let test_after_idx = wrap_up_block();

                let blockidx_before_body = wrap_up_block();
                stat_to_ssa(&whil.body);

                // loop back to start
                {
                    let mut exits = exits.borrow_mut();
                    *exits.last_mut().unwrap() = Some(SsaExit::Jump(blockidx_start));
                }

                let blockidx_after_body = wrap_up_block();

                {
                    let mut exits = exits.borrow_mut();
                    exits[test_after_idx] = Some(SsaExit::Cond(
                        test,
                        blockidx_before_body,
                        blockidx_after_body,
                    ));
                }
            }
            Stmt::Block(block) => {
                for stat in &block.stmts {
                    stat_to_ssa(stat);
                    wrap_up_block();
                }
            }
            _ => {
                todo!("statements_to_ssa: stat_to_ssa: {:?} not implemented", stat)
            }
        }
    });

    for stat in statements {
        stat_to_ssa(stat);
        wrap_up_block();
    }

    let exits = exits.borrow();
    let exit_count = exits.len();
    let exits = exits
        .iter()
        .enumerate()
        .map(|(i, e)| match e {
            Some(exit) => exit.clone(),
            None => {
                if i + 1 >= exit_count {
                    let undef_ret = nth_ssa_push(SsaAstNode::Undefined, i);
                    SsaExit::ExitFn(ExitType::Return, undef_ret)
                } else {
                    SsaExit::Jump(i + 1)
                }
            }
        })
        .collect::<Vec<_>>();

    let asts = exits
        .iter()
        .zip(basic_blocks.borrow().iter())
        .map(|(exit, block)| SsaAst::new(block.clone(), exit.clone()))
        .collect::<Vec<_>>();

    let ssa = SsaFn::from_asts(asts);

    ssa
}

#[cfg(test)]
mod tests {
    use crate::ssa::testutils::{test_ssa, test_ssa_block};

    #[test]
    fn simple_add() {
        let s = test_ssa("10 + 20 + 30;");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 10
            $1 = 20
            $2 = $0 + $1
            $3 = 30
            $4 = $2 + $3
            exit = jump @1
        }
        @1: {
            $5 = undefined
            exit = return $5
        }
        "###);
    }

    #[test]
    fn simple_cond() {
        let s = test_ssa("1 ? 10 : 20;");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            exit = cond $0 ? jump @1 : jump @2
        }
        @1: {
            $1 = 10
            exit = jump @2
        }
        @2: {
            $2 = 20
            exit = jump @3
        }
        @3: {
            exit = jump @4
        }
        @4: {
            $3 = either($1, $2)
            exit = jump @5
        }
        @5: {
            $4 = undefined
            exit = return $4
        }
        "###);
    }

    #[test]
    fn cond_nested() {
        let s = test_ssa("1 ? (2 ? 10 : 15) : 20;");
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
            exit = jump @3
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
            exit = jump @6
        }
        @6: {
            $5 = 20
            exit = jump @7
        }
        @7: {
            exit = jump @8
        }
        @8: {
            $6 = either($4, $5)
            exit = jump @9
        }
        @9: {
            $7 = undefined
            exit = return $7
        }
        "###);
    }

    #[test]
    fn simple_vars() {
        let s = test_ssa_block("var x = 1; x + 2");
        insta::assert_debug_snapshot!(s, @r###"
        @0: {
            $0 = 1
            exit = jump @1
        }
        @1: {
            $1 = $0
            $2 = 2
            $3 = $1 + $2
            exit = jump @2
        }
        @2: {
            $4 = undefined
            exit = return $4
        }
        "###);
    }

    #[test]
    fn cond_nested_reassign() {
        let s = test_ssa_block("var x = 1; 123 ? (x = 2, 1) : x = 3; x + 2");
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
            exit = jump @3
        }
        @3: {
            $4 = 3
            exit = jump @4
        }
        @4: {
            $5 = either($2, $4)
            exit = jump @5
        }
        @5: {
            $6 = either($3, $4)
            exit = jump @6
        }
        @6: {
            $7 = $5
            $8 = 2
            $9 = $7 + $8
            exit = jump @7
        }
        @7: {
            $10 = undefined
            exit = return $10
        }
        "###);
    }

    #[test]
    fn cond_nested_reassign_2() {
        let s = test_ssa_block("var x = 1; 123 ? ((x = 1234) ? (x = 567) : 890, 1) : x = 3; x + 2");
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
            exit = jump @4
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
            exit = jump @7
        }
        @7: {
            $7 = 3
            exit = jump @8
        }
        @8: {
            $8 = either($2, $7)
            exit = jump @9
        }
        @9: {
            $9 = either($6, $7)
            exit = jump @10
        }
        @10: {
            $10 = $8
            $11 = 2
            $12 = $10 + $11
            exit = jump @11
        }
        @11: {
            $13 = undefined
            exit = return $13
        }
        "###);
    }

    #[test]
    fn a_loop() {
        let s = test_ssa_block("123; while (123) { 456; }");
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
            exit = jump @6
        }
        @6: {
            $3 = undefined
            exit = return $3
        }
        "###);
    }
}
