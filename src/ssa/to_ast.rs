use super::ssa_ast::{ExitType, SsaAstNode, SsaExit};
use super::ssa_fn::SsaFn;
use std::collections::{HashMap, HashSet};
use swc_ecma_ast::{Expr, Ident, ReturnStmt, Stmt};

impl SsaFn {
    pub fn to_js_ast(&self) -> Vec<Stmt> {
        let mut stats = vec![];

        let mut vars: HashMap<usize, Expr> = Default::default();
        let mut used_vars: HashSet<usize> = Default::default();

        for (block_idx, ast) in self.iter() {
            for (var_number, value) in ast.body.iter() {
                match value {
                    SsaAstNode::LitNumber(n) => {
                        vars.insert(var_number.clone(), Expr::Lit((*n).into()));
                    }
                    SsaAstNode::Undefined => {
                        let ident = Expr::Ident(Ident::new("undefined".into(), Default::default()));
                        vars.insert(var_number.clone(), ident);
                    }
                    _ => todo!("to_js_ast: {:?}", value),
                }

                for used_var in value.used_vars() {
                    used_vars.insert(used_var);
                }
            }

            // process the exit
            match ast.exit {
                SsaExit::Jump(next_block) => {
                    if next_block != block_idx + 1 {
                        todo!()
                    }
                }
                // SsaExit::Cond(, , )
                SsaExit::ExitFn(ExitType::Return, returned) => {
                    let returned = vars.get(&returned).cloned().map(Box::new);
                    let returned = Stmt::Return(ReturnStmt {
                        arg: returned,
                        span: Default::default(),
                    });
                    stats.push(returned);
                }
                _ => todo!("to_js_ast (exits): {:?}", ast.exit),
            }
        }

        stats
    }
}

#[cfg(test)]
mod tests {
    use crate::ssa::testutils::test_ssa_block;

    #[test]
    fn mk_stats() {
        let func = test_ssa_block("return 1");
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = 1
            exit = return $0
        }
        @1: {
            exit = jump @2
        }
        @2: {
            $1 = undefined
            exit = return $1
        }
        "###);

        let stats = func.to_js_ast();
        insta::assert_debug_snapshot!(stats, @r###"
        [
            Return(
                ReturnStmt {
                    span: Span {
                        lo: BytePos(
                            0,
                        ),
                        hi: BytePos(
                            0,
                        ),
                        ctxt: #0,
                    },
                    arg: Some(
                        Lit(
                            Num(
                                Number {
                                    span: Span {
                                        lo: BytePos(
                                            0,
                                        ),
                                        hi: BytePos(
                                            0,
                                        ),
                                        ctxt: #0,
                                    },
                                    value: 1.0,
                                    raw: None,
                                },
                            ),
                        ),
                    ),
                },
            ),
            Return(
                ReturnStmt {
                    span: Span {
                        lo: BytePos(
                            0,
                        ),
                        hi: BytePos(
                            0,
                        ),
                        ctxt: #0,
                    },
                    arg: Some(
                        Ident(
                            Ident {
                                span: Span {
                                    lo: BytePos(
                                        0,
                                    ),
                                    hi: BytePos(
                                        0,
                                    ),
                                    ctxt: #0,
                                },
                                sym: Atom('undefined' type=static),
                                optional: false,
                            },
                        ),
                    ),
                },
            ),
        ]
        "###);
    }
}
