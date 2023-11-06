use std::collections::{BTreeMap, HashSet};

use swc_ecma_ast::{BlockStmt, Expr, FnExpr, Function, Param, ParenExpr, Pat, RestPat};

use crate::{
    basic_blocks::{BasicBlockGroup, BasicBlockInstruction, StructuredFlow},
    block_ops::block_group_to_structured_flow,
    to_ast::build_binding_identifier,
};

use super::{to_statements, ToAstContext};

pub fn function_to_ast(ctx: &mut ToAstContext, func: BasicBlockGroup) -> Expr {
    let mut blocks = block_group_to_structured_flow(func.blocks);
    let params = take_param_readers(ctx, &mut blocks);
    let stmts = to_statements(ctx, &blocks);

    let (is_generator, is_async) = func.environment.unwrap_function();

    Expr::Paren(ParenExpr {
        expr: Box::new(Expr::Fn(FnExpr {
            ident: None,
            function: Box::new(Function {
                span: Default::default(),
                decorators: Default::default(),
                params,
                body: Some(BlockStmt {
                    span: Default::default(),
                    stmts,
                }),
                is_generator,
                is_async,
                type_params: None,
                return_type: None,
            }),
        })),
        span: Default::default(),
    })
}

fn take_param_readers(ctx: &mut ToAstContext, func: &mut StructuredFlow) -> Vec<Param> {
    let mut params = BTreeMap::new();
    let mut vars_to_remove = HashSet::new();

    for (varname, ins) in func.iter_all_instructions() {
        match ins {
            BasicBlockInstruction::ArgumentRead(n) => {
                let ident = build_binding_identifier(&ctx.create_varname_for(varname));
                vars_to_remove.insert(varname);
                params.insert(
                    *n,
                    Param {
                        pat: ident,
                        span: Default::default(),
                        decorators: Default::default(),
                    },
                );
            }
            BasicBlockInstruction::ArgumentRest(from_n) => {
                let ident = build_binding_identifier(&ctx.create_varname_for(varname));
                vars_to_remove.insert(varname);
                params.insert(
                    *from_n,
                    Param {
                        pat: Pat::Rest(RestPat {
                            arg: Box::new(ident),
                            span: Default::default(),
                            dot3_token: Default::default(),
                            type_ann: None,
                        }),
                        span: Default::default(),
                        decorators: Default::default(),
                    },
                );
            }
            _ => {}
        }
    }

    if let Some((last_idx, _)) = params.last_key_value() {
        func.retain_instructions(&mut |(varname, _ins)| !vars_to_remove.contains(&varname));

        (0..=*last_idx)
            .map(|param_idx| {
                if let Some(param) = params.remove(&param_idx) {
                    param
                } else {
                    todo!("unused params")
                }
            })
            .collect()
    } else {
        vec![]
    }
}

#[cfg(test)]
mod tests {
    use crate::{testutils::*, to_ast::module_to_ast};

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

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        var a = undefined;
        var e = (function() {
            var b = undefined;
            var d = (function(c) {
                return c;
            });
            b = d;
            return d(123);
        });
        a = e;
        var f = undefined;
        var g = (function() {
            return 456;
        });
        f = g;
        e() + g();
        return undefined;
        "###);
    }
}
