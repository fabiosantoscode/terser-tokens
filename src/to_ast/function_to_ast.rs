use std::collections::{BTreeMap, HashSet};

use swc_ecma_ast::{BlockStmt, Expr, FnExpr, Function, Param, ParenExpr, Pat, RestPat};

use crate::{
    basic_blocks::{Instruction, StructuredFlow, StructuredFunction},
    to_ast::build_binding_identifier,
};

use super::{build_ident_param, to_blockgroup_statements, ToAstContext};

pub fn function_expr_to_ast(ctx: &mut ToAstContext, func: StructuredFunction) -> Expr {
    Expr::Paren(ParenExpr {
        expr: Box::new(Expr::Fn(FnExpr {
            ident: None,
            function: Box::new(function_to_ast(ctx, func)),
        })),
        span: Default::default(),
    })
}

pub fn function_to_ast(ctx: &mut ToAstContext, func: StructuredFunction) -> Function {
    let mut blocks = StructuredFlow::from_vec(func.blocks);
    let params = take_param_readers(ctx, &mut blocks);
    let stmts = to_blockgroup_statements(ctx, &blocks);

    let (is_generator, is_async) = func.environment.unwrap_function();

    Function {
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
    }
}

fn take_param_readers(ctx: &mut ToAstContext, func: &mut StructuredFlow) -> Vec<Param> {
    let mut params = BTreeMap::new();
    let mut vars_to_remove = HashSet::new();

    for (varname, ins) in func.iter_all_instructions() {
        match ins {
            Instruction::ArgumentRead(n) => {
                let ident = &ctx.create_varname_for(varname);
                vars_to_remove.insert(varname);
                params.insert(*n, build_ident_param(ident));
            }
            Instruction::ArgumentRest(from_n) => {
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
        println!("func: {:?}", func);
        println!("vars_to_remove: {:?}", vars_to_remove);
        func.retain_instructions(&mut |(varname, _ins)| !vars_to_remove.contains(&varname));
        println!("func: {:?}", func);

        (0..=*last_idx)
            .map(|param_idx| {
                if let Some(param) = params.remove(&param_idx) {
                    param
                } else {
                    build_ident_param(&ctx.create_varname_dummy())
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
        "###);
    }

    #[test]
    fn to_functions_make_dummy_args() {
        let block_group = parse_test_module(vec![
            "{
                $0 = FunctionId(1)
                Return $0
            }",
            "{
                $1 = arguments[0]
                $3 = arguments[3]
                Return $3
            }",
        ]);

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        return (function(a, c, d, b) {
            return b;
        });
        "###);
    }
}
