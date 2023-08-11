use super::{find_nonlocals, statements_to_basic_blocks, FromAstCtx, FunctionLike};
use crate::basic_blocks::{BasicBlockGroup, BasicBlockInstruction};

pub fn function_to_basic_blocks<'a>(
    ctx: &'a mut FromAstCtx,
    function: FunctionLike<'a>,
) -> Result<&'a BasicBlockGroup, String> {
    // count function.length
    let arg_count: usize = function.function_length();

    ctx.go_into_function(arg_count, Some(find_nonlocals(function.clone())), |ctx| {
        function
            .get_params()
            .into_iter()
            .enumerate()
            .for_each(|(i, pat)| match pat {
                swc_ecma_ast::Pat::Ident(ident) => {
                    let arg = ctx.push_instruction(BasicBlockInstruction::ArgumentRead(i));
                    ctx.assign_name(&ident.id.sym.to_string(), arg);
                }
                swc_ecma_ast::Pat::Rest(ident) => {
                    let name = ident.arg.as_ident().as_ref().unwrap().id.sym.to_string();
                    let arg = ctx.push_instruction(BasicBlockInstruction::ArgumentRest(i));
                    ctx.assign_name(&name, arg);
                }
                _ => todo!("non-ident function param"),
            });

        statements_to_basic_blocks(ctx, &function.get_statements());

        Ok(())
    })
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::*;
    use crate::basic_blocks::{BasicBlockGroup, FunctionId};
    use crate::swc_parse::swc_parse;

    fn conv_fn(src: &str) -> BTreeMap<FunctionId, BasicBlockGroup> {
        let mut ctx = FromAstCtx::new();
        let func = swc_parse(src);
        let decl = func.body[0]
            .clone()
            .expect_stmt()
            .expect_decl()
            .expect_fn_decl();
        function_to_basic_blocks(&mut ctx, FunctionLike::FnDecl(&decl)).unwrap();

        ctx.functions.into_iter().collect()
    }

    #[test]
    fn test_basic_blocks_function() {
        let func = conv_fn("function _(y, z) { return y + z }");
        insta::assert_debug_snapshot!(func, @r###"
        {
            FunctionId(1): function():
            @0: {
                $0 = arguments[0]
                $1 = arguments[1]
                $2 = $0
                $3 = $1
                $4 = $2 + $3
                exit = return $4
            },
        }
        "###);
    }

    #[test]
    fn test_basic_blocks_function_rest() {
        let func = conv_fn("function _(y, ...z) { return z }");
        insta::assert_debug_snapshot!(func, @r###"
        {
            FunctionId(1): function():
            @0: {
                $0 = arguments[0]
                $1 = arguments[1...]
                $2 = $1
                exit = return $2
            },
        }
        "###);
    }

    #[test]
    fn test_closure() {
        let func = conv_fn(
            "function _() {
                var outer = 1
                var bar = function bar() { return outer; }
            }",
        );
        insta::assert_debug_snapshot!(func, @r###"
        {
            FunctionId(1): function():
            @0: {
                $0 = undefined
                $2 = write_non_local $$1 $0
                $3 = 1
                $4 = write_non_local $$1 $3
                $8 = FunctionId(2)
                $9 = undefined
                exit = return $9
            },
            FunctionId(2): function():
            @0: {
                $5 = read_non_local $$1
                $6 = $5
                exit = return $6
            },
        }
        "###);
    }

    #[test]
    fn test_closure_nonlocal_arg() {
        let func = conv_fn(
            "function _(outer_arg) {
                var bar = function bar() { return outer_arg; }
            }",
        );
        insta::assert_debug_snapshot!(func, @r###"
        {
            FunctionId(1): function():
            @0: {
                $0 = undefined
                $2 = write_non_local $$1 $0
                $3 = arguments[0]
                $4 = write_non_local $$1 $3
                $8 = FunctionId(2)
                $9 = undefined
                exit = return $9
            },
            FunctionId(2): function():
            @0: {
                $5 = read_non_local $$1
                $6 = $5
                exit = return $6
            },
        }
        "###);
    }

    #[test]
    fn test_closure_duplicate_name() {
        let func = conv_fn(
            "function f1() {
                var dupe = 1
                var f2 = function f2() {
                    return dupe;
                }
                var f3 = function f3() {
                    var dupe = 2; // SHADOWS outer `dupe`
                    var f4 = function f4() { return dupe; }
                }
            }",
        );
        insta::assert_debug_snapshot!(func, @r###"
        {
            FunctionId(1): function():
            @0: {
                $0 = undefined
                $2 = write_non_local $$1 $0
                $3 = 1
                $4 = write_non_local $$1 $3
                $8 = FunctionId(2)
                $19 = FunctionId(3)
                $20 = undefined
                exit = return $20
            },
            FunctionId(2): function():
            @0: {
                $5 = read_non_local $$1
                $6 = $5
                exit = return $6
            },
            FunctionId(3): function():
            @0: {
                $9 = undefined
                $11 = write_non_local $$10 $9
                $12 = 2
                $13 = write_non_local $$10 $12
                $17 = FunctionId(4)
                $18 = undefined
                exit = return $18
            },
            FunctionId(4): function():
            @0: {
                $14 = read_non_local $$10
                $15 = $14
                exit = return $15
            },
        }
        "###);
    }

    #[test]
    fn test_closure_nonlocal_and_also_yet_to_be_defined() {
        let func = conv_fn(
            "function _() {
                var x = dupe; // `dupe` is nonlocal
                var dupe = 999
                var foo = function foo() {
                    return dupe;
                }
            }",
        );
        insta::assert_debug_snapshot!(func, @r###"
        {
            FunctionId(1): function():
            @0: {
                $0 = undefined
                $2 = write_non_local $$1 $0
                $3 = read_non_local $$1
                $4 = $3
                $5 = 999
                $6 = write_non_local $$1 $5
                $10 = FunctionId(2)
                $11 = undefined
                exit = return $11
            },
            FunctionId(2): function():
            @0: {
                $7 = read_non_local $$1
                $8 = $7
                exit = return $8
            },
        }
        "###);
    }
}
