use swc_ecma_ast::{ArrowExpr, FnDecl, FnExpr, Pat};

use super::{
    block_to_basic_blocks, expr_to_basic_blocks, find_nonlocals, pat_to_basic_blocks, FromAstCtx,
    FuncBlockOrRetExpr, FunctionLike, PatType,
};
use crate::basic_blocks::{
    BasicBlockEnvironment, BasicBlockExit, BasicBlockInstruction, ExitType, FunctionId, NonLocalId,
};

/// Convert a function declaration to basic blocks. Function declarations are special because since they're hoisted, we don't want to create any variables here.
pub fn function_to_basic_blocks(
    ctx: &mut FromAstCtx,
    function: FunctionLike,
    fn_decl_varname: Option<usize>,
) -> Result<usize, String> {
    // Only a named FnExpr can have two simultaneous bindings. Outside (maybe) and inside.
    let (outer_varname, inner_varname) = match function {
        FunctionLike::FnExpr(FnExpr {
            ident: Some(ident), ..
        }) => {
            let undef = ctx.push_instruction(BasicBlockInstruction::Undefined);
            let non_local_id = NonLocalId(ctx.bump_var_index());
            ctx.push_instruction(BasicBlockInstruction::WriteNonLocal(non_local_id, undef));

            let outer_varname = ctx.push_instruction(BasicBlockInstruction::Function(FunctionId(
                ctx.function_index.0 + 1, // future function name
            )));

            let write_non_local = BasicBlockInstruction::WriteNonLocal(non_local_id, outer_varname);
            ctx.push_instruction(write_non_local);

            (outer_varname, Some((ident.sym.to_string(), non_local_id)))
        }
        FunctionLike::FnDecl(_) => {
            let outer_varname = fn_decl_varname.expect("FnDecl needs a reserved varname");
            ctx.arbitrarily_set_id(
                outer_varname,
                BasicBlockInstruction::Function(FunctionId(
                    ctx.function_index.0 + 1, // future function name
                )),
            );
            (outer_varname, None)
        }
        _ => {
            let outer_varname = ctx.push_instruction(BasicBlockInstruction::Function(FunctionId(
                ctx.function_index.0 + 1, // future function name
            )));

            (outer_varname, None)
        }
    };

    // count function.length
    let env = match function {
        FunctionLike::ArrowExpr(ArrowExpr {
            is_generator,
            is_async,
            ..
        }) => {
            assert!(!is_generator);
            BasicBlockEnvironment::Function(*is_generator, *is_async)
        }
        FunctionLike::FnDecl(FnDecl { function, .. })
        | FunctionLike::FnExpr(FnExpr { function, .. }) => {
            BasicBlockEnvironment::Function(function.is_generator, function.is_async)
        }
    };

    ctx.go_into_function(env, Some(find_nonlocals(function.clone())), |ctx| {
        function
            .get_params()
            .into_iter()
            .enumerate()
            .for_each(|(i, pat)| match pat {
                Pat::Rest(rest_pat) => {
                    let arg = ctx.push_instruction(BasicBlockInstruction::ArgumentRest(i));
                    pat_to_basic_blocks(ctx, PatType::FunArg, &rest_pat.arg, arg);
                }
                _ => {
                    let arg = ctx.push_instruction(BasicBlockInstruction::ArgumentRead(i));
                    pat_to_basic_blocks(ctx, PatType::FunArg, pat, arg);
                }
            });

        // If this is a named FnExpr, we need another binding here.
        if let Some((name, nloc)) = inner_varname {
            let nloc = ctx.push_instruction(BasicBlockInstruction::ReadNonLocal(nloc));
            ctx.declare_name(&name, nloc);
        }

        match function.get_body() {
            FuncBlockOrRetExpr::Block(block) => block_to_basic_blocks(ctx, &block.stmts)?,
            FuncBlockOrRetExpr::RetExpr(expr) => {
                let varname = expr_to_basic_blocks(ctx, expr);
                let block_ret = ctx.wrap_up_block();
                ctx.set_exit(block_ret, BasicBlockExit::ExitFn(ExitType::Return, varname));
            }
        };

        Ok(())
    })?;

    Ok(outer_varname)
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
        let fn_decl_varname = ctx.bump_var_index();
        function_to_basic_blocks(&mut ctx, FunctionLike::FnDecl(&decl), Some(fn_decl_varname))
            .unwrap();

        ctx.functions.into_iter().collect()
    }

    #[test]
    fn test_basic_blocks_function() {
        let func = conv_fn("function _(y, z) { return y + z }");
        insta::assert_debug_snapshot!(func, @r###"
        {
            FunctionId(1): function():
            @0: {
                $1 = arguments[0]
                $2 = arguments[1]
                $3 = $1
                $4 = $2
                $5 = $3 + $4
                exit = return $5
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
                $1 = arguments[0]
                $2 = arguments[1...]
                $3 = $2
                exit = return $3
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
                $1 = undefined
                $3 = write_non_local $$2 $1
                $4 = 1
                $5 = write_non_local $$2 $4
                $6 = undefined
                $8 = write_non_local $$7 $6
                $9 = FunctionId(2)
                $10 = write_non_local $$7 $9
                $15 = undefined
                exit = return $15
            },
            FunctionId(2): function():
            @0: {
                $11 = read_non_local $$7
                $12 = read_non_local $$2
                $13 = $12
                exit = return $13
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
                $1 = undefined
                $3 = write_non_local $$2 $1
                $4 = arguments[0]
                $5 = write_non_local $$2 $4
                $6 = undefined
                $8 = write_non_local $$7 $6
                $9 = FunctionId(2)
                $10 = write_non_local $$7 $9
                $15 = undefined
                exit = return $15
            },
            FunctionId(2): function():
            @0: {
                $11 = read_non_local $$7
                $12 = read_non_local $$2
                $13 = $12
                exit = return $13
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
                $1 = undefined
                $3 = write_non_local $$2 $1
                $4 = 1
                $5 = write_non_local $$2 $4
                $6 = undefined
                $8 = write_non_local $$7 $6
                $9 = FunctionId(2)
                $10 = write_non_local $$7 $9
                $15 = undefined
                $17 = write_non_local $$16 $15
                $18 = FunctionId(3)
                $19 = write_non_local $$16 $18
                $36 = undefined
                exit = return $36
            },
            FunctionId(2): function():
            @0: {
                $11 = read_non_local $$7
                $12 = read_non_local $$2
                $13 = $12
                exit = return $13
            },
            FunctionId(3): function():
            @0: {
                $20 = undefined
                $22 = write_non_local $$21 $20
                $23 = read_non_local $$16
                $24 = 2
                $25 = write_non_local $$21 $24
                $26 = undefined
                $28 = write_non_local $$27 $26
                $29 = FunctionId(4)
                $30 = write_non_local $$27 $29
                $35 = undefined
                exit = return $35
            },
            FunctionId(4): function():
            @0: {
                $31 = read_non_local $$27
                $32 = read_non_local $$21
                $33 = $32
                exit = return $33
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
                $1 = undefined
                $3 = write_non_local $$2 $1
                $4 = read_non_local $$2
                $5 = $4
                $6 = 999
                $7 = write_non_local $$2 $6
                $8 = undefined
                $10 = write_non_local $$9 $8
                $11 = FunctionId(2)
                $12 = write_non_local $$9 $11
                $17 = undefined
                exit = return $17
            },
            FunctionId(2): function():
            @0: {
                $13 = read_non_local $$9
                $14 = read_non_local $$2
                $15 = $14
                exit = return $15
            },
        }
        "###);
    }
}
