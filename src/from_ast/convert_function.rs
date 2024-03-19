use swc_ecma_ast::{ArrowExpr, ClassMethod, FnDecl, FnExpr, MethodProp, Pat, PrivateMethod};

use super::{
    block_to_basic_blocks, expr_to_basic_blocks, find_nonlocals, pat_to_basic_blocks, FromAstCtx,
    FuncBlockOrRetExpr, FunctionLike, PatType,
};
use crate::basic_blocks::{
    BasicBlockEnvironment, BasicBlockInstruction, ExitType, FunctionId, NonLocalId, StructuredFlow,
    LHS,
};

/// Convert a function to basic blocks. Function declarations are special because since they're hoisted, we don't create a variable for them. Instead, the caller provides it.
pub fn function_to_basic_blocks<'a, 'decl>(
    ctx: &'a mut FromAstCtx,
    function: FunctionLike,
    mut fn_decl_varname: Option<usize>,
) -> Result<(Vec<StructuredFlow>, usize, FunctionId), String> {
    let mut function_flow = vec![];

    let fn_id = FunctionId(ctx.function_index.0 + 1); // future function name

    // Only a named FnExpr can have two simultaneous bindings. Outside (maybe) and inside.
    let (outer_varname, inner_varname) = match function {
        FunctionLike::ArrowExpr(_) | FunctionLike::FnExpr(FnExpr { ident: None, .. }) => {
            let (flow, outer_varname) =
                ctx.push_instruction(BasicBlockInstruction::Function(fn_id));
            function_flow.extend(flow);

            (outer_varname, None)
        }
        FunctionLike::FnExpr(FnExpr {
            ident: Some(ident), ..
        }) => {
            let (flow, undef) = ctx.push_instruction(BasicBlockInstruction::Undefined);
            function_flow.extend(flow);

            let non_local_id = NonLocalId(ctx.get_var_index());
            let (flow, _) = ctx.push_instruction(BasicBlockInstruction::Write(
                LHS::NonLocal(non_local_id),
                undef,
            ));
            function_flow.extend(flow);

            let (flow, outer_varname) =
                ctx.push_instruction(BasicBlockInstruction::Function(fn_id));
            function_flow.extend(flow);

            let write_non_local =
                BasicBlockInstruction::Write(LHS::NonLocal(non_local_id), outer_varname);
            let (flow, _) = ctx.push_instruction(write_non_local);
            function_flow.extend(flow);

            (outer_varname, Some((ident.sym.to_string(), non_local_id)))
        }
        FunctionLike::FnDecl(_) => {
            let outer_varname = fn_decl_varname
                .take()
                .expect("FnDecl needs a reserved varname");
            (outer_varname, None)
        }
        FunctionLike::ClassMethod(_)
        | FunctionLike::PrivateMethod(_)
        | FunctionLike::ClassConstructor(_)
        | FunctionLike::ObjectMethod(_)
        | FunctionLike::ObjectGetter(_)
        | FunctionLike::ObjectSetter(_) => {
            let outer_varname = usize::MAX; // unused

            (outer_varname, None)
        }
    };

    assert!(
        fn_decl_varname.is_none(),
        "only FnDecl needs a reserved varname"
    );

    let env = match function {
        FunctionLike::ArrowExpr(ArrowExpr {
            is_generator,
            is_async,
            ..
        }) => {
            assert!(!is_generator);
            BasicBlockEnvironment::Function(*is_generator, *is_async)
        }
        FunctionLike::ClassConstructor(_)
        | FunctionLike::ObjectGetter(_)
        | FunctionLike::ObjectSetter(_) => BasicBlockEnvironment::Function(false, false),
        FunctionLike::FnDecl(FnDecl { function, .. })
        | FunctionLike::FnExpr(FnExpr { function, .. })
        | FunctionLike::ClassMethod(ClassMethod { function, .. })
        | FunctionLike::ObjectMethod(MethodProp { function, .. })
        | FunctionLike::PrivateMethod(PrivateMethod { function, .. }) => {
            BasicBlockEnvironment::Function(function.is_generator, function.is_async)
        }
    };

    ctx.go_into_function(env, Some(find_nonlocals(function.clone())), |ctx| {
        let mut func_body = vec![];

        function.get_params().into_iter().enumerate().try_for_each(
            |(i, pat)| -> Result<(), String> {
                match pat {
                    Pat::Rest(rest_pat) => {
                        let (flow, arg) =
                            ctx.push_instruction(BasicBlockInstruction::ArgumentRest(i));
                        func_body.extend(flow);
                        let (flow, _) =
                            pat_to_basic_blocks(ctx, PatType::FunArg, &rest_pat.arg, arg)?;
                        func_body.extend(flow);
                    }
                    _ => {
                        let (flow, arg) =
                            ctx.push_instruction(BasicBlockInstruction::ArgumentRead(i));
                        func_body.extend(flow);
                        let (flow, _) = pat_to_basic_blocks(ctx, PatType::FunArg, pat, arg)?;
                        func_body.extend(flow);
                    }
                };
                Ok(())
            },
        )?;

        // If this is a named FnExpr, we need another binding here.
        if let Some((name, nloc)) = inner_varname {
            let (flow, nloc) =
                ctx.push_instruction(BasicBlockInstruction::Read(LHS::NonLocal(nloc)));
            func_body.extend(flow);

            let (flow, _) = ctx.declare_name(&name, nloc);
            func_body.extend(flow);
        }

        match function.get_body() {
            FuncBlockOrRetExpr::Block(block) => {
                let flow = block_to_basic_blocks(ctx, block.stmts.iter())?;
                func_body.extend(flow);
            }
            FuncBlockOrRetExpr::RetExpr(expr) => {
                let (flow, varname) = expr_to_basic_blocks(ctx, expr)?;
                func_body.extend(flow);
                func_body.push(StructuredFlow::Return(ExitType::Return, varname));
            }
        };

        Ok(func_body)
    })?;

    Ok((
        StructuredFlow::simplify_vec(function_flow),
        outer_varname,
        fn_id,
    ))
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
        let fn_decl_varname = Some(ctx.get_var_index());

        let (_flow, _, _) =
            function_to_basic_blocks(&mut ctx, FunctionLike::FnDecl(&decl), fn_decl_varname)
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
            }
            @1: {
                $3 = $1
                $4 = $2
                $5 = $3 + $4
            }
            @2: {
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
            },
            FunctionId(2): function():
            @0: {
                $11 = read_non_local $$7
                $12 = read_non_local $$2
                exit = return $12
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
            },
            FunctionId(2): function():
            @0: {
                $11 = read_non_local $$7
                $12 = read_non_local $$2
                exit = return $12
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
                $13 = undefined
                $15 = write_non_local $$14 $13
                $16 = FunctionId(3)
                $17 = write_non_local $$14 $16
            },
            FunctionId(2): function():
            @0: {
                $11 = read_non_local $$7
                $12 = read_non_local $$2
                exit = return $12
            },
            FunctionId(3): function():
            @0: {
                $18 = undefined
                $20 = write_non_local $$19 $18
                $21 = read_non_local $$14
                $22 = 2
                $23 = write_non_local $$19 $22
                $24 = undefined
                $26 = write_non_local $$25 $24
                $27 = FunctionId(4)
                $28 = write_non_local $$25 $27
            },
            FunctionId(4): function():
            @0: {
                $29 = read_non_local $$25
                $30 = read_non_local $$19
                exit = return $30
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
                $5 = 999
                $6 = write_non_local $$2 $5
                $7 = undefined
                $9 = write_non_local $$8 $7
                $10 = FunctionId(2)
                $11 = write_non_local $$8 $10
            },
            FunctionId(2): function():
            @0: {
                $12 = read_non_local $$8
                $13 = read_non_local $$2
                exit = return $13
            },
        }
        "###);
    }
}
