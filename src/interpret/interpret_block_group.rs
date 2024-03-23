use crate::{
    basic_blocks::{BreakableId, ExitType, StructuredFlow, StructuredFunction},
    interpret::JsType,
};

use super::{interpret, InterpretCtx, JsArgs, JsCompletion};

pub fn interpret_blocks(
    ctx: &mut InterpretCtx,
    blocks: &Vec<StructuredFlow>,
) -> Option<JsCompletion> {
    interp_vec(ctx, &blocks, BreakableId(None)).and_then(normal_or_return)
}

/// Interpret a function, or get cached, or give up if it can't be cached (IE too many versions of that function call in cache)
pub fn interpret_function<'a>(
    ctx: &'a mut InterpretCtx,
    f: &'a StructuredFunction,
    args: JsArgs,
    is_canonical: bool,
) -> Option<JsCompletion> {
    if !is_canonical {
        ctx.start_branch();
    }
    let old_args = ctx.start_function(is_canonical, f.id, args);
    let interpretation = interp_vec(ctx, &f.blocks, BreakableId(None));
    ctx.end_function(is_canonical, f.id, old_args, interpretation.clone());
    if !is_canonical {
        ctx.end_branch();
    }
    interpretation.and_then(normal_or_return)
}

fn normal_or_return(completion: JsCompletion) -> Option<JsCompletion> {
    match &completion {
        JsCompletion::Normal(_) | JsCompletion::Return(_) => Some(completion),
        _ => None,
    }
}

fn interp_flow(ctx: &mut InterpretCtx, flow: &StructuredFlow) -> Option<JsCompletion> {
    match flow {
        StructuredFlow::Block(brk, blocks) => {
            return match interp_vec(ctx, blocks, *brk)? {
                JsCompletion::Break(b) if brk == &b => {
                    Some(JsCompletion::Normal(JsType::Undefined))
                }
                breaking @ (JsCompletion::Break(_) | JsCompletion::Continue(_)) => Some(breaking),
                ret @ JsCompletion::Return(_) => Some(ret),
                norm @ JsCompletion::Normal(_) => Some(norm),
            };
        }
        StructuredFlow::Instruction(varname, instruction) => {
            let yielded_type = interpret(ctx, instruction)?.into_normal()?;

            ctx.assign_variable(*varname, yielded_type.clone())?;

            return Some(JsCompletion::Normal(yielded_type));
        }
        StructuredFlow::Break(jump) => {
            return Some(JsCompletion::Break(*jump));
        }
        StructuredFlow::Continue(target) => {
            return Some(JsCompletion::Continue(*target));
        }
        StructuredFlow::Return(exit_type, returned) => match exit_type {
            ExitType::Return => {
                let ret = ctx.get_variable(*returned)?.clone();
                return Some(JsCompletion::Return(ret));
            }
            ExitType::Throw => return None,
        },
        StructuredFlow::Cond(brk, cond_var, cons, alt) => {
            let cond = ctx.get_variable(*cond_var)?.clone();
            let truthy = cond.is_truthy();

            let branch_completion = match truthy {
                Some(true) => interp_vec(ctx, cons, *brk)?,
                Some(false) => interp_vec(ctx, alt, *brk)?,
                None => {
                    ctx.start_branch();

                    let a = interp_vec(ctx, cons, *brk);
                    let a_vars = ctx.end_branch();

                    ctx.start_branch();
                    let b = interp_vec(ctx, alt, *brk);
                    let b_vars = ctx.end_branch();

                    // merge the two realities
                    ctx.merge_branch_mutations(a_vars, b_vars);

                    a?.merge(&b?)?
                }
            };

            match branch_completion {
                JsCompletion::Break(b) if b == *brk => {
                    return Some(JsCompletion::Normal(JsType::Undefined))
                }
                JsCompletion::Break(_) => return Some(branch_completion),
                JsCompletion::Normal(_) => return Some(branch_completion),
                JsCompletion::Return(_) => return Some(branch_completion),
                _ => return None,
            }
        }
        // do not interpret loops, try..catch, etc.
        _ => return None,
    };
}

fn interp_vec(
    ctx: &mut InterpretCtx,
    vec: &Vec<StructuredFlow>,
    catch_breaks: BreakableId,
) -> Option<JsCompletion> {
    let mut out_norm = None;

    for block in vec.iter() {
        match interp_flow(ctx, block)? {
            JsCompletion::Break(me) if me == catch_breaks => {
                return Some(JsCompletion::Normal(JsType::Undefined))
            }
            brk @ (JsCompletion::Break(_) | JsCompletion::Continue(_)) => return Some(brk),
            ret @ JsCompletion::Return(_) => return Some(ret),
            norm @ JsCompletion::Normal(_) => {
                out_norm = Some(norm);
                continue;
            }
        };
    }

    out_norm
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        basic_blocks::{FunctionId, StructuredModule},
        interpret::JsType,
        testutils::*,
    };

    const ANY_VAR: usize = 666;
    fn test_interp(source: &str) -> Option<JsCompletion> {
        let module = parse_test_module(vec![source]);
        let module: StructuredModule = module.into();
        let ctx = &mut InterpretCtx::from_module(&module);
        ctx.assign_variable(ANY_VAR, JsType::Any);
        interpret_blocks(ctx, &module.get_function(FunctionId(0)).unwrap().blocks)
    }

    fn test_interp_ret(source: &str) -> JsType {
        match test_interp(source) {
            Some(JsCompletion::Return(t)) => t,
            comp => panic!("expected normal completion, got {:?}", comp),
        }
    }

    #[test]
    fn interp_math() {
        insta::assert_debug_snapshot!(
            test_interp_ret("
                {
                    $1 = 1
                    $2 = 2
                    $3 = $1 + $2
                    Return $3
                }
            "),
            @"TheNumber(3)"
        )
    }

    #[test]
    fn interp_logical() {
        insta::assert_debug_snapshot!(
            test_interp_ret("
                {
                    $1 = 1
                    $2 = 2
                    $3 = $1 > $2
                    Return $3
                }
            "),
            @"TheBoolean(false)"
        )
    }

    #[test]
    fn interp_throw() {
        assert_eq!(test_interp("{ Throw $1 }"), None);
    }

    #[test]
    fn interp_if() {
        let cond_with = |cond_var: &str| {
            format!(
                "{{
                    $0 = {}
                    if ($0) {{
                        $3 = 2
                        Return $3
                    }} else {{
                        $4 = undefined
                        Return $4
                    }}
                }}",
                cond_var
            )
        };

        // could be one or the other
        insta::assert_debug_snapshot!(
            test_interp_ret(&cond_with("$666")),
            @"Any"
        );

        // "true" branch
        insta::assert_debug_snapshot!(
            test_interp_ret(&cond_with("1")),
            @"TheNumber(2)"
        );

        // "false" branch
        insta::assert_debug_snapshot!(
            test_interp_ret(&cond_with("0")),
            @"Undefined"
        );
    }

    #[test]
    fn interp_if_break() {
        // could be one or the other
        insta::assert_debug_snapshot!(
            test_interp_ret("
                {
                    $0 = $666
                    if ($0) {
                        $3 = 2
                    } else {
                        $4 = 3
                    }
                    $5 = either($3, $4)
                    Return $5
                }
            "),
            @"Number"
        );
    }

    #[test]
    fn interp_if_weird_branch() {
        // incompatible branches (jump @3 and jump @4)
        insta::assert_debug_snapshot!(
            test_interp(
                "{
                    $0 = $666
                    if ($0) {
                        $3 = 2
                    } else {
                        $4 = undefined
                    }
                    Return $0
                }"
            ),
            @r###"
        Some(
            Return(
                Any,
            ),
        )
        "###
        );

        // incompatible branches (jump @3 and return)
        insta::assert_debug_snapshot!(
            test_interp(
                "{
                    $0 = $666
                    if ($0) {
                        $3 = 2
                    } else {
                        $4 = undefined
                        Return $0
                    }
                    Return $0
                }"
            ),
            @"None"
        );
    }

    #[test]
    fn interp_loop() {
        // Any loop just exits
        insta::assert_debug_snapshot!(
            test_interp(
                "{
                    $0 = 1
                    loop {
                        Return $0
                    }
                }"
            ),
            @"None"
        );
    }

    #[test]
    fn interp_trycatch() {
        // returns None because we don't care
        insta::assert_debug_snapshot!(
            test_interp("
                {
                    try {
                        $0 = 777
                    } catch {
                        $1 = 888
                    } finally {
                    }
                    $2 = 999
                    Return $2
                }
            "),
            @"None"
        );
    }
}
