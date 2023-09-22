use crate::basic_blocks::{BasicBlock, BasicBlockExit, BasicBlockGroup, ExitType};

use super::{interpret, InterpretCompletion, InterpretCtx, JsArgs};

pub fn interpret_block_group(
    ctx: &mut InterpretCtx,
    block_group: &BasicBlockGroup,
) -> Option<InterpretCompletion> {
    let completion =
        interpret_block_group_inner(ctx, block_group, 0..=(block_group.blocks.len() - 1))?
            .as_known()?;
    ctx.mark_function_evaluated(block_group.id);
    Some(completion)
}

/// Interpret a function, or get cached, or give up if it can't be cached (IE too many versions of that function call in cache)
pub fn interpret_function(
    ctx: &mut InterpretCtx,
    block_group: &BasicBlockGroup,
    args: JsArgs,
) -> Option<InterpretCompletion> {
    if let Some(cached) = ctx.get_cached(block_group.id, &args) {
        cached.clone()
    } else if ctx.can_cache(block_group.id) {
        ctx.start_function(block_group.id, args);
        let interpretation = interpret_block_group(ctx, block_group);
        ctx.end_function(block_group.id, interpretation.clone());
        interpretation
    } else {
        println!("interpret_function: {:?} {:?}", block_group.id, args);
        None
    }
}

/// Prevent infinite loops and attempting to understand really really large functions
const MAX_ITERATIONS: usize = 1000;

fn interpret_block_group_inner(
    ctx: &mut InterpretCtx,
    block_group: &BasicBlockGroup,
    range: std::ops::RangeInclusive<usize>,
) -> Option<InterpretCompletion> {
    let mut index = *range.start();

    for _safety in 0..MAX_ITERATIONS {
        let BasicBlock { instructions, exit } = &block_group.blocks[index];

        for (varname, instruction) in instructions {
            let yielded_type = interpret(ctx, instruction)?.as_normal()?;

            ctx.assign_variable(*varname, yielded_type);
        }

        match exit {
            BasicBlockExit::Jump(jump) | BasicBlockExit::Break(jump) => {
                assert!(*jump > index);
                if jump <= range.end() {
                    index = *jump;
                } else {
                    return Some(InterpretCompletion::Break(*jump));
                }
            }
            BasicBlockExit::Continue(target) => {
                assert!(*target < index);
                if target >= range.start() {
                    index = *target;
                } else {
                    return Some(InterpretCompletion::Continue(*target));
                }
            }
            BasicBlockExit::ExitFn(exit_type, returned) => match exit_type {
                ExitType::Return => {
                    let ret = ctx.get_variable(*returned)?.clone();
                    return Some(InterpretCompletion::Return(ret));
                }
                ExitType::Throw => return None,
            },
            BasicBlockExit::Cond(cond_var, cons, cons_end, alt, alt_end) => {
                let cond = ctx.get_variable(*cond_var)?.clone();
                let truthy = cond.is_truthy();

                let branch_completion = match truthy {
                    Some(true) => interpret_block_group_inner(ctx, block_group, *cons..=*cons_end)?,
                    Some(false) => interpret_block_group_inner(ctx, block_group, *alt..=*alt_end)?,
                    None => {
                        let a = interpret_block_group_inner(ctx, block_group, *cons..=*cons_end)?;
                        let b = interpret_block_group_inner(ctx, block_group, *alt..=*alt_end)?;

                        a.merge(&b)?
                    }
                };

                // propagate the branch completion. If we returned or broke out of our range, return that.
                match &branch_completion {
                    InterpretCompletion::Return(_) | InterpretCompletion::Unknown => {
                        return Some(branch_completion);
                    }
                    InterpretCompletion::Break(b) | InterpretCompletion::Continue(b) => {
                        if range.contains(b) {
                            index = *b;
                        } else {
                            return Some(branch_completion);
                        }
                    }
                    InterpretCompletion::Normal(_) => {
                        if index < *range.end() {
                            index += 1;
                        } else {
                            return Some(branch_completion);
                        }
                    }
                }
            }
            // do not interpret loops, try..catch, etc.
            _ => return None,
        }
    }

    None
}

// TODO tests

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{interpret::JsType, testutils::*};

    const ANY_VAR: usize = 666;
    fn test_interp(source: &str) -> Option<InterpretCompletion> {
        let ctx = &mut InterpretCtx::new();
        ctx.assign_variable(ANY_VAR, JsType::Any);
        let block_group = parse_instructions(source);
        interpret_block_group(ctx, &block_group)
    }

    fn test_interp_ret(source: &str) -> JsType {
        match test_interp(source) {
            Some(InterpretCompletion::Return(t)) => t,
            comp => panic!("expected normal completion, got {:?}", comp),
        }
    }

    #[test]
    fn interp_math() {
        insta::assert_debug_snapshot!(
            test_interp_ret("
                @0: {
                    $1 = 1
                    $2 = 2
                    exit = jump @1
                }
                @1: {
                    $3 = $1 + $2
                    exit = return $3
                }
            "),
            @"TheNumber(3)"
        )
    }

    #[test]
    fn interp_logical() {
        insta::assert_debug_snapshot!(
            test_interp_ret("
                @0: {
                    $1 = 1
                    $2 = 2
                    exit = jump @1
                }
                @1: {
                    $3 = $1 > $2
                    exit = return $3
                }
            "),
            @"TheBoolean(false)"
        )
    }

    #[test]
    fn interp_throw() {
        assert_eq!(test_interp("@0: { exit = throw $1 }"), None);
    }

    #[test]
    fn interp_if() {
        let cond_with = |cond_var: &str| {
            format!(
                "@0: {{
                    $0 = {}
                    exit = cond $0 ? @1..@1 : @2..@2
                }}
                @1: {{
                    $3 = 2
                    exit = return $3
                }}
                @2: {{
                    $4 = undefined
                    exit = return $4
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
                @0: {
                    $0 = $666
                    exit = cond $0 ? @1..@1 : @2..@2
                }
                @1: {
                    $3 = 2
                    exit = break @3
                }
                @2: {
                    $4 = 3
                    exit = break @3
                }
                @3: {
                    $5 = either($3, $4)
                    exit = return $5
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
                "@0: {
                    $0 = $666
                    exit = cond $0 ? @1..@1 : @2..@2
                }
                @1: {
                    $3 = 2
                    exit = jump @4
                }
                @2: {
                    $4 = undefined
                    exit = jump @3
                }
                @3: {
                    exit = jump @4
                }
                @4: {
                    exit = return $0
                }"
            ),
            @"None"
        );

        // incompatible branches (jump @3 and return)
        insta::assert_debug_snapshot!(
            test_interp(
                "@0: {
                    $0 = $666
                    exit = cond $0 ? @1..@1 : @2..@2
                }
                @1: {
                    $3 = 2
                    exit = jump @3
                }
                @2: {
                    $4 = undefined
                    exit = return $0
                }
                @3: {
                    exit = return $0
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
                "@0: {
                    $0 = 1
                    exit = loop @1..@1
                }
                @1: {
                    exit = return $0
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
                @0: {
                    exit = jump @1
                }
                @1: {
                    exit = try @2 catch @4 finally @6 after @7
                }
                @2: {
                    $0 = 777
                    exit = jump @3
                }
                @3: {
                    exit = error ? jump @4 : jump @5
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
            "),
            @"None"
        );
    }
}
