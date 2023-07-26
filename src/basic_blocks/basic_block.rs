use std::collections::HashMap;

use super::BasicBlockInstruction;

#[derive(Clone, Default, PartialEq)]
pub struct BasicBlock {
    pub instructions: Vec<(usize, BasicBlockInstruction)>,
    pub exit: BasicBlockExit,
}

impl BasicBlock {
    pub fn new(body: Vec<(usize, BasicBlockInstruction)>, exit: BasicBlockExit) -> Self {
        Self {
            instructions: body,
            exit,
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum BasicBlockExit {
    /// unconditional jump to target
    Jump(usize),
    /// (cond_var, true_target, false_target). If cond_var is true, go to true_target. Otherwise, go to false_target.
    Cond(usize, usize, usize),
    /// (exit_type, returned). Used when we see "return" or "throw". "returned" holds the value returned or thrown.
    ExitFn(ExitType, usize),
    /// (try_block, catch_block, finally_block, after_finally_block). Used when we see "try {". It just goes to try_block, the rest is book-keeping.
    SetTryAndCatch(usize, usize, usize, usize),
    /// (catch_block, finally). Used when we see "} catch". If we had an exception, we go to catch_block. Otherwise, we go to finally_or_after.
    PopCatch(usize, usize),
    /// (finally_block, after_finally_block). Used when we see "} finally". We go to the finally-block.
    PopFinally(usize, usize),
    EndFinally(usize),
}

impl BasicBlockExit {
    pub(crate) fn swap_labels(&self, swap_key: &HashMap<usize, usize>) -> BasicBlockExit {
        let swap = |x: &usize| *swap_key.get(&x).unwrap_or(&x);

        match self {
            BasicBlockExit::Jump(target) => BasicBlockExit::Jump(swap(target)),
            BasicBlockExit::Cond(cond, true_target, false_target) => {
                BasicBlockExit::Cond(*cond, swap(true_target), swap(false_target))
            }
            BasicBlockExit::ExitFn(exit_type, target) => {
                BasicBlockExit::ExitFn(exit_type.clone(), *target)
            }
            BasicBlockExit::SetTryAndCatch(
                try_block,
                catch_block,
                finally_block,
                after_finally,
            ) => BasicBlockExit::SetTryAndCatch(
                swap(try_block),
                swap(catch_block),
                swap(finally_block),
                swap(after_finally),
            ),
            BasicBlockExit::PopCatch(catch_block, finally_or_after) => {
                BasicBlockExit::PopCatch(swap(catch_block), swap(finally_or_after))
            }
            BasicBlockExit::PopFinally(finally_block, after_finally) => {
                BasicBlockExit::PopFinally(swap(finally_block), swap(after_finally))
            }
            BasicBlockExit::EndFinally(finally_block) => {
                BasicBlockExit::EndFinally(swap(finally_block))
            }
        }
    }

    pub fn jump_targets(&self) -> Vec<usize> {
        match self {
            BasicBlockExit::Jump(target) => vec![*target],
            BasicBlockExit::Cond(_, true_target, false_target) => vec![*true_target, *false_target],
            BasicBlockExit::ExitFn(_, _) => vec![],
            BasicBlockExit::SetTryAndCatch(try_target, _, _, _) => vec![*try_target],
            BasicBlockExit::PopCatch(catch_target, finally_target) => {
                vec![*catch_target, *finally_target]
            }
            BasicBlockExit::PopFinally(finally_target, _after_finally_target) => {
                vec![*finally_target]
            }
            BasicBlockExit::EndFinally(after_finally_target) => vec![*after_finally_target],
        }
    }
}

impl Default for BasicBlockExit {
    fn default() -> Self {
        BasicBlockExit::Jump(0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ExitType {
    Return,
    Throw,
}
