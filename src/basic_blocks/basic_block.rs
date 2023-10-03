use super::BasicBlockInstruction;

/// A basic block encapsulates a control-free sequence of instructions. It contains an "exit" which encodes control flow.
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

    pub(crate) fn iter(&self) -> impl Iterator<Item = (usize, &BasicBlockInstruction)> {
        self.instructions
            .iter()
            .map(|(varname, ins)| (*varname, ins))
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = (usize, &mut BasicBlockInstruction)> {
        self.instructions
            .iter_mut()
            .map(|(varname, ins)| (*varname, ins))
    }

    pub(crate) fn iter_varnames_mut(&mut self) -> impl Iterator<Item = &mut usize> {
        self.instructions.iter_mut().map(|(varname, _)| varname)
    }
}

/// Conceptually, an exit occurs after the instructions in a block. It denotes what happens to control flow.
/// Ranges, such as the numbers in Loop(), are inclusive.
#[derive(Clone, PartialEq)]
pub enum BasicBlockExit {
    /// unconditional jump to target
    Jump(usize),
    /// (cond_var, true_target, false_target). If cond_var is true, go to true_target..true_target_end. Otherwise, go to false_target..false_target_end.
    Cond(usize, usize, usize, usize, usize),
    /// (start, end). Loop from start to end until something jumps out.
    Loop(usize, usize),
    /// Just like Jump() but signals a break out of a loop or block
    Break(usize),
    /// Just like a backwards Jump() but signals a continue in a loop
    Continue(usize),
    /// (exit_type, returned). Used when we see "return" or "throw". "returned" holds the value returned or thrown.
    ExitFn(ExitType, usize),
    /// (try_block, catch_block, finally_block, end_finally_block). Used when we see "try {". It just goes to try_block, the rest is book-keeping.
    SetTryAndCatch(usize, usize, usize, usize),
    /// (catch_block, finally). Used when we see "} catch". If we had an exception, we go to catch_block. Otherwise, we go to finally.
    PopCatch(usize, usize),
    /// (finally_block, after_finally_block). Used when we see "} finally". We go to the finally-block.
    PopFinally(usize, usize),
    /// (after_finally_block). Used when we see "}" after a finally block. We go to the after_finally_block.
    EndFinally(usize),
}

impl BasicBlockExit {
    pub fn jump_targets(&self) -> Vec<usize> {
        match self {
            BasicBlockExit::Jump(target)
            | BasicBlockExit::Break(target)
            | BasicBlockExit::Continue(target) => vec![*target],
            BasicBlockExit::Cond(
                _,
                true_target,
                _true_target_end,
                false_target,
                _false_target_end,
            ) => vec![*true_target, *false_target],
            BasicBlockExit::Loop(start, _end) => vec![*start],
            BasicBlockExit::ExitFn(_, _) => vec![],
            BasicBlockExit::SetTryAndCatch(jump_forward, _, _, _)
            | BasicBlockExit::PopFinally(jump_forward, _)
            | BasicBlockExit::EndFinally(jump_forward) => vec![*jump_forward],
            BasicBlockExit::PopCatch(catch_target, finally_target) => {
                vec![*catch_target, *finally_target]
            }
        }
    }

    pub fn used_vars(&self) -> Vec<usize> {
        match self {
            BasicBlockExit::Cond(cond_var, _, _, _, _) => vec![*cond_var],
            BasicBlockExit::ExitFn(_, returned) => vec![*returned],
            BasicBlockExit::Jump(_)
            | BasicBlockExit::Break(_)
            | BasicBlockExit::Continue(_)
            | BasicBlockExit::SetTryAndCatch(_, _, _, _)
            | BasicBlockExit::PopCatch(_, _)
            | BasicBlockExit::PopFinally(_, _)
            | BasicBlockExit::EndFinally(_)
            | BasicBlockExit::Loop(_, _) => vec![],
        }
    }

    pub(crate) fn used_vars_mut(&mut self) -> Vec<&mut usize> {
        match self {
            BasicBlockExit::Cond(cond_var, _, _, _, _) => vec![cond_var],
            BasicBlockExit::ExitFn(_, returned) => vec![returned],
            BasicBlockExit::Jump(_)
            | BasicBlockExit::Break(_)
            | BasicBlockExit::Continue(_)
            | BasicBlockExit::SetTryAndCatch(_, _, _, _)
            | BasicBlockExit::PopCatch(_, _)
            | BasicBlockExit::PopFinally(_, _)
            | BasicBlockExit::EndFinally(_)
            | BasicBlockExit::Loop(_, _) => vec![],
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
