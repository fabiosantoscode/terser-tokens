use super::{BasicBlockInstruction, FunctionId, ObjectKey};

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
    /// (looped_var, for_in_of_kind, start, end). A for-in or for-of loop. looped_var is the variable that gets assigned to.
    /// https://262.ecma-international.org/#sec-runtime-semantics-forin-div-ofbodyevaluation-lhs-stmt-iterator-lhskind-labelset
    ForInOfLoop(usize, ForInOfKind, usize, usize),
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
    /// (class_var, start, end). Start a class. Inside it we can use Class* exits.
    ClassStart(usize, usize, usize),
    /// (Class only): (prop, next). Create a property
    ClassProperty(ClassProperty, usize),
    /// (Class only): (start, end). Start a static block
    ClassPushStaticBlock(usize, usize),
    /// (Class only): (next_member) End a static block.
    ClassPopStaticBlock(usize),
    /// (Class only): (next_block) Ends a class
    ClassEnd(usize),
}

impl BasicBlockExit {
    pub fn used_vars(&self) -> Vec<usize> {
        match self {
            BasicBlockExit::Cond(cond_var, _, _, _, _) => vec![*cond_var],
            BasicBlockExit::ExitFn(_, returned) => vec![*returned],
            BasicBlockExit::ForInOfLoop(looped_var, _, _, _) => vec![*looped_var],
            BasicBlockExit::ClassStart(class_var, _, _) => vec![*class_var],
            BasicBlockExit::ClassProperty(prop, _) => prop.used_vars(),
            BasicBlockExit::Jump(_)
            | BasicBlockExit::Break(_)
            | BasicBlockExit::Continue(_)
            | BasicBlockExit::SetTryAndCatch(_, _, _, _)
            | BasicBlockExit::PopCatch(_, _)
            | BasicBlockExit::PopFinally(_, _)
            | BasicBlockExit::EndFinally(_)
            | BasicBlockExit::ClassPushStaticBlock(_, _)
            | BasicBlockExit::ClassPopStaticBlock(_)
            | BasicBlockExit::ClassEnd(_)
            | BasicBlockExit::Loop(_, _) => vec![],
        }
    }

    pub(crate) fn used_vars_mut(&mut self) -> Vec<&mut usize> {
        match self {
            BasicBlockExit::Cond(cond_var, _, _, _, _) => vec![cond_var],
            BasicBlockExit::ExitFn(_, returned) => vec![returned],
            BasicBlockExit::ForInOfLoop(looped_var, _, _, _) => vec![looped_var],
            BasicBlockExit::ClassStart(class_var, _, _) => vec![class_var],
            BasicBlockExit::ClassProperty(prop, _) => prop.used_vars_mut(),
            BasicBlockExit::Jump(_)
            | BasicBlockExit::Break(_)
            | BasicBlockExit::Continue(_)
            | BasicBlockExit::SetTryAndCatch(_, _, _, _)
            | BasicBlockExit::PopCatch(_, _)
            | BasicBlockExit::PopFinally(_, _)
            | BasicBlockExit::EndFinally(_)
            | BasicBlockExit::ClassPushStaticBlock(_, _)
            | BasicBlockExit::ClassPopStaticBlock(_)
            | BasicBlockExit::ClassEnd(_)
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
pub struct ClassProperty {
    pub is_static: bool,
    pub is_private: bool,
    pub key: ObjectKey,
    pub value: ClassPropertyValue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ClassPropertyValue {
    Property(usize),
    Method(FunctionId),
}

impl ClassProperty {
    pub fn used_vars(&self) -> Vec<usize> {
        let key = match &self.key {
            ObjectKey::KeyValue(_) => vec![],
            ObjectKey::Private(_) => vec![],
            ObjectKey::Computed(var) => vec![*var],
        };
        let value = match &self.value {
            ClassPropertyValue::Property(var) => vec![*var],
            ClassPropertyValue::Method(_fn) => vec![],
        };

        key.into_iter().chain(value.into_iter()).collect()
    }

    pub fn used_vars_mut(&mut self) -> Vec<&mut usize> {
        let key = match &mut self.key {
            ObjectKey::KeyValue(_) => vec![],
            ObjectKey::Private(_) => vec![],
            ObjectKey::Computed(var) => vec![var],
        };
        let value = match &mut self.value {
            ClassPropertyValue::Property(var) => vec![var],
            ClassPropertyValue::Method(_fn) => vec![],
        };

        key.into_iter().chain(value.into_iter()).collect()
    }
}

#[repr(u8)]
#[derive(Clone, Debug, PartialEq, Copy)]
pub enum ExitType {
    Return,
    Throw,
}

#[repr(u8)]
#[derive(Clone, PartialEq, Copy)]
pub enum ForInOfKind {
    ForIn,
    ForOf,
    ForAwaitOf,
}
