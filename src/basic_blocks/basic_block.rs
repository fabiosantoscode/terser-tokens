use super::basic_block_group::FunctionId;
use std::{
    collections::HashMap,
    fmt::{Debug, Formatter},
};

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

    pub fn jump_targets(&self) -> Vec<usize> {
        self.exit.jump_targets()
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
        let swap = |x: &usize| swap_key.get(&x).unwrap_or(&x).clone();

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

    fn jump_targets(&self) -> Vec<usize> {
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

#[derive(Clone, PartialEq)]
pub enum BasicBlockInstruction {
    LitNumber(f64),
    Ref(usize),
    BinOp(String, usize, usize),
    Undefined,
    This,
    CaughtError,
    Array(Vec<ArrayElement>),
    TempExit(TempExitType, usize),
    Phi(Vec<usize>),
    Function(FunctionId),
    Call(usize, Vec<usize>),
    ArgumentRead(usize),
    ArgumentRest(usize),
}

#[derive(Clone, PartialEq)]
pub enum ArrayElement {
    Hole,
    Item(usize),
    Spread(usize),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TempExitType {
    Yield,
    YieldStar,
    Await,
}

impl BasicBlockInstruction {
    pub fn used_vars_mut(&mut self) -> Vec<&mut usize> {
        match self {
            BasicBlockInstruction::LitNumber(_) => vec![],
            BasicBlockInstruction::Ref(id) => vec![id],
            BasicBlockInstruction::BinOp(_, l, r) => vec![l, r],
            BasicBlockInstruction::Phi(vars) => vars.iter_mut().collect(),
            BasicBlockInstruction::Undefined => vec![],
            BasicBlockInstruction::This => vec![],
            BasicBlockInstruction::Array(elements) => elements
                .iter_mut()
                .filter_map(|e| match e {
                    ArrayElement::Hole => None,
                    ArrayElement::Item(id) => Some(id),
                    ArrayElement::Spread(id) => Some(id),
                })
                .collect(),
            BasicBlockInstruction::Function(_) => vec![],
            BasicBlockInstruction::Call(callee, args) => {
                let mut res = vec![callee];
                res.extend(args);
                res
            }
            BasicBlockInstruction::ArgumentRead(_) => vec![],
            BasicBlockInstruction::ArgumentRest(_) => vec![],
            BasicBlockInstruction::TempExit(_, arg) => vec![arg],
            BasicBlockInstruction::CaughtError => vec![],
        }
    }

    pub fn unwrap_ref(&self) -> usize {
        match self {
            BasicBlockInstruction::Ref(id) => *id,
            _ => panic!("Expected Ref"),
        }
    }
}

impl Debug for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{\n")?;
        for (id, node) in &self.instructions {
            write!(f, "    ${} = {:?}\n", id, node)?;
        }
        write!(f, "    exit = {:?}\n", &self.exit)?;
        write!(f, "}}")
    }
}

impl Debug for BasicBlockInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BasicBlockInstruction::LitNumber(num) => {
                write!(f, "{}", num)
            }
            BasicBlockInstruction::BinOp(op, l, r) => {
                write!(f, "${} {} ${}", l, op, r)
            }
            BasicBlockInstruction::Ref(id) => {
                write!(f, "${}", id)
            }
            BasicBlockInstruction::Undefined => {
                write!(f, "undefined")
            }
            BasicBlockInstruction::This => {
                write!(f, "this")
            }
            BasicBlockInstruction::Array(elements) => {
                write!(
                    f,
                    "[{}]",
                    elements
                        .iter()
                        .map(|e| match e {
                            ArrayElement::Hole => format!(","),
                            ArrayElement::Item(id) => format!("${},", id),
                            ArrayElement::Spread(id) => format!("...${},", id),
                        })
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            BasicBlockInstruction::Function(id) => {
                write!(f, "{:?}", id)
            }
            BasicBlockInstruction::Call(callee, args) => {
                write!(
                    f,
                    "call ${}({})",
                    callee,
                    args.iter()
                        .map(|a| format!("${}", a))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            BasicBlockInstruction::ArgumentRead(idx) => {
                write!(f, "arguments[{}]", idx)
            }
            BasicBlockInstruction::ArgumentRest(idx) => {
                write!(f, "arguments[{}...]", idx)
            }
            BasicBlockInstruction::TempExit(exit_type, arg) => {
                write!(f, "{:?} ${}", exit_type, arg)
            }

            BasicBlockInstruction::Phi(vars) => {
                write!(
                    f,
                    "either({})",
                    vars.iter()
                        .map(|v| format!("${}", v))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            BasicBlockInstruction::CaughtError => {
                write!(f, "caught_error()")
            }
        }
    }
}

impl Debug for BasicBlockExit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BasicBlockExit::Cond(cond, cons, alt) => {
                write!(f, "cond ${} ? jump @{} : jump @{}", cond, cons, alt)
            }
            BasicBlockExit::Jump(to) => {
                write!(f, "jump @{}", to)
            }
            BasicBlockExit::ExitFn(exit_type, val) => match exit_type {
                ExitType::Return => {
                    write!(f, "return ${}", val)
                }
                ExitType::Throw => {
                    write!(f, "throw ${}", val)
                }
            },
            BasicBlockExit::SetTryAndCatch(try_block, catch_block, finally_block, after_block) => {
                write!(
                    f,
                    "try @{} catch @{} finally @{} after @{}",
                    try_block, catch_block, finally_block, after_block
                )
            }
            BasicBlockExit::PopCatch(catch_block, finally_or_after) => {
                write!(
                    f,
                    "error ? jump @{} : jump @{}",
                    catch_block, finally_or_after
                )
            }
            BasicBlockExit::PopFinally(finally_block, after_block) => {
                write!(f, "finally @{} after @{}", finally_block, after_block)
            }
            BasicBlockExit::EndFinally(after_block) => {
                write!(f, "end finally after @{}", after_block)
            }
        }
    }
}
