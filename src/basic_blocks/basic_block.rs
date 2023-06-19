use std::fmt::{Debug, Formatter};

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

    pub fn extend(&mut self, other: Self) {
        self.instructions.extend(other.instructions);
        self.exit = other.exit;
    }

    pub fn outgoing_edges(&self) -> Vec<usize> {
        match self.exit {
            BasicBlockExit::Jump(target) => vec![target],
            BasicBlockExit::Cond(_, true_target, false_target) => vec![true_target, false_target],
            BasicBlockExit::ExitFn(_, _) => vec![],
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum BasicBlockExit {
    Jump(usize),
    Cond(usize, usize, usize),
    ExitFn(ExitType, usize),
}

impl BasicBlockExit {
    pub(crate) fn subtract_labels(&self, eliminated_count: usize) -> BasicBlockExit {
        match self {
            BasicBlockExit::Jump(target) => BasicBlockExit::Jump(*target - eliminated_count),
            BasicBlockExit::Cond(cond, true_target, false_target) => BasicBlockExit::Cond(
                *cond,
                *true_target - eliminated_count,
                *false_target - eliminated_count,
            ),
            BasicBlockExit::ExitFn(exit_type, target) => {
                BasicBlockExit::ExitFn(exit_type.clone(), *target - eliminated_count)
            }
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
}

#[derive(Clone, PartialEq)]
pub enum BasicBlockInstruction {
    LitNumber(f64),
    Ref(usize),
    BinOp(String, usize, usize),
    Undefined,
    This,
    Array(Vec<ArrayElement>),
    TempExit(TempExitType, usize),
    Phi(Vec<usize>),
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
            BasicBlockInstruction::TempExit(_, arg) => vec![arg],
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
            },
        }
    }
}
