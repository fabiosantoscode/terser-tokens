use std::{fmt::{Debug, Formatter}};

#[derive(Clone, Default)]
pub struct SsaAst {
    pub body: Vec<(usize, SsaAstNode)>,
    pub exit: SsaExit,
}

impl SsaAst {
    pub fn new(body: Vec<(usize, SsaAstNode)>, exit: SsaExit) -> Self {
        Self { body, exit }
    }

    pub fn extend(&mut self, other: Self) {
        self.body.extend(other.body);
        self.exit = other.exit;
    }

    pub fn outgoing_edges(&self) -> Vec<usize>{
        match self.exit {
            SsaExit::Jump(target) => vec![target],
            SsaExit::Cond(_, true_target, false_target) => vec![true_target, false_target],
            SsaExit::ExitFn(_, _) => vec![],
        }
    }
}

#[derive(Clone)]
pub enum SsaExit {
    Jump(usize),
    Cond(usize, usize, usize),
    ExitFn(ExitType, usize),
}

impl SsaExit {
    pub(crate) fn subtract_labels(&self, eliminated_count: usize) -> SsaExit {
        match self {
            SsaExit::Jump(target) => SsaExit::Jump(*target - eliminated_count),
            SsaExit::Cond(cond, true_target, false_target) => {
                SsaExit::Cond(
                    *cond,
                    *true_target - eliminated_count,
                    *false_target - eliminated_count,
                )
            }
            SsaExit::ExitFn(exit_type, target) => {
                SsaExit::ExitFn(exit_type.clone(), *target - eliminated_count)
            }
        }
    }
}

impl Default for SsaExit {
    fn default() -> Self {
        SsaExit::Jump(0)
    }
}

#[derive(Clone, Debug)]
pub enum ExitType {
    Return,
}

#[derive(Clone)]
pub enum SsaAstNode {
    LitNumber(f64),
    Ref(usize),
    BinOp(String, usize, usize),
    Phi(Vec<usize>),
    Undefined,
}

impl SsaAstNode {
    pub fn used_vars(&self) -> Vec<usize> {
        match self {
            SsaAstNode::LitNumber(_) => vec![],
            SsaAstNode::Ref(id) => vec![*id],
            SsaAstNode::BinOp(_, l, r) => vec![*l, *r],
            SsaAstNode::Phi(vars) => vars.clone(),
            SsaAstNode::Undefined => vec![],
        }
    }
}

impl Debug for SsaAst {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{\n")?;
        for (id, node) in &self.body {
            write!(f, "    ${} = {:?}\n", id, node)?;
        }
        write!(f, "    exit = {:?}\n", &self.exit)?;
        write!(f, "}}")
    }
}

impl Debug for SsaAstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SsaAstNode::LitNumber(num) => {
                write!(f, "{}", num)
            }
            SsaAstNode::BinOp(op, l, r) => {
                write!(f, "${} {} ${}", l, op, r)
            }
            SsaAstNode::Phi(vars) => {
                write!(
                    f,
                    "either({})",
                    vars.iter()
                        .map(|v| format!("${}", v))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            SsaAstNode::Ref(id) => {
                write!(f, "${}", id)
            }
            SsaAstNode::Undefined => {
                write!(f, "undefined")
            }
        }
    }
}

impl Debug for SsaExit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SsaExit::Cond(cond, cons, alt) => {
                write!(f, "cond ${} ? jump @{} : jump @{}", cond, cons, alt)
            }
            SsaExit::Jump(to) => {
                write!(f, "jump @{}", to)
            }
            SsaExit::ExitFn(exit_type, val) => match exit_type {
                ExitType::Return => {
                    write!(f, "return ${}", val)
                }
            },
        }
    }
}
