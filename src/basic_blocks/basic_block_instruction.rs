use super::FunctionId;

#[derive(Clone, PartialEq)]
pub enum BasicBlockInstruction {
    LitNumber(f64),
    LitBool(bool),
    Ref(usize),
    BinOp(swc_ecma_ast::BinaryOp, usize, usize),
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
    ReadNonLocal(NonLocalId),
    WriteNonLocal(NonLocalId, usize),
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Ord, PartialOrd, Eq, Hash)]
pub struct NonLocalId(pub usize);

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
            BasicBlockInstruction::LitBool(_) => vec![],
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
            BasicBlockInstruction::CaughtError => vec![],
            BasicBlockInstruction::Function(_) => vec![],
            BasicBlockInstruction::Call(callee, args) => {
                let mut res = vec![callee];
                res.extend(args);
                res
            }
            BasicBlockInstruction::ArgumentRead(_) => vec![],
            BasicBlockInstruction::ArgumentRest(_) => vec![],
            BasicBlockInstruction::ReadNonLocal(_) => vec![],
            BasicBlockInstruction::WriteNonLocal(_, val) => vec![val],
        }
    }

    pub fn used_vars(&self) -> Vec<usize> {
        match self {
            BasicBlockInstruction::LitNumber(_) => vec![],
            BasicBlockInstruction::LitBool(_) => vec![],
            BasicBlockInstruction::Ref(id) => vec![*id],
            BasicBlockInstruction::BinOp(_, l, r) => vec![*l, *r],
            BasicBlockInstruction::Phi(vars) => vars.iter().cloned().collect(),
            BasicBlockInstruction::Undefined => vec![],
            BasicBlockInstruction::This => vec![],
            BasicBlockInstruction::Array(elements) => elements
                .iter()
                .filter_map(|e| match e {
                    ArrayElement::Hole => None,
                    ArrayElement::Item(id) => Some(*id),
                    ArrayElement::Spread(id) => Some(*id),
                })
                .collect(),
            BasicBlockInstruction::TempExit(_, arg) => vec![*arg],
            BasicBlockInstruction::CaughtError => vec![],
            BasicBlockInstruction::Function(_) => vec![],
            BasicBlockInstruction::Call(callee, args) => {
                let mut res = vec![*callee];
                res.extend(args);
                res
            }
            BasicBlockInstruction::ArgumentRead(_) => vec![],
            BasicBlockInstruction::ArgumentRest(_) => vec![],
            BasicBlockInstruction::ReadNonLocal(_) => vec![],
            BasicBlockInstruction::WriteNonLocal(_, val) => vec![*val],
        }
    }

    #[allow(dead_code)]
    pub fn unwrap_ref(&self) -> usize {
        match self {
            BasicBlockInstruction::Ref(id) => *id,
            _ => panic!("Expected Ref"),
        }
    }
}
