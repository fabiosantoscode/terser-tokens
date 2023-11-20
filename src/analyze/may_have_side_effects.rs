use crate::basic_blocks::{ArrayElement, BasicBlockInstruction, ObjectProp, LHS};

impl BasicBlockInstruction {
    /// Will this instruction throw an error or change the state of the program besides assigning its variable?
    pub fn may_have_side_effects(&self) -> bool {
        match self {
            BasicBlockInstruction::LitNumber(_) => false,
            BasicBlockInstruction::LitBool(_) => false,
            BasicBlockInstruction::LitString(_) => false,
            BasicBlockInstruction::Ref(_) => false,
            BasicBlockInstruction::UnaryOp(_, _) => false,
            // may throw due to bigint
            BasicBlockInstruction::BinOp(_, _, _) => true,
            // Can throw a conversion error (some_symbol++)
            BasicBlockInstruction::IncrDecr(_, _) => true,
            BasicBlockInstruction::IncrDecrPostfix(_, _) => true,
            BasicBlockInstruction::Undefined => false,
            BasicBlockInstruction::Null => false,
            BasicBlockInstruction::This => false,
            BasicBlockInstruction::TypeOf(_) => false,
            BasicBlockInstruction::TypeOfGlobal(_) => false,
            BasicBlockInstruction::CaughtError => false,
            BasicBlockInstruction::ForInOfValue => false,
            // may throw due to unspreadable array items
            BasicBlockInstruction::Array(items) => {
                items.iter().any(|it| matches!(it, ArrayElement::Spread(_)))
            }
            // may throw due to unspreadable object items
            BasicBlockInstruction::Object(_, props) => {
                props.iter().any(|p| matches!(p, ObjectProp::Spread(_)))
            }
            // extending things like "1" or "undefined" can throw
            BasicBlockInstruction::CreateClass(_) => true,
            // may throw due to unspreadable array items
            BasicBlockInstruction::ArrayPattern(_, _) => true,
            // may throw due to unspreadable object items
            BasicBlockInstruction::ObjectPattern(_, _) => true,
            // just unpacking what's conceptually already there
            BasicBlockInstruction::PatternUnpack(_, _) => false,
            BasicBlockInstruction::TempExit(_, _) => true,
            // don't mess with phi
            BasicBlockInstruction::Phi(_) => true,
            BasicBlockInstruction::Function(_) => false,
            BasicBlockInstruction::Call(_, _) => true,
            // may throw but shouldn't
            BasicBlockInstruction::New(_, _) => true,
            BasicBlockInstruction::ArgumentRead(_) => false,
            BasicBlockInstruction::ArgumentRest(_) => false,
            BasicBlockInstruction::Read(lhs) => lhs.read_may_have_side_effects(),
            BasicBlockInstruction::Write(_, _) => true,
        }
    }
}

impl LHS {
    fn read_may_have_side_effects(&self) -> bool {
        match self {
            LHS::Local(_) => false,
            LHS::NonLocal(_) => false,
            LHS::Global(_) => true,
            LHS::Member(_, _) => true,
        }
    }
}
