use crate::basic_blocks::{ArrayElement, BasicBlockInstruction, ObjectProp};

impl BasicBlockInstruction {
    /// Will this instruction throw an error or change the state of the program besides assigning its variable?
    pub fn may_have_side_effects(&self) -> bool {
        match self {
            BasicBlockInstruction::LitNumber(_) => false,
            BasicBlockInstruction::LitBool(_) => false,
            BasicBlockInstruction::LitString(_) => false,
            BasicBlockInstruction::Ref(_) => false,
            BasicBlockInstruction::BinOp(_, _, _) => true, // may throw due to bigint
            BasicBlockInstruction::Undefined => false,
            BasicBlockInstruction::This => false,
            BasicBlockInstruction::CaughtError => false,
            BasicBlockInstruction::Array(items) => {
                // may throw due to unspreadable array items
                items.iter().any(|it| matches!(it, ArrayElement::Spread(_)))
            }
            BasicBlockInstruction::Object(_, props) => {
                // may throw due to unspreadable object items
                props.iter().any(|p| matches!(p, ObjectProp::Spread(_)))
            }
            BasicBlockInstruction::TempExit(_, _) => true,
            BasicBlockInstruction::Phi(_) => true, // don't mess with phi
            BasicBlockInstruction::Function(_) => false,
            BasicBlockInstruction::Call(_, _) => true,
            BasicBlockInstruction::ArgumentRead(_) => false,
            BasicBlockInstruction::ArgumentRest(_) => false,
            BasicBlockInstruction::ReadNonLocal(_) => false,
            BasicBlockInstruction::WriteNonLocal(_, _) => true,
        }
    }
}
