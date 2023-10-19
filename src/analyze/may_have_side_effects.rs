use crate::basic_blocks::{ArrayElement, BasicBlockInstruction, ObjectProp};

impl BasicBlockInstruction {
    /// Will this instruction throw an error or change the state of the program besides assigning its variable?
    pub fn may_have_side_effects(&self) -> bool {
        match self {
            BasicBlockInstruction::LitNumber(_) => false,
            BasicBlockInstruction::LitBool(_) => false,
            BasicBlockInstruction::LitString(_) => false,
            BasicBlockInstruction::Ref(_) => false,
            // may throw due to bigint
            BasicBlockInstruction::BinOp(_, _, _) => true,
            BasicBlockInstruction::Undefined => false,
            BasicBlockInstruction::This => false,
            BasicBlockInstruction::CaughtError => false,
            // may throw due to unspreadable array items
            BasicBlockInstruction::Array(items) => {
                items.iter().any(|it| matches!(it, ArrayElement::Spread(_)))
            }
            // may throw due to unspreadable object items
            BasicBlockInstruction::Object(_, props) => {
                props.iter().any(|p| matches!(p, ObjectProp::Spread(_)))
            }
            // base may be nullish, prop may be getter
            BasicBlockInstruction::Member(_, _) => false,
            // may throw due to setter or nullish base, also changes a value
            BasicBlockInstruction::MemberSet(_, _, _) => true,
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
            BasicBlockInstruction::ArgumentRead(_) => false,
            BasicBlockInstruction::ArgumentRest(_) => false,
            BasicBlockInstruction::ReadNonLocal(_) => false,
            BasicBlockInstruction::WriteNonLocal(_, _) => true,
        }
    }
}
