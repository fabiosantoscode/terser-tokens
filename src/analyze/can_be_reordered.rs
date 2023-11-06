use crate::basic_blocks::{BasicBlockInstruction, LHS};

impl BasicBlockInstruction {
    /// Whether this instruction can be reordered with other instructions.
    pub fn can_be_reordered(&self) -> bool {
        match self {
            BasicBlockInstruction::This
            | BasicBlockInstruction::BinOp(_, _, _)
            | BasicBlockInstruction::UnaryOp(_, _)
            | BasicBlockInstruction::ArgumentRead(_)
            | BasicBlockInstruction::ArgumentRest(_)
            | BasicBlockInstruction::TypeOf(_)
            | BasicBlockInstruction::TypeOfGlobal(_)
            | BasicBlockInstruction::Read(LHS::Local(_))
            | BasicBlockInstruction::Function(_)
            | BasicBlockInstruction::Ref(_)
            | BasicBlockInstruction::Undefined
            | BasicBlockInstruction::LitNumber(_)
            | BasicBlockInstruction::LitBool(_)
            | BasicBlockInstruction::PatternUnpack(_, _) => true,
            _ => false,
        }
    }
}
