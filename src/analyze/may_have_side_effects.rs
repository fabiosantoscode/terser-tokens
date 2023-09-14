use crate::basic_blocks::BasicBlockInstruction;

impl BasicBlockInstruction {
    pub fn may_have_side_effects(&self) -> bool {
        match self {
            BasicBlockInstruction::LitNumber(_) => false,
            BasicBlockInstruction::LitBool(_) => false,
            BasicBlockInstruction::Ref(_) => false,
            BasicBlockInstruction::BinOp(_, _, _) => true, // may throw due to bigint
            BasicBlockInstruction::Undefined => false,
            BasicBlockInstruction::This => false,
            BasicBlockInstruction::CaughtError => false,
            BasicBlockInstruction::Array(_) => false,
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
