use crate::basic_blocks::{Instruction, LHS};

impl Instruction {
    /// Whether this instruction can be reordered with other instructions.
    pub fn can_be_reordered(&self) -> bool {
        match self {
            Instruction::This
            | Instruction::BinOp(_, _, _)
            | Instruction::UnaryOp(_, _)
            | Instruction::ArgumentRead(_)
            | Instruction::ArgumentRest(_)
            | Instruction::TypeOf(_)
            | Instruction::TypeOfGlobal(_)
            | Instruction::Read(LHS::Local(_))
            | Instruction::Function(_)
            | Instruction::Ref(_)
            | Instruction::Undefined
            | Instruction::LitNumber(_)
            | Instruction::LitBigInt(_)
            | Instruction::LitBool(_)
            | Instruction::LitString(_)
            | Instruction::LitRegExp(_, _)
            | Instruction::PatternUnpack(_, _) => true,
            _ => false,
        }
    }
}
