use crate::basic_blocks::{ArrayElement, Instruction, ObjectProperty, LHS};

impl Instruction {
    /// Will this instruction throw an error or change the state of the program besides assigning its variable?
    pub fn may_have_side_effects(&self) -> bool {
        match self {
            Instruction::LitNumber(_) => false,
            Instruction::LitBigInt(_) => false,
            Instruction::LitBool(_) => false,
            Instruction::LitString(_) => false,
            Instruction::LitRegExp(_, _) => false,
            Instruction::Ref(_) => false,
            Instruction::UnaryOp(_, _) => false,
            // may throw due to bigint
            Instruction::BinOp(_, _, _) => true,
            Instruction::PrivateIn(_, _) => false,
            // Can throw a conversion error (some_symbol++)
            Instruction::IncrDecr(_, _) => true,
            Instruction::IncrDecrPostfix(_, _) => true,
            Instruction::Undefined => false,
            Instruction::Null => false,
            Instruction::This => false,
            Instruction::TypeOf(_) => false,
            Instruction::TypeOfGlobal(_) => false,
            Instruction::CaughtError => false,
            Instruction::ForInOfValue => false,
            // may throw due to unspreadable array items
            Instruction::Array(items) => {
                items.iter().any(|it| matches!(it, ArrayElement::Spread(_)))
            }
            // may throw due to unspreadable object items
            Instruction::Object(_, props) => {
                props.iter().any(|p| matches!(p, ObjectProperty::Spread(_)))
            }
            // Moving super around is dangerous but it doesn't have side effects itself
            Instruction::Super => false,
            // extending things like "1" or "undefined" can throw
            Instruction::CreateClass(extends) => extends.is_some(),
            // may throw due to unspreadable array items
            Instruction::ArrayPattern(_, _) => true,
            // may throw due to unspreadable object items
            Instruction::ObjectPattern(_, _) => true,
            // we shouldn't move this around
            Instruction::PatternUnpack(_, _) => true,
            Instruction::TempExit(_, _) => true,
            // don't mess with phi
            Instruction::Phi(_) => true,
            Instruction::Function(_) => false,
            Instruction::Call(_, _) => true,
            // may throw but shouldn't
            Instruction::New(_, _) => true,
            Instruction::ArgumentRead(_) => false,
            Instruction::ArgumentRest(_) => false,
            Instruction::Read(lhs) => lhs.read_may_have_side_effects(),
            Instruction::Write(_, _) => true,
            Instruction::Delete(_) => true,
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
