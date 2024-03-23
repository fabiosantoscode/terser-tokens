use super::{ArrayElement, ArrayPatternPiece, ObjectKey, ObjectPatternPiece, ObjectProperty};

/// A usize that uniquely points to a function.
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Hash, PartialOrd, Ord, Eq, Default)]
pub struct FunctionId(pub usize);

/// A single instruction in a basic block. This can encode basic actions, free of syntatic sugar
/// and multiple ways of doing the same thing.
/// These cannot do control flow, but can do function calls, binops and other basic operations.
#[derive(Clone, PartialEq)]
pub enum Instruction {
    LitNumber(f64),
    LitBool(bool),
    LitString(String),
    Ref(usize),
    UnaryOp(swc_ecma_ast::UnaryOp, usize),
    BinOp(swc_ecma_ast::BinaryOp, usize, usize),
    /// (varname, is_incr)
    IncrDecr(LHS, IncrDecr),
    /// (varname, is_incr)
    IncrDecrPostfix(LHS, IncrDecr),
    Undefined,
    Null,
    This,
    TypeOf(usize),
    TypeOfGlobal(String),
    CaughtError,
    ForInOfValue,
    Array(Vec<ArrayElement>),
    /// __proto__, object props
    Object(Option<usize>, Vec<ObjectProperty>),
    /// maybe_extends
    CreateClass(Option<usize>),
    Super,
    ArrayPattern(usize, Vec<ArrayPatternPiece>),
    ObjectPattern(usize, Vec<ObjectPatternPiece>),
    /// pattern, index
    PatternUnpack(usize, usize),
    TempExit(TempExitType, usize),
    Phi(Vec<usize>),
    Function(FunctionId),
    /// callee, arguments
    Call(usize, Vec<usize>),
    /// callee, arguments
    New(usize, Vec<usize>),
    ArgumentRead(usize),
    ArgumentRest(usize),
    Read(LHS),
    Write(LHS, usize),
    Delete(LHS),
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Ord, PartialOrd, Eq, Hash)]
pub struct NonLocalId(pub usize);

/// Increment or decrement
#[derive(Clone, PartialEq, Debug)]
pub enum IncrDecr {
    Incr,
    Decr,
}

impl IncrDecr {
    pub fn as_float_incr(&self) -> f64 {
        match self {
            IncrDecr::Incr => 1.0,
            IncrDecr::Decr => -1.0,
        }
    }
    pub fn op_string(&self) -> &'static str {
        match self {
            IncrDecr::Incr => "++",
            IncrDecr::Decr => "--",
        }
    }
}

/// A left hand side expression. This can be used for assignments, typeof, and ++/--.
#[derive(Clone, PartialEq, Ord, Eq, PartialOrd)]
pub enum LHS {
    Local(usize),
    NonLocal(NonLocalId),
    Global(String),
    Member(Box<LHS>, ObjectKey),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TempExitType {
    Yield,
    YieldStar,
    Await,
}

impl Instruction {
    pub fn used_vars(&self) -> Vec<usize> {
        match self {
            Instruction::LitNumber(_) => vec![],
            Instruction::LitBool(_) => vec![],
            Instruction::LitString(_) => vec![],
            Instruction::Ref(id) => vec![*id],
            Instruction::UnaryOp(_, v) => vec![*v],
            Instruction::BinOp(_, l, r) => vec![*l, *r],
            Instruction::IncrDecr(v, _) => v.used_vars(),
            Instruction::IncrDecrPostfix(v, _) => v.used_vars(),
            Instruction::Phi(vars) => vars.iter().cloned().collect(),
            Instruction::Undefined => vec![],
            Instruction::Null => vec![],
            Instruction::This => vec![],
            Instruction::TypeOf(v) => vec![*v],
            Instruction::TypeOfGlobal(_) => vec![],
            Instruction::Array(elements) => elements
                .iter()
                .filter_map(|e| match e {
                    ArrayElement::Hole => None,
                    ArrayElement::Item(id) => Some(*id),
                    ArrayElement::Spread(id) => Some(*id),
                })
                .collect(),
            Instruction::Object(proto, props) => proto
                .iter()
                .cloned()
                .chain(
                    props
                        .iter()
                        .flat_map(|prop| match prop {
                            ObjectProperty::KeyValue(k, v) => vec![k.used_vars(), v.used_vars()],
                            ObjectProperty::Spread(spread_obj) => vec![vec![*spread_obj], vec![]],
                        })
                        .flatten(),
                )
                .collect(),
            Instruction::CreateClass(maybe_extends) => maybe_extends.iter().cloned().collect(),
            Instruction::Super => vec![],
            Instruction::ArrayPattern(input, _) => vec![*input],
            Instruction::ObjectPattern(input, pat_pieces) => {
                let mut res = vec![*input];
                for piece in pat_pieces.iter() {
                    if let ObjectPatternPiece::TakeComputedKey(id) = piece {
                        res.push(*id);
                    }
                }
                res
            }
            Instruction::PatternUnpack(base, _idx) => vec![*base],
            Instruction::TempExit(_, arg) => vec![*arg],
            Instruction::CaughtError => vec![],
            Instruction::ForInOfValue => vec![],
            Instruction::Function(_) => vec![],
            Instruction::Call(callee, args) | Instruction::New(callee, args) => {
                let mut res = Vec::with_capacity(args.len() + 1);
                res.push(*callee);
                res.extend(args);
                res
            }
            Instruction::ArgumentRead(_) => vec![],
            Instruction::ArgumentRest(_) => vec![],
            Instruction::Read(lhs) => lhs.used_vars(),
            Instruction::Write(lhs, val) => {
                let mut res = lhs.used_vars();
                res.push(*val);
                res
            }
            Instruction::Delete(lhs) => lhs.used_vars(),
        }
    }

    pub fn used_vars_mut(&mut self) -> Vec<&mut usize> {
        match self {
            Instruction::LitNumber(_) => vec![],
            Instruction::LitBool(_) => vec![],
            Instruction::LitString(_) => vec![],
            Instruction::Ref(id) => vec![id],
            Instruction::UnaryOp(_, v) => vec![v],
            Instruction::BinOp(_, l, r) => vec![l, r],
            Instruction::IncrDecr(v, _) => v.used_vars_mut(),
            Instruction::IncrDecrPostfix(v, _) => v.used_vars_mut(),
            Instruction::Phi(vars) => vars.iter_mut().collect(),
            Instruction::Undefined => vec![],
            Instruction::Null => vec![],
            Instruction::This => vec![],
            Instruction::TypeOf(v) => vec![v],
            Instruction::TypeOfGlobal(_) => vec![],
            Instruction::Array(elements) => elements
                .iter_mut()
                .filter_map(|e| match e {
                    ArrayElement::Hole => None,
                    ArrayElement::Item(id) => Some(id),
                    ArrayElement::Spread(id) => Some(id),
                })
                .collect(),
            Instruction::Object(proto, props) => proto
                .iter_mut()
                .map(|proto| proto)
                .chain(
                    props
                        .iter_mut()
                        .flat_map(|e| match e {
                            ObjectProperty::KeyValue(k, v) => {
                                vec![k.used_vars_mut(), v.used_vars_mut()]
                            }
                            ObjectProperty::Spread(spread_obj) => vec![vec![spread_obj], vec![]],
                        })
                        .flatten(),
                )
                .collect(),
            Instruction::CreateClass(optional_extends) => optional_extends.iter_mut().collect(),
            Instruction::Super => vec![],
            Instruction::ArrayPattern(input, _) => vec![input],
            Instruction::ObjectPattern(input, pieces) => {
                let mut res = vec![input];
                for piece in pieces.iter_mut() {
                    if let ObjectPatternPiece::TakeComputedKey(id) = piece {
                        res.push(id);
                    }
                }
                res
            }
            Instruction::PatternUnpack(base, _idx) => vec![base],
            Instruction::TempExit(_, arg) => vec![arg],
            Instruction::CaughtError => vec![],
            Instruction::ForInOfValue => vec![],
            Instruction::Function(_) => vec![],
            Instruction::Call(callee, args) | Instruction::New(callee, args) => {
                let mut res = Vec::with_capacity(args.len() + 1);
                res.push(callee);
                res.extend(args);
                res
            }
            Instruction::ArgumentRead(_) => vec![],
            Instruction::ArgumentRest(_) => vec![],
            Instruction::Read(lhs) => lhs.used_vars_mut(),
            Instruction::Write(lhs, val) => {
                let mut res = lhs.used_vars_mut();
                res.push(val);
                res
            }
            Instruction::Delete(lhs) => lhs.used_vars_mut(),
        }
    }

    pub fn get_read_nonlocal_id(&self) -> Option<usize> {
        self.get_read_lhs()?.get_nonlocal_id()
    }

    pub fn get_read_lhs(&self) -> Option<&LHS> {
        match self {
            Instruction::Read(lhs) => Some(lhs),
            Instruction::IncrDecr(lhs, _) => Some(lhs),
            Instruction::IncrDecrPostfix(lhs, _) => Some(lhs),
            _ => None,
        }
    }

    pub fn get_written_lhs(&self) -> Option<&LHS> {
        match self {
            Instruction::Write(lhs, _) => Some(lhs),
            Instruction::IncrDecr(lhs, _) => Some(lhs),
            Instruction::IncrDecrPostfix(lhs, _) => Some(lhs),
            Instruction::Delete(lhs) => Some(lhs),
            _ => None,
        }
    }

    pub fn get_read_vars_and_nonlocals(&self) -> Vec<usize> {
        let mut vars = self.used_vars();

        if let Some(id) = self.get_read_nonlocal_id() {
            vars.push(id);
        }

        vars
    }

    #[allow(dead_code)]
    pub fn unwrap_ref(&self) -> usize {
        match self {
            Instruction::Ref(id) => *id,
            _ => panic!("Expected Ref"),
        }
    }

    pub(crate) fn is_immutable_primitive(&self) -> bool {
        match self {
            Instruction::Undefined
            | Instruction::Null
            | Instruction::LitBool(_)
            | Instruction::LitNumber(_)
            | Instruction::LitString(_) => true,
            _ => false,
        }
    }
}

impl LHS {
    pub fn used_vars(&self) -> Vec<usize> {
        match self {
            LHS::Local(v) => vec![*v],
            LHS::NonLocal(_) => vec![],
            LHS::Global(_) => vec![],
            LHS::Member(base, memb) => {
                let mut res = base.used_vars();
                res.extend(memb.used_vars());
                res
            }
        }
    }

    pub fn used_vars_mut(&mut self) -> Vec<&mut usize> {
        match self {
            LHS::Local(v) => vec![v],
            LHS::NonLocal(_) => vec![],
            LHS::Global(_) => vec![],
            LHS::Member(base, memb) => {
                let mut res = base.used_vars_mut();
                res.extend(memb.used_vars_mut());
                res
            }
        }
    }

    pub fn var_or_nonlocal_base(&self) -> Option<usize> {
        match self {
            LHS::Local(v) => Some(*v),
            LHS::NonLocal(id) => Some(id.0),
            LHS::Global(_) => None,
            LHS::Member(memb, _) => memb.var_or_nonlocal_base(),
        }
    }

    pub fn get_nonlocal_id(&self) -> Option<usize> {
        match self {
            LHS::NonLocal(id) => Some(id.0),
            LHS::Member(base, _) => base.get_nonlocal_id(),
            _ => None,
        }
    }
}

pub fn identifier_needs_quotes(key: &str) -> bool {
    match key {
        "" => true,
        "await" | "break" | "case" | "catch" | "class" | "const" | "continue" | "debugger"
        | "default" | "delete" | "do" | "else" | "enum" | "export" | "extends" | "false"
        | "finally" | "for" | "function" | "if" | "import" | "in" | "instanceof" | "new"
        | "null" | "return" | "super" | "switch" | "this" | "throw" | "true" | "try" | "typeof"
        | "var" | "void" | "while" | "with" | "yield" | "let" | "static" | "implements"
        | "interface" | "package" | "private" | "protected" | "public" | "as" | "async"
        | "from" | "get" | "meta" | "of" | "set" | "target" => true,
        _ => !key.chars().enumerate().all(|(index, c)| match c {
            'a'..='z' | 'A'..='Z' | '_' | '$' => true,
            '0'..='9' if index > 0 => true,
            _ => false,
        }),
    }
}
