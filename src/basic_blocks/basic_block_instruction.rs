use super::FunctionId;

/// A single instruction in a basic block. This can encode basic actions, free of syntatic sugar
/// and multiple ways of doing the same thing.
/// These cannot do control flow, but can do function calls, binops and other basic operations.
#[derive(Clone, PartialEq)]
pub enum BasicBlockInstruction {
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
    Object(Option<usize>, Vec<ObjectProp>),
    ArrayPattern(usize, Vec<ArrayPatternPiece>),
    ObjectPattern(usize, Vec<ObjectPatternPiece>),
    /// pattern, index
    PatternUnpack(usize, usize),
    TempExit(TempExitType, usize),
    Phi(Vec<usize>),
    Function(FunctionId),
    Call(usize, Vec<usize>),
    ArgumentRead(usize),
    ArgumentRest(usize),
    Read(LHS),
    Write(LHS, usize),
}

#[derive(Clone, PartialEq, Debug)]
pub enum ArrayPatternPiece {
    Item,
    Spread,
}

#[derive(Clone, PartialEq, Debug, Eq)]
pub enum ObjectPatternPiece {
    TakeKey(String),
    TakeComputedKey(usize),
    Spread,
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

impl ArrayElement {
    pub fn as_item(&self) -> Option<usize> {
        match self {
            ArrayElement::Item(it) => Some(*it),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum ObjectProp {
    KeyValue(String, usize),
    Computed(usize, usize),
    Spread(usize),
}

#[derive(Clone, PartialEq)]
pub enum ObjectMember {
    KeyValue(String),
    Private(String),
    Computed(usize),
}

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
#[derive(Clone, PartialEq)]
pub enum LHS {
    Local(usize),
    NonLocal(NonLocalId),
    Global(String),
    Member(Box<LHS>, ObjectMember),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TempExitType {
    Yield,
    YieldStar,
    Await,
}

impl BasicBlockInstruction {
    pub fn used_vars(&self) -> Vec<usize> {
        match self {
            BasicBlockInstruction::LitNumber(_) => vec![],
            BasicBlockInstruction::LitBool(_) => vec![],
            BasicBlockInstruction::LitString(_) => vec![],
            BasicBlockInstruction::Ref(id) => vec![*id],
            BasicBlockInstruction::UnaryOp(_, v) => vec![*v],
            BasicBlockInstruction::BinOp(_, l, r) => vec![*l, *r],
            BasicBlockInstruction::IncrDecr(v, _) => v.used_vars(),
            BasicBlockInstruction::IncrDecrPostfix(v, _) => v.used_vars(),
            BasicBlockInstruction::Phi(vars) => vars.iter().cloned().collect(),
            BasicBlockInstruction::Undefined => vec![],
            BasicBlockInstruction::Null => vec![],
            BasicBlockInstruction::This => vec![],
            BasicBlockInstruction::TypeOf(v) => vec![*v],
            BasicBlockInstruction::TypeOfGlobal(_) => vec![],
            BasicBlockInstruction::Array(elements) => elements
                .iter()
                .filter_map(|e| match e {
                    ArrayElement::Hole => None,
                    ArrayElement::Item(id) => Some(*id),
                    ArrayElement::Spread(id) => Some(*id),
                })
                .collect(),
            BasicBlockInstruction::Object(proto, props) => proto
                .iter()
                .cloned()
                .chain(props.iter().flat_map(|prop| match prop {
                    ObjectProp::KeyValue(_, val) => vec![*val],
                    ObjectProp::Computed(key, val) => vec![*key, *val],
                    ObjectProp::Spread(spread_obj) => vec![*spread_obj],
                }))
                .collect(),
            BasicBlockInstruction::ArrayPattern(input, _) => vec![*input],
            BasicBlockInstruction::ObjectPattern(input, _) => vec![*input],
            BasicBlockInstruction::PatternUnpack(base, _idx) => vec![*base],
            BasicBlockInstruction::TempExit(_, arg) => vec![*arg],
            BasicBlockInstruction::CaughtError => vec![],
            BasicBlockInstruction::ForInOfValue => vec![],
            BasicBlockInstruction::Function(_) => vec![],
            BasicBlockInstruction::Call(callee, args) => {
                let mut res = Vec::with_capacity(args.len() + 1);
                res.push(*callee);
                res.extend(args);
                res
            }
            BasicBlockInstruction::ArgumentRead(_) => vec![],
            BasicBlockInstruction::ArgumentRest(_) => vec![],
            BasicBlockInstruction::Read(lhs) => lhs.used_vars(),
            BasicBlockInstruction::Write(lhs, val) => {
                let mut res = lhs.used_vars();
                res.push(*val);
                res
            }
        }
    }

    pub fn used_vars_mut(&mut self) -> Vec<&mut usize> {
        match self {
            BasicBlockInstruction::LitNumber(_) => vec![],
            BasicBlockInstruction::LitBool(_) => vec![],
            BasicBlockInstruction::LitString(_) => vec![],
            BasicBlockInstruction::Ref(id) => vec![id],
            BasicBlockInstruction::UnaryOp(_, v) => vec![v],
            BasicBlockInstruction::BinOp(_, l, r) => vec![l, r],
            BasicBlockInstruction::IncrDecr(v, _) => v.used_vars_mut(),
            BasicBlockInstruction::IncrDecrPostfix(v, _) => v.used_vars_mut(),
            BasicBlockInstruction::Phi(vars) => vars.iter_mut().collect(),
            BasicBlockInstruction::Undefined => vec![],
            BasicBlockInstruction::Null => vec![],
            BasicBlockInstruction::This => vec![],
            BasicBlockInstruction::TypeOf(v) => vec![v],
            BasicBlockInstruction::TypeOfGlobal(_) => vec![],
            BasicBlockInstruction::Array(elements) => elements
                .iter_mut()
                .filter_map(|e| match e {
                    ArrayElement::Hole => None,
                    ArrayElement::Item(id) => Some(id),
                    ArrayElement::Spread(id) => Some(id),
                })
                .collect(),
            BasicBlockInstruction::Object(proto, props) => proto
                .iter_mut()
                .map(|proto| proto)
                .chain(props.iter_mut().flat_map(|e| match e {
                    ObjectProp::KeyValue(_, val) => vec![val],
                    ObjectProp::Computed(key, val) => vec![key, val],
                    ObjectProp::Spread(spread_obj) => vec![spread_obj],
                }))
                .collect(),
            BasicBlockInstruction::ArrayPattern(input, _) => vec![input],
            BasicBlockInstruction::ObjectPattern(input, _) => vec![input],
            BasicBlockInstruction::PatternUnpack(base, _idx) => vec![base],
            BasicBlockInstruction::TempExit(_, arg) => vec![arg],
            BasicBlockInstruction::CaughtError => vec![],
            BasicBlockInstruction::ForInOfValue => vec![],
            BasicBlockInstruction::Function(_) => vec![],
            BasicBlockInstruction::Call(callee, args) => {
                let mut res = Vec::with_capacity(args.len() + 1);
                res.push(callee);
                res.extend(args);
                res
            }
            BasicBlockInstruction::ArgumentRead(_) => vec![],
            BasicBlockInstruction::ArgumentRest(_) => vec![],
            BasicBlockInstruction::Read(lhs) => lhs.used_vars_mut(),
            BasicBlockInstruction::Write(lhs, val) => {
                let mut res = lhs.used_vars_mut();
                res.push(val);
                res
            }
        }
    }

    pub fn get_nonlocal_id_mut(&mut self) -> Option<&mut usize> {
        self.get_lhs_mut()?.get_nonlocal_id_mut()
    }

    pub fn get_nonlocal_id(&self) -> Option<usize> {
        self.get_lhs()?.get_nonlocal_id()
    }

    pub fn get_lhs(&self) -> Option<&LHS> {
        match self {
            BasicBlockInstruction::Read(lhs) => Some(lhs),
            BasicBlockInstruction::Write(lhs, _) => Some(lhs),
            BasicBlockInstruction::IncrDecr(lhs, _) => Some(lhs),
            BasicBlockInstruction::IncrDecrPostfix(lhs, _) => Some(lhs),
            _ => None,
        }
    }

    pub fn get_lhs_mut(&mut self) -> Option<&mut LHS> {
        match self {
            BasicBlockInstruction::Read(lhs) => Some(lhs),
            BasicBlockInstruction::Write(lhs, _) => Some(lhs),
            BasicBlockInstruction::IncrDecr(lhs, _) => Some(lhs),
            BasicBlockInstruction::IncrDecrPostfix(lhs, _) => Some(lhs),
            _ => None,
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

    pub fn get_nonlocal_id(&self) -> Option<usize> {
        match self {
            LHS::NonLocal(id) => Some(id.0),
            LHS::Member(base, _) => base.get_nonlocal_id(),
            _ => None,
        }
    }

    pub fn get_nonlocal_id_mut(&mut self) -> Option<&mut usize> {
        match self {
            LHS::NonLocal(id) => Some(&mut id.0),
            LHS::Member(base, _) => base.get_nonlocal_id_mut(),
            _ => None,
        }
    }
}

impl ObjectMember {
    pub fn used_vars(&self) -> Vec<usize> {
        match self {
            ObjectMember::KeyValue(_) => vec![],
            ObjectMember::Private(_) => vec![],
            ObjectMember::Computed(v) => vec![*v],
        }
    }

    pub fn used_vars_mut(&mut self) -> Vec<&mut usize> {
        match self {
            ObjectMember::KeyValue(_) => vec![],
            ObjectMember::Private(_) => vec![],
            ObjectMember::Computed(v) => vec![v],
        }
    }
}
