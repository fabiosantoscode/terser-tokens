use ordered_float::NotNan;

use crate::basic_blocks::FunctionId;

#[derive(Clone, PartialEq, Eq, Ord, PartialOrd)]
pub enum JsType {
    Undefined,
    Boolean,
    TheBoolean(bool),
    Number,
    TheNumber(NotNan<f64>),
    Function,
    TheFunction(FunctionId),
    Array,
    Any,
}

impl JsType {
    pub(crate) fn new_number(arg: f64) -> JsType {
        match NotNan::new(arg) {
            Ok(n) => JsType::TheNumber(n),
            _ => JsType::Number,
        }
    }

    pub fn is_numeric(&self) -> bool {
        match self {
            JsType::Number => true,
            JsType::TheNumber(_) => true,
            _ => false,
        }
    }

    pub fn is_nullish(&self) -> bool {
        match self {
            JsType::Undefined => true,
            _ => false,
        }
    }

    pub fn is_truthy(&self) -> Option<bool> {
        match self {
            JsType::Undefined => Some(false),
            JsType::Boolean => None,
            JsType::TheBoolean(b) => Some(*b),
            JsType::Number => None,
            JsType::TheNumber(n) => Some(n != &NotNan::new(0.0).unwrap()),
            JsType::Function => Some(true),
            JsType::TheFunction(_) => Some(true),
            JsType::Array => Some(true),
            JsType::Any => None,
        }
    }

    pub(crate) fn union_all<'it, It>(alternatives: It) -> JsType
    where
        It: IntoIterator<Item = &'it JsType> + Clone,
    {
        let mut alternatives = alternatives.into_iter();

        let mut result = alternatives
            .next()
            .expect("phi with no alternatives")
            .clone();

        for t in alternatives {
            result = result.union(t);
        }

        result
    }

    /// Returns a type that represents all values that are either `self` or `other`.
    pub(crate) fn union(&self, other: &JsType) -> JsType {
        if self == other {
            self.clone()
        } else {
            use JsType::*;

            match (self, other) {
                (TheBoolean(_) | Boolean, TheBoolean(_) | Boolean) => Boolean,
                (TheNumber(_) | Number, TheNumber(_) | Number) => Number,
                (TheFunction(_) | Function, TheFunction(_) | Function) => Function,
                _ => Any,
            }
        }
    }

    /// Returns a type that represents all values that are both `self` and `other`.
    /// If the types are incompatible, returns `None`.
    pub(crate) fn intersect(&self, other: &JsType) -> Option<JsType> {
        if self == other {
            Some(self.clone())
        } else {
            use JsType::*;

            match (self, other) {
                (Boolean, TheBoolean(b)) | (TheBoolean(b), Boolean) => Some(TheBoolean(*b)),
                (Number, TheNumber(n)) | (TheNumber(n), Number) => Some(TheNumber(*n)),
                (Function, TheFunction(id)) | (TheFunction(id), Function) => Some(TheFunction(*id)),
                _ => None,
            }
        }
    }

    pub(crate) fn as_function_id(&self) -> Option<FunctionId> {
        match self {
            JsType::TheFunction(id) => Some(*id),
            _ => None,
        }
    }
}

impl std::fmt::Debug for JsType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JsType::Undefined => write!(f, "Undefined"),
            JsType::Boolean => write!(f, "Boolean"),
            JsType::TheBoolean(b) => write!(f, "TheBoolean({})", b),
            JsType::Number => write!(f, "Number"),
            JsType::TheNumber(n) => write!(f, "TheNumber({})", n),
            JsType::Function => write!(f, "Function"),
            JsType::TheFunction(id) => write!(f, "TheFunction({})", id.0),
            JsType::Array => write!(f, "Array"),
            JsType::Any => write!(f, "Any"),
        }
    }
}

impl From<f64> for JsType {
    fn from(arg: f64) -> Self {
        JsType::new_number(arg)
    }
}
