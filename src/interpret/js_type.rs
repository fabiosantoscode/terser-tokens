use std::collections::BTreeMap;

use ordered_float::NotNan;

use crate::basic_blocks::{BasicBlockInstruction, FunctionId};

#[derive(Clone, PartialEq, Eq, Ord, PartialOrd, Default)]
pub enum JsType {
    Undefined,
    Null,
    Boolean,
    TheBoolean(bool),
    String,
    TheString(String),
    Number,
    TheNumber(NotNan<f64>),
    Function,
    /// (function_id, properties) an instance of a function.
    TheFunction(FunctionId, BTreeMap<String, JsType>),
    Array,
    TheArray(Vec<JsType>),
    Object,
    /// This only expresses POJO's with only string keys, not the full power of JS objects.
    TheObject(BTreeMap<String, JsType>),
    /// A virtual object that represents an unpacking of an array or object.
    Pattern(Vec<JsType>),
    #[default]
    Any,
}

impl JsType {
    pub(crate) fn new_number(arg: f64) -> JsType {
        match NotNan::new(arg) {
            Ok(n) => JsType::TheNumber(n),
            _ => JsType::Number,
        }
    }

    /// Returns `Some(true)` if the type is definitely truthy, `Some(false)` if the type is
    /// definitely falsy, or `None` if the type is unknown.
    ///
    /// Usable for "if" statements, "||", "&&", etc.
    pub fn is_truthy(&self) -> Option<bool> {
        match self {
            JsType::Undefined => Some(false),
            JsType::Null => Some(false),
            JsType::Boolean => None,
            JsType::TheBoolean(b) => Some(*b),
            JsType::String => None,
            JsType::TheString(s) => Some(s.len() > 0),
            JsType::Number => None,
            JsType::TheNumber(n) => Some(n != &NotNan::new(0.0).unwrap()),
            JsType::Function => Some(true),
            JsType::TheFunction { .. } => Some(true),
            JsType::Array => Some(true),
            JsType::TheArray(_) => Some(true),
            JsType::TheObject(_) => Some(true),
            JsType::Object => Some(true),
            JsType::Pattern(_) => None,
            JsType::Any => None,
        }
    }

    pub(crate) fn to_string(&self) -> Option<String> {
        match self {
            JsType::TheString(s) => Some(s.clone()),
            JsType::TheBoolean(b) => Some(b.to_string()),
            JsType::TheNumber(num) => {
                let mut buf = ryu_js::Buffer::new();
                Some(buf.format(num.into_inner()).to_string())
            }
            JsType::Undefined => Some("undefined".to_string()),
            JsType::Null => Some("null".to_string()),
            _ => None,
        }
    }

    /// Returns the type that encompasses all `alternatives`. If there are none, returns None
    pub(crate) fn union_all<'it, It>(alternatives: It) -> Option<JsType>
    where
        It: IntoIterator<Item = &'it JsType> + Clone,
    {
        let mut alternatives = alternatives.into_iter();

        let mut result = alternatives.next()?.clone();

        for t in alternatives {
            result = result.union(t);
            if let JsType::Any = result {
                break;
            }
        }

        Some(result)
    }

    /// Returns a type that represents all values that are either `self` or `other`.
    pub(crate) fn union(&self, other: &JsType) -> JsType {
        if self == other {
            self.clone()
        } else {
            use JsType::*;

            match (self, other) {
                (TheArray(a), TheArray(b)) if a.len() == b.len() => {
                    TheArray(a.iter().zip(b.iter()).map(|(a, b)| a.union(b)).collect())
                }
                (TheObject(a), TheObject(b)) if a.len() == b.len() => {
                    let mut out_obj = BTreeMap::new();

                    let mut a_iter = a.iter();
                    let mut b_iter = b.iter();

                    while let (Some(a), Some(b)) = (a_iter.next(), b_iter.next()) {
                        if a.0 == b.0 {
                            out_obj.insert(a.0.clone(), a.1.union(b.1));
                        } else {
                            return Object;
                        }
                    }

                    TheObject(out_obj)
                }
                (TheNumber(_) | Number, TheNumber(_) | Number) => Number,
                (TheBoolean(_) | Boolean, TheBoolean(_) | Boolean) => Boolean,
                (TheString(_) | String, TheString(_) | String) => String,
                (TheFunction { .. } | Function, TheFunction { .. } | Function) => Function,
                (TheObject(_) | Object, TheObject(_) | Object) => Object,
                (TheArray(_) | Array, TheArray(_) | Array) => Array,
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
                (Number, TheNumber(n)) | (TheNumber(n), Number) => Some(TheNumber(*n)),
                (Boolean, TheBoolean(b)) | (TheBoolean(b), Boolean) => Some(TheBoolean(*b)),
                (String, TheString(s)) | (TheString(s), String) => Some(TheString(s.clone())),
                (Function, f @ TheFunction { .. }) | (f @ TheFunction { .. }, Function) => {
                    Some(f.clone())
                }
                (Object, TheObject(pps)) | (TheObject(pps), Object) => Some(TheObject(pps.clone())),
                _ => None,
            }
        }
    }

    pub(crate) fn as_function_id(&self) -> Option<FunctionId> {
        match self {
            JsType::TheFunction(id, _) => Some(*id),
            _ => None,
        }
    }

    pub(crate) fn as_array(&self) -> Option<&Vec<JsType>> {
        match self {
            JsType::TheArray(arr) => Some(arr),
            _ => None,
        }
    }

    pub(crate) fn as_object(&self) -> Option<&BTreeMap<String, JsType>> {
        match self {
            JsType::TheObject(obj) => Some(obj),
            _ => None,
        }
    }

    /// https://262.ecma-international.org/#sec-toboolean
    pub fn to_boolean(&self) -> Option<bool> {
        match self {
            JsType::TheBoolean(b) => Some(*b),
            JsType::Undefined => Some(false),
            JsType::Null => Some(false),
            JsType::TheNumber(n) => Some(n.into_inner() != 0.0 && n.into_inner() != -0.0),
            JsType::TheString(s) => Some(s.len() > 0),
            JsType::TheObject(_)
            | JsType::Object
            | JsType::Function
            | JsType::TheFunction { .. }
            | JsType::Array
            | JsType::TheArray(_) => Some(true),
            _ => None,
        }
    }

    /// https://262.ecma-international.org/#sec-tonumeric
    /// Note: can't return BigInt yet
    pub fn to_numeric(&self) -> Option<f64> {
        match self {
            JsType::TheNumber(n) => Some(n.into_inner()),
            JsType::Undefined => None, // Actually NaN, but we don't handle it in here
            JsType::TheBoolean(b) => Some(if *b { 1.0 } else { 0.0 }),
            // Objects, strings, and arrays can be turned into numbers but we don't care
            _ => None,
        }
    }

    pub(crate) fn as_small_literal_instruction(&self) -> Option<BasicBlockInstruction> {
        match self {
            JsType::TheBoolean(b) => Some(BasicBlockInstruction::LitBool(*b)),
            JsType::TheNumber(n) => Some(BasicBlockInstruction::LitNumber((*n).into())),
            JsType::TheString(s) => Some(BasicBlockInstruction::LitString(s.clone())),
            _ => None,
        }
    }

    pub(crate) fn typeof_string(&self) -> JsType {
        match self {
            JsType::Undefined => JsType::TheString("undefined".to_string()),
            JsType::Null => JsType::TheString("object".to_string()),
            JsType::Boolean => JsType::TheString("boolean".to_string()),
            JsType::TheBoolean(_) => JsType::TheString("boolean".to_string()),
            JsType::String => JsType::TheString("string".to_string()),
            JsType::TheString(_) => JsType::TheString("string".to_string()),
            JsType::Number => JsType::TheString("number".to_string()),
            JsType::TheNumber(_) => JsType::TheString("number".to_string()),
            JsType::Function => JsType::TheString("function".to_string()),
            JsType::TheFunction { .. } => JsType::TheString("function".to_string()),
            JsType::Array => JsType::TheString("object".to_string()),
            JsType::TheArray(_) => JsType::TheString("object".to_string()),
            JsType::Object => JsType::TheString("object".to_string()),
            JsType::TheObject(_) => JsType::TheString("object".to_string()),
            JsType::Any => JsType::String,
            JsType::Pattern(_) => unreachable!(),
        }
    }
}

impl std::fmt::Debug for JsType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JsType::Undefined => write!(f, "Undefined"),
            JsType::Null => write!(f, "Null"),
            JsType::Boolean => write!(f, "Boolean"),
            JsType::TheBoolean(b) => write!(f, "TheBoolean({})", b),
            JsType::String => write!(f, "String"),
            JsType::TheString(s) => write!(f, "TheString({:?})", s),
            JsType::Number => write!(f, "Number"),
            JsType::TheNumber(n) => write!(f, "TheNumber({})", n),
            JsType::Function => write!(f, "Function"),
            JsType::TheFunction(id, x) => {
                if x.is_empty() {
                    write!(f, "TheFunction({})", id.0)
                } else {
                    write!(f, "TheFunction({} with {:?})", id.0, x)
                }
            },
            JsType::Array => write!(f, "Array"),
            JsType::TheArray(items) => write!(f, "TheArray({:?})", items),
            JsType::Object => write!(f, "Object"),
            JsType::TheObject(props) => write!(f, "TheObject({:?})", props),
            JsType::Pattern(items) => write!(f, "Pattern({:?})", items),
            JsType::Any => write!(f, "Any"),
        }
    }
}

impl From<f64> for JsType {
    fn from(arg: f64) -> Self {
        JsType::new_number(arg)
    }
}
