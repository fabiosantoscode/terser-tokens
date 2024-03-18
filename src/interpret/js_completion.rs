use super::JsType;

#[derive(Debug, Clone, PartialEq)]
pub enum JsCompletion {
    Normal(JsType),
    Break(usize),
    Continue(usize),
    Return(JsType),
}

impl JsCompletion {
    pub fn into_normal(self) -> Option<JsType> {
        match self {
            JsCompletion::Normal(t) => Some(t),
            _ => None,
        }
    }

    pub(crate) fn into_return(self) -> Option<JsType> {
        match self {
            JsCompletion::Return(t) => Some(t),
            _ => None,
        }
    }

    pub(crate) fn as_return(&self) -> Option<&JsType> {
        match self {
            JsCompletion::Return(t) => Some(t),
            _ => None,
        }
    }

    /// Returns a completion that merges `self` and `other`, or None if the completions are not compatible.
    pub(crate) fn merge(self, other: &JsCompletion) -> Option<JsCompletion> {
        use JsCompletion::*;
        match (&self, other) {
            (Normal(t1), Normal(t2)) => Some(Normal(t1.union(&t2))),
            (Return(t1), Return(t2)) => Some(Return(t1.union(&t2))),
            (Break(b1), Break(b2)) if b1 == b2 => Some(self),
            (Continue(c1), Continue(c2)) if c1 == c2 => Some(self),
            _ => None,
        }
    }
}
