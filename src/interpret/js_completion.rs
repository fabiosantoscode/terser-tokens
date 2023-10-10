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

    /// Merges all possible return types
    pub(crate) fn merge_all_return<'comp, It>(completions: It) -> Option<JsType>
    where
        It: IntoIterator<Item = &'comp JsCompletion>,
    {
        let mut completions = completions.into_iter();

        let mut accum = match completions.next().and_then(|c| match c {
            JsCompletion::Return(t) => Some(t.clone()),
            _ => None,
        }) {
            Some(first) => first,
            None => return None,
        };

        for next_completion in completions {
            accum = accum.union(next_completion.as_return()?);

            if let JsType::Any = accum {
                return Some(JsType::Any);
            }
        }

        Some(accum)
    }
}
