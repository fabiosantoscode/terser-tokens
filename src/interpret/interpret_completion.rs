use super::JsType;

#[derive(Debug, Clone, PartialEq)]
pub enum InterpretCompletion {
    Normal(JsType),
    Break(usize),
    Continue(usize),
    Return(JsType),
    Unknown,
}

impl InterpretCompletion {
    pub fn as_normal(self) -> Option<JsType> {
        match self {
            InterpretCompletion::Normal(t) => Some(t),
            _ => None,
        }
    }

    pub(crate) fn as_return(self) -> Option<JsType> {
        match self {
            InterpretCompletion::Return(t) => Some(t),
            _ => None,
        }
    }

    pub(crate) fn as_return_ref(&self) -> Option<&JsType> {
        match self {
            InterpretCompletion::Return(t) => Some(t),
            _ => None,
        }
    }

    pub fn as_known(self) -> Option<InterpretCompletion> {
        match self {
            InterpretCompletion::Unknown => None,
            _ => Some(self),
        }
    }

    /// Returns a completion that merges `self` and `other`, or None if the completions are not compatible.
    pub(crate) fn merge(&self, other: &InterpretCompletion) -> Option<InterpretCompletion> {
        use InterpretCompletion::*;
        match (self, other) {
            (Normal(t1), Normal(t2)) => Some(Normal(t1.union(&t2))),
            (Return(t1), Return(t2)) => Some(Return(t1.union(&t2))),
            (Break(b1), Break(b2)) if b1 == b2 => Some(self.clone()),
            (Continue(c1), Continue(c2)) if c1 == c2 => Some(self.clone()),
            _ => None,
        }
    }

    /// Merges all possible return types
    pub(crate) fn merge_all_return<'comp, It>(completions: It) -> Option<JsType>
    where
        It: IntoIterator<Item = &'comp InterpretCompletion>,
    {
        let mut completions = completions.into_iter();

        let mut accum = match completions.next().and_then(|c| match c {
            InterpretCompletion::Return(t) => Some(t.clone()),
            _ => None,
        }) {
            Some(first) => first,
            None => return None,
        };

        for next_completion in completions {
            accum = accum.union(next_completion.as_return_ref()?);

            if let JsType::Any = accum {
                return Some(JsType::Any);
            }
        }

        Some(accum)
    }
}
