use super::JsType;

#[derive(Debug, Default, PartialEq, PartialOrd, Eq, Ord, Clone)]
pub enum JsArgs {
    #[default]
    Unknown,
    Known(Vec<JsType>),
}

impl JsArgs {
    pub fn as_known(&self) -> Option<&Vec<JsType>> {
        match self {
            JsArgs::Known(v) => Some(v),
            _ => None,
        }
    }

    // Get the nth argument, if known
    pub fn nth(&self, n: usize) -> Option<&JsType> {
        self.as_known()?.get(n)
    }

    /// Spread the arguments from `n` onwards, if known
    pub fn spread_from(&self, n: usize) -> Option<&[JsType]> {
        let known = self.as_known()?;
        if known.len() > n {
            Some(&known[n..])
        } else {
            None
        }
    }

    pub fn from_argvecs<It>(args: It) -> JsArgs
    where
        It: IntoIterator<Item = JsArgs>,
    {
        let mut args = args.into_iter();
        let mut accum = if let Some(first) = args.next() {
            first
        } else {
            return JsArgs::Unknown;
        };

        for next_arg in args {
            accum = accum.consolidate(&next_arg);
            if let JsArgs::Unknown = accum {
                break;
            }
        }

        accum
    }

    pub fn consolidate(&self, other: &JsArgs) -> JsArgs {
        match (self, other) {
            (JsArgs::Known(a), JsArgs::Known(b)) if a.len() == b.len() => {
                let v = a
                    .into_iter()
                    .zip(b.into_iter())
                    .map(|(a, b)| a.union(b))
                    .collect();
                JsArgs::Known(v)
            }
            _ => JsArgs::Unknown,
        }
    }
}

impl From<Option<Vec<JsType>>> for JsArgs {
    fn from(v: Option<Vec<JsType>>) -> Self {
        match v {
            Some(v) => JsArgs::Known(v),
            None => JsArgs::Unknown,
        }
    }
}
