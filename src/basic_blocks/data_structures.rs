use super::FunctionId;

#[derive(Clone, PartialEq)]
pub enum ArrayElement {
    Hole,
    Item(usize),
    Spread(usize),
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum MethodKind {
    Method,
    Getter,
    Setter,
}

#[derive(Clone, PartialEq)]
pub enum ObjectProperty {
    KeyValue(String, usize),
    Computed(usize, usize),
    Spread(usize),
}

#[derive(Clone, PartialEq)]
pub enum ObjectKey {
    KeyValue(String),
    Private(String),
    Computed(usize),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassProperty {
    pub is_static: bool,
    pub is_private: bool,
    pub key: ObjectKey,
    pub value: ClassPropertyValue,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ClassPropertyValue {
    Property(usize),
    Method(MethodKind, FunctionId),
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

impl ArrayElement {
    pub fn as_item(&self) -> Option<usize> {
        match self {
            ArrayElement::Item(it) => Some(*it),
            _ => None,
        }
    }
}

impl ObjectKey {
    pub fn used_vars(&self) -> Vec<usize> {
        match self {
            ObjectKey::KeyValue(_) => vec![],
            ObjectKey::Private(_) => vec![],
            ObjectKey::Computed(v) => vec![*v],
        }
    }

    pub fn used_vars_mut(&mut self) -> Vec<&mut usize> {
        match self {
            ObjectKey::KeyValue(_) => vec![],
            ObjectKey::Private(_) => vec![],
            ObjectKey::Computed(v) => vec![v],
        }
    }
}

impl From<swc_ecma_ast::MethodKind> for MethodKind {
    fn from(kind: swc_ecma_ast::MethodKind) -> Self {
        match kind {
            swc_ecma_ast::MethodKind::Method => MethodKind::Method,
            swc_ecma_ast::MethodKind::Getter => MethodKind::Getter,
            swc_ecma_ast::MethodKind::Setter => MethodKind::Setter,
        }
    }
}

impl Into<swc_ecma_ast::MethodKind> for MethodKind {
    fn into(self) -> swc_ecma_ast::MethodKind {
        match self {
            MethodKind::Method => swc_ecma_ast::MethodKind::Method,
            MethodKind::Getter => swc_ecma_ast::MethodKind::Getter,
            MethodKind::Setter => swc_ecma_ast::MethodKind::Setter,
        }
    }
}

impl ClassProperty {
    pub fn used_vars(&self) -> Vec<usize> {
        let key = match &self.key {
            ObjectKey::KeyValue(_) => vec![],
            ObjectKey::Private(_) => vec![],
            ObjectKey::Computed(var) => vec![*var],
        };
        let value = match &self.value {
            ClassPropertyValue::Property(var) => vec![*var],
            ClassPropertyValue::Method(_kind, _fn) => vec![],
        };

        key.into_iter().chain(value.into_iter()).collect()
    }

    pub fn used_vars_mut(&mut self) -> Vec<&mut usize> {
        let key = match &mut self.key {
            ObjectKey::KeyValue(_) => vec![],
            ObjectKey::Private(_) => vec![],
            ObjectKey::Computed(var) => vec![var],
        };
        let value = match &mut self.value {
            ClassPropertyValue::Property(var) => vec![var],
            ClassPropertyValue::Method(_kind, _fn) => vec![],
        };

        key.into_iter().chain(value.into_iter()).collect()
    }
}
