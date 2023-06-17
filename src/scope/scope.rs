use std::collections::HashMap;

#[derive(Debug, Default, Clone)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,
    pub vars: HashMap<String, usize>,
    pub is_block: bool,
}

impl Scope {
    pub fn new(is_block: bool) -> Self {
        Self {
            is_block,
            ..Default::default()
        }
    }

    pub fn get(&self, name: &str) -> Option<usize> {
        if let Some(idx) = self.vars.get(name) {
            return Some(*idx);
        }

        if let Some(parent) = &self.parent {
            return parent.get(name);
        }

        None
    }

    pub fn insert(&mut self, name: String, idx: usize) {
        self.vars.insert(name, idx);
    }

    pub fn go_into_block(&mut self) -> Self {
        let mut new_scope = Self::new(true);
        new_scope.parent = Some(Box::new(self.clone()));
        new_scope
    }
}
