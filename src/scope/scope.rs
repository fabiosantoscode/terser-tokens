use hamt::HamtRc;
use std::fmt::{Debug, Formatter};

#[derive(Clone)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,
    pub vars: HamtRc<String, usize>,
    pub is_block: bool,
}

impl Scope {
    pub fn new(is_block: bool) -> Self {
        Self {
            is_block,
            vars: HamtRc::new(),
            parent: None,
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
        self.vars = self.vars.insert(&name, &idx);
    }

    pub fn go_into_block(&mut self) -> Self {
        let mut new_scope = Self::new(true);
        new_scope.parent = Some(Box::new(self.clone()));
        new_scope
    }

    pub fn go_into_function(&self) -> Self {
        let mut new_scope = Self::new(false);
        new_scope.parent = Some(Box::new(self.clone()));
        new_scope
    }

    pub fn leave(&mut self) -> Self {
        if let Some(parent) = &self.parent {
            return *parent.clone();
        }

        self.clone()
    }
}

impl Debug for Scope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut d = if self.is_block {
            f.debug_struct("Scope (block)")
        } else {
            f.debug_struct("Scope (function)")
        };

        let mut vars = vec![];
        let mut sorted_keys = self.vars.keys().collect::<Vec<_>>();
        sorted_keys.sort_unstable();
        for key in sorted_keys {
            vars.push(format!("{}: {}", key, self.vars.get(key).unwrap()));
        }

        d.field("vars", &vars);

        if let Some(parent) = &self.parent {
            d.field("parent", &parent);
        }

        d.finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_get_set() {
        let mut scope = Scope::new(false);
        scope.insert("a".to_string(), 0);
        scope.insert("b".to_string(), 1);
        scope.insert("c".to_string(), 2);

        assert_eq!(scope.get("a"), Some(0));
        assert_eq!(scope.get("b"), Some(1));
        assert_eq!(scope.get("c"), Some(2));
        assert_eq!(scope.get("d"), None);
    }

    #[test]
    fn can_get_set_in_block() {
        let mut scope = Scope::new(true);
        scope.insert("a".to_string(), 0);
        let mut scope = scope.go_into_block();
        scope.insert("b".to_string(), 1);
        scope.insert("c".to_string(), 2);

        assert_eq!(scope.get("a"), Some(0));
        assert_eq!(scope.get("b"), Some(1));
        assert_eq!(scope.get("c"), Some(2));
        assert_eq!(scope.get("d"), None);

        let scope = scope.leave();

        assert_eq!(scope.get("a"), Some(0));
        assert_eq!(scope.get("b"), None);
        assert_eq!(scope.get("c"), None);
        assert_eq!(scope.get("d"), None);
    }
}
