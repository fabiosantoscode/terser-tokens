use hamt::HamtRc;

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
