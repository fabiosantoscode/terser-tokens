use std::collections::HashMap;

struct ScopeTreeNode<Of = usize> {
    pub parent: Option<ScopeTreeHandle>,
    pub is_block: bool, // TODO there are actually two "function" scopes
    pub vars: HashMap<String, Of>,
}

pub struct ScopeTree<Of = usize> {
    scopes: Vec<ScopeTreeNode<Of>>,
    pub current_scope: ScopeTreeHandle,
}

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ScopeTreeHandle(pub usize);

impl<Of> ScopeTree<Of>
where
    Of: Clone,
{
    pub fn new() -> Self {
        Self {
            scopes: vec![ScopeTreeNode {
                parent: None,
                is_block: false,
                vars: HashMap::new(),
            }],
            current_scope: ScopeTreeHandle(0),
        }
    }

    /// Select which scope is current
    pub fn go_to_scope(&mut self, scope: ScopeTreeHandle) {
        self.current_scope = scope;
    }

    /// Create a new child func scope and make it the current scope
    pub fn go_into_function_scope(&mut self) -> ScopeTreeHandle {
        let parent = Some(self.current_scope);
        self.scopes.push(ScopeTreeNode {
            parent,
            is_block: false,
            vars: HashMap::new(),
        });
        let id = ScopeTreeHandle(self.scopes.len() - 1);
        self.current_scope = id;
        id
    }

    /// Create a new child block scope and make it the current scope
    pub fn go_into_block_scope(&mut self) -> ScopeTreeHandle {
        let parent = Some(self.current_scope);
        self.scopes.push(ScopeTreeNode {
            parent: parent,
            is_block: true,
            vars: HashMap::new(),
        });
        let id = ScopeTreeHandle(self.scopes.len() - 1);
        self.current_scope = id;
        id
    }

    pub fn leave_scope(&mut self) {
        self.current_scope = self.parent().expect("Tried to leave the root scope");
    }

    pub fn insert(&mut self, name: String, value: Of) {
        let scope = &mut self.scopes[self.current_scope.0];
        scope.vars.insert(name, value);
    }

    fn get_at(&self, n: ScopeTreeHandle, name: &str) -> Option<&Of> {
        self.scopes[n.0].vars.get(name)
    }

    fn parent_at(&self, n: ScopeTreeHandle) -> Option<ScopeTreeHandle> {
        self.scopes[n.0].parent
    }

    fn parent(&self) -> Option<ScopeTreeHandle> {
        self.parent_at(self.current_scope)
    }

    pub(crate) fn get_scope_of(&self, name: &str) -> Option<ScopeTreeHandle> {
        let mut cur = Some(self.current_scope);

        while let Some(scope) = cur {
            if let Some(_) = self.get_at(scope, name) {
                return Some(scope);
            }
            cur = self.parent_at(scope);
        }

        None
    }

    fn get_closest_function_scope_at(&self, at: ScopeTreeHandle) -> Option<ScopeTreeHandle> {
        let s = &self.scopes[at.0];
        if !s.is_block {
            Some(at)
        } else {
            self.get_closest_function_scope_at(s.parent?)
        }
    }

    pub(crate) fn same_function_as(&self, a: ScopeTreeHandle, b: ScopeTreeHandle) -> bool {
        let a = self.get_closest_function_scope_at(a);
        let b = self.get_closest_function_scope_at(b);
        a == b
    }
}
