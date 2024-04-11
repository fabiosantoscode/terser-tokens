use std::{borrow::Borrow, collections::BTreeMap, fmt::Debug};

#[derive(Clone, Debug, PartialEq, Eq)]
struct ScopeTreeNode<Key = String, Of = usize> {
    pub parent: Option<ScopeTreeHandle>,
    pub is_block: bool,
    pub vars: BTreeMap<Key, Of>,
}

#[derive(Debug)]
pub struct ScopeTree<Key = String, Of = usize> {
    scopes: Vec<ScopeTreeNode<Key, Of>>,
    pub current_scope: ScopeTreeHandle,
}

impl<Key, Of> Default for ScopeTree<Key, Of>
where
    Key: Clone + Ord,
    Of: Clone,
{
    fn default() -> Self {
        Self::new()
    }
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeTreeHandle(pub usize);

impl<Key, Of> ScopeTree<Key, Of>
where
    Key: Clone + Ord,
    Of: Clone,
{
    pub fn new() -> Self {
        Self {
            scopes: vec![ScopeTreeNode {
                parent: None,
                is_block: false,
                vars: BTreeMap::new(),
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
        self.scopes.push(ScopeTreeNode {
            parent: Some(self.current_scope),
            is_block: false,
            vars: BTreeMap::new(),
        });
        let id = ScopeTreeHandle(self.scopes.len() - 1);
        self.current_scope = id;
        id
    }

    /// Create a new child block scope and make it the current scope
    pub fn go_into_block_scope(&mut self) -> ScopeTreeHandle {
        self.scopes.push(ScopeTreeNode {
            parent: Some(self.current_scope),
            is_block: true,
            vars: BTreeMap::new(),
        });
        let id = ScopeTreeHandle(self.scopes.len() - 1);
        self.current_scope = id;
        id
    }

    pub fn leave_scope(&mut self) {
        self.current_scope = self.parent().expect("Tried to leave the root scope");
    }

    pub fn get_current_scope_handle(&self, at_block: bool) -> ScopeTreeHandle {
        if at_block {
            self.get_current_block_scope_handle()
        } else {
            self.get_current_function_scope_handle()
        }
    }

    pub fn get_current_block_scope_handle(&self) -> ScopeTreeHandle {
        self.current_scope
    }

    pub fn get_current_function_scope_handle(&self) -> ScopeTreeHandle {
        self.get_closest_function_scope_at(self.current_scope)
            .expect("no function scope available")
    }

    pub fn insert(&mut self, name: Key, value: Of, at_block: bool) {
        if at_block {
            self.insert_at_block(name, value);
        } else {
            self.insert_at_function(name, value);
        }
    }

    pub fn insert_at_block(&mut self, name: Key, value: Of) {
        self.insert_at(self.current_scope, name, value);
    }

    pub fn insert_at_function(&mut self, name: Key, value: Of) {
        let fscope = self
            .get_closest_function_scope_at(self.current_scope)
            .expect("no function scope available");
        self.insert_at(fscope, name, value);
    }

    pub fn insert_at(&mut self, n: ScopeTreeHandle, name: Key, value: Of) {
        let scope = &mut self.scopes[n.0];
        scope.vars.insert(name, value);
    }

    fn has_at<Q: ?Sized>(&self, n: ScopeTreeHandle, name: &Q) -> bool
    where
        Key: Borrow<Q> + Ord,
        Q: Ord,
    {
        self.scopes[n.0].vars.contains_key(name)
    }

    pub fn get_at<Q: ?Sized>(&self, n: ScopeTreeHandle, name: &Q) -> Option<&Of>
    where
        Key: Borrow<Q> + Ord,
        Q: Ord,
    {
        self.scopes[n.0].vars.get(name)
    }

    fn parent_at(&self, n: ScopeTreeHandle) -> Option<ScopeTreeHandle> {
        self.scopes[n.0].parent
    }

    fn parent(&self) -> Option<ScopeTreeHandle> {
        self.parent_at(self.current_scope)
    }

    pub fn lookup<Q: ?Sized>(&self, name: &Q) -> Option<Of>
    where
        Key: Borrow<Q> + Ord,
        Q: Ord,
    {
        self.lookup_at(self.current_scope, name)
    }

    pub fn lookup_at<Q: ?Sized>(&self, at: ScopeTreeHandle, name: &Q) -> Option<Of>
    where
        Key: Borrow<Q> + Ord,
        Q: Ord,
    {
        if let Some(v) = self.get_at(at, name) {
            Some(v.clone())
        } else {
            let parent = self.parent_at(at)?;
            self.lookup_at(parent, name)
        }
    }

    pub fn lookup_handled_at<Q: ?Sized>(
        &self,
        at: ScopeTreeHandle,
        name: &Q,
    ) -> Option<(ScopeTreeHandle, Of)>
    where
        Key: Borrow<Q> + Ord,
        Q: Ord,
    {
        if let Some(v) = self.get_at(at, name) {
            Some((at, v.clone()))
        } else {
            let parent = self.parent_at(at)?;
            self.lookup_handled_at(parent, name)
        }
    }

    pub fn lookup_in_function<Q: ?Sized>(&self, name: &Q) -> Option<Of>
    where
        Key: Borrow<Q> + Ord,
        Q: Ord,
    {
        self.lookup_in_function_at(self.current_scope, name)
    }

    pub fn lookup_in_function_at<Q: ?Sized>(&self, at: ScopeTreeHandle, name: &Q) -> Option<Of>
    where
        Key: Borrow<Q> + Ord,
        Q: Ord,
    {
        if let Some(v) = self.get_at(at, name) {
            Some(v.clone())
        } else {
            let parent = self.parent_at(at)?;
            if self.scopes[parent.0].is_block {
                self.lookup_in_function_at(parent, name)
            } else {
                self.get_at(parent, name).cloned()
            }
        }
    }

    pub fn vars_at(&self, at: ScopeTreeHandle) -> impl Iterator<Item = (&Key, &Of)> {
        self.scopes[at.0].vars.iter()
    }

    pub fn scopes_till_function(&self) -> impl Iterator<Item = ScopeTreeHandle> + '_ {
        std::iter::successors(Some(self.current_scope), |&c| {
            match self.scopes[c.0].is_block {
                true => self.parent_at(c),
                false => None,
            }
        })
    }

    pub fn lookup_scope_of<Q: ?Sized>(&self, name: &Q) -> Option<ScopeTreeHandle>
    where
        Key: Borrow<Q> + Ord,
        Q: Ord,
    {
        self.lookup_scope_of_at(self.current_scope, name)
    }

    pub fn lookup_scope_of_at<Q: ?Sized>(
        &self,
        at: ScopeTreeHandle,
        name: &Q,
    ) -> Option<ScopeTreeHandle>
    where
        Key: Borrow<Q> + Ord,
        Q: Ord,
    {
        if self.has_at(at, name) {
            Some(at)
        } else {
            let parent = self.parent_at(at)?;
            self.lookup_scope_of_at(parent, name)
        }
    }

    fn get_closest_function_scope_at(&self, at: ScopeTreeHandle) -> Option<ScopeTreeHandle> {
        let s = &self.scopes[at.0];
        if !s.is_block {
            Some(at)
        } else {
            self.get_closest_function_scope_at(s.parent?)
        }
    }

    pub fn same_function_as(&self, a: ScopeTreeHandle, b: ScopeTreeHandle) -> bool {
        let a = self.get_closest_function_scope_at(a);
        let b = self.get_closest_function_scope_at(b);
        a == b
    }
}

impl Debug for ScopeTreeHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ScopeTreeHandle({})", self.0)
    }
}
