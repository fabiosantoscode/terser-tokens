use std::collections::HashSet;

use crate::basic_blocks::{Instruction, NonLocalId, StructuredFlow, LHS};

use super::{FromAstCtx, NonLocalInfo, NonLocalOrLocal};

impl FromAstCtx {
    /// Declare or re-declare a name {name} to contain the instruction varname {value}
    pub fn declare_name(&mut self, name: &str, value: usize, is_let: bool) -> Vec<StructuredFlow> {
        if let Some(NonLocalOrLocal::NonLocal(nonlocal)) = self.scope_tree.lookup(name) {
            let (flow, _id) =
                self.push_instruction(Instruction::Write(LHS::NonLocal(nonlocal), value));

            flow
        } else {
            let scope = self.scope_tree.get_current_scope_handle(is_let);

            match self.scope_tree.get_at(scope, name) {
                Some(NonLocalOrLocal::NonLocal(_)) => {
                    unreachable!("handled above");
                }
                Some(NonLocalOrLocal::Local(names)) => {
                    let mut names = names.clone();
                    names.push(value);

                    self.scope_tree
                        .insert_at(scope, name.into(), NonLocalOrLocal::Local(names));

                    vec![]
                }
                None => {
                    self.scope_tree.insert_at(
                        scope,
                        name.into(),
                        NonLocalOrLocal::Local(vec![value]),
                    );

                    vec![]
                }
            }
        }
    }

    /// Assign or reassign {name}, which can be global, already-declared, or a nonlocal
    pub fn assign_name(&mut self, name: &str, value: usize) -> Vec<StructuredFlow> {
        if self.is_global_name(name) {
            let (flow, _id) =
                self.push_instruction(Instruction::Write(LHS::Global(name.to_string()), value));

            flow
        } else {
            match self
                .scope_tree
                .lookup_handled_at(self.scope_tree.current_scope, name)
            {
                Some((_, NonLocalOrLocal::NonLocal(nonlocal))) => {
                    let (flow, _id) =
                        self.push_instruction(Instruction::Write(LHS::NonLocal(nonlocal), value));

                    flow
                }
                Some((found_at_scope, NonLocalOrLocal::Local(mut local_names))) => {
                    local_names.push(value);

                    self.scope_tree.insert_at(
                        found_at_scope,
                        name.into(),
                        NonLocalOrLocal::Local(local_names),
                    );

                    vec![]
                }
                None => {
                    assert!(self.is_unwritten_funscoped(name));

                    self.scope_tree
                        .insert_at_function(name.into(), NonLocalOrLocal::Local(vec![value]));

                    vec![]
                }
            }
        }
    }

    /// Assign or reassign {name}, which can be global, already-declared, or a nonlocal
    pub fn get_lhs_for_name(&mut self, name: &str) -> Result<(Vec<StructuredFlow>, LHS), String> {
        if self.is_global_name(name) {
            Ok((vec![], LHS::Global(name.to_string())))
        } else if let Some(NonLocalOrLocal::NonLocal(nonlocal)) = self.scope_tree.lookup(name) {
            Ok((vec![], LHS::NonLocal(nonlocal)))
        } else if self.is_unwritten_funscoped(name) {
            let mut write_flow = vec![];

            let (flow, deferred_undefined) = self.push_instruction(Instruction::Undefined);
            write_flow.extend(flow);

            let flow = self.assign_name(name, deferred_undefined);
            write_flow.extend(flow);

            Ok((vec![], LHS::Local(deferred_undefined)))
        } else if let Some(NonLocalOrLocal::Local(loc)) = self.scope_tree.lookup_in_function(name) {
            Ok((vec![], LHS::Local(loc.last().unwrap().clone())))
        } else {
            unreachable!()
        }
    }

    pub fn read_name(&mut self, name: &str) -> (Vec<StructuredFlow>, usize) {
        if let Some(NonLocalOrLocal::NonLocal(nonlocal)) = self.scope_tree.lookup(name) {
            // TODO what if there's a nonlocal with the same name as a local?
            self.push_instruction(Instruction::Read(LHS::NonLocal(nonlocal)))
        } else if self.is_unwritten_funscoped(name) {
            println!("{name} is unwritten funscoped");
            let mut read_name_flow = vec![];

            let (flow, deferred_undefined) = self.push_instruction(Instruction::Undefined);
            read_name_flow.extend(flow);

            let flow = self.assign_name(name, deferred_undefined);
            read_name_flow.extend(flow);

            (read_name_flow, deferred_undefined)
        } else if let Some(local) = self.scope_tree.lookup_in_function(name) {
            (vec![], local.unwrap_local().last().unwrap().clone())
        } else if let Some(nonlocal) = self.scope_tree.lookup(name) {
            unreachable!("nonlocal {:?} not in nonlocalinfo", nonlocal)
        } else {
            self.push_instruction(Instruction::Read(LHS::Global(name.to_string())))
        }
    }

    pub fn enter_conditional_branch(&mut self) {
        self.conditionals_depth += 1;
    }

    pub fn leave_conditional_branch(&mut self) -> Vec<StructuredFlow> {
        assert!(self.conditionals_depth > 0);
        self.conditionals_depth -= 1;

        let mut to_insert = vec![];

        for scope in self.scope_tree.scopes_till_function() {
            for (name, var) in self.scope_tree.vars_at(scope) {
                if let NonLocalOrLocal::Local(phies) = var {
                    if phies.len() > 0 {
                        to_insert.push((scope, name.clone(), phies.clone()));
                    }
                }
            }
        }

        let mut phi_nodes = vec![];

        for (scope, name, phies) in to_insert {
            let mut existing_phies = self
                .scope_tree
                .get_at(scope, &name)
                .unwrap()
                .unwrap_local()
                .clone();

            if phies.len() > 1 {
                let (flow, new_value) = self.push_instruction(Instruction::Phi(phies));
                phi_nodes.extend(flow);

                existing_phies.push(new_value);
                self.scope_tree
                    .insert_at(scope, name, NonLocalOrLocal::Local(existing_phies));
            }
        }

        phi_nodes
    }

    pub fn go_into_block<Cb>(&mut self, cb: Cb) -> Result<Vec<StructuredFlow>, String>
    where
        Cb: FnOnce(&mut Self) -> Result<Vec<StructuredFlow>, String>,
    {
        self.scope_tree.go_into_block_scope();
        let ret = cb(self);
        self.scope_tree.leave_scope();
        ret
    }

    pub fn is_global_name(&self, name: &str) -> bool {
        match name {
            "undefined" | "Infinity" => false,
            _ => !self.is_unwritten_funscoped(name) && self.scope_tree.lookup(name).is_none(),
        }
    }

    pub fn is_nonlocal(&self, name: &str) -> Option<NonLocalId> {
        match name {
            "undefined" | "Infinity" => None,
            _ => {
                if !self.is_unwritten_funscoped(name) {
                    match self.scope_tree.lookup(name)? {
                        NonLocalOrLocal::NonLocal(nonloc) => Some(nonloc),
                        _ => None,
                    }
                } else {
                    None
                }
            }
        }
    }

    fn is_unwritten_funscoped(&self, name: &str) -> bool {
        match self.nonlocalinfo.as_ref() {
            Some(nli) => {
                self.scope_tree.lookup_in_function(name).is_none()
                    && nli.funscoped.contains(&name.into())
                    && !nli.nonlocals.contains(&name.into())
            }
            None => false,
        }
    }

    pub fn embed_nonlocals<'b>(
        &mut self,
        mut nonlocalinfo: NonLocalInfo,
        parent: Option<&'b NonLocalInfo>,
    ) -> Vec<StructuredFlow> {
        let mut nonlocals_flow = vec![];
        let mut parent_nonlocals: HashSet<&'b str> = HashSet::new();

        if let Some(nli) = parent {
            for name in nli.nonlocals.iter() {
                if !nonlocalinfo.nonlocals.contains(name) {
                    nonlocalinfo.nonlocals.push(name.clone());
                    parent_nonlocals.insert(name.as_str());
                }
            }
        }

        for name in nonlocalinfo.nonlocals.iter() {
            let nonlocal_id = if !parent_nonlocals.contains(name.as_str()) {
                let (flow, nonlocal_undef) = self.push_instruction(Instruction::Undefined);
                nonlocals_flow.extend(flow);

                let wanted_id = NonLocalId(self.get_var_index());
                let read = Instruction::Write(LHS::NonLocal(wanted_id), nonlocal_undef);
                let (flow, _) = self.push_instruction(read);
                nonlocals_flow.extend(flow);

                NonLocalOrLocal::NonLocal(wanted_id)
            } else {
                self.scope_tree
                    .lookup(name)
                    .expect("nonlocal not in scope tree")
            };

            self.scope_tree
                .insert_at_function(name.clone(), nonlocal_id);
        }

        self.nonlocalinfo = Some(nonlocalinfo);

        nonlocals_flow
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_convctx_conditionals() {
        let mut ctx = FromAstCtx::new();
        ctx.var_index = 999; // will generate here

        ctx.enter_conditional_branch();
        {
            // cond branch
            ctx.declare_name("a", 1, false);
            ctx.declare_name("b", 2, false);

            assert_eq!(ctx.read_name("a"), (vec![], 1));
            assert_eq!(ctx.read_name("b"), (vec![], 2));
        }

        let flow = ctx.leave_conditional_branch();
        assert_eq!(flow.len(), 0);

        assert_eq!(ctx.read_name("a"), (vec![], 1));
        assert_eq!(ctx.read_name("b"), (vec![], 2));

        ctx.enter_conditional_branch();
        {
            // cond branch
            ctx.declare_name("a", 3, false);
            assert_eq!(ctx.read_name("a"), (vec![], 3));
        }
        let flow = ctx.leave_conditional_branch();

        assert_eq!(flow.len(), 1);

        assert_eq!(ctx.read_name("a"), (vec![], 999));

        if let StructuredFlow::Instruction(_, Instruction::Phi(phis)) = &flow[0] {
            assert_eq!(phis.len(), 2);
            assert!(phis.contains(&1));
            assert!(!phis.contains(&2));
            assert!(phis.contains(&3));
        } else {
            panic!("expected phi instruction");
        }
    }

    #[test]
    fn test_convctx_conditionals_samename() {
        let mut ctx = FromAstCtx::new();

        ctx.declare_name("name", 1, false);

        ctx.enter_conditional_branch();
        ctx.declare_name("name", 2, false);
        let flow = ctx.leave_conditional_branch();

        assert_eq!(flow.len(), 1);
    }
}
