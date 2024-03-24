use std::collections::{BTreeMap, HashSet};

use crate::basic_blocks::{Instruction, NonLocalId, StructuredFlow, LHS};

use super::{FromAstCtx, NonLocalInfo, NonLocalOrLocal};

impl FromAstCtx {
    /// Declare or re-declare a name {name} to contain the instruction varname {value}
    pub fn declare_name(&mut self, name: &str, value: usize) -> (Vec<StructuredFlow>, usize) {
        if let Some(NonLocalOrLocal::NonLocal(nonlocal)) = self.scope_tree.lookup(name) {
            let (flow, _id) =
                self.push_instruction(Instruction::Write(LHS::NonLocal(nonlocal), value));

            (flow, value)
        } else {
            let mut conditionals = self.conditionals.last_mut();

            if let Some(ref mut conditionals) = conditionals {
                let entry = conditionals.entry(name.into()).or_insert_with(|| {
                    match self.scope_tree.lookup_in_function(name) {
                        Some(NonLocalOrLocal::Local(existing_var)) => vec![existing_var],
                        _ => vec![],
                    }
                });

                entry.push(value);
            }

            self.scope_tree
                .insert(name.into(), NonLocalOrLocal::Local(value));

            (vec![], value)
        }
    }

    /// Assign or reassign {name}, which can be global, already-declared, or a nonlocal
    pub fn assign_name(&mut self, name: &str, value: usize) -> (Vec<StructuredFlow>, usize) {
        if self.is_global_name(name) {
            let (flow, _id) =
                self.push_instruction(Instruction::Write(LHS::Global(name.to_string()), value));
            (flow, value)
        } else {
            self.declare_name(name, value)
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

            let (flow, deferred_undefined) = self.assign_name(name, deferred_undefined);
            write_flow.extend(flow);

            Ok((vec![], LHS::Local(deferred_undefined)))
        } else if let Some(NonLocalOrLocal::Local(local)) = self.scope_tree.lookup_in_function(name)
        {
            Ok((vec![], LHS::Local(local)))
        } else {
            unreachable!()
        }
    }

    pub fn read_name(&mut self, name: &str) -> (Vec<StructuredFlow>, usize) {
        if let Some(NonLocalOrLocal::NonLocal(nonlocal)) = self.scope_tree.lookup(name) {
            self.push_instruction(Instruction::Read(LHS::NonLocal(nonlocal)))
        } else if self.is_unwritten_funscoped(name) {
            let mut read_name_flow = vec![];

            let (flow, deferred_undefined) = self.push_instruction(Instruction::Undefined);
            read_name_flow.extend(flow);

            let (flow, _) = self.assign_name(name, deferred_undefined);
            read_name_flow.extend(flow);

            (read_name_flow, deferred_undefined)
        } else if let Some(local) = self.scope_tree.lookup_in_function(name) {
            (vec![], local.unwrap_local())
        } else if let Some(nonlocal) = self.scope_tree.lookup(name) {
            unreachable!("nonlocal {:?} not in nonlocalinfo", nonlocal)
        } else {
            self.push_instruction(Instruction::Read(LHS::Global(name.to_string())))
        }
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

    pub fn enter_conditional_branch(&mut self) {
        self.conditionals.push(match self.conditionals.last() {
            Some(cond) => cond.clone(),
            _ => BTreeMap::new(),
        });
    }

    pub fn leave_conditional_branch(&mut self) -> Vec<StructuredFlow> {
        // phi nodes for conditionally assigned variables
        let to_phi = self
            .conditionals
            .pop()
            .expect("unbalanced conditional branch")
            .into_iter()
            .filter(|(_, phies)| phies.len() > 1);

        let mut out = vec![];
        for (varname, phies) in to_phi {
            if let Some(existing_phies) = self
                .conditionals
                .last_mut()
                .and_then(|cond| cond.get_mut(&varname))
            {
                for phi in phies.iter() {
                    if !existing_phies.contains(&phi) {
                        existing_phies.push(*phi);
                    }
                }
            }

            let phi_idx = self.get_var_index();

            out.push(StructuredFlow::Instruction(
                phi_idx,
                Instruction::Phi(phies),
            ));

            self.scope_tree
                .insert(varname, NonLocalOrLocal::Local(phi_idx));
        }

        out
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

            self.scope_tree.insert(name.clone(), nonlocal_id);
        }

        self.nonlocalinfo = Some(nonlocalinfo);

        nonlocals_flow
    }
}
