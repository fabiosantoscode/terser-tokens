use std::collections::{BTreeMap, HashSet};

use crate::basic_blocks::{BasicBlockInstruction, NonLocalId, StructuredFlow, LHS};

use super::{FromAstCtx, NonLocalInfo, NonLocalOrLocal};

impl FromAstCtx {
    /// Declare or re-declare a name {name} to contain the instruction varname {value}
    pub fn declare_name(&mut self, name: &str, value: usize) -> (Vec<StructuredFlow>, usize) {
        if let Some(NonLocalOrLocal::NonLocal(nonlocal)) = self.scope_tree.lookup(name) {
            let (flow, _id) =
                self.push_instruction(BasicBlockInstruction::Write(LHS::NonLocal(nonlocal), value));

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
            let (flow, _id) = self.push_instruction(BasicBlockInstruction::Write(
                LHS::Global(name.to_string()),
                value,
            ));
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

            let (flow, deferred_undefined) =
                self.push_instruction(BasicBlockInstruction::Undefined);
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
            self.push_instruction(BasicBlockInstruction::Read(LHS::NonLocal(nonlocal)))
        } else if self.is_unwritten_funscoped(name) {
            let mut read_name_flow = vec![];

            let (flow, deferred_undefined) =
                self.push_instruction(BasicBlockInstruction::Undefined);
            read_name_flow.extend(flow);

            let (flow, _) = self.assign_name(name, deferred_undefined);
            read_name_flow.extend(flow);

            (read_name_flow, deferred_undefined)
        } else if let Some(local) = self.scope_tree.lookup_in_function(name) {
            (vec![], local.unwrap_local())
        } else if let Some(nonlocal) = self.scope_tree.lookup(name) {
            unreachable!("nonlocal {:?} not in nonlocalinfo", nonlocal)
        } else {
            self.push_instruction(BasicBlockInstruction::Read(LHS::Global(name.to_string())))
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
                BasicBlockInstruction::Phi(phies),
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
                let (flow, nonlocal_undef) =
                    self.push_instruction(BasicBlockInstruction::Undefined);
                nonlocals_flow.extend(flow);

                let wanted_id = NonLocalId(self.get_var_index());
                let read = BasicBlockInstruction::Write(LHS::NonLocal(wanted_id), nonlocal_undef);
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

#[cfg(test)]
mod tests {
    use crate::{basic_blocks::BasicBlockEnvironment, from_ast::NonLocalInfo};

    use super::*;

    /*
    #[test]
    fn deferred_funscoped() {
        let mut ctx = FromAstCtx::new();

        ctx.embed_nonlocals(
            NonLocalInfo {
                funscoped: vec![
                    "read_after_assign".into(),
                    "read_before_assign".into(),
                    "nonlocal_assigned_later".into(),
                ],
                nonlocals: vec!["nonlocal_assigned_later".into()],
            },
            None,
        );

        assert_eq!(ctx.get_block(0).len(), 3);

        ctx.read_name("read_before_assign"); // produces undefined instruction
        assert_eq!(ctx.get_block(0).len(), 4);

        ctx.assign_name("read_after_assign", 1);
        ctx.read_name("read_after_assign"); // just reads
        assert_eq!(ctx.get_block(0).len(), 4);

        insta::assert_debug_snapshot!(ctx.basic_blocks, @r###"
        {
            0: [
                (
                    0,
                    Some(
                        undefined,
                    ),
                ),
                (
                    1,
                    None,
                ),
                (
                    2,
                    Some(
                        write_non_local $$1 $0,
                    ),
                ),
                (
                    3,
                    Some(
                        undefined,
                    ),
                ),
            ],
        }
        "###);

        insta::assert_debug_snapshot!(ctx.scope_tree, @r###"
        ScopeTree {
            scopes: [
                ScopeTreeNode {
                    parent: None,
                    is_block: false,
                    vars: {
                        "nonlocal_assigned_later": NonLocal(1),
                        "read_after_assign": Local(1),
                        "read_before_assign": Local(3),
                    },
                },
            ],
            current_scope: ScopeTreeHandle(0),
        }
        "###);
    }

    #[test]
    fn test_nonlocals() {
        let mut ctx = FromAstCtx::new();

        ctx.embed_nonlocals(
            NonLocalInfo {
                funscoped: vec!["assigned_later".into(), "nonlocal_assigned_later".into()],
                nonlocals: vec!["provided_nonlocal".into(), "nonlocal_assigned_later".into()],
            },
            None,
        );

        insta::assert_debug_snapshot!(ctx.scope_tree, @r###"
        ScopeTree {
            scopes: [
                ScopeTreeNode {
                    parent: None,
                    is_block: false,
                    vars: {
                        "nonlocal_assigned_later": NonLocal(4),
                        "provided_nonlocal": NonLocal(1),
                    },
                },
            ],
            current_scope: ScopeTreeHandle(0),
        }
        "###);

        // time to read a not-yet-declared var
        assert_eq!(ctx.read_name("assigned_later"), 6);

        // time to read a nonlocal
        assert_eq!(ctx.read_name("provided_nonlocal"), 7);
        insta::assert_debug_snapshot!(ctx.get_block(0)[7], @r###"
        (
            7,
            Some(
                read_non_local $$1,
            ),
        )
        "###);

        // time to write it!
        ctx.assign_name("provided_nonlocal", 123);
        insta::assert_debug_snapshot!(ctx.get_block(0)[8], @r###"
        (
            8,
            Some(
                write_non_local $$1 $123,
            ),
        )
        "###);

        // nonlocals are never conditional
        ctx.enter_conditional_branch();
        ctx.assign_name("provided_nonlocal", 777);
        insta::assert_debug_snapshot!(ctx.get_block(0)[9], @r###"
        (
            9,
            Some(
                write_non_local $$1 $777,
            ),
        )
        "###);
        assert!(ctx.conditionals[0].is_empty());
    }

    #[test]
    fn test_general() {
        let mut ctx = FromAstCtx::new();

        ctx.declare_name("varname", 123);

        ctx.enter_conditional_branch();

        ctx.declare_name("conditional_varname", 456);
        ctx.assign_name("conditional_varname", 789);

        ctx.go_into_function(BasicBlockEnvironment::Function(false, false), None, |ctx| {
            ctx.assign_name("conditional_varname", 999);

            insta::assert_debug_snapshot!(ctx.conditionals, @"[]");
            insta::assert_debug_snapshot!(ctx.scope_tree, @r###"
            ScopeTree {
                scopes: [
                    ScopeTreeNode {
                        parent: None,
                        is_block: false,
                        vars: {
                            "conditional_varname": Local(789),
                            "varname": Local(123),
                        },
                    },
                    ScopeTreeNode {
                        parent: Some(
                            ScopeTreeHandle(0),
                        ),
                        is_block: false,
                        vars: {
                            "conditional_varname": Local(999),
                        },
                    },
                ],
                current_scope: ScopeTreeHandle(1),
            }
            "###);

            Ok(vec![])
        })
        .unwrap();

        insta::assert_debug_snapshot!(ctx.conditionals, @r###"
        [
            {
                "conditional_varname": [
                    456,
                    789,
                ],
            },
        ]
        "###);
        insta::assert_debug_snapshot!(ctx.scope_tree, @r###"
        ScopeTree {
            scopes: [
                ScopeTreeNode {
                    parent: None,
                    is_block: false,
                    vars: {
                        "conditional_varname": Local(789),
                        "varname": Local(123),
                    },
                },
                ScopeTreeNode {
                    parent: Some(
                        ScopeTreeHandle(0),
                    ),
                    is_block: false,
                    vars: {
                        "conditional_varname": Local(999),
                    },
                },
            ],
            current_scope: ScopeTreeHandle(0),
        }
        "###);

        // this pops the conditionals, creates phi nodes and assigns the conditional var to the phied version
        ctx.leave_conditional_branch();

        insta::assert_debug_snapshot!(ctx.conditionals, @"[]");
        insta::assert_debug_snapshot!(ctx.scope_tree, @r###"
        ScopeTree {
            scopes: [
                ScopeTreeNode {
                    parent: None,
                    is_block: false,
                    vars: {
                        "conditional_varname": Local(0),
                        "varname": Local(123),
                    },
                },
                ScopeTreeNode {
                    parent: Some(
                        ScopeTreeHandle(0),
                    ),
                    is_block: false,
                    vars: {
                        "conditional_varname": Local(999),
                    },
                },
            ],
            current_scope: ScopeTreeHandle(0),
        }
        "###);
        insta::assert_debug_snapshot!(ctx.basic_blocks, @r###"
        {
            0: [
                (
                    0,
                    Some(
                        either($456, $789),
                    ),
                ),
            ],
        }
        "###);
    }

    #[test]
    fn test_phi() {
        let mut ctx = FromAstCtx::new();

        ctx.declare_name("varname_before_if", 11);

        ctx.enter_conditional_branch();

        ctx.assign_name("varname_before_if", 12);
        ctx.declare_name("varname_in_if", 21);
        ctx.assign_name("varname_in_if", 22);

        insta::assert_debug_snapshot!(ctx.conditionals, @r###"
        [
            {
                "varname_before_if": [
                    11,
                    12,
                ],
                "varname_in_if": [
                    21,
                    22,
                ],
            },
        ]
        "###);

        // this pops the conditionals, creates phi nodes and assigns the conditional var to the phied version
        ctx.leave_conditional_branch();

        insta::assert_debug_snapshot!(ctx.conditionals, @"[]");
        insta::assert_debug_snapshot!(ctx.scope_tree, @r###"
        ScopeTree {
            scopes: [
                ScopeTreeNode {
                    parent: None,
                    is_block: false,
                    vars: {
                        "varname_before_if": Local(0),
                        "varname_in_if": Local(1),
                    },
                },
            ],
            current_scope: ScopeTreeHandle(0),
        }
        "###);
        insta::assert_debug_snapshot!(ctx.basic_blocks, @r###"
        {
            0: [
                (
                    0,
                    Some(
                        either($11, $12),
                    ),
                ),
                (
                    1,
                    Some(
                        either($21, $22),
                    ),
                ),
            ],
        }
        "###);
    }

    #[test]
    fn test_nested_phi() {
        let mut ctx = FromAstCtx::new();

        ctx.declare_name("varname", 11);
        insta::assert_debug_snapshot!(ctx.conditionals, @"[]");

        ctx.enter_conditional_branch();
        ctx.assign_name("varname", 12);
        insta::assert_debug_snapshot!(ctx.conditionals[0].get("varname").unwrap(), @r###"
        [
            11,
            12,
        ]
        "###);

        ctx.enter_conditional_branch();
        ctx.assign_name("varname", 13);
        insta::assert_debug_snapshot!(ctx.conditionals[1].get("varname").unwrap(), @r###"
        [
            11,
            12,
            13,
        ]
        "###);

        ctx.leave_conditional_branch();
        insta::assert_debug_snapshot!(ctx.conditionals[0].get("varname").unwrap(), @r###"
        [
            11,
            12,
            13,
        ]
        "###);

        ctx.leave_conditional_branch();
        insta::assert_debug_snapshot!(ctx.conditionals, @"[]");

        insta::assert_debug_snapshot!(ctx.scope_tree, @r###"
        ScopeTree {
            scopes: [
                ScopeTreeNode {
                    parent: None,
                    is_block: false,
                    vars: {
                        "varname": Local(1),
                    },
                },
            ],
            current_scope: ScopeTreeHandle(0),
        }
        "###);
        insta::assert_debug_snapshot!(ctx.get_block(0), @r###"
        [
            (
                0,
                Some(
                    either($11, $12, $13),
                ),
            ),
            (
                1,
                Some(
                    either($11, $12, $13),
                ),
            ),
        ]
        "###);
    } */
}
