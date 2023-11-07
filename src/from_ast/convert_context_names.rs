use std::collections::HashSet;

use crate::basic_blocks::{BasicBlockInstruction, NonLocalId, LHS};

use super::{FromAstCtx, NonLocalInfo, NonLocalOrLocal};

impl FromAstCtx {
    /// Check if a name exists in the current function
    pub fn read_local_name(&self, name: &str) -> Option<usize> {
        match self.scope_tree.lookup(name) {
            Some(NonLocalOrLocal::Local(loc)) => Some(loc),
            _ => None,
        }
    }

    /// Declare or re-declare a name {name} to contain the instruction varname {value}
    pub fn declare_name(&mut self, name: &str, value: usize) {
        match self.scope_tree.lookup(name) {
            Some(NonLocalOrLocal::NonLocal(nonlocal)) => {
                self.push_instruction(BasicBlockInstruction::Write(LHS::NonLocal(nonlocal), value));
            }
            Some(NonLocalOrLocal::Local(already_assigned)) => {
                self.push_instruction_with_varname(
                    already_assigned,
                    BasicBlockInstruction::Ref(value),
                );
            }
            None => {
                self.scope_tree
                    .insert(name.into(), NonLocalOrLocal::Local(value));
            }
        }
    }

    /// Assign or reassign {name}, which can be global, already-declared, or a nonlocal
    pub fn assign_name(&mut self, name: &str, value: usize) {
        if self.is_global_name(name) {
            self.push_instruction(BasicBlockInstruction::Write(
                LHS::Global(name.to_string()),
                value,
            ));
        } else {
            self.declare_name(name, value)
        }
    }

    /// Assign or reassign {name}, which can be global, already-declared, or a nonlocal
    pub fn get_lhs_for_name(&mut self, name: &str) -> LHS {
        if self.is_global_name(name) {
            LHS::Global(name.to_string())
        } else if let Some(NonLocalOrLocal::NonLocal(nonlocal)) = self.scope_tree.lookup(name) {
            LHS::NonLocal(nonlocal)
        } else if self.is_unwritten_funscoped(name) {
            let deferred_undefined = self.push_instruction(BasicBlockInstruction::Undefined);
            self.assign_name(name, deferred_undefined);
            LHS::Local(deferred_undefined)
        } else if let Some(NonLocalOrLocal::Local(local)) = self.scope_tree.lookup_in_function(name)
        {
            LHS::Local(local)
        } else {
            unreachable!()
        }
    }

    pub fn read_name(&mut self, name: &str) -> usize {
        if let Some(NonLocalOrLocal::NonLocal(nonlocal)) = self.scope_tree.lookup(name) {
            self.push_instruction(BasicBlockInstruction::Read(LHS::NonLocal(nonlocal)))
        } else if self.is_unwritten_funscoped(name) {
            let deferred_undefined = self.push_instruction(BasicBlockInstruction::Undefined);
            self.assign_name(name, deferred_undefined);

            deferred_undefined
        } else if let Some(local) = self.scope_tree.lookup_in_function(name) {
            local.unwrap_local()
        } else if let Some(nonlocal) = self.scope_tree.lookup(name) {
            unreachable!("nonlocal {:?} not in nonlocalinfo", nonlocal)
        } else {
            let read_global_ins =
                self.push_instruction(BasicBlockInstruction::Read(LHS::Global(name.to_string())));
            read_global_ins
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

    pub fn embed_nonlocals<'b>(
        &mut self,
        mut nonlocalinfo: NonLocalInfo,
        parent: Option<&'b NonLocalInfo>,
    ) {
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
                let nonlocal_undef = self.push_instruction(BasicBlockInstruction::Undefined);
                let wanted_id = NonLocalId(self.bump_var_index());
                let read = BasicBlockInstruction::Write(LHS::NonLocal(wanted_id), nonlocal_undef);
                self.push_instruction(read);

                NonLocalOrLocal::NonLocal(wanted_id)
            } else {
                self.scope_tree
                    .lookup(name)
                    .expect("nonlocal not in scope tree")
            };

            self.scope_tree.insert(name.clone(), nonlocal_id);
        }

        self.nonlocalinfo = Some(nonlocalinfo);
    }
}

#[cfg(test)]
mod tests {
    use crate::{basic_blocks::BasicBlockEnvironment, from_ast::NonLocalInfo};

    use super::*;

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

        assert_eq!(ctx.basic_blocks[0].len(), 3);

        ctx.read_name("read_before_assign"); // produces undefined instruction
        assert_eq!(ctx.basic_blocks[0].len(), 4);

        ctx.assign_name("read_after_assign", 1);
        ctx.read_name("read_after_assign"); // just reads
        assert_eq!(ctx.basic_blocks[0].len(), 4);

        insta::assert_debug_snapshot!(ctx.basic_blocks, @r###"
        [
            [
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
        ]
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
    fn test_general() {
        let mut ctx = FromAstCtx::new();

        ctx.declare_name("varname", 123);

        ctx.declare_name("conditional_varname", 456);
        ctx.assign_name("conditional_varname", 789);

        ctx.go_into_function(BasicBlockEnvironment::Function(false, false), None, |ctx| {
            ctx.assign_name("conditional_varname", 999);

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

            Ok(())
        })
        .unwrap();

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
    }
}
