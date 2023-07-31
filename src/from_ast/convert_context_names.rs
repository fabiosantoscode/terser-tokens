use std::collections::{HashMap, HashSet};

use crate::basic_blocks::{BasicBlockInstruction, NonLocalId};

use super::{FromAstCtx, NonLocalInfo};

impl FromAstCtx {
    pub fn assign_name(&mut self, name: &str, value: usize) {
        if let Some(true) = self
            .nonlocalinfo
            .as_ref()
            .map(|loc| loc.nonlocals.contains(&name.into()))
        {
            // it's a nonlocal and deserves special treatment
            let nonlocal = self
                .scope_tree
                .lookup(name)
                .expect("nonlocal not in scope tree");
            self.push_instruction(BasicBlockInstruction::WriteNonLocal(
                NonLocalId(nonlocal),
                value,
            ));
        } else {
            let mut conditionals = self.conditionals.last_mut();
            match conditionals {
                Some(ref mut conditionals) => {
                    if let Some(conditional) = conditionals.get_mut(name) {
                        conditional.push(value);
                    } else {
                        conditionals.insert(name.to_string(), vec![value]);
                    }
                }
                None => {}
            };

            self.scope_tree.insert(name.into(), value);
        }
    }

    pub fn read_name(&mut self, name: &str) -> Option<usize> {
        if let Some(true) = self
            .nonlocalinfo
            .as_ref()
            .map(|loc| loc.nonlocals.contains(&name.into()))
        {
            let nonlocal = self
                .scope_tree
                .lookup(name)
                .expect("nonlocal not in scope tree");
            let nonlocal_read =
                self.push_instruction(BasicBlockInstruction::ReadNonLocal(NonLocalId(nonlocal)));
            Some(nonlocal_read)
        } else if self.is_unwritten_funscoped(name) {
            let deferred_undefined = self.push_instruction(BasicBlockInstruction::Undefined);
            self.assign_name(name, deferred_undefined);

            Some(deferred_undefined)
        } else if let Some(local) = self.scope_tree.lookup_in_function(name) {
            Some(local)
        } else if let Some(nonlocal) = self.scope_tree.lookup(name) {
            unreachable!("nonlocal {} not in nonlocalinfo", nonlocal)
        } else {
            None
        }
    }

    fn is_unwritten_funscoped(&mut self, name: &str) -> bool {
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
        self.conditionals.push(HashMap::new())
    }

    pub fn leave_conditional_branch(&mut self) {
        // phi nodes for conditionally assigned variables
        let to_phi = self
            .conditionals
            .pop()
            .expect("unbalanced conditional branch");
        let mut to_phi = to_phi
            .into_iter()
            .filter(|(_, phies)| phies.len() > 1)
            .collect::<Vec<_>>();
        to_phi.sort_by(|(a, _), (b, _)| a.cmp(b));

        for (varname, phies) in to_phi.into_iter() {
            let phi = BasicBlockInstruction::Phi(phies);
            let phi_idx = self.push_instruction(phi);
            self.scope_tree.insert(varname, phi_idx);
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
                let wanted_id = nonlocal_undef + 1;
                let read =
                    BasicBlockInstruction::WriteNonLocal(NonLocalId(wanted_id), nonlocal_undef);
                let nonlocal_id = self.push_instruction(read);
                assert_eq!(nonlocal_id, wanted_id);

                nonlocal_id
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
    use crate::from_ast::NonLocalInfo;

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

        assert_eq!(ctx.basic_blocks[0].len(), 2);

        ctx.read_name("read_before_assign"); // produces undefined instruction
        assert_eq!(ctx.basic_blocks[0].len(), 3);

        ctx.assign_name("read_after_assign", 1);
        ctx.read_name("read_after_assign"); // just reads
        assert_eq!(ctx.basic_blocks[0].len(), 3);

        insta::assert_debug_snapshot!(ctx.basic_blocks, @r###"
        [
            [
                (
                    0,
                    undefined,
                ),
                (
                    1,
                    write_non_local $$1 $0,
                ),
                (
                    2,
                    undefined,
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
                        "nonlocal_assigned_later": 1,
                        "read_after_assign": 1,
                        "read_before_assign": 2,
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
                        "nonlocal_assigned_later": 3,
                        "provided_nonlocal": 1,
                    },
                },
            ],
            current_scope: ScopeTreeHandle(0),
        }
        "###);

        // time to read a not-yet-declared var
        assert_eq!(ctx.read_name("assigned_later"), Some(4));

        // time to read a nonlocal
        assert_eq!(ctx.read_name("provided_nonlocal"), Some(5));
        insta::assert_debug_snapshot!(ctx.basic_blocks.get(0).unwrap()[5], @r###"
        (
            5,
            read_non_local $$1,
        )
        "###);

        // time to write it!
        ctx.assign_name("provided_nonlocal", 123);
        insta::assert_debug_snapshot!(ctx.basic_blocks.get(0).unwrap()[6], @r###"
        (
            6,
            write_non_local $$1 $123,
        )
        "###);

        // nonlocals are never conditional
        ctx.enter_conditional_branch();
        ctx.assign_name("provided_nonlocal", 777);
        insta::assert_debug_snapshot!(ctx.basic_blocks.get(0).unwrap()[7], @r###"
        (
            7,
            write_non_local $$1 $777,
        )
        "###);
        assert!(ctx.conditionals[0].is_empty());
    }

    #[test]
    fn test_general() {
        let mut ctx = FromAstCtx::new();

        ctx.assign_name("varname", 123);

        ctx.enter_conditional_branch();

        ctx.assign_name("conditional_varname", 456);
        ctx.assign_name("conditional_varname", 789);

        ctx.go_into_function(1, None, |ctx| {
            ctx.assign_name("conditional_varname", 999);

            insta::assert_debug_snapshot!(ctx.conditionals, @"[]");
            insta::assert_debug_snapshot!(ctx.scope_tree, @r###"
            ScopeTree {
                scopes: [
                    ScopeTreeNode {
                        parent: None,
                        is_block: false,
                        vars: {
                            "conditional_varname": 789,
                            "varname": 123,
                        },
                    },
                    ScopeTreeNode {
                        parent: Some(
                            ScopeTreeHandle(0),
                        ),
                        is_block: false,
                        vars: {
                            "conditional_varname": 999,
                        },
                    },
                ],
                current_scope: ScopeTreeHandle(1),
            }
            "###);

            Ok(())
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
                        "conditional_varname": 789,
                        "varname": 123,
                    },
                },
                ScopeTreeNode {
                    parent: Some(
                        ScopeTreeHandle(0),
                    ),
                    is_block: false,
                    vars: {
                        "conditional_varname": 999,
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
                        "conditional_varname": 1,
                        "varname": 123,
                    },
                },
                ScopeTreeNode {
                    parent: Some(
                        ScopeTreeHandle(0),
                    ),
                    is_block: false,
                    vars: {
                        "conditional_varname": 999,
                    },
                },
            ],
            current_scope: ScopeTreeHandle(0),
        }
        "###);
        insta::assert_debug_snapshot!(ctx.basic_blocks, @r###"
        [
            [
                (
                    1,
                    either($456, $789),
                ),
            ],
        ]
        "###);
    }
}
