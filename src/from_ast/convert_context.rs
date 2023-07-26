use std::collections::HashMap;

use swc_ecma_ast::{Expr, Ident};

use crate::basic_blocks::{
    BasicBlockExit, BasicBlockGroup, BasicBlockInstruction, Export, FunctionId, Import,
};
use crate::scope::Scope;

use super::convert::expr_to_basic_blocks;

#[derive(Debug, PartialEq, Eq)]
pub enum NestedIntoStatement {
    Labelled(String),
    Unlabelled,
}

#[derive(Debug)]
pub struct FromAstCtx {
    pub basic_blocks: Vec<Vec<(usize, BasicBlockInstruction)>>,
    pub exits: Vec<Option<BasicBlockExit>>,
    pub var_index: usize,
    pub conditionals: Vec<HashMap<String, Vec<usize>>>,
    pub scope: Scope,
    pub label_tracking: Vec<(NestedIntoStatement, Vec<usize>)>,
    function_index: FunctionId,
    pub functions: HashMap<FunctionId, BasicBlockGroup>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
}

impl FromAstCtx {
    pub fn new() -> Self {
        Self {
            basic_blocks: vec![vec![]],
            exits: vec![None],
            var_index: 0,
            conditionals: vec![],
            scope: Scope::new(false),
            label_tracking: vec![],
            function_index: FunctionId(0),
            functions: HashMap::new(),
            imports: vec![],
            exports: vec![],
        }
    }

    pub fn go_into_function<C>(&mut self, convert_in_function: C) -> Result<FunctionId, String>
    where
        C: FnOnce(&mut Self) -> Result<BasicBlockGroup, String>,
    {
        let function_index = self.function_index;
        self.function_index.0 += 1;

        // functions are shared between contexts
        let functions = std::mem::replace(&mut self.functions, HashMap::new());

        let mut ctx = Self {
            basic_blocks: vec![vec![]],
            exits: vec![None],
            var_index: self.var_index,
            conditionals: vec![],
            scope: self.scope.go_into_function(),
            label_tracking: vec![],
            function_index: self.function_index,
            functions,
            imports: vec![],
            exports: vec![],
        };

        let blocks = convert_in_function(&mut ctx)?;

        self.functions = std::mem::replace(&mut ctx.functions, HashMap::new());

        // collect global function registry, global var numbering
        self.var_index = ctx.var_index;
        self.function_index = ctx.function_index;

        // Add the new function
        self.functions.insert(function_index, blocks);

        Ok(function_index)
    }

    pub fn push_instruction(&mut self, node: BasicBlockInstruction) -> usize {
        let id = self.var_index;
        self.var_index += 1;
        self.basic_blocks.last_mut().unwrap().push((id, node));
        id
    }

    pub fn push_instruction_to_nth_block(
        &mut self,
        node: BasicBlockInstruction,
        n: usize,
    ) -> usize {
        let id = self.var_index;
        self.var_index += 1;
        self.basic_blocks[n].push((id, node));
        id
    }

    pub fn set_exit(&mut self, at: usize, new_exit: BasicBlockExit) {
        self.exits[at] = Some(new_exit)
    }

    pub fn assign_name(&mut self, name: &str, value: usize) {
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

        self.scope.insert(name.into(), value);
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
            self.scope.insert(varname, phi_idx);
        }
    }

    pub fn current_block_index(&self) -> usize {
        self.basic_blocks.len() - 1
    }

    pub fn wrap_up_block(&mut self) -> usize {
        self.basic_blocks.push(vec![]);
        self.exits.push(None);
        self.current_block_index()
    }

    pub fn create_gapped_block(&mut self, expr: &Expr) -> (usize, usize, usize) {
        let block_idx = self.current_block_index();
        let expr_idx = expr_to_basic_blocks(self, expr);
        let next_block_idx = self.current_block_index();
        self.wrap_up_block();

        (block_idx, expr_idx, next_block_idx)
    }

    pub fn push_label(&mut self, label: NestedIntoStatement) {
        self.label_tracking.push((label, vec![]));
    }

    pub fn pop_label(&mut self) -> Vec<usize> {
        self.label_tracking.pop().unwrap().1
    }

    pub fn register_break(&mut self, label: &Option<Ident>) {
        let jump_from = self.wrap_up_block();

        match label {
            Some(l) => {
                let needle = NestedIntoStatement::Labelled(l.sym.to_string());
                let position = self
                    .label_tracking
                    .iter_mut()
                    .rev()
                    .find(|(target, _)| target == &needle)
                    .expect("parse step does not allow `break` to jump to a non-existent label");

                (*position).1.push(jump_from);
            }
            None => {
                self.label_tracking
                    .last_mut()
                    .expect("parse step does not allow `break` when there's nothing to break from")
                    .1
                    .push(jump_from);
            }
        }
    }

    pub fn register_import(&mut self, import: Import) {
        self.imports.push(import);
    }

    pub fn register_export(&mut self, export: Export) {
        self.exports.push(export);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_general() {
        let mut ctx = FromAstCtx::new();

        ctx.assign_name("varname", 123);

        ctx.enter_conditional_branch();

        ctx.assign_name("conditional_varname", 456);
        ctx.assign_name("conditional_varname", 789);

        ctx.go_into_function(|ctx| {
            ctx.assign_name("conditional_varname", 999);

            insta::assert_debug_snapshot!(ctx, @r###"
            FromAstCtx {
                basic_blocks: [
                    [],
                ],
                exits: [
                    None,
                ],
                var_index: 0,
                conditionals: [],
                scope: Scope (function) {
                    vars: [
                        "conditional_varname: 999",
                    ],
                    parent: Scope (function) {
                        vars: [
                            "conditional_varname: 789",
                            "varname: 123",
                        ],
                    },
                },
                label_tracking: [],
                function_index: FunctionId(1),
                functions: {},
                imports: [],
                exports: [],
            }
            "###);

            Ok(BasicBlockGroup::from_asts(vec![]))
        })
        .unwrap();

        insta::assert_debug_snapshot!(ctx, @r###"
        FromAstCtx {
            basic_blocks: [
                [],
            ],
            exits: [
                None,
            ],
            var_index: 0,
            conditionals: [
                {
                    "conditional_varname": [
                        456,
                        789,
                    ],
                },
            ],
            scope: Scope (function) {
                vars: [
                    "conditional_varname: 789",
                    "varname: 123",
                ],
            },
            label_tracking: [],
            function_index: FunctionId(1),
            functions: {
                FunctionId(0): ,
            },
            imports: [],
            exports: [],
        }
        "###);

        // this pops the conditionals, creates phi nodes and assigns the conditional var to the phied version
        ctx.leave_conditional_branch();

        insta::assert_debug_snapshot!(ctx.conditionals, @"[]");
        insta::assert_debug_snapshot!(ctx.basic_blocks, @r###"
        [
            [
                (
                    0,
                    either($456, $789),
                ),
            ],
        ]
        "###);
        insta::assert_debug_snapshot!(ctx.scope, @r###"
        Scope (function) {
            vars: [
                "conditional_varname: 0",
                "varname: 123",
            ],
        }
        "###);
    }
}
