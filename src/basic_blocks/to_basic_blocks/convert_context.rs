use std::collections::HashMap;

use crate::basic_blocks::basic_block::{BasicBlockExit, BasicBlockInstruction};
use crate::scope::scope::Scope;
use swc_ecma_ast::{Expr, Ident};

use super::convert::expr_to_basic_blocks;

#[derive(Debug, PartialEq, Eq)]
pub enum NestedIntoStatement {
    Labelled(String),
    Unlabelled,
}

pub struct ConvertContext {
    pub basic_blocks: Vec<Vec<(usize, BasicBlockInstruction)>>,
    pub exits: Vec<Option<BasicBlockExit>>,
    pub var_index: usize,
    pub conditionals: Vec<HashMap<String, Vec<usize>>>,
    pub scope: Scope,
    pub label_tracking: Vec<(NestedIntoStatement, Vec<usize>)>,
}

impl ConvertContext {
    pub fn new() -> Self {
        Self {
            basic_blocks: vec![vec![]],
            exits: vec![None],
            var_index: 0,
            conditionals: vec![],
            scope: Scope::new(false),
            label_tracking: vec![],
        }
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

    pub fn assign_maybe_conditionally(&mut self, name: &str, value: usize) {
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

    pub fn push_conditionals_context(&mut self) {
        self.conditionals.push(HashMap::new())
    }

    pub fn pop_conditionals_context(&mut self) -> Option<HashMap<String, Vec<usize>>> {
        self.conditionals.pop()
    }

    pub fn push_phi_assignments(
        &mut self,
        conditionally_assigned: Option<HashMap<String, Vec<usize>>>,
    ) {
        // phi nodes for conditionally assigned variables
        let to_phi = conditionally_assigned.unwrap();
        let mut to_phi = to_phi
            .iter()
            .filter(|(_name, phies)| phies.len() > 1)
            .collect::<Vec<_>>();
        if to_phi.len() > 0 {
            to_phi.sort_by_key(|(name, _)| *name);
            for (varname, phies) in to_phi {
                let phi = BasicBlockInstruction::Phi(phies.clone());
                let phi_idx = self.push_instruction(phi);
                self.scope.insert(varname.clone(), phi_idx);
            }
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
}
