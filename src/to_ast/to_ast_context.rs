use std::collections::{BTreeMap, BTreeSet};

use crate::{
    analyze::count_variable_uses,
    basic_blocks::{BasicBlockInstruction, BasicBlockModule, BreakableId, StructuredFlow},
    block_ops::{block_group_to_structured_flow, remove_phi},
};

use super::{get_inlined_variables, Base54};

#[derive(Debug)]
pub struct ToAstContext<'a> {
    pub caught_error: Option<Base54>,
    pub for_in_of_value: Option<Base54>,
    pub module: &'a mut BasicBlockModule,
    pub inlined_variables: BTreeMap<usize, BasicBlockInstruction>,
    pub variable_use_count: BTreeMap<usize, u32>,
    pub emitted_vars: BTreeMap<usize, Base54>,
    pub gen_var_index: Base54,

    /// Variables that we want to be emitted later, when the function ends
    pub vars_later: BTreeSet<usize>,
    /// If true, we will not emit any variable declarations and instead enqueue with
    /// queue_var_for_later
    pub forbid_var_decls: bool,

    /// break/continue - tracking
    pub breakable_stack: Vec<BreakableStackItem>,
    pub gen_label_index: Base54,

    /// Object/Array patterns
    pub destructuring_patterns: BTreeMap<usize, Vec<Base54>>,

    /// Classes
    pub classes: BTreeMap<usize, Option<usize>>,
}

#[derive(Debug)]
pub struct BreakableStackItem {
    brk_id: BreakableId,
    label: Option<Base54>,
    /// loop and switch don't need labels
    is_anonymous: bool,
}

impl<'a> ToAstContext<'a> {
    pub fn new(block_module: &'a mut BasicBlockModule) -> (ToAstContext<'a>, StructuredFlow) {
        let phied = block_module
            .iter_all_instructions()
            .flat_map(|(_, _, varname, ins)| match ins {
                BasicBlockInstruction::Phi(vars) => vec![&vec![varname], vars]
                    .into_iter()
                    .flatten()
                    .copied()
                    .collect(),
                _ => vec![],
            })
            .collect();

        for (_, block_group) in block_module.iter_mut() {
            remove_phi(block_group);
        }

        let variable_use_count = count_variable_uses(&block_module);
        let inlined_variables = get_inlined_variables(&block_module, &variable_use_count, phied);

        let tree = block_group_to_structured_flow(block_module.take_top_level_stats().blocks);

        let ctx = ToAstContext {
            caught_error: None,
            for_in_of_value: None,
            module: block_module,
            inlined_variables,
            vars_later: Default::default(),
            forbid_var_decls: false,
            variable_use_count,
            emitted_vars: BTreeMap::new(),
            gen_var_index: Base54::new(0),
            breakable_stack: vec![],
            gen_label_index: Base54::new(0),
            destructuring_patterns: BTreeMap::new(),
            classes: BTreeMap::new(),
        };

        (ctx, tree)
    }

    /// get/create a var, and return whether we should create a var decl for it
    pub fn create_or_assign_to_var(&mut self, variable: usize) -> (bool, String) {
        if self.forbid_var_decls {
            self.queue_var_for_later(variable);

            let variable = if self.has_varname_for(variable) {
                self.get_varname_for(variable)
            } else {
                self.create_varname_for(variable)
            };

            (false, variable)
        } else if self.has_varname_for(variable) {
            let variable = self.get_varname_for(variable);

            (false, variable)
        } else {
            let varname = self.create_varname_for(variable);

            (true, varname)
        }
    }

    /// In a fake IIFE (IE the compiler needs to output an IIFE but it's not encoded in the
    /// instructions), we need to create a varname later (outside the IIFE)
    pub fn queue_var_for_later(&mut self, var_idx: usize) {
        assert!(
            self.forbid_var_decls,
            "queueing a var when vars are not forbidden"
        );

        self.vars_later.insert(var_idx);
    }

    pub fn dequeue_enqueued_vars(&mut self) -> BTreeSet<usize> {
        std::mem::take(&mut self.vars_later)
    }

    pub fn get_caught_error(&mut self) -> String {
        let err = self
            .caught_error
            .take()
            .expect("reference to caught error must be inside catch block");

        err.to_string()
    }

    pub fn set_caught_error(&mut self) -> String {
        let error_index = self.gen_var_index;
        self.gen_var_index = error_index.next();
        self.caught_error = Some(error_index);
        error_index.to_string()
    }

    pub fn get_for_in_of_value(&mut self) -> String {
        let value = self
            .for_in_of_value
            .take()
            .expect("reference to for-in/of value must be inside for-in/of block");

        value.to_string()
    }

    pub fn set_for_in_of_value(&mut self) -> String {
        let value_index = self.gen_var_index;
        self.gen_var_index = value_index.next();
        self.for_in_of_value = Some(value_index);
        value_index.to_string()
    }

    /// Create variables found in a destructuring pattern
    pub(crate) fn create_pattern(&mut self, variable: usize, len: usize) -> Vec<Base54> {
        let new_pattern = (0..len)
            .map(|_| {
                let gen = self.gen_var_index;
                self.gen_var_index = gen.next();
                gen
            })
            .collect::<Vec<_>>();

        self.destructuring_patterns
            .insert(variable, new_pattern.clone());

        new_pattern
    }

    pub fn register_class(&mut self, varname: usize, extends: Option<usize>) {
        self.classes.insert(varname, extends);
    }

    pub fn take_class(&mut self, varname: usize) -> Option<usize> {
        self.classes
            .remove(&varname)
            .expect("taking a class that hasn't been prepared")
    }

    pub(crate) fn get_varname_for_pattern(&self, variable: usize, idx: usize) -> String {
        self.destructuring_patterns
            .get(&variable)
            .expect("pattern not found")
            .get(idx)
            .expect("pattern index out of bounds")
            .clone()
            .to_string()
    }

    pub fn will_be_inlined(&self, variable: usize) -> bool {
        self.inlined_variables.contains_key(&variable)
    }

    pub fn get_inlined_expression(&mut self, var_idx: usize) -> Option<BasicBlockInstruction> {
        self.inlined_variables.remove(&var_idx)
    }

    pub fn peek_inlined_expression(&mut self, var_idx: usize) -> Option<&BasicBlockInstruction> {
        self.inlined_variables.get(&var_idx)
    }

    pub fn variable_has_uses(&self, variable: usize) -> bool {
        self.variable_use_count.get(&variable).unwrap_or(&0) > &0
    }

    pub(crate) fn has_varname_for(&self, var_idx: usize) -> bool {
        self.emitted_vars.contains_key(&var_idx)
    }

    pub(crate) fn get_varname_for(&self, var_idx: usize) -> String {
        if let Some(var_idx) = self.emitted_vars.get(&var_idx) {
            var_idx.to_string()
        } else {
            panic!("variable ${var_idx} not found")
        }
    }

    pub(crate) fn create_varname_for(&mut self, var_idx: usize) -> String {
        assert!(!self.emitted_vars.contains_key(&var_idx));
        let gen = self.gen_var_index;
        self.gen_var_index = gen.next();
        self.emitted_vars.insert(var_idx, gen);
        gen.to_string()
    }

    pub(crate) fn enter_breakable(
        &mut self,
        brk_id: &BreakableId,
        is_anonymous: bool,
        in_breakable: impl Fn(&mut ToAstContext<'_>) -> swc_ecma_ast::Stmt,
    ) -> swc_ecma_ast::Stmt {
        self.breakable_stack.push(BreakableStackItem {
            brk_id: brk_id.clone(),
            label: None,
            is_anonymous,
        });
        let stmt = in_breakable(self);
        let collected = self.breakable_stack.pop().unwrap();

        if let Some(label) = collected.label {
            swc_ecma_ast::Stmt::Labeled(swc_ecma_ast::LabeledStmt {
                label: swc_ecma_ast::Ident::new(label.to_string().into(), Default::default()),
                body: Box::new(stmt),
                span: Default::default(),
            })
        } else {
            stmt
        }
    }

    pub(crate) fn break_label_for(&mut self, brk_id: &BreakableId) -> Option<Base54> {
        let closest_anonymous = self.breakable_stack.iter().rev().find_map(|b| {
            if b.is_anonymous {
                Some(b.brk_id)
            } else {
                None
            }
        });

        let ref mut target = self
            .breakable_stack
            .iter_mut()
            .rev()
            .find_map(|b| if b.brk_id == *brk_id { Some(b) } else { None })
            .expect("breakable id must be in breakable stack");

        if closest_anonymous == Some(*brk_id) {
            None // no label needed
        } else if let Some(label) = target.label {
            Some(label)
        } else {
            let new_label = Some(self.gen_label_index);
            self.gen_label_index = self.gen_label_index.next();
            target.label = new_label;
            new_label
        }
    }
}
