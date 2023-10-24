use std::collections::BTreeMap;

use crate::{
    basic_blocks::{BasicBlockInstruction, BasicBlockModule, BreakableId},
    to_ast::Base54,
};

#[derive(Debug)]
pub struct ToAstContext<'a> {
    pub caught_error: Option<Base54>,
    pub for_in_of_value: Option<Base54>,
    pub module: &'a mut BasicBlockModule,
    pub inlined_variables: BTreeMap<usize, BasicBlockInstruction>,
    pub variable_use_count: BTreeMap<usize, u32>,
    pub emitted_vars: BTreeMap<usize, Base54>,
    pub gen_var_index: Base54,

    /// break/continue - tracking
    pub breakable_stack: Vec<BreakableStackItem>,
    pub gen_label_index: Base54,

    /// Object/Array patterns
    pub destructuring_patterns: BTreeMap<usize, Vec<Base54>>,
}

#[derive(Debug)]
pub struct BreakableStackItem {
    brk_id: BreakableId,
    label: Option<Base54>,
    /// loop and switch don't need labels
    is_anonymous: bool,
}

impl ToAstContext<'_> {
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

    pub fn variable_has_uses(&self, variable: usize) -> bool {
        self.variable_use_count.get(&variable).unwrap_or(&0) > &0
    }

    pub(crate) fn get_varname_for(&self, var_idx: usize) -> String {
        let var_idx = *self.emitted_vars.get(&var_idx).unwrap();
        var_idx.to_string()
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
