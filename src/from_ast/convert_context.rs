use std::collections::BTreeMap;

use swc_ecma_ast::Ident;

use crate::basic_blocks::{
    BasicBlock, BasicBlockEnvironment, BasicBlockExit, BasicBlockGroup, BasicBlockInstruction,
    BasicBlockModule, Export, FunctionId, Import, ModuleSummary, NonLocalId, StructuredFlow,
};
use crate::block_ops::{block_group_to_structured_flow, normalize_basic_blocks, normalize_module};
use crate::scope::ScopeTree;

use super::NonLocalInfo;

#[derive(Debug)]
pub struct FromAstCtx {
    pub basic_blocks: BTreeMap<usize, Vec<(usize, Option<BasicBlockInstruction>)>>,
    pub exits: BTreeMap<usize, BasicBlockExit>,
    pub current_function_index: Option<FunctionId>,
    pub function_index: FunctionId, // TODO duplicate of current_function_index?
    pub block_index: usize,
    pub var_index: usize,
    pub conditionals: Vec<BTreeMap<String, Vec<usize>>>,
    pub scope_tree: ScopeTree<String, NonLocalOrLocal>,
    pub label_tracking: Vec<(NestedIntoStatement, Vec<usize>)>,
    pub functions: BTreeMap<FunctionId, BasicBlockGroup>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub nonlocalinfo: Option<NonLocalInfo>,
}

impl FromAstCtx {
    pub fn new() -> Self {
        Self {
            basic_blocks: BTreeMap::from_iter(vec![(0, vec![])]),
            exits: BTreeMap::from_iter(vec![(0, BasicBlockExit::Fallthrough)]),
            current_function_index: Some(FunctionId(0)),
            function_index: FunctionId(0),
            block_index: 0,
            var_index: 0,
            conditionals: vec![],
            scope_tree: ScopeTree::new(),
            label_tracking: vec![],
            functions: BTreeMap::new(),
            imports: vec![],
            exports: vec![],
            nonlocalinfo: None,
        }
    }

    pub fn push_instruction(&mut self, node: BasicBlockInstruction) -> usize {
        let id = self.var_index;
        self.var_index += 1;
        self.basic_blocks
            .get_mut(&self.block_index)
            .unwrap()
            .push((id, Some(node)));
        id
    }

    /// This is used to arbitrarily set a variable name. Get it from .bump_var_index() to ensure it won't be used elsewhere, and then call this function later.
    /// This is used for function declarations, which are hoisted to the top of the scope before they're even converted.
    pub fn arbitrarily_set_id(&mut self, id: usize, node: BasicBlockInstruction) {
        assert!(
            id < self.var_index,
            "fulfill_deferred_instruction: id {} is out of bounds {}",
            id,
            self.var_index
        );

        let instructions = self
            .basic_blocks
            .get_mut(&self.block_index)
            .unwrap();
        let position = instructions
            .iter()
            .position(|(i, _)| *i == id)
            .expect("arbitrarily_set_id: id not found");

        let prev_value = instructions[position].1.replace(node);

        assert!(
            prev_value.is_none(),
            "arbitrarily_set_id: id {} is already used",
            id
        );
    }

    /// Get a new ID. It can be used to make sure varnames don't collide with other kinds of IDs, or to generate an ID that will be fulfilled later.
    pub fn bump_var_index(&mut self) -> usize {
        let id = self.var_index;
        self.var_index += 1;
        self.basic_blocks
            .get_mut(&self.block_index)
            .unwrap()
            .push((id, None));
        id
    }

    #[cfg(test)]
    pub fn get_block(&self, index: usize) -> &[(usize, Option<BasicBlockInstruction>)] {
        &self.basic_blocks[&index]
    }

    pub fn set_exit(&mut self, at: usize, new_exit: BasicBlockExit) {
        self.exits.insert(at, new_exit);
    }

    pub fn current_block_index(&self) -> usize {
        self.block_index
    }

    pub fn wrap_up_block(&mut self) -> usize {
        self.block_index += 1;
        self.basic_blocks.insert(self.block_index, vec![]);
        self.exits
            .insert(self.block_index, Default::default());
        self.block_index
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

    fn snip_blocks(&mut self) -> BTreeMap<usize, BasicBlock> {
        self.block_index += 1;

        let basic_blocks = std::mem::replace(
            &mut self.basic_blocks,
            BTreeMap::from_iter(vec![(self.block_index, Default::default())]),
        );
        let exits = std::mem::replace(
            &mut self.exits,
            BTreeMap::from_iter(vec![(self.block_index, Default::default())]),
        );

        exits
            .into_iter()
            .zip(basic_blocks.into_iter())
            .map(|((exit_id, exit), (block_id, block))| {
                assert_eq!(exit_id, block_id);

                let block = block.into_iter().filter_map(|(varname, ins)| match ins {
                    Some(ins) => Some((varname, ins)),
                    None => None,
                });
                (block_id, BasicBlock::new(block.collect(), exit))
            })
            .collect()
    }

    pub(crate) fn wrap_up_blocks(&mut self) -> (FunctionId, BTreeMap<usize, BasicBlock>) {
        let blocks = self.snip_blocks();
        let blocks = normalize_basic_blocks(blocks);

        (
            self.current_function_index.take().expect(
                "wrap_up_blocks must be called within a current function (or module) context",
            ),
            blocks,
        )
    }

    pub(crate) fn wrap_up_module(&mut self, summary: ModuleSummary) -> BasicBlockModule {
        let (id, blocks) = self.wrap_up_blocks();
        let module_bg = BasicBlockGroup {
            id,
            blocks,
            environment: BasicBlockEnvironment::Module,
        };

        self.functions.insert(id, module_bg);

        let mut module = BasicBlockModule {
            summary,
            functions: std::mem::take(&mut self.functions),
            imports: std::mem::take(&mut self.imports),
            exports: std::mem::take(&mut self.exports),
        };

        normalize_module(&mut module);

        module
    }

    pub fn go_into_function<C>(
        &mut self,
        environment: BasicBlockEnvironment,
        nonlocalinfo: Option<NonLocalInfo>,
        convert_in_function: C,
    ) -> Result<&BasicBlockGroup, String>
    where
        C: FnOnce(&mut Self) -> Result<(), String>,
    {
        self.function_index.0 += 1;
        let function_index = self.function_index;

        self.scope_tree.go_into_function_scope();

        // functions are shared between contexts
        let functions = std::mem::take(&mut self.functions);
        let scope_tree = std::mem::take(&mut self.scope_tree);

        let blk_index = 0;

        let mut inner_ctx = Self {
            basic_blocks: BTreeMap::from_iter(vec![(blk_index, vec![])]),
            exits: BTreeMap::from_iter(vec![(blk_index, Default::default())]),
            current_function_index: Some(function_index),
            function_index: self.function_index,
            block_index: blk_index,
            var_index: self.var_index,
            conditionals: vec![],
            scope_tree,
            label_tracking: vec![],
            functions,
            imports: vec![],
            exports: vec![],
            nonlocalinfo: None,
        };
        if let Some(nonlocalinfo) = nonlocalinfo {
            inner_ctx.embed_nonlocals(nonlocalinfo, self.nonlocalinfo.as_ref());
        }

        convert_in_function(&mut inner_ctx)?;

        let (id, blocks) = inner_ctx.wrap_up_blocks();
        let function_bg = BasicBlockGroup {
            id,
            blocks,
            environment,
        };

        inner_ctx.functions.insert(id, function_bg);

        self.functions = std::mem::take(&mut inner_ctx.functions);
        self.scope_tree = std::mem::take(&mut inner_ctx.scope_tree);

        self.scope_tree.leave_scope();

        // collect global function registry, global var numbering
        self.var_index = inner_ctx.var_index;
        self.function_index = inner_ctx.function_index;

        Ok(self
            .functions
            .get(&function_index)
            .expect("convert_in_function callback never called wrap_up_function()"))
    }

    pub fn with_temporary_instructions<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        assert!(self.basic_blocks.last_key_value().unwrap().1.is_empty(), "in with_temporary_instructions, you must clear preceding blocks before entering the callback");
        //self.current_block_index += 1;

        let old_basic_blocks = std::mem::replace(
            &mut self.basic_blocks,
            BTreeMap::from_iter(vec![(self.block_index, Default::default())]),
        );
        let old_exits = std::mem::replace(
            &mut self.exits,
            BTreeMap::from_iter(vec![(self.block_index, Default::default())]),
        );

        let result = f(self);

        assert!(self.basic_blocks.len() == 1 && self.basic_blocks.first_key_value().unwrap().1.is_empty(), "in with_temporary_instructions, you must snip all the blocks before returning from the callback");
        self.block_index -= 1;

        self.exits = old_exits;
        self.basic_blocks = old_basic_blocks;
        result
    }

    pub fn snip_into_structured_flow(&mut self) -> StructuredFlow {
        block_group_to_structured_flow(self.snip_blocks())
    }

    /// Inject `blocks` into the current context. The `blocks` are expected to have indices just after our own.
    pub fn inject_blocks(&mut self, blocks: BTreeMap<usize, BasicBlock>) {
        for (id, BasicBlock { instructions, exit }) in blocks.into_iter() {
            let old_exi = self.exits.insert(id, exit);
            let old_blk = self.basic_blocks.insert(
                id,
                instructions
                    .into_iter()
                    .map(|(varname, ins)| (varname, Some(ins)))
                    .collect(),
            );

            assert!(old_exi == None, "inject_blocks: block {id} already exists");
            assert!(old_blk == None, "inject_blocks: block {id} already exists");

            self.block_index += 1;
            assert_eq!(id, self.block_index);
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum NestedIntoStatement {
    Labelled(String),
    Unlabelled,
}

#[derive(PartialEq, Eq, Clone)]
pub enum NonLocalOrLocal {
    NonLocal(NonLocalId),
    Local(usize),
}

impl NonLocalOrLocal {
    pub fn unwrap_local(&self) -> usize {
        match self {
            NonLocalOrLocal::NonLocal(_) => panic!("unwrap_local called on a nonlocal variable"),
            NonLocalOrLocal::Local(id) => *id,
        }
    }
}

impl std::fmt::Debug for NonLocalOrLocal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NonLocalOrLocal::NonLocal(id) => write!(f, "NonLocal({})", id.0),
            NonLocalOrLocal::Local(id) => write!(f, "Local({})", id),
        }
    }
}
