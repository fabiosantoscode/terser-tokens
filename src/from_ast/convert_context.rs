use std::collections::BTreeMap;

use swc_ecma_ast::Ident;

use crate::basic_blocks::{
    BasicBlock, BasicBlockEnvironment, BasicBlockExit, BasicBlockGroup, BasicBlockInstruction,
    BasicBlockModule, ExitType, Export, FunctionId, Import, ModuleSummary, NonLocalId,
};
use crate::block_ops::{normalize_basic_blocks, normalize_module};
use crate::scope::ScopeTree;

use super::NonLocalInfo;

#[derive(Debug)]
pub struct FromAstCtx {
    pub basic_blocks: Vec<Vec<(usize, Option<BasicBlockInstruction>)>>,
    pub exits: Vec<Option<BasicBlockExit>>,
    pub var_index: usize,
    pub conditionals: Vec<BTreeMap<String, Vec<usize>>>,
    pub scope_tree: ScopeTree<NonLocalOrLocal>,
    pub label_tracking: Vec<(NestedIntoStatement, Vec<usize>)>,
    pub current_function_index: Option<FunctionId>,
    pub function_index: FunctionId,
    pub functions: BTreeMap<FunctionId, BasicBlockGroup>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub nonlocalinfo: Option<NonLocalInfo>,
}

impl FromAstCtx {
    pub fn new() -> Self {
        Self {
            basic_blocks: vec![vec![]],
            exits: vec![None],
            var_index: 0,
            conditionals: vec![],
            scope_tree: ScopeTree::new(),
            label_tracking: vec![],
            function_index: FunctionId(0),
            current_function_index: Some(FunctionId(0)),
            functions: BTreeMap::new(),
            imports: vec![],
            exports: vec![],
            nonlocalinfo: None,
        }
    }

    pub fn push_instruction(&mut self, node: BasicBlockInstruction) -> usize {
        let id = self.var_index;
        self.var_index += 1;
        self.basic_blocks.last_mut().unwrap().push((id, Some(node)));
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

        let instructions = self.basic_blocks.last_mut().unwrap();
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
        self.basic_blocks.last_mut().unwrap().push((id, None));
        id
    }

    pub fn push_instruction_to_nth_block(
        &mut self,
        node: BasicBlockInstruction,
        n: usize,
    ) -> usize {
        let id = self.var_index;
        self.var_index += 1;
        self.basic_blocks[n].push((id, Some(node)));
        id
    }

    pub fn set_exit(&mut self, at: usize, new_exit: BasicBlockExit) {
        self.exits[at] = Some(new_exit)
    }

    pub fn current_block_index(&self) -> usize {
        self.basic_blocks.len() - 1
    }

    pub fn wrap_up_block(&mut self) -> usize {
        self.basic_blocks.push(vec![]);
        self.exits.push(None);
        self.current_block_index()
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

    pub(crate) fn wrap_up_blocks(&mut self) -> (FunctionId, BTreeMap<usize, BasicBlock>) {
        let exit_count = self.exits.len();
        let mut exits = Vec::with_capacity(exit_count);

        for i in 0..exit_count {
            let e = &self.exits[i];
            match e {
                Some(exit) => exits.push(exit.clone()),
                None => {
                    if i + 1 >= exit_count {
                        let undef_ret =
                            self.push_instruction_to_nth_block(BasicBlockInstruction::Undefined, i);
                        exits.push(BasicBlockExit::ExitFn(ExitType::Return, undef_ret));
                    } else {
                        exits.push(BasicBlockExit::Jump(i + 1));
                    }
                }
            }
        }

        let basic_blocks = std::mem::replace(&mut self.basic_blocks, vec![]);

        let blocks: BTreeMap<usize, BasicBlock> = exits
            .into_iter()
            .zip(basic_blocks.into_iter())
            .enumerate()
            .map(|(i, (exit, block))| {
                let block = block.into_iter().filter_map(|(varname, ins)| match ins {
                    Some(ins) => Some((varname, ins)),
                    None => None,
                });
                (i, BasicBlock::new(block.collect(), exit))
            })
            .collect();

        let blocks = normalize_basic_blocks(blocks);

        // reset state
        self.basic_blocks.clear();
        self.exits.clear();

        (
            self.current_function_index.take().expect(
                "wrap_up_blocks must be called within a current function (or module) context",
            ),
            blocks,
        )
    }

    pub(crate) fn wrap_up_module(&mut self, summary: ModuleSummary) -> BasicBlockModule {
        use std::mem::replace;

        let (id, blocks) = self.wrap_up_blocks();
        let module_bg = BasicBlockGroup {
            id,
            blocks,
            environment: BasicBlockEnvironment::Module,
        };

        self.functions.insert(id, module_bg);

        let mut module = BasicBlockModule {
            summary,
            functions: replace(&mut self.functions, Default::default()),
            imports: replace(&mut self.imports, Default::default()),
            exports: replace(&mut self.exports, Default::default()),
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
        let functions = std::mem::replace(&mut self.functions, BTreeMap::new());
        let scope_tree = std::mem::replace(&mut self.scope_tree, ScopeTree::new());

        let mut inner_ctx = Self {
            basic_blocks: vec![vec![]],
            exits: vec![None],
            var_index: self.var_index,
            conditionals: vec![],
            scope_tree,
            label_tracking: vec![],
            function_index: self.function_index,
            current_function_index: Some(function_index),
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

        self.functions = std::mem::replace(&mut inner_ctx.functions, BTreeMap::new());
        self.scope_tree = std::mem::replace(&mut inner_ctx.scope_tree, ScopeTree::new());

        self.scope_tree.leave_scope();

        // collect global function registry, global var numbering
        self.var_index = inner_ctx.var_index;
        self.function_index = inner_ctx.function_index;

        Ok(self
            .functions
            .get(&function_index)
            .expect("convert_in_function callback never called wrap_up_function()"))
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
    pub fn unwrap_nonlocal(&self) -> NonLocalId {
        match self {
            NonLocalOrLocal::NonLocal(id) => *id,
            NonLocalOrLocal::Local(_) => panic!("unwrap_nonlocal called on a local variable"),
        }
    }
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
