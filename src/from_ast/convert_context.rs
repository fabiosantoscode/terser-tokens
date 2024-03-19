use std::collections::BTreeMap;

use swc_ecma_ast::Ident;

use crate::basic_blocks::{
    BasicBlock, BasicBlockEnvironment, BasicBlockGroup, BasicBlockInstruction, BasicBlockModule,
    BreakableId, Export, FunctionId, Import, ModuleSummary, NonLocalId, StructuredFlow,
};
use crate::block_ops::{normalize_basic_blocks_tree, normalize_module};
use crate::scope::ScopeTree;

use super::NonLocalInfo;

#[derive(Debug)]
pub struct FromAstCtx {
    pub current_function_index: Option<FunctionId>,
    pub function_index: FunctionId, // TODO duplicate of current_function_index?
    pub var_index: usize,
    pub conditionals: Vec<BTreeMap<String, Vec<usize>>>,
    pub scope_tree: ScopeTree<String, NonLocalOrLocal>,
    pub label_tracking: Vec<(NestedIntoStatement, BreakableId)>,
    pub break_index: usize,
    pub functions: BTreeMap<FunctionId, BasicBlockGroup>,
    pub imports: Vec<Import>,
    pub exports: Vec<Export>,
    pub nonlocalinfo: Option<NonLocalInfo>,
}

impl FromAstCtx {
    pub fn new() -> Self {
        Self {
            current_function_index: Some(FunctionId(0)),
            function_index: FunctionId(0),
            var_index: 0,
            conditionals: vec![],
            scope_tree: ScopeTree::new(),
            label_tracking: vec![],
            break_index: 0,
            functions: BTreeMap::new(),
            imports: vec![],
            exports: vec![],
            nonlocalinfo: None,
        }
    }

    pub fn push_instruction(
        &mut self,
        node: BasicBlockInstruction,
    ) -> (Vec<StructuredFlow>, usize) {
        let id = self.var_index;
        self.var_index += 1;
        let flow = StructuredFlow::Instruction(id, node);

        (vec![flow], id)
    }

    /// Get a new ID.
    pub fn get_var_index(&mut self) -> usize {
        let id = self.var_index;
        self.var_index += 1;
        id
    }

    pub fn push_label(&mut self, label: NestedIntoStatement) -> BreakableId {
        let brk = BreakableId(Some(self.break_index));
        self.break_index += 1;
        self.label_tracking.push((label, brk));
        brk
    }

    pub fn pop_label(&mut self) {
        self.label_tracking
            .pop()
            .expect("pop_label: no label to pop");
    }

    pub fn label_to_break_id(&self, label: &Option<Ident>) -> BreakableId {
        match label {
            Some(l) => {
                let needle = NestedIntoStatement::Labelled(l.sym.to_string());
                self.label_tracking
                    .iter()
                    .rev()
                    .find(|(target, _)| target == &needle)
                    .expect("parse step does not allow `break` to jump to a non-existent label")
                    .1
            }
            None => {
                self.label_tracking
                    .last()
                    .expect("parse step does not allow `break` when there's nothing to break from")
                    .1
            }
        }
    }

    pub fn register_import(&mut self, import: Import) {
        self.imports.push(import);
    }

    pub fn register_export(&mut self, export: Export) {
        self.exports.push(export);
    }

    pub(crate) fn wrap_up_blocks(
        &mut self,
        blocks: Vec<StructuredFlow>,
    ) -> (FunctionId, BTreeMap<usize, BasicBlock>) {
        let blocks = normalize_basic_blocks_tree(blocks);

        (
            self.current_function_index.take().expect(
                "wrap_up_blocks must be called within a current function (or module) context",
            ),
            blocks,
        )
    }

    pub(crate) fn wrap_up_module(
        &mut self,
        summary: ModuleSummary,
        structured_flow: Vec<StructuredFlow>,
    ) -> BasicBlockModule {
        let blocks = normalize_basic_blocks_tree(structured_flow);

        let module_bg = BasicBlockGroup {
            id: FunctionId(0),
            blocks,
            environment: BasicBlockEnvironment::Module,
        };

        self.functions.insert(FunctionId(0), module_bg);

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
    ) -> Result<(), String>
    where
        C: FnOnce(&mut Self) -> Result<Vec<StructuredFlow>, String>,
    {
        self.function_index.0 += 1;
        let function_index = self.function_index;

        self.scope_tree.go_into_function_scope();

        // functions are shared between contexts
        let functions = std::mem::take(&mut self.functions);
        let scope_tree = std::mem::take(&mut self.scope_tree);

        let mut inner_ctx = Self {
            current_function_index: Some(function_index),
            function_index: self.function_index,
            var_index: self.var_index,
            conditionals: vec![],
            scope_tree,
            label_tracking: vec![],
            break_index: 0,
            functions,
            imports: vec![],
            exports: vec![],
            nonlocalinfo: None,
        };

        let mut func_flow = vec![];

        if let Some(nonlocalinfo) = nonlocalinfo {
            let flow = inner_ctx.embed_nonlocals(nonlocalinfo, self.nonlocalinfo.as_ref());
            func_flow.extend(flow);
        }

        let flow = convert_in_function(&mut inner_ctx)?;
        func_flow.extend(flow);

        let (id, blocks) = inner_ctx.wrap_up_blocks(func_flow);
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

        Ok(())
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
