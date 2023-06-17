use std::{collections::HashMap, fmt::Debug};

use swc_ecma_ast::Module;

use crate::parser::parse_module;

use super::convert::module_to_ssa;
use super::ssa_ast::SsaAst;

#[derive(Default)]
pub struct SsaFn {
    pub asts: HashMap<usize, StatementOrBlock>,
}

pub enum StatementOrBlock {
    SsaAst(SsaAst),
    SsaFn(SsaFn),
}

impl SsaFn {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn from_asts(asts: Vec<SsaAst>) -> Self {
        Self {
            asts: asts
                .into_iter()
                .enumerate()
                .map(|(index, ast)| (index, StatementOrBlock::SsaAst(ast)))
                .collect(),
        }
    }

    pub fn from_module(m: &Module) -> Self {
        module_to_ssa(m)
    }

    pub fn from_module_source(m: &str) -> Self {
        module_to_ssa(&parse_module(m))
    }

    pub fn iter<'a>(&'a self) -> SsaFnIterator<'a> {
        let mut asts_sorted_keys = self
            .asts
            .iter()
            .flat_map(|(idx, st)| match st {
                StatementOrBlock::SsaAst(ast) => vec![(idx, ast)],
                StatementOrBlock::SsaFn(ast) => ast.iter().collect(),
            })
            .collect::<Vec<_>>();
        asts_sorted_keys.sort_by_key(|(k, _)| *k);

        SsaFnIterator {
            asts_sorted_keys,
            current: 0,
        }
    }
}

impl Debug for SsaFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut asts = self.asts.iter().collect::<Vec<_>>();
        asts.sort_by_key(|(k, _)| *k);
        for (k, v) in asts {
            writeln!(f, "@{}: {:?}", k, v)?;
        }
        Ok(())
    }
}

impl Debug for StatementOrBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StatementOrBlock::SsaAst(ast) => write!(f, "{:?}", ast),
            StatementOrBlock::SsaFn(ast) => write!(f, "{:?}", ast),
        }
    }
}

pub struct SsaFnIterator<'a> {
    asts_sorted_keys: Vec<(&'a usize, &'a SsaAst)>,
    current: usize,
}

impl<'a> Iterator for SsaFnIterator<'a> {
    type Item = (&'a usize, &'a SsaAst);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ast) = self.asts_sorted_keys.get(self.current) {
            self.current += 1;
            Some(*ast)
        } else {
            None
        }
    }
}
