use super::{convert::statements_to_ssa, basic_block_group::BasicBlockGroup};
use crate::parser::{parse_expression, parse_module};

use swc_ecma_ast::ModuleItem;
use swc_ecma_ast::{ExprStmt, Stmt};

pub fn test_ssa(source: &str) -> BasicBlockGroup {
    let m = parse_expression(source);
    let s = statements_to_ssa(&vec![&Stmt::Expr(ExprStmt {
        span: Default::default(),
        expr: Box::new(m),
    })]);
    s
}

pub fn test_ssa_block(source: &str) -> BasicBlockGroup {
    let m = parse_module(source);
    let s = statements_to_ssa(
        &m.body
            .iter()
            .flat_map(|item| match item {
                ModuleItem::Stmt(stmt) => Some(stmt),
                _ => None,
            })
            .collect::<Vec<_>>(),
    );
    s
}
