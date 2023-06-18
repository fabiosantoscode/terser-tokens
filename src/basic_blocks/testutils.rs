use super::{basic_block_group::BasicBlockGroup, convert::convert::statements_to_basic_blocks};
use crate::parser::{parse_expression, parse_module};

use swc_ecma_ast::ModuleItem;
use swc_ecma_ast::{ExprStmt, Stmt};

pub fn test_basic_blocks_expr(source: &str) -> BasicBlockGroup {
    let m = parse_expression(source);
    let s = statements_to_basic_blocks(&vec![&Stmt::Expr(ExprStmt {
        span: Default::default(),
        expr: Box::new(m),
    })]);
    s
}

pub fn test_basic_blocks(source: &str) -> BasicBlockGroup {
    let m = parse_module(source);
    let s = statements_to_basic_blocks(
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
