mod expression;
mod primary_expression;

use swc_common::{BytePos, SourceFile};
use swc_ecma_ast::Module;
use swc_ecma_parser::{parse_file_as_expr, parse_file_as_module, EsConfig};

pub fn parse_module(source: &str) -> Module {
    let file = SourceFile::new(
        swc_common::FileName::Anon,
        false,
        swc_common::FileName::Anon,
        source.into(),
        BytePos(1),
    );
    parse_file_as_module(
        &file,
        swc_ecma_parser::Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        Default::default(),
        None,
        &mut vec![],
    )
    .unwrap()
}

pub fn parse_expression(source: &str) -> swc_ecma_ast::Expr {
    let file = SourceFile::new(
        swc_common::FileName::Anon,
        false,
        swc_common::FileName::Anon,
        source.into(),
        BytePos(1),
    );
    *parse_file_as_expr(
        &file,
        swc_ecma_parser::Syntax::Es(EsConfig {
            jsx: true,
            ..Default::default()
        }),
        Default::default(),
        None,
        &mut vec![],
    )
    .unwrap()
}

#[test]
fn test_parse_module() {
    let _ = parse_module("function foo() { return 1; }");
}

#[test]
fn test_parse_expression() {
    let _ = parse_expression("1 + 2 + 3");
}
