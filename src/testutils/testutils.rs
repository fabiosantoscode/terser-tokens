use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

use crate::basic_blocks::{BasicBlockGroup, BasicBlockModule};
use crate::from_ast::{
    convert_context::FromAstCtx, module_to_basic_blocks, statements_to_basic_blocks,
};
use crate::swc_parse::swc_parse;

use swc_common::SourceMap;
use swc_common::{BytePos, SourceFile};
use swc_ecma_ast::{ExprStmt, Script, Stmt};
use swc_ecma_codegen::{text_writer::JsWriter, Emitter};
use swc_ecma_parser::{parse_file_as_expr, EsConfig};

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

pub fn test_basic_blocks_expr(source: &str) -> BasicBlockGroup {
    let m = parse_expression(source);
    statements_to_basic_blocks(
        &mut FromAstCtx::new(),
        &vec![&Stmt::Expr(ExprStmt {
            span: Default::default(),
            expr: Box::new(m),
        })],
    )
}

pub fn test_basic_blocks(source: &str) -> BasicBlockGroup {
    let m = test_basic_blocks_module(source);
    m.top_level_stats
}

pub fn test_basic_blocks_module(source: &str) -> BasicBlockModule {
    let m = swc_parse(source);
    module_to_basic_blocks("test.js", &m).unwrap()
}

pub fn stats_to_string(stats: Vec<Stmt>) -> String {
    struct Buf {
        pub buf: Rc<RefCell<String>>,
    }
    impl Write for Buf {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            self.buf
                .borrow_mut()
                .push_str(std::str::from_utf8(buf).unwrap());
            Ok(buf.len())
        }
        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    let script: Script = Script {
        span: Default::default(),
        body: stats,
        shebang: None,
    };
    let str = Rc::new(RefCell::new(String::new()));
    let buf = Buf { buf: str.clone() };
    let writer = Box::new(buf);
    let sourcemapper: Rc<SourceMap> = Default::default();
    let mut emitter = Emitter {
        cfg: Default::default(),
        comments: None,
        cm: sourcemapper,
        wr: JsWriter::new(Default::default(), "\n", writer, None),
    };

    emitter.emit_script(&script).unwrap();

    str.take()
}
