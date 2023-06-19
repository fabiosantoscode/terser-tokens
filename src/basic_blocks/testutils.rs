use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

use super::{basic_block_group::BasicBlockGroup, convert::convert::statements_to_basic_blocks};
use crate::parser::{parse_asyncgen, parse_expression};

use swc_common::SourceMap;
use swc_ecma_ast::Script;
use swc_ecma_ast::{ExprStmt, Stmt};
use swc_ecma_codegen::{text_writer::JsWriter, Emitter};

pub fn test_basic_blocks_expr(source: &str) -> BasicBlockGroup {
    let m = parse_expression(source);
    statements_to_basic_blocks(&vec![&Stmt::Expr(ExprStmt {
        span: Default::default(),
        expr: Box::new(m),
    })])
}

pub fn test_basic_blocks(source: &str) -> BasicBlockGroup {
    let m = parse_asyncgen(source);
    statements_to_basic_blocks(&m.function.body.unwrap().stmts.iter().collect::<Vec<_>>())
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

    str.clone().borrow().clone()
}
