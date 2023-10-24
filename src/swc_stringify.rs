use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

use swc_common::SourceMap;
use swc_ecma_ast::Module;
use swc_ecma_codegen::{text_writer::JsWriter, Emitter};

pub fn swc_stringify(module: Module) -> String {
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

    let out_str = Rc::new(RefCell::new(String::new()));
    let buf = Buf {
        buf: out_str.clone(),
    };
    let writer = Box::new(buf);
    let sourcemapper: Rc<SourceMap> = Default::default();
    let mut emitter = Emitter {
        cfg: Default::default(),
        comments: None,
        cm: sourcemapper,
        wr: JsWriter::new(Default::default(), "\n", writer, None),
    };

    emitter.emit_module(&module).unwrap();

    out_str.take()
}
