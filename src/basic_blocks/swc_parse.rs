use swc_common::{BytePos, SourceFile};
use swc_ecma_ast::Module;
use swc_ecma_parser::{parse_file_as_module, EsConfig};

pub fn swc_parse(source: &str) -> Module {
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
