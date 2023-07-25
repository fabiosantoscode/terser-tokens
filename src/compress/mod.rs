use crate::from_ast::module_to_basic_blocks;
use crate::swc_parse::swc_parse;
use crate::swc_stringify::swc_stringify;
use crate::to_ast::module_to_ast;

pub fn compress(input: &str) -> String {
    let module = swc_parse(input);

    let module = module_to_basic_blocks("input.js", &module).unwrap();

    let module = module_to_ast(module);

    swc_stringify(module)
}
