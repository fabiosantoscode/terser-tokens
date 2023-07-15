use crate::basic_blocks::to_ast::to_ast::module_to_ast;
use crate::basic_blocks::to_basic_blocks::convert_module::{
    module_to_basic_blocks, ConvertModuleCtx,
};
use crate::swc_parse::swc_parse;
use crate::swc_stringify::swc_stringify;

pub fn compress(input: &str) -> String {
    let module = swc_parse(input);

    let mut ctx = ConvertModuleCtx {};
    let module = module_to_basic_blocks(&mut ctx, "input.js", &module).unwrap();

    let module = module_to_ast(&module);

    swc_stringify(module)
}
