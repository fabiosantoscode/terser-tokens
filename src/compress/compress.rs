use crate::from_ast::module_to_basic_blocks;
use crate::swc_parse::swc_parse;
use crate::swc_stringify::swc_stringify;
use crate::to_ast::module_to_ast;

use super::compress_step_evaluate;
use super::compress_step_remove_dead_code;

pub fn compress(input: &str, passes: usize) -> String {
    let module = swc_parse(input);

    let mut module = module_to_basic_blocks("input.js", &module).unwrap();

    for _ in 0..passes {
        compress_step_remove_dead_code(&mut module);
        compress_step_evaluate(&mut module);
    }

    let module = module_to_ast(module);

    swc_stringify(module)
}
