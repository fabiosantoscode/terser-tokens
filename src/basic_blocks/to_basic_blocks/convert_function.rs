use super::super::basic_block_group::FunctionId;
use super::convert::statements_to_basic_blocks;

use super::convert_context::ConvertContext;
use swc_ecma_ast::Function;

pub fn function_to_basic_blocks(
    ctx: &mut ConvertContext,
    function: &Function,
) -> Result<FunctionId, String> {
    ctx.go_into_function(|ctx| {
        Ok(statements_to_basic_blocks(
            ctx,
            function
                .body
                .clone()
                .expect("function body")
                .stmts
                .iter()
                .collect::<Vec<_>>()
                .as_slice(),
        ))
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::swc_parse::swc_parse;

    #[test]
    fn test_basic_blocks_function() {
        let mut ctx = ConvertContext::new();
        let func = swc_parse("function f() { 1 + 1 }");
        let module = function_to_basic_blocks(
            &mut ctx,
            func.body[0]
                .clone()
                .expect_stmt()
                .expect_decl()
                .expect_fn_decl()
                .function
                .as_ref(),
        );
        insta::assert_debug_snapshot!(module, @r###"
        Ok(
            FunctionId(
                0,
            ),
        )
        "###);
    }
}
