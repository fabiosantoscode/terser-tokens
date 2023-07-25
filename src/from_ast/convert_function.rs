use super::convert::statements_to_basic_blocks;
use super::convert_context::ConvertContext;
use crate::basic_blocks::basic_block::BasicBlockInstruction;
use crate::basic_blocks::basic_block_group::{BasicBlockEnvironmentType, FunctionId};

use swc_ecma_ast::Function;

pub fn function_to_basic_blocks(
    ctx: &mut ConvertContext,
    function: &Function,
) -> Result<FunctionId, String> {
    // count function.length
    let arg_count: usize = function
        .params
        .iter()
        .filter(|param| match param.pat {
            swc_ecma_ast::Pat::Ident(_) => true,
            swc_ecma_ast::Pat::Rest(_) => false,
            _ => todo!("non-ident function param"),
        })
        .count();

    ctx.go_into_function(|ctx| {
        function
            .params
            .iter()
            .enumerate()
            .for_each(|(i, param)| match &param.pat {
                swc_ecma_ast::Pat::Ident(ident) => {
                    let arg = ctx.push_instruction(BasicBlockInstruction::ArgumentRead(i));
                    ctx.assign_name(&ident.id.sym.to_string(), arg);
                }
                swc_ecma_ast::Pat::Rest(ident) => {
                    let name = ident.arg.clone().expect_ident().id.sym.to_string();
                    let arg = ctx.push_instruction(BasicBlockInstruction::ArgumentRest(i));
                    ctx.assign_name(&name, arg);
                }
                _ => todo!("non-ident function param"),
            });

        let mut func = statements_to_basic_blocks(
            ctx,
            function
                .body
                .clone()
                .expect("function body")
                .stmts
                .iter()
                .collect::<Vec<_>>()
                .as_slice(),
        );
        func.environment.env_type = BasicBlockEnvironmentType::Function(arg_count);
        // TODO refactor note:
        // because statements_to_basic_blocks is not a method of `ctx`, it doesn't know if it's
        // supposed to be a function. We should make it a method of `ctx` so it returns the
        // correct type. Same for function_to_basic_blocks probably.
        Ok(func)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_blocks::basic_block_group::BasicBlockGroup;
    use crate::swc_parse::swc_parse;

    fn conv_fn(src: &str) -> BasicBlockGroup {
        let mut ctx = ConvertContext::new();
        let func = swc_parse(src);
        let idx = function_to_basic_blocks(
            &mut ctx,
            func.body[0]
                .clone()
                .expect_stmt()
                .expect_decl()
                .expect_fn_decl()
                .function
                .as_ref(),
        )
        .expect("function_to_basic_blocks");

        ctx.get_function(idx).expect("get_function").clone()
    }

    #[test]
    fn test_basic_blocks_function() {
        let func = conv_fn("function _(y, z) { return y + z }");
        insta::assert_debug_snapshot!(func, @r###"
        function():
        @0: {
            $0 = arguments[0]
            $1 = arguments[1]
            $2 = $0
            $3 = $1
            $4 = $2 + $3
            exit = return $4
        }
        "###);
    }

    #[test]
    fn test_basic_blocks_function_rest() {
        let func = conv_fn("function _(y, ...z) { return z }");
        insta::assert_debug_snapshot!(func, @r###"
        function():
        @0: {
            $0 = arguments[0]
            $1 = arguments[1...]
            $2 = $1
            exit = return $2
        }
        "###);
    }
}
