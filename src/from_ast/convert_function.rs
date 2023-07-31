use super::{find_nonlocals, statements_to_basic_blocks, FromAstCtx, FunctionLike};
use crate::basic_blocks::{BasicBlockGroup, BasicBlockInstruction};

pub fn function_to_basic_blocks<'a>(
    ctx: &'a mut FromAstCtx,
    function: FunctionLike<'a>,
) -> Result<&'a BasicBlockGroup, String> {
    // count function.length
    let arg_count: usize = function.function_length();

    ctx.go_into_function(arg_count, Some(find_nonlocals(function.clone())), |ctx| {
        function
            .get_params()
            .into_iter()
            .enumerate()
            .for_each(|(i, pat)| match pat {
                swc_ecma_ast::Pat::Ident(ident) => {
                    let arg = ctx.push_instruction(BasicBlockInstruction::ArgumentRead(i));
                    ctx.assign_name(&ident.id.sym.to_string(), arg);
                }
                swc_ecma_ast::Pat::Rest(ident) => {
                    let name = ident.arg.as_ident().as_ref().unwrap().id.sym.to_string();
                    let arg = ctx.push_instruction(BasicBlockInstruction::ArgumentRest(i));
                    ctx.assign_name(&name, arg);
                }
                _ => todo!("non-ident function param"),
            });

        statements_to_basic_blocks(ctx, &function.get_statements());

        // TODO refactor note:
        // because statements_to_basic_blocks is not a method of `ctx`, it doesn't know if it's
        // supposed to be a function. We should make it a method of `ctx` so it returns the
        // correct type. Same for function_to_basic_blocks probably.
        Ok(())
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_blocks::BasicBlockGroup;
    use crate::swc_parse::swc_parse;

    fn conv_fn(src: &str) -> BasicBlockGroup {
        let mut ctx = FromAstCtx::new();
        let func = swc_parse(src);
        let decl = func.body[0]
            .clone()
            .expect_stmt()
            .expect_decl()
            .expect_fn_decl();
        let func = function_to_basic_blocks(&mut ctx, FunctionLike::FnDecl(&decl))
            .expect("function_to_basic_blocks");

        func.clone()
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
