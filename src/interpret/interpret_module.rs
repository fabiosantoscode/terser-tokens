use crate::basic_blocks::BasicBlockModule;

use super::{interpret_block_group, InterpretCtx, JsCompletion};

pub fn interpret_module(ctx: &mut InterpretCtx, module: &BasicBlockModule) -> Option<JsCompletion> {
    let ret = interpret_block_group(ctx, module.top_level_stats());
    ctx.wrap_up_module();
    ret
}

#[cfg(test)]
mod tests {
    use crate::testutils::*;

    use super::*;

    #[test]
    fn interp_basic_module() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                $1 = FunctionId(1)
                $2 = call $1($0)
                exit = return $2
            }",
            "@0: {
                $3 = 123
                exit = return $3
            }",
        ]);
        let mut ctx = InterpretCtx::from_module(&module);
        assert_eq!(
            interpret_module(&mut ctx, &module),
            Some(JsCompletion::Return(123.0.into()))
        );
    }
}
