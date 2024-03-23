use crate::{
    analyze::construct_call_graph,
    basic_blocks::{FunctionId, StructuredModule},
    interpret::JsArgs,
};

use super::{interpret_blocks, interpret_function, InterpretCtx, JsCompletion};

pub fn interpret_module(ctx: &mut InterpretCtx, module: &StructuredModule) -> Option<JsCompletion> {
    let call_graph = construct_call_graph(&module);

    // First, evaluate top-down to gather information for later
    let top = module.functions.get(&FunctionId(0))?;
    interpret_blocks(ctx, &top.blocks);
    for interp_func in call_graph.traversal_order.iter().rev() {
        if interp_func == &FunctionId(0) {
            continue; // Not a function, but the top-level block
        }
        let known_calls = call_graph.function_calls.get(interp_func);

        let func = module
            .get_function(*interp_func)
            .expect("the function must exist");

        let args = match known_calls {
            None => continue,
            Some(not_called) if not_called.len() == 0 => continue,
            Some(some_calls) => {
                // note that if some_calls.len() == 1 this does NOT mean it was called once. This gets deduped beforehand.
                JsArgs::from_argvecs(
                    some_calls
                        .iter()
                        .map(|call| JsArgs::from(ctx.get_variables(&call.args))),
                )
            }
        };

        interpret_function(ctx, func, args, true);
    }

    // Then, evaluate bottom-up to propagate returned values upward
    for interp_func in call_graph.traversal_order.iter() {
        if interp_func == &FunctionId(0) {
            continue; // Not a function, but the top-level block
        }
        let known_calls = call_graph.function_calls.get(interp_func);

        let func = module
            .get_function(*interp_func)
            .expect("the function must exist");

        let args = match known_calls {
            None => JsArgs::Unknown,
            Some(not_called) if not_called.len() == 0 => {
                // This function was NEVER called.
                // TODO mark this as a dead function
                JsArgs::Unknown
            }
            Some(some_calls) => {
                // note that if some_calls.len() == 1 this does NOT mean it was called once. This gets deduped beforehand.
                JsArgs::from_argvecs(
                    some_calls
                        .iter()
                        .map(|call| JsArgs::from(ctx.get_variables(&call.args))),
                )
            }
        };

        interpret_function(ctx, func, args, true);
    }

    let top = module.functions.get(&FunctionId(0))?;
    interpret_blocks(ctx, &top.blocks)
}

#[cfg(test)]
mod tests {
    use crate::testutils::*;

    use super::*;

    #[test]
    fn interp_basic_module() {
        let module = parse_test_module(vec![
            "{
                $0 = 1
                $1 = FunctionId(1)
                $2 = call $1($0)
                Return $2
            }",
            "{
                $3 = 123
                Return $3
            }",
        ]);
        let module = module.into();
        let mut ctx = InterpretCtx::from_module(&module);
        assert_eq!(
            interpret_module(&mut ctx, &module),
            Some(JsCompletion::Return(123.0.into()))
        );
    }

    #[test]
    fn interp_knownarg_calls() {
        let module = parse_test_module(vec![
            "{
                $0 = 1
                $1 = FunctionId(1)
                $2 = call $1($0)
                Return $2
            }",
            "{
                $3 = arguments[0]
                Return $3
            }",
        ]);
        let module = module.into();
        let mut ctx = InterpretCtx::from_module(&module);
        ctx.disable_tinyfuncs = true;
        assert_eq!(
            interpret_module(&mut ctx, &module),
            Some(JsCompletion::Return(1.0.into()))
        );

        let module = parse_test_module(vec![
            "{
                $0 = 1
                $1 = FunctionId(1)
                $2 = call $1($0)
                $3 = call $1($0)
                $4 = $2 + $3
                Return $4
            }",
            "{
                $5 = arguments[0]
                Return $5
            }",
        ]);
        let module = module.into();
        let mut ctx = InterpretCtx::from_module(&module);
        ctx.disable_tinyfuncs = true;
        assert_eq!(
            interpret_module(&mut ctx, &module),
            Some(JsCompletion::Return(2.0.into()))
        );
    }
}
