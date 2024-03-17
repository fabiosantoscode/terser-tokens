use std::collections::BTreeSet;

use crate::{
    analyze::{construct_call_graph, count_variable_uses},
    basic_blocks::{BasicBlockInstruction, BasicBlockModule},
    block_ops::normalize_module,
};

pub fn compress_step_remove_dead_code(module: &mut BasicBlockModule) {
    let call_graph = construct_call_graph(module);

    let mut removed_funcs = BTreeSet::new();

    module
        .functions
        .retain(|k, _| match call_graph.function_calls.get(k) {
            Some(calls) if calls.is_empty() => {
                removed_funcs.insert(*k);
                false
            }
            _ => true,
        });

    let uses = count_variable_uses(module);
    for (_function_id, function) in module.functions.iter_mut() {
        // TODO: remove never-executed branches
        for (_block_id, block) in function.blocks.iter_mut() {
            block.instructions.retain_mut(|(var, ins)| match ins {
                BasicBlockInstruction::Function(id) if removed_funcs.contains(id) => {
                    *ins = BasicBlockInstruction::Undefined;
                    true
                }
                _ => ins.may_have_side_effects() || *uses.get(var).unwrap_or(&0) > 0,
            });
        }
    }

    normalize_module(module);
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::basic_blocks::FunctionId;
    use crate::testutils::*;

    #[test]
    fn test_drop_dead_code() {
        let mut module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                $1 = 2
                $2 = 3
                exit = return $2
            }",
        ]);

        compress_step_remove_dead_code(&mut module);

        insta::assert_debug_snapshot!(module.get_function(FunctionId(0)).unwrap().blocks[&0], @r###"
        {
            $2 = 3
            exit = return $2
        }
        "###);
    }

    #[test]
    fn test_drop_dead_functions() {
        let mut module = parse_instructions_module(vec![
            "@0: {
                $0 = FunctionId(1)
                $1 = call $0()
                $404 = FunctionId(2)
                exit = return $1
            }",
            "@0: {
                $2 = 1
                exit = return $2
            }",
            "@0: {
                $3 = 404
                exit = return $3
            }",
        ]);

        compress_step_remove_dead_code(&mut module);

        insta::assert_debug_snapshot!(module.functions, @r###"
        {
            FunctionId(0): @0: {
                $0 = FunctionId(1)
                $1 = call $0()
                $404 = undefined
                exit = return $1
            },
            FunctionId(1): function():
            @0: {
                $2 = 1
                exit = return $2
            },
        }
        "###);
    }
}
