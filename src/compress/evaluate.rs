use std::collections::BTreeSet;

use crate::{
    basic_blocks::{BasicBlockExit, BasicBlockGroup, BasicBlockModule},
    block_ops::{normalize_basic_blocks, normalize_module},
    interpret::{interpret_module, InterpretCtx, JsType},
};

pub fn compress_step_evaluate(module: &mut BasicBlockModule) {
    let mut types = InterpretCtx::from_module(module);
    interpret_module(&mut types, &module);

    let types = types.into_all_variables();

    // statically-analyzable conditions
    for (_func_id, block_group) in module.iter_mut() {
        let mut blocks_to_suppress = BTreeSet::new();

        for (block_id, block) in block_group.iter_mut() {
            if blocks_to_suppress.contains(&block_id) {
                continue;
            }

            match &block.exit {
                BasicBlockExit::Cond(test, cons_start, cons_end, alt_start, alt_end) => {
                    let is_truthy = types.get(&test).and_then(JsType::is_truthy);

                    // Known truth-values can eliminate branches
                    match is_truthy {
                        Some(true) => {
                            blocks_to_suppress.extend(*alt_start..=*alt_end);
                            block.exit = BasicBlockExit::Jump(*cons_start);
                        }
                        Some(false) => {
                            blocks_to_suppress.extend(*cons_start..=*cons_end);
                            block.exit = BasicBlockExit::Jump(*alt_start);
                        }
                        None => continue,
                    }
                }
                _ => {}
            }
        }

        remove_blocks(block_group, blocks_to_suppress);
    }

    for (_func_id, _blk_id, varname, ins) in module.iter_all_instructions_mut() {
        match types.get(&varname) {
            Some(value) => {
                if let Some(new_ins) = value.as_small_literal_instruction(/* TODO calculate and pass desired max size here */)
                {
                    *ins = new_ins;
                }
            }
            None => {}
        }
    }

    normalize_module(module);
}

fn remove_blocks(block_group: &mut BasicBlockGroup, blocks_to_suppress: BTreeSet<usize>) {
    if blocks_to_suppress.len() > 0 {
        let mut blocks = std::mem::take(&mut block_group.blocks);

        blocks.retain(|block_id, _| !blocks_to_suppress.contains(block_id));

        block_group.blocks = normalize_basic_blocks(blocks);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::basic_blocks::FunctionId;
    use crate::testutils::*;

    #[test]
    fn test_evaluate_block() {
        let mut module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                $1 = 2
                $2 = $0 + $1
                exit = return $2
            }",
        ]);

        compress_step_evaluate(&mut module);

        insta::assert_debug_snapshot!(module.get_function(FunctionId(0)).unwrap().blocks[&0], @r###"
        {
            $0 = 1
            $1 = 2
            $2 = 3
            exit = return $2
        }
        "###);
    }

    #[test]
    fn test_evaluate_cond() {
        let mut module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                exit = cond $0 ? @1..@1 : @2..@2
            }
            @1: {
                $1 = 1
                exit = return $1
            }
            @2: {
                $2 = 2
                exit = return $2
            }
            ",
        ]);

        compress_step_evaluate(&mut module);

        insta::assert_debug_snapshot!(module.get_function(FunctionId(0)).unwrap(), @r###"
        @0: {
            $0 = 1
            $1 = 1
            exit = return $1
        }
        "###);
    }

    #[test]
    fn test_evaluate_cond_nested() {
        let mut module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                $1 = $0
                $2 = 1
                $3 = $1 == $2
                exit = cond $3 ? @1..@4 : @5..@5
            }
            @1: {
                $4 = $0
                $5 = 1
                $6 = $4 == $5
                exit = cond $6 ? @2..@2 : @3..@3
            }
            @2: {
                $7 = $0
                $8 = 2000
                $9 = $7 + $8
                $10 = $9
                exit = jump @4
            }
            @3: {
                $11 = 3
                $12 = $11
                exit = jump @4
            }
            @4: {
                $13 = either($0, $9, $11)
                $14 = $13
                $15 = 1000
                $16 = $14 + $15
                $17 = $16
                exit = jump @6
            }
            @5: {
                $18 = 3
                $19 = $18
                exit = jump @6
            }
            @6: {
                $20 = either($13, $16, $18)
                $21 = $20
                exit = return $21
            }",
        ]);

        compress_step_evaluate(&mut module);

        insta::assert_debug_snapshot!(module.get_function(FunctionId(0)).unwrap(), @r###"
        @0: {
            $0 = 1
            $1 = 1
            $2 = 1
            $3 = true
            $4 = 1
            $5 = 1
            $6 = true
            $7 = 1
            $8 = 2000
            $9 = 2001
            $10 = 2001
            $11 = $9
            $12 = 1000
            $13 = $11 + $12
            $14 = $13
            $15 = $13
            exit = return $15
        }
        "###);
    }

    #[test]
    fn test_evaluate_cond_nested_2() {
        let mut module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                $1 = $0
                $2 = 1
                $3 = $1 == $2
                exit = cond $3 ? @1..@4 : @5..@5
            }
            @1: {
                $4 = $0
                $5 = true
                $6 = $4 == $5
                exit = cond $6 ? @2..@2 : @3..@3
            }
            @2: {
                $7 = $0
                $8 = 2000
                $9 = $7 + $8
                $10 = $9
                exit = jump @4
            }
            @3: {
                $11 = 3
                $12 = $11
                exit = jump @4
            }
            @4: {
                $13 = either($0, $9, $11)
                $14 = $13
                $15 = 1000
                $16 = $14 + $15
                $17 = $16
                exit = jump @6
            }
            @5: {
                $18 = 3
                $19 = $18
                exit = jump @6
            }
            @6: {
                $20 = either($13, $16, $18)
                $21 = $20
                exit = return $21
            }",
        ]);

        compress_step_evaluate(&mut module);

        insta::assert_debug_snapshot!(module.get_function(FunctionId(0)).unwrap(), @r###"
        @0: {
            $0 = 1
            $1 = 1
            $2 = 1
            $3 = true
            $4 = 1
            $5 = true
            $6 = $4 == $5
            exit = cond $6 ? @1..@1 : @2..@2
        }
        @1: {
            $7 = $0
            $8 = 2000
            $9 = $7 + $8
            $10 = $9
            exit = jump @3
        }
        @2: {
            $11 = 3
            $12 = $11
            exit = jump @3
        }
        @3: {
            $13 = either($0, $9, $11)
            $14 = $13
            $15 = 1000
            $16 = $14 + $15
            $17 = $16
            $18 = $16
            exit = return $18
        }
        "###);
    }

    #[test]
    fn evaluate_nonlocal_1() {
        let mut module = parse_instructions_module(vec![
            "@0: {
                $0 = undefined
                $3 = FunctionId(1)
                $4 = 100
                $5 = write_non_local $$1 $4
                $6 = $3
                $7 = call $6()
                exit = return $7
            }",
            "@0: {
                $8 = read_non_local $$1
                $9 = 1
                $10 = $8 + $9
                $11 = $10
                exit = return $11
            }",
        ]);

        compress_step_evaluate(&mut module);

        insta::assert_debug_snapshot!(module, @r###"
        BasicBlockModule {
            summary: ModuleSummary {
                filename: "",
            },
            top_level_stats: @0: {
                $0 = undefined
                $1 = FunctionId(1)
                $2 = 100
                $3 = 100
                $4 = $1
                $5 = 101
                exit = return $5
            },
            functions: [
                function():
                @0: {
                    $6 = 100
                    $7 = 1
                    $8 = 101
                    $9 = 101
                    exit = return $9
                },
            ],
        }
        "###);
    }

    /*
    #[test]
    fn evaluate_nonlocal_2() {
        // TODO: our code generates a duplicate write_non_local $$1 so we can't easily statically evaluate this
        let mut module = parse_instructions_module(vec![
            "@0: {
                $0 = undefined
                $2 = write_non_local $$1 $0
                $3 = FunctionId(1)
                $4 = 100
                $5 = write_non_local $$1 $4
                $6 = $3
                $7 = call $6()
                exit = return $7
            }",
            "@0: {
                $8 = read_non_local $$1
                $9 = 1
                $10 = $8 + $9
                $11 = $10
                exit = return $11
            }",
        ]);

        compress_step_evaluate(&mut module);

        insta::assert_debug_snapshot!(module, @r###"
        BasicBlockModule {
            summary: ModuleSummary {
                filename: "",
            },
            top_level_stats: @0: {
                $0 = undefined
                $3 = FunctionId(1)
                $4 = 100
                $5 = write_non_local $$1 $4
                $6 = $3
                $7 = call $6()
                exit = return $7
            },
            functions: [
                function():
                @0: {
                    $8 = read_non_local $$1
                    $9 = 1
                    $10 = $8 + $9
                    $11 = $10
                    exit = return $11
                },
            ],
        }
        "###);
    }
    */
}
