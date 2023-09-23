use std::collections::BTreeSet;

use crate::{
    analyze::count_variable_uses,
    basic_blocks::{BasicBlockExit, BasicBlockGroup, BasicBlockInstruction, BasicBlockModule},
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

        if blocks_to_suppress.len() > 0 {
            // Suppress some blocks and book-keep the phi nodes

            remove_blocks(block_group, blocks_to_suppress);
        }
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
}

fn remove_blocks(block_group: &mut BasicBlockGroup, blocks_to_suppress: BTreeSet<usize>) {
    let mut removed_variables = BTreeSet::new();

    for block in blocks_to_suppress.iter() {
        let removed = block_group.blocks.remove(block).unwrap();
        removed_variables.extend(removed.iter().map(|(varname, _)| varname));
    }

    for (blk, _varname, ins) in block_group.iter_all_instructions_mut() {
        if blocks_to_suppress.contains(&blk) {
            continue;
        }

        match ins {
            BasicBlockInstruction::Phi(phi) => {
                phi.retain(|var| !removed_variables.contains(var));

                match phi.len() {
                    0 => *ins = BasicBlockInstruction::Undefined,
                    1 => *ins = BasicBlockInstruction::Ref(phi[0]),
                    _ => {}
                }
            }
            _ => {
                #[cfg(debug_assertions)]
                {
                    // Sanity check
                    for used_var in ins.used_vars() {
                        debug_assert!(
                            !removed_variables.contains(&used_var),
                            "used var {:?} in {:?} is removed",
                            used_var,
                            ins
                        );
                    }
                }
            }
        }
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
            exit = jump @1
        }
        @1: {
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
            exit = jump @1
        }
        @1: {
            $4 = 1
            $5 = 1
            $6 = true
            exit = jump @2
        }
        @2: {
            $7 = 1
            $8 = 2000
            $9 = 2001
            $10 = 2001
            exit = jump @4
        }
        @4: {
            $13 = either($0, $9)
            $14 = $13
            $15 = 1000
            $16 = $14 + $15
            $17 = $16
            exit = jump @6
        }
        @6: {
            $20 = either($13, $16)
            $21 = $20
            exit = return $21
        }
        "###);
    }
}
