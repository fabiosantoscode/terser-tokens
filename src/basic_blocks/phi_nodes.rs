use std::collections::{BTreeMap, BTreeSet};

use crate::basic_blocks::BasicBlock;

use super::{
    normalize_varnames, BasicBlockExit, BasicBlockGroup, BasicBlockInstruction, BasicBlockModule,
};

pub fn generate_phi_nodes(module: &mut BasicBlockModule) {
    let mut unique_name = module
        .iter_all_instructions()
        .map(|(_, _, varname, _)| varname)
        .max()
        .unwrap_or(0)
        + 1;

    let rename_all = |to_rename: Vec<&mut usize>, rename_key: &BTreeMap<usize, usize>| {
        for varname in to_rename {
            if let Some(new_name) = rename_key.get(varname) {
                *varname = *new_name;
            }
        }
    };

    for (_blk_id, block_group) in module.iter_mut() {
        let mut renamed_vars = BTreeMap::new();
        let mut vars_as_phi: Vec<BTreeMap<usize, BTreeSet<usize>>> = vec![BTreeMap::new()];
        let mut condition_ends = vec![];
        let mut phi_instructions_for_next_block = vec![];

        for (block_id, block) in block_group.iter_mut() {
            for (varname, instruction) in block.instructions.iter_mut() {
                let phied_vars = vars_as_phi.last_mut().unwrap();
                let phied_var = phied_vars.entry(*varname).or_default();

                let is_new_var = phied_var.insert(*varname);
                if !is_new_var {
                    let new_varname = unique_name;
                    unique_name += 1;

                    phied_var.insert(new_varname);

                    renamed_vars.insert(*varname, new_varname);
                    *varname = new_varname;
                }

                rename_all(instruction.used_vars_mut(), &renamed_vars);
            }

            rename_all(block.exit.used_vars_mut(), &renamed_vars);

            if phi_instructions_for_next_block.len() > 0 {
                block
                    .instructions
                    .splice(0..0, phi_instructions_for_next_block.drain(..))
                    .for_each(drop);
            }

            // Enter conditional branches
            if let Some(end_cond) = jumps_to_conditional_branch(&block.exit) {
                condition_ends.push(end_cond);
                vars_as_phi.push(vars_as_phi.last().cloned().unwrap_or_default());
            }

            // Leave conditional branches
            while condition_ends.last() == Some(&block_id) {
                condition_ends.pop();

                let renamed = vars_as_phi
                    .pop()
                    .expect("unbalanced conditional branch")
                    .into_iter()
                    .filter(|(_, phis)| phis.len() > 1)
                    .collect::<Vec<_>>();

                for (unphied_name, alternatives) in renamed.into_iter() {
                    let new_varname = unique_name;
                    unique_name += 1;

                    if let Some(existing_phies) =
                        vars_as_phi.last_mut().unwrap().get_mut(&unphied_name)
                    {
                        for phi in alternatives.iter() {
                            existing_phies.insert(*phi);
                            renamed_vars.insert(*phi, new_varname);
                        }
                    }
                    renamed_vars.insert(unphied_name, new_varname);

                    phi_instructions_for_next_block.push((
                        new_varname,
                        BasicBlockInstruction::Phi(alternatives.into_iter().collect()),
                    ));
                }
            }
        }
    }

    normalize_varnames(module);
}

fn jumps_to_conditional_branch(exit: &BasicBlockExit) -> Option<usize> {
    match exit {
        BasicBlockExit::Cond(_, _, _, _, alt_end) => Some(*alt_end),
        BasicBlockExit::Loop(_, loop_end) => Some(*loop_end),
        BasicBlockExit::PopCatch(_, end_catch) => Some(*end_catch),
        _ => None,
    }
}

pub fn remove_phi(group: &mut BasicBlockGroup) {
    let mut phies_to_final_name: BTreeMap<usize, usize> = collect_phi(group);

    for (_, block) in group.iter_mut() {
        remove_phi_inner(block, &mut phies_to_final_name);
    }
}

pub fn remove_phi_module(module: &mut BasicBlockModule) {
    for (_, group) in module.iter_mut() {
        remove_phi(group);
    }
}

fn collect_phi(group: &BasicBlockGroup) -> BTreeMap<usize, usize> {
    let mut phies_to_final_name: BTreeMap<usize, usize> = Default::default();
    let mut final_names: BTreeSet<usize> = Default::default();

    for (_, varname, ins) in group.iter_all_instructions() {
        if let BasicBlockInstruction::Phi(alternatives) = ins {
            let indirect_name = alternatives
                .iter()
                .find(|alt_var| final_names.contains(alt_var));

            match indirect_name {
                Some(indirect_name) => {
                    phies_to_final_name.insert(varname, *indirect_name);

                    for phi in alternatives {
                        phies_to_final_name.insert(*phi, *indirect_name);
                        final_names.insert(varname);
                    }
                }
                None => {
                    for phi in alternatives {
                        phies_to_final_name.insert(*phi, varname);
                        final_names.insert(varname);
                    }
                }
            }
        }
    }

    phies_to_final_name
}

fn remove_phi_inner(block: &mut BasicBlock, phies_to_final_name: &mut BTreeMap<usize, usize>) {
    block.instructions.retain(|(_, ins)| match ins {
        BasicBlockInstruction::Phi(_) => false,
        _ => true,
    });

    for x in block.iter_varnames_mut() {
        if let Some(final_name) = phies_to_final_name.get(&x) {
            *x = *final_name;
        }
    }

    for (_, ins) in block.iter_mut() {
        for used_var in ins.used_vars_mut() {
            if let Some(final_name) = phies_to_final_name.get(used_var) {
                *used_var = *final_name;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::testutils::*;

    use super::*;

    #[test]
    fn test_generate_phi() {
        let mut module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                $1 = 1
                exit = cond $0 ? @1..@1 : @2..@2
            }
            @1: {
                $1 = 2
                exit = jump @3
            }
            @2: {
                $1 = 3
                exit = jump @3
            }
            @3: {
                exit = return $1
            }",
        ]);

        generate_phi_nodes(&mut module);

        insta::assert_debug_snapshot!(module.top_level_stats(), @r###"
        @0: {
            $0 = 1
            $1 = 1
            exit = cond $0 ? @1..@1 : @2..@2
        }
        @1: {
            $2 = 2
            exit = jump @3
        }
        @2: {
            $3 = 3
            exit = jump @3
        }
        @3: {
            $4 = either($1, $2, $3)
            exit = return $4
        }
        "###);
    }

    #[test]
    fn test_generate_phi_2() {
        let mut module = parse_instructions_module(vec![
            "@0: {
                $0 = 999
                $1 = 1
                exit = cond $1 ? @1..@1 : @2..@2
            }
            @1: {
                $2 = 2
                $3 = $2
                exit = jump @3
            }
            @2: {
                $4 = 3
                exit = jump @3
            }
            @3: {
                $5 = either($0, $2)
                $6 = either($3, $4)
                $7 = $5
                exit = return $7
            }",
        ]);

        remove_phi_module(&mut module);

        generate_phi_nodes(&mut module);

        insta::assert_debug_snapshot!(module.top_level_stats(), @r###"
        @0: {
            $0 = 999
            $1 = 1
            exit = cond $1 ? @1..@1 : @2..@2
        }
        @1: {
            $2 = 2
            $3 = $2
            exit = jump @3
        }
        @2: {
            $4 = 3
            exit = jump @3
        }
        @3: {
            $5 = either($0, $2)
            $6 = either($3, $4)
            $7 = $5
            exit = return $7
        }
        "###);
    }

    #[test]
    fn test_remove_phi_1() {
        let mut blocks = parse_instructions(
            r###"
            @0: {
                $0 = 777
                $1 = $0
                $2 = either($0, $1)
                exit = return $2
            }
            "###,
        );

        remove_phi(&mut blocks);

        insta::assert_debug_snapshot!(blocks,
        @r###"
        @0: {
            $2 = 777
            $2 = $2
            exit = return $2
        }
        "###
        );
    }

    #[test]
    fn test_remove_phi_2() {
        let mut blocks = parse_instructions(
            r###"
            @0: {
                $0 = 999
                exit = jump @1
            }
            @1: {
                $1 = 1
                exit = cond $1 ? @2..@3 : @3..@4
            }
            @2: {
                $2 = 2
                $3 = $2
                exit = jump @4
            }
            @3: {
                $4 = 3
                exit = jump @4
            }
            @4: {
                $5 = either($0, $2)
                $6 = either($3, $4)
                $7 = $5
                exit = return $7
            }
            "###,
        );

        remove_phi(&mut blocks);

        insta::assert_debug_snapshot!(blocks,
        @r###"
        @0: {
            $5 = 999
            exit = jump @1
        }
        @1: {
            $1 = 1
            exit = cond $1 ? @2..@3 : @3..@4
        }
        @2: {
            $5 = 2
            $6 = $5
            exit = jump @4
        }
        @3: {
            $6 = 3
            exit = jump @4
        }
        @4: {
            $7 = $5
            exit = return $7
        }
        "###
        );
    }

    #[test]
    fn test_remove_phi_3() {
        let mut blocks = parse_instructions(
            r###"
            @0: {
                $0 = 1
                $1 = $0
                $2 = 1
                $3 = $1 == $2
                exit = jump @1
            }
            @1: {
                exit = cond $3 ? @2..@9 : @10..@11
            }
            @2: {
                $4 = $0
                $5 = 1
                $6 = $4 == $5
                exit = jump @3
            }
            @3: {
                exit = cond $6 ? @4..@5 : @6..@7
            }
            @4: {
                $7 = $0
                $8 = 2000
                $9 = $7 + $8
                $10 = $9
                exit = jump @5
            }
            @5: {
                exit = jump @8
            }
            @6: {
                $11 = 3
                $12 = $11
                exit = jump @7
            }
            @7: {
                exit = jump @8
            }
            @8: {
                $13 = either($0, $9, $11)
                $14 = $13
                $15 = 1000
                $16 = $14 + $15
                $17 = $16
                exit = jump @9
            }
            @9: {
                exit = jump @12
            }
            @10: {
                $18 = 3
                $19 = $18
                exit = jump @11
            }
            @11: {
                exit = jump @12
            }
            @12: {
                $20 = either($13, $16, $18)
                $21 = $20
                exit = return $21
            }
            "###,
        );

        remove_phi(&mut blocks);

        insta::assert_debug_snapshot!(blocks,
        @r###"
        @0: {
            $13 = 1
            $1 = $13
            $2 = 1
            $3 = $1 == $2
            exit = jump @1
        }
        @1: {
            exit = cond $3 ? @2..@9 : @10..@11
        }
        @2: {
            $4 = $13
            $5 = 1
            $6 = $4 == $5
            exit = jump @3
        }
        @3: {
            exit = cond $6 ? @4..@5 : @6..@7
        }
        @4: {
            $7 = $13
            $8 = 2000
            $13 = $7 + $8
            $10 = $13
            exit = jump @5
        }
        @5: {
            exit = jump @8
        }
        @6: {
            $13 = 3
            $12 = $13
            exit = jump @7
        }
        @7: {
            exit = jump @8
        }
        @8: {
            $14 = $13
            $15 = 1000
            $13 = $14 + $15
            $17 = $13
            exit = jump @9
        }
        @9: {
            exit = jump @12
        }
        @10: {
            $13 = 3
            $19 = $13
            exit = jump @11
        }
        @11: {
            exit = jump @12
        }
        @12: {
            $21 = $13
            exit = return $21
        }
        "###
        );
    }
}
