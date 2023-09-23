// The $0 = either($1 $2 $3) is a phi node, which is a special node that can be either $1, $2, or $3.
// This is no longer useful
// We need to find these and replace $1 $2 $3, whenever they're found, with just $0

use std::collections::{HashMap, HashSet};

use crate::basic_blocks::{BasicBlock, BasicBlockGroup, BasicBlockInstruction};

pub fn remove_phi(group: &mut BasicBlockGroup) {
    let mut phies_to_final_name: HashMap<usize, usize> = collect_phi(group);

    for (_, block) in group.iter_mut() {
        remove_phi_inner(block, &mut phies_to_final_name);
    }
}

fn collect_phi(group: &BasicBlockGroup) -> HashMap<usize, usize> {
    let mut phies_to_final_name: HashMap<usize, usize> = Default::default();
    let mut final_names: HashSet<usize> = Default::default();

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

fn remove_phi_inner(block: &mut BasicBlock, phies_to_final_name: &mut HashMap<usize, usize>) {
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
    use super::*;
    use crate::testutils::*;

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
