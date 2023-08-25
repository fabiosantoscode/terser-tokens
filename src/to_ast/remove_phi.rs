// The $0 = either($1 $2 $3) is a phi node, which is a special node that can be either $1, $2, or $3.
// This is no longer useful
// We need to find these and replace $1 $2 $3, whenever they're found, with just $0

use std::collections::HashMap;

use crate::basic_blocks::{BasicBlock, BasicBlockGroup, BasicBlockInstruction};

pub fn remove_phi(group: &mut BasicBlockGroup) {
    let mut phies_to_final_name: HashMap<usize, usize> = collect_phi(group);

    for block in group.blocks.iter_mut() {
        remove_phi_inner(block, &mut phies_to_final_name);
    }
}

fn collect_phi(group: &BasicBlockGroup) -> HashMap<usize, usize> {
    let mut phies_to_final_name: HashMap<usize, usize> = Default::default();

    for (_, varname, ins) in group.iter_all_instructions() {
        if let BasicBlockInstruction::Phi(alternatives) = ins {
            for phi in alternatives {
                phies_to_final_name.insert(*phi, varname);
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

    for x in block.instructions.iter_mut() {
        if let Some(final_name) = phies_to_final_name.get(&x.0) {
            (*x).0 = *final_name;
        }
    }

    for (_, ins) in block.instructions.iter_mut() {
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
}
