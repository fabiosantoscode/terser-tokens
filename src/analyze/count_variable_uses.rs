use std::collections::BTreeMap;

use crate::basic_blocks::BasicBlockModule;

/// Count how often a variable is used. If unused, the map entry will be absent.
pub fn count_variable_uses(module: &BasicBlockModule) -> BTreeMap<usize, u32> {
    let mut ret: BTreeMap<usize, u32> = BTreeMap::new();

    for (_, _, block) in module.iter_all_blocks() {
        for (_, ins) in block.instructions.iter() {
            // Increment the use count for each operand
            for var in ins.used_vars() {
                *ret.entry(var).or_insert(0) += 1;
            }
        }

        // increment the use count for return $123 or cond $123
        for var in block.exit.used_vars() {
            *ret.entry(var).or_insert(0) += 1;
        }
    }

    ret
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutils::*;

    #[test]
    fn test_count_uses() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = 2
                $1 = $0
                $2 = $0 + $1
                $3 = 0
                exit = return $2
            }",
        ]);

        let uses = count_variable_uses(&module);

        insta::assert_debug_snapshot!(uses, @r###"
        {
            0: 2,
            1: 1,
            2: 1,
        }
        "###);
    }
}
