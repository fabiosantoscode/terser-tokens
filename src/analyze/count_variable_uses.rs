use std::collections::BTreeMap;

use crate::basic_blocks::{StructuredFlow, StructuredModule};

/// Count how often a variable is used.
pub fn count_variable_uses(module: &StructuredModule) -> BTreeMap<usize, u32> {
    let mut usages: BTreeMap<usize, u32> = BTreeMap::new();

    for (_, func) in module.functions.iter() {
        count_flow(&mut usages, &func.blocks);
    }

    usages
}

fn count_flow(usages: &mut BTreeMap<usize, u32>, block: &Vec<StructuredFlow>) {
    for block in block.iter().flat_map(|blk| blk.nested_iter()) {
        match block {
            StructuredFlow::Instruction(_, ins) => {
                // Increment the use count for each operand
                for var in ins.get_read_vars_and_nonlocals() {
                    *usages.entry(var).or_insert(0) += 1;
                }
            }
            block => {
                // increment the use count for return $123 or cond $123
                for var in block.used_vars() {
                    *usages.entry(var).or_insert(0) += 1;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutils::*;

    #[test]
    fn test_count_uses() {
        let module = parse_test_module(vec![
            "{
                $0 = 2
                $1 = $0
                $2 = $0 + $1
                $3 = 0
                Return $2
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
