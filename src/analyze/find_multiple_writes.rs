use std::collections::{BTreeMap, BTreeSet};

use crate::basic_blocks::{StructuredFlow, StructuredModule, LHS};

/// Find variables which are written to many times. Useful for interpretation, since it tells us what's safe to assume.
pub fn find_multiple_writes(module: &StructuredModule) -> BTreeSet<usize> {
    let mut ctx = MultipleWritesCtx {
        block_unobserved_writes: BTreeSet::new(),
        block_writes: BTreeMap::new(),
        block_reads: BTreeSet::new(),
        global_writes: BTreeMap::new(),
    };

    for (_, block) in module.iter_all_flows() {
        match block {
            StructuredFlow::Instruction(varname, ins) => {
                for read in ins.get_read_vars_and_nonlocals() {
                    ctx.register_read(read);
                }

                if let Some(wrote) = ins.get_written_lhs().and_then(LHS::var_or_nonlocal_base) {
                    ctx.register_write(wrote);
                }

                ctx.register_write(*varname);

                if !ins.can_be_reordered() {
                    ctx.flush_block_to_global();
                }
            }
            other => {
                for used_var in other.used_vars() {
                    ctx.register_read(used_var);
                }
                ctx.flush_block_to_global();
            }
        }
    }

    ctx.flush_block_to_global();

    ctx.global_writes
        .into_iter()
        .filter(|(_, count)| *count > 1)
        .map(|(k, _)| k.clone())
        .collect()
}

struct MultipleWritesCtx {
    block_unobserved_writes: BTreeSet<usize>,
    block_writes: BTreeMap<usize, u32>,
    block_reads: BTreeSet<usize>,
    global_writes: BTreeMap<usize, u32>,
}

impl MultipleWritesCtx {
    fn flush_block_to_global(&mut self) {
        for (var, count) in std::mem::take(&mut self.block_writes) {
            *self.global_writes.entry(var).or_insert(0) += count;
        }
        for var in std::mem::take(&mut self.block_unobserved_writes) {
            self.global_writes.entry(var).or_insert(1);
        }
        std::mem::take(&mut self.block_reads);
    }

    fn register_read(&mut self, var: usize) {
        if !self.global_writes.contains_key(&var) {
            self.block_reads.insert(var);
        }
    }

    fn register_write(&mut self, var: usize) {
        if self.global_writes.contains_key(&var) {
            *self.global_writes.entry(var).or_insert(0) += 1;
        } else if self.block_reads.contains(&var) {
            *self.block_writes.entry(var).or_insert(0) += 1;
        } else {
            self.block_unobserved_writes.insert(var);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutils::*;

    #[test]
    fn test_find_multiple_writes() {
        let module = parse_test_module(vec![
            "{
                $0 = 1
                Throw $999
                $0 = 2
                $1 = 3
            }",
        ]);
        let module = module.into();

        let multiple_writes = find_multiple_writes(&module);
        insta::assert_debug_snapshot!(multiple_writes, @r###"
        {
            0,
        }
        "###);
    }

    #[test]
    fn test_find_multiple_writes_2() {
        let module = parse_test_module(vec![
            "{
                $0 = 1
                $0 = 2
                $1 = 3
            }",
        ]);
        let module = module.into();

        let multiple_writes = find_multiple_writes(&module);
        insta::assert_debug_snapshot!(multiple_writes, @"{}");
    }
}
