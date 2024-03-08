use std::collections::{BTreeMap, BTreeSet};

use crate::basic_blocks::{BasicBlockModule, LHS};

/// Find variables which are written to many times. Useful for interpretation, since it tells us what's safe to assume.
pub fn find_multiple_writes(module: &BasicBlockModule) -> BTreeSet<usize> {
    let mut writes = BTreeMap::new();

    for (_, _, varname, ins) in module.iter_all_instructions() {
        if let Some(wrote) = ins.get_written_lhs().and_then(LHS::var_or_nonlocal_base) {
            *writes.entry(wrote).or_insert(0) += 1;
        }

        *writes.entry(varname).or_insert(0) += 1;
    }

    writes
        .into_iter()
        .filter(|(_, count)| *count > 1)
        .map(|(k, _)| k.clone())
        .collect()
}
