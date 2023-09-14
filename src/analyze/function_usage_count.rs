use crate::basic_blocks::{BasicBlockInstruction, BasicBlockModule, FunctionId};
use std::collections::BTreeMap;

/// How many times each function was seen in text.
pub fn function_usage_count(module: &BasicBlockModule) -> BTreeMap<FunctionId, usize> {
    let mut function_vars_report = module
        .functions
        .keys()
        .map(|id| (*id, 0))
        .collect::<BTreeMap<_, _>>();

    let mut functions_by_varname = BTreeMap::new();

    for (_, _, varname, instruction) in module.iter_all_instructions() {
        if let BasicBlockInstruction::Function(id) = instruction {
            functions_by_varname.insert(varname, id);
        }
    }
    for (_, _, _, instruction) in module.iter_all_instructions() {
        for used_var in instruction.used_vars() {
            if let Some(func) = functions_by_varname.get(&used_var) {
                let report_entry = function_vars_report
                    .get_mut(func)
                    .expect("unknown FunctionId found in instruction");
                *report_entry += 1;
            }
        }
    }

    function_vars_report
}
