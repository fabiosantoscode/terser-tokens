use std::collections::{BTreeMap, BTreeSet};

use crate::basic_blocks::{BasicBlockInstruction, BasicBlockModule, FunctionId, NonLocalId, LHS};

/// Construct the CallGraph, which will tell us how to traverse the functions in the correct order in order to find most types without yet-unknown return types.
pub fn construct_call_graph(module: &BasicBlockModule) -> CallGraph {
    let function_calls = get_all_function_calls(module);

    CallGraph {
        traversal_order: get_traversal_order(&function_calls),
        function_calls,
    }
}

pub struct CallGraph {
    pub function_calls: BTreeMap<FunctionId, Vec<CallLog>>,
    pub traversal_order: Vec<FunctionId>,
}

/// Get a list, starting with dependency-less functions, and ending with the top-level FunctionId(0).
pub fn get_traversal_order(function_calls: &BTreeMap<FunctionId, Vec<CallLog>>) -> Vec<FunctionId> {
    let mut traversal_order = Vec::new();
    let mut visited = BTreeSet::new();

    // Map { caller: callees }
    let dependencies: BTreeMap<FunctionId, BTreeSet<FunctionId>> = function_calls
        .iter()
        .flat_map(|(k, v)| v.iter().map(|call| (call.called_in_function, *k)))
        .fold(Default::default(), |mut acc, (caller, callee)| {
            acc.entry(caller).or_default().insert(callee);
            acc
        });

    // Start with nodes without in-edges
    let mut to_visit = vec![FunctionId(0)];

    while let Some(next) = to_visit.pop() {
        if visited.contains(&next) {
            continue;
        }
        visited.insert(next);

        traversal_order.push(next);

        if let Some(dependencies) = dependencies.get(&next) {
            for dep in dependencies {
                if !visited.contains(dep) {
                    to_visit.push(*dep);
                }
            }
        }
    }

    traversal_order.reverse();

    traversal_order
}

/// Given a module, create a graph of what functions call each other. Every function present on the map has a fully known list of calls.
fn get_all_function_calls(module: &BasicBlockModule) -> BTreeMap<FunctionId, Vec<CallLog>> {
    let all_function_vars = get_all_function_vars(module);

    let mut function_calls = BTreeMap::new();

    let mark_known_call = |function_calls: &mut BTreeMap<FunctionId, Option<BTreeSet<CallLog>>>,
                           called_in_function: FunctionId,
                           callee: usize,
                           args: &[usize]|
     -> Option<()> {
        let func_id = all_function_vars.get(&callee)?;
        let calls = function_calls
            .entry(*func_id)
            .or_insert_with(|| Some(BTreeSet::new()))
            .as_mut();

        calls?.insert(CallLog {
            called_in_function,
            args: args.to_vec(),
        });

        Some(())
    };

    let mark_unknowable = |function_calls: &mut BTreeMap<FunctionId, Option<BTreeSet<CallLog>>>,
                           the_var: usize| {
        if let Some(func_id) = all_function_vars.get(&the_var) {
            function_calls.insert(func_id.clone(), None);
        }
    };

    // Now we track what happens to function vars. None marks that we don't know what happens to the function. Non-existence means we didn't get to it yet.
    for (in_function, _, _, ins) in module.iter_all_instructions() {
        match ins {
            BasicBlockInstruction::Function(id) => {
                // Make sure it exists, so we can find never-called functions
                function_calls
                    .entry(*id)
                    .or_insert_with(|| Some(BTreeSet::new()));
            }
            BasicBlockInstruction::Call(callee, args) => {
                for arg in args {
                    mark_unknowable(&mut function_calls, *arg);
                }
                mark_known_call(&mut function_calls, in_function, *callee, args);
            }
            BasicBlockInstruction::Read(LHS::Local(_) | LHS::NonLocal(NonLocalId(_)))
            | BasicBlockInstruction::Ref(_)
            | BasicBlockInstruction::Write(LHS::Local(_) | LHS::NonLocal(NonLocalId(_)), _) => {
                // Reassignment doesn't mean losing track
            }
            unknown_instruction => {
                for var in unknown_instruction.get_read_vars_and_nonlocals() {
                    mark_unknowable(&mut function_calls, var);
                }
            }
        }
    }

    for (_, _, block) in module.iter_all_blocks() {
        block.exit.used_vars().iter().for_each(|var| {
            mark_unknowable(&mut function_calls, *var);
        });
    }

    // filter map to remove None
    function_calls
        .into_iter()
        .filter_map(|(k, v)| v.map(|v| (k, v)))
        .map(|(k, v)| (k, v.into_iter().collect()))
        .collect()
}

/// Retrieve a vector, whose index is the varname, and whose value is the function id that the varname is assigned to.
fn get_all_function_vars(module: &BasicBlockModule) -> BTreeMap<usize, FunctionId> {
    let mut function_calls = BTreeMap::new();

    for (_, _, varname, ins) in module.iter_all_instructions() {
        if let BasicBlockInstruction::Function(id) = ins {
            function_calls.insert(varname, *id);
        }
    }

    // find indirect references to functions
    let mut changed = true;
    while changed {
        changed = false;
        for (_, _, varname, ins) in module.iter_all_instructions() {
            match ins {
                BasicBlockInstruction::Read(
                    LHS::Local(reference) | LHS::NonLocal(NonLocalId(reference)),
                )
                | BasicBlockInstruction::Ref(reference) => {
                    if let Some(id) = function_calls.get(&reference) {
                        let had_item = function_calls.insert(varname, *id);

                        changed = changed || had_item.is_none()
                    }
                }
                BasicBlockInstruction::Write(
                    LHS::Local(varname) | LHS::NonLocal(NonLocalId(varname)),
                    origin,
                ) => {
                    if let Some(id) = function_calls.get(&origin) {
                        let had_item = function_calls.insert(*varname, *id);

                        changed = changed || had_item.is_none()
                    }
                }
                _ => {}
            }
        }
    }

    function_calls
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct CallLog {
    pub called_in_function: FunctionId,
    pub args: Vec<usize>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutils::*;

    #[test]
    fn test_get_all_function_vars() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = 100
                $1 = FunctionId(1)
                $2 = $3
                $3 = $1
                $4 = read_non_local $$1
                exit = return $0
            }",
        ]);

        let function_calls = get_all_function_vars(&module);

        assert_eq!(function_calls.get(&0), None);
        assert_eq!(function_calls.get(&1), Some(&FunctionId(1)));
        assert_eq!(
            function_calls.get(&2),
            Some(&FunctionId(1)),
            "forward reference"
        );
        assert_eq!(
            function_calls.get(&3),
            Some(&FunctionId(1)),
            "backward reference"
        );
        assert_eq!(
            function_calls.get(&4),
            Some(&FunctionId(1)),
            "nonlocal reference"
        );
    }

    #[test]
    fn test_get_all_function_calls() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = 100
                $1 = FunctionId(1)
                $2 = call $1($0)
                $404 = FunctionId(404)
                exit = return $0
            }",
        ]);
        let knowable_calls = get_all_function_calls(&module);

        assert_eq!(
            knowable_calls.get(&FunctionId(1)).unwrap(),
            &vec![CallLog {
                called_in_function: FunctionId(0),
                args: vec![0]
            }],
            "the total extent of calls is knowable"
        );
        assert_eq!(
            knowable_calls.get(&FunctionId(404)).unwrap(),
            &Vec::<CallLog>::new(),
            "known to never have been called"
        );

        // ---

        let module = parse_instructions_module(vec![
            "@0: {
                $0 = 100
                $1 = FunctionId(1)
                $2 = $1
                $3 = call $2($0)
                $4 = read_non_local $$1
                $5 = call $4($0)
                exit = return $0
            }",
        ]);
        let knowable_calls = get_all_function_calls(&module);

        assert_eq!(
            knowable_calls
                .get(&FunctionId(1))
                .unwrap()
                .into_iter()
                .map(|x| x.args.clone())
                .collect::<Vec<_>>(),
            vec![vec![0]],
            "function was read in a non-standard way"
        );
    }

    #[test]
    fn test_construct_call_graph() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = FunctionId(2)
                $1 = call $0()
                exit = return $1
            }",
            "@0: {
                $2 = 100
                exit = return $2
            }",
            "@0: {
                $3 = FunctionId(1)
                $4 = call $3()
                exit = return $4
            }",
        ]);
        let call_graph = construct_call_graph(&module);
        assert_eq!(
            call_graph.traversal_order,
            vec![FunctionId(1), FunctionId(2), FunctionId(0)],
        );
    }
}
