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

    let mut function_calls: BTreeMap<FunctionId, Vec<CallLog>> = all_function_vars
        .values()
        .map(|func_id| (*func_id, Vec::with_capacity(0)))
        .collect();

    for (in_function, _, _, ins) in module.iter_all_instructions() {
        if let BasicBlockInstruction::Call(callee, args) = ins {
            if let Some(func_id) = all_function_vars.get(&callee) {
                let calls = function_calls
                    .get_mut(func_id)
                    .expect("all keys must be present from initialization");

                calls.push(CallLog {
                    called_in_function: in_function,
                    args: args.to_vec(),
                });
            }
        }
    }

    // filter map to remove None
    function_calls
}

/// Retrieve a map of variables and the functions they hold. Also return a set of vars that have multiple writes.
fn get_all_function_vars(module: &BasicBlockModule) -> BTreeMap<usize, FunctionId> {
    let mut function_vars = BTreeMap::new();
    let leaked = find_leaked_vars(module);

    for (_, _, varname, ins) in module.iter_all_instructions() {
        if let BasicBlockInstruction::Function(id) = ins {
            function_vars.insert(varname, *id);
        }
    }

    // find indirect references to functions
    loop {
        let len_before = function_vars.len();

        for (_, _, varname, ins) in module.iter_all_instructions() {
            match ins {
                BasicBlockInstruction::Read(
                    LHS::Local(reference) | LHS::NonLocal(NonLocalId(reference)),
                )
                | BasicBlockInstruction::Ref(reference) => {
                    if !leaked.contains(&varname) {
                        if let Some(id) = function_vars.get(&reference) {
                            function_vars.insert(varname, *id);
                        }
                    }
                }
                BasicBlockInstruction::Write(
                    LHS::Local(varname) | LHS::NonLocal(NonLocalId(varname)),
                    origin,
                ) => {
                    if !leaked.contains(varname) {
                        if let Some(id) = function_vars.get(&origin) {
                            function_vars.insert(*varname, *id);
                        }
                    }
                }
                _ => {}
            }
        }

        if function_vars.len() == len_before {
            break;
        }
    }

    for var in leaked {
        function_vars.remove(&var);
    }

    function_vars
}

/// Find all variables that are leaked, IE we can't track their value
fn find_leaked_vars(module: &BasicBlockModule) -> BTreeSet<usize> {
    let mut unknowable = BTreeSet::new();

    let mark_unknowable = |unknowable: &mut BTreeSet<_>, the_var: usize| {
        unknowable.insert(the_var);
    };

    loop {
        let unknowable_count_before = unknowable.len();
        for (_, _, block) in module.iter_all_blocks() {
            for (varname, ins) in block.iter() {
                match ins {
                    BasicBlockInstruction::Call(callee, args) => {
                        for arg in args {
                            mark_unknowable(&mut unknowable, *arg);
                        }
                    }
                    BasicBlockInstruction::Write(
                        LHS::Local(id) | LHS::NonLocal(NonLocalId(id)),
                        inp,
                    ) => {
                        // if multi_write_vars.contains(&id) {
                        // TODO only mark the value if it's written to multiple times. Also, track if the original value was a function. This will handle an initial value of undefined which is usually given to nonlocals before reassignment.
                        mark_unknowable(&mut unknowable, *id);
                        mark_unknowable(&mut unknowable, *inp);
                        //}
                    }
                    BasicBlockInstruction::Read(LHS::Local(id) | LHS::NonLocal(NonLocalId(id)))
                    | BasicBlockInstruction::Ref(id) => {
                        // Reassignment doesn't mean losing track
                        // unless it's a reassignment of an unknowable
                        if unknowable.contains(id) {
                            unknowable.insert(varname);
                        }
                    }
                    unknown_instruction => {
                        for var in unknown_instruction.get_read_vars_and_nonlocals() {
                            mark_unknowable(&mut unknowable, var);
                        }
                    }
                }
            }

            block.exit.used_vars().iter().for_each(|var| {
                mark_unknowable(&mut unknowable, *var);
            });
        }

        if unknowable.len() == unknowable_count_before {
            break;
        }
    }

    unknowable
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

        let function_vars = get_all_function_vars(&module);

        assert_eq!(function_vars.get(&0), None);
        assert_eq!(function_vars.get(&1), Some(&FunctionId(1)));
        assert_eq!(
            function_vars.get(&2),
            Some(&FunctionId(1)),
            "forward reference"
        );
        assert_eq!(
            function_vars.get(&3),
            Some(&FunctionId(1)),
            "backward reference"
        );
        assert_eq!(
            function_vars.get(&4),
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
                $100 = 100
                $110 = 110
                $1 = FunctionId(1)
                $2 = $1
                $3 = call $2($100)
                $4 = read_non_local $$1
                $5 = call $4($110)
                exit = return $100
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
            vec![vec![100], vec![110]],
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

    #[test]
    fn test_nonlocal_function_calls() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = undefined
                $2 = write_non_local $$1 $0
                $3 = FunctionId(1)
                $100 = call $3()
                $4 = FunctionId(2)
                $5 = write_non_local $$1 $4
                $101 = call $3()
                $6 = FunctionId(3)
                $7 = write_non_local $$1 $6
                $103 = call $3()
                $8 = $6
                $9 = $3
                $10 = 1
                $11 = call $9($10)
                exit = return $11
            }",
            "@0: {
                $12 = arguments[0]
                $13 = $12
                $14 = read_non_local $$1
                $15 = call $14()
                $16 = $13 + $15
                exit = return $16
            }",
            "@0: {
                $17 = 99999
                exit = return $17
            }",
            "@0: {
                $18 = 1
                exit = return $18
            }",
        ]);
        let call_graph = construct_call_graph(&module);
        assert!(
            call_graph.function_calls.get(&FunctionId(1)).is_some(),
            "we know FunctionId(1) is called"
        );
        assert!(
            call_graph.function_calls.get(&FunctionId(2)).is_none(),
            "we don't know if FunctionId(2) is called"
        );
        assert!(
            call_graph.function_calls.get(&FunctionId(3)).is_none(),
            "we don't know if FunctionId(3) is called"
        );
    }

    #[test]
    fn test_leaked_funcs() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = undefined
                $2 = write_non_local $$1 $0
                $3 = FunctionId(1)
                $100 = call $3()
                $4 = FunctionId(2)
                $5 = write_non_local $$1 $4
                $101 = call $3()
                $6 = FunctionId(3)
                $7 = write_non_local $$1 $6
                $103 = call $3()
                $8 = $6
                $9 = $3
                $10 = 1
                $11 = call $9($10)
                exit = return $11
            }",
            "@0: {
                $12 = arguments[0]
                $13 = $12
                $14 = read_non_local $$1
                $15 = call $14()
                $16 = $13 + $15
                exit = return $16
            }",
            "@0: {
                $17 = 99999
                exit = return $17
            }",
            "@0: {
                $18 = 1
                exit = return $18
            }",
        ]);
        let leaked = find_leaked_vars(&module);
        insta::assert_debug_snapshot!(leaked, @"
        {
            0,
            1,
            4,
            6,
            8,
            10,
            11,
            13,
            14,
            15,
            16,
            17,
            18,
        }
        ");
    }
}
