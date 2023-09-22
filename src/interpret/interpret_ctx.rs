use std::collections::{BTreeMap, BTreeSet};

use crate::analyze::function_usage_count;
use crate::basic_blocks::{BasicBlockGroup, BasicBlockModule, FunctionId};

use super::{interpret_function, InterpretCompletion, JsArgs, JsType};

#[derive(Debug, Default)]
pub struct InterpretCtx<'module> {
    variables: Vec<BTreeMap<usize, JsType>>,
    arguments: Vec<JsArgs>,
    pub(crate) cached_functions: BTreeMap<FunctionId, CachedFunction>,
    module: Option<&'module BasicBlockModule>,
    functions_evaluated: BTreeMap<FunctionId, usize>,
}

impl<'module> InterpretCtx<'_> {
    pub(crate) fn new() -> InterpretCtx<'module> {
        InterpretCtx {
            variables: vec![BTreeMap::new()],
            arguments: vec![Default::default()],
            ..Default::default()
        }
    }

    pub(crate) fn from_module(module: &'module BasicBlockModule) -> InterpretCtx<'module> {
        InterpretCtx {
            variables: vec![BTreeMap::new()],
            arguments: vec![Default::default()],
            module: Some(module),
            ..Default::default()
        }
    }

    pub(crate) fn assign_variable(&mut self, var_idx: usize, value: JsType) {
        self.variables.last_mut().unwrap().insert(var_idx, value);
    }
    pub(crate) fn get_variable(&self, var_idx: usize) -> Option<&JsType> {
        self.variables.last().unwrap().get(&var_idx)
    }

    pub(crate) fn get_variables(&self, items: Vec<usize>) -> Option<Vec<JsType>> {
        let mut result = Vec::with_capacity(items.len());
        for item in items {
            result.push(self.get_variable(item)?.clone());
        }
        Some(result)
    }

    pub fn get_argument(&self, n: usize) -> Option<&JsType> {
        self.arguments.last()?.nth(n)
    }
    pub fn get_spread_argument(&self, n: usize) -> Option<&[JsType]> {
        self.arguments.last()?.spread_from(n)
    }

    pub(crate) fn get_function(&self, the_function: FunctionId) -> Option<&BasicBlockGroup> {
        self.module?.functions.get(&the_function)
    }

    pub(crate) fn get_cached(
        &self,
        fn_id: FunctionId,
        args: &JsArgs,
    ) -> Option<&Option<InterpretCompletion>> {
        self.cached_functions
            .get(&fn_id)
            .and_then(|cached_function| cached_function.get_cached_for_args(args))
    }

    pub fn can_cache(&mut self, fn_id: FunctionId) -> bool {
        self.cached_functions
            .get(&fn_id)
            .map(|cached_fn| cached_fn.can_cache())
            .unwrap_or(true)
    }

    pub(crate) fn start_function(&mut self, function_id: FunctionId, args: JsArgs) {
        self.cached_functions
            .entry(function_id)
            .or_insert_with(CachedFunction::new)
            .start_caching(&args);
        self.variables.push(Default::default());
        self.arguments.push(args);
    }

    pub(crate) fn end_function(
        &mut self,
        function_id: FunctionId,
        result: Option<InterpretCompletion>,
    ) {
        let assigned_vars = self
            .variables
            .pop()
            .expect("unbalanced start_function() and end_function()");
        // Merge the types used in this call, with the types we had before
        let current_vars = self.variables.last_mut().unwrap();
        for (var_idx, var_type) in assigned_vars {
            current_vars
                .entry(var_idx)
                .and_modify(|existing| *existing = existing.union(&var_type))
                .or_insert(var_type);
        }
        let args = self
            .arguments
            .pop()
            .expect("unbalanced start_function() and end_function()");
        self.cached_functions
            .get_mut(&function_id)
            .expect("unbalanced start_function() and end_function()")
            .end_caching(&args, result);
    }

    /// The module has ended. Functions that we don't have complete knowledge about all their
    /// calls,
    /// are re-evaluated with unknown args. This will let us investigate their return value later.
    pub(crate) fn wrap_up_module(&mut self) {
        let module = self
            .module
            .expect("cannot call wrap_up_module if there is no module");

        let function_vars_report = function_usage_count(module);

        // we'll clear if the cache appears to be full, or if we haven't evaluated all references
        // to the function
        let needs_clearing =
            |in_code: Option<&usize>, evaluated: Option<&usize>, can_cache: bool| {
                if !can_cache {
                    false
                } else if let (Some(in_code), Some(evaluated)) = (in_code, evaluated) {
                    in_code > evaluated
                } else {
                    true
                }
            };

        let mut to_clear = vec![];
        for (function_id, cached) in self.cached_functions.iter() {
            let in_code = function_vars_report.get(&function_id);
            let evaluated = self.functions_evaluated.get(&function_id);

            if needs_clearing(in_code, evaluated, cached.can_cache()) {
                to_clear.push(*function_id);
            }
        }

        for function_id in to_clear {
            self.cached_functions.remove(&function_id);
        }

        let to_reeval = module
            .functions
            .keys()
            .filter(|func_id| !self.cached_functions.contains_key(*func_id))
            .collect::<Vec<_>>();

        for function_id in to_reeval.into_iter() {
            let func = self.get_function(*function_id).unwrap().clone(/*TODO*/);

            interpret_function(self, &func, JsArgs::Unknown);
        }
    }

    pub(crate) fn mark_function_evaluated(&mut self, id: FunctionId) {
        let count = self.functions_evaluated.entry(id).or_insert(0);
        *count += 1;
    }
}

const CACHED_VARIANTS_LIMIT: usize = 10;

#[derive(Debug, Default, PartialEq)]
pub struct CachedFunction {
    caching: BTreeSet<JsArgs>,
    cached: BTreeMap<JsArgs, Option<InterpretCompletion>>,
}

impl CachedFunction {
    fn new() -> CachedFunction {
        Default::default()
    }

    fn can_cache(&self) -> bool {
        self.cached.len() < CACHED_VARIANTS_LIMIT
    }

    fn get_cached_for_args(&self, args: &JsArgs) -> Option<&Option<InterpretCompletion>> {
        self.cached.get(args)
    }

    fn start_caching(&mut self, args: &JsArgs) {
        assert!(
            !self.caching.contains(args),
            "caching already started for args {:?}",
            args
        );
        self.caching.insert(args.clone());
    }

    fn end_caching(&mut self, args: &JsArgs, result: Option<InterpretCompletion>) {
        let was_cached = self.caching.remove(args);
        assert!(was_cached);
        self.cached.insert(args.clone(), result);
    }

    pub(crate) fn consolidate(&self) -> Option<(JsArgs, Option<JsType>)> {
        if self.cached.len() >= CACHED_VARIANTS_LIMIT {
            return None;
        }

        assert_eq!(self.caching, BTreeSet::new(), "still caching, cannot consolidate");

        let args = JsArgs::consolidate_all(self.cached.iter().map(|(args, _)| args));
        let completions = self
            .cached
            .iter()
            .map(|(_, completion)| completion.as_ref())
            .collect::<Option<Vec<_>>>()
            .and_then(InterpretCompletion::merge_all_return);

        Some((args, completions))
    }
}

#[cfg(test)]
mod tests {
    use crate::testutils::*;

    use super::*;

    use super::super::interpret_module;

    fn get_known_args(ctx: &InterpretCtx) -> JsArgs {
        let func_result = ctx.cached_functions.get(&FunctionId(1));

        // the function is marked as unknown because we didn't evaluate any calls to it
        func_result.unwrap().consolidate().unwrap().0
    }

    #[test]
    fn wrap_up_module_nocalls() {
        let m = parse_instructions_module(vec![
            "@0: { $0 = undefined exit = return $0 }",
            "@0: { $1 = undefined exit = return $1 }",
        ]);
        let mut ctx = InterpretCtx::from_module(&m);

        interpret_module(&mut ctx, &m);

        ctx.wrap_up_module();

        // the function is marked as unknown because we didn't evaluate any calls to it
        insta::assert_debug_snapshot!(get_known_args(&ctx), @"Unknown");
    }

    #[test]
    fn wrap_up_module_leakedfns() {
        let m = parse_instructions_module(vec![
            "@0: {
                $0 = FunctionId(1)
                $1 = call $0()
                $2 = $0
                exit = return $0
            }",
            "@0: { $3 = undefined exit = return $3 }",
        ]);
        let mut ctx = InterpretCtx::from_module(&m);

        interpret_module(&mut ctx, &m);

        ctx.wrap_up_module();

        // the function is marked as unknown because not all references to it
        // resulted in a call that we know about
        insta::assert_debug_snapshot!(get_known_args(&ctx), @"Unknown");
    }

    #[test]
    fn wrap_up_module_knowncalls() {
        let m = parse_instructions_module(vec![
            "@0: {
                $0 = FunctionId(1)
                $1 = call $0()
                exit = return $0
            }",
            "@0: { $3 = undefined exit = return $3 }",
        ]);
        let mut ctx = InterpretCtx::from_module(&m);

        interpret_module(&mut ctx, &m);

        ctx.wrap_up_module();

        // the function arguments are known
        insta::assert_debug_snapshot!(get_known_args(&ctx).as_known().unwrap(), @"[]");
    }
}
