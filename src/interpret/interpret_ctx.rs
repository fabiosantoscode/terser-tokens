use std::collections::{BTreeMap, BTreeSet};

use crate::analyze::function_usage_count;
use crate::basic_blocks::{BasicBlockGroup, BasicBlockModule, FunctionId, ObjectKey, LHS};
use crate::data_structures::CowMap;

use super::{interpret_function, JsArgs, JsCompletion, JsType};

#[derive(Debug)]
pub struct InterpretCtx<'module> {
    variables: Vec<CowMap<usize, JsType>>,
    arguments: Vec<JsArgs>,
    pub(crate) cached_functions: BTreeMap<FunctionId, CachedFunction>,
    module: Option<&'module BasicBlockModule>,
    functions_evaluated: BTreeMap<FunctionId, usize>,
}

impl<'module> Default for InterpretCtx<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'module> InterpretCtx<'_> {
    pub(crate) fn new() -> InterpretCtx<'module> {
        InterpretCtx {
            variables: vec![CowMap::new()],
            arguments: vec![Default::default()],
            cached_functions: Default::default(),
            module: Default::default(),
            functions_evaluated: Default::default(),
        }
    }

    pub(crate) fn from_module(module: &'module BasicBlockModule) -> InterpretCtx<'module> {
        InterpretCtx {
            variables: vec![CowMap::new()],
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

    pub(crate) fn get_lhs(&self, lhs: &LHS) -> Option<&JsType> {
        match lhs {
            LHS::Local(v) => self.get_variable(*v),
            LHS::NonLocal(_) => None, // TODO
            LHS::Global(_) => None,   // TODO
            LHS::Member(base, member) => {
                let base = self.get_lhs(base)?;

                let string_key = match member {
                    ObjectKey::KeyValue(string_key) => string_key.clone(),
                    ObjectKey::Computed(varname) => self.get_variable(*varname)?.to_string()?,
                    _ => todo!("private fields"),
                };

                match base {
                    JsType::TheObject(o) => o.get(&string_key),
                    JsType::TheArray(a) => {
                        let as_num = string_key.parse::<usize>().ok()?;
                        a.get(as_num)
                    }
                    _ => return None,
                }
            }
        }
    }

    pub(crate) fn set_lhs(&mut self, lhs: &LHS, new_value: &JsType) -> Option<()> {
        match lhs {
            LHS::Local(v) => {
                let old = self.get_variable(*v)?;

                if old == new_value {
                    return Some(()); // Nothing was changed
                }

                self.assign_variable(*v, new_value.clone());

                Some(())
            }
            LHS::NonLocal(_) => None, // TODO
            LHS::Global(_) => None,   // TODO
            LHS::Member(base, member) => {
                let old_base = self.get_lhs(base)?;
                let string_key = match member {
                    ObjectKey::KeyValue(string_key) => string_key.clone(),
                    ObjectKey::Computed(varname) => self.get_variable(*varname)?.to_string()?,
                    _ => return None,
                };

                let old = match old_base {
                    JsType::Object | JsType::Array => {
                        return Some(()); // No-op
                    }
                    JsType::TheObject(o) => o.get(&string_key)?,
                    JsType::TheArray(a) => {
                        let as_num = string_key.parse::<usize>().ok()?;
                        a.get(as_num)?
                    }
                    _ => return None,
                };

                if old == new_value {
                    return Some(()); // Nothing was changed
                }

                let mut new_t = old_base.clone();
                match &mut new_t {
                    JsType::TheObject(ref mut o) => {
                        o.insert(string_key, new_value.clone());
                    }
                    JsType::TheArray(ref mut a) => {
                        let as_num = string_key.parse::<usize>().ok()?;
                        a[as_num] = new_value.clone();
                    }
                    _ => unreachable!(),
                };

                self.set_lhs(base, &new_t)
            }
        }
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
    ) -> Option<&Option<JsCompletion>> {
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

    pub(crate) fn start_condition(&mut self) {
        self.variables.last_mut().unwrap().fork();
    }

    pub(crate) fn end_condition(&mut self) -> BTreeMap<usize, JsType> {
        self.variables.last_mut().unwrap().unfork()
    }

    pub(crate) fn merge_branch_mutations(
        &mut self,
        mut a_vars: BTreeMap<usize, JsType>,
        mut b_vars: BTreeMap<usize, JsType>,
    ) {
        let vars = self.variables.last_mut().unwrap();
        let changed_vars: BTreeSet<_> = a_vars.keys().chain(b_vars.keys()).cloned().collect();

        for changed in changed_vars {
            let merged_conds = match (a_vars.remove(&changed), b_vars.remove(&changed)) {
                (Some(a), Some(b)) => a.union(&b),
                (Some(only), None) | (None, Some(only)) => only,
                _ => unreachable!(),
            };

            match vars.get_mut(&changed) {
                Some(entry) => {
                    *entry = entry.union(&merged_conds);
                }
                None => {
                    vars.insert(changed, merged_conds);
                }
            };
        }
    }

    pub(crate) fn start_function(&mut self, function_id: FunctionId, args: JsArgs) {
        self.cached_functions
            .entry(function_id)
            .or_insert_with(CachedFunction::new)
            .start_caching(&args);
        self.variables.push(CowMap::new());
        self.arguments.push(args);
    }

    pub(crate) fn end_function(&mut self, function_id: FunctionId, result: Option<JsCompletion>) {
        let assigned_vars = self
            .variables
            .pop()
            .expect("unbalanced start_function() and end_function()");
        // Merge the types used in this call, with the types we had before
        let current_vars = self.variables.last_mut().unwrap();
        for (var_idx, var_type) in assigned_vars.into_iter() {
            match current_vars.get_mut(&var_idx) {
                Some(existing) => {
                    *existing = existing.union(&var_type);
                }
                None => {
                    current_vars.insert(var_idx, var_type);
                }
            };
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
            // TODO we must also re-eval where we haven't seen all the calls
            let func = self.get_function(*function_id).unwrap().clone(/*TODO*/);

            interpret_function(self, &func, JsArgs::Unknown);
        }
    }

    pub(crate) fn mark_function_evaluated(&mut self, id: FunctionId) {
        let count = self.functions_evaluated.entry(id).or_insert(0);
        *count += 1;
    }

    /// Coalesce this context into an index of all variables and their values
    pub(crate) fn into_all_variables(mut self) -> CowMap<usize, JsType> {
        assert_eq!(
            self.variables.len(),
            1,
            "this must be called outside of an interpretation phase"
        );

        std::mem::take(&mut self.variables[0])
    }
}

const CACHED_VARIANTS_LIMIT: usize = 10;

#[derive(Debug, Default, PartialEq)]
pub struct CachedFunction {
    caching: BTreeSet<JsArgs>,
    cached: BTreeMap<JsArgs, Option<JsCompletion>>,
}

impl CachedFunction {
    fn new() -> CachedFunction {
        Default::default()
    }

    fn can_cache(&self) -> bool {
        self.cached.len() < CACHED_VARIANTS_LIMIT
    }

    fn get_cached_for_args(&self, args: &JsArgs) -> Option<&Option<JsCompletion>> {
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

    fn end_caching(&mut self, args: &JsArgs, result: Option<JsCompletion>) {
        let was_cached = self.caching.remove(args);
        assert!(was_cached);
        self.cached.insert(args.clone(), result);
    }

    pub(crate) fn consolidate(&self) -> Option<(JsArgs, Option<JsType>)> {
        if self.cached.len() >= CACHED_VARIANTS_LIMIT {
            return None;
        }

        assert_eq!(
            self.caching,
            BTreeSet::new(),
            "still caching, cannot consolidate"
        );

        let args = JsArgs::consolidate_all(self.cached.iter().map(|(args, _)| args));
        let completions = self
            .cached
            .iter()
            .map(|(_, completion)| completion.as_ref())
            .collect::<Option<Vec<_>>>()
            .and_then(JsCompletion::merge_all_return);

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
