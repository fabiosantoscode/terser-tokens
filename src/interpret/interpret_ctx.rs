use std::collections::{BTreeMap, BTreeSet};

use crate::analyze::find_multiple_writes;
use crate::basic_blocks::{
    BasicBlockExit, BasicBlockModule, ExitType, FunctionId, NonLocalId, ObjectKey, LHS,
};
use crate::data_structures::CowMap;

use super::{interpret_function, JsArgs, JsCompletion, JsType};

#[derive(Debug)]
pub struct InterpretCtx<'module> {
    variables: CowMap<usize, JsType>,
    arguments: JsArgs,
    cached_functions: BTreeMap<FunctionId, CachedFunction>,
    non_canon: bool,
    module: Option<&'module BasicBlockModule>,
    multiple_writes: BTreeSet<usize>,
    pub disable_tinyfuncs: bool,
}

impl<'module> InterpretCtx<'_> {
    pub(crate) fn from_module(module: &'module BasicBlockModule) -> InterpretCtx<'module> {
        InterpretCtx {
            variables: Default::default(),
            arguments: Default::default(),
            cached_functions: Default::default(),
            non_canon: false,
            multiple_writes: find_multiple_writes(module),
            module: Some(module),
            disable_tinyfuncs: false,
        }
    }

    pub(crate) fn assign_variable(&mut self, var_idx: usize, value: JsType) -> Option<()> {
        self.set_lhs(&LHS::Local(var_idx), value)
    }
    pub(crate) fn get_variable(&self, var_idx: usize) -> Option<&JsType> {
        self.get_lhs(&LHS::Local(var_idx))
    }

    pub(crate) fn get_variables(&self, items: &Vec<usize>) -> Option<Vec<JsType>> {
        let mut result = Vec::with_capacity(items.len());
        for item in items {
            result.push(self.get_variable(*item)?.clone());
        }
        Some(result)
    }

    pub(crate) fn get_lhs(&self, lhs: &LHS) -> Option<&JsType> {
        match lhs {
            LHS::Local(v) | LHS::NonLocal(NonLocalId(v)) => {
                if self.multiple_writes.contains(v) {
                    return None;
                } else {
                    self.variables.get(v)
                }
            }
            LHS::Global(_) => None, // TODO
            LHS::Member(base, member) => {
                let base = self.get_lhs(base)?;

                let string_key = match member {
                    ObjectKey::NormalKey(string_key) => string_key.clone(),
                    ObjectKey::Computed(varname) => self.get_variable(*varname)?.to_string()?,
                    ObjectKey::Private(_) => None?, // private fields not supported
                };

                match base {
                    JsType::TheObject(o) | JsType::TheFunction(_, o) => o.get(&string_key),
                    JsType::TheArray(a) => {
                        let as_num = string_key.parse::<usize>().ok()?;
                        a.get(as_num)
                    }
                    _ => return None,
                }
            }
        }
    }

    pub(crate) fn set_lhs(&mut self, lhs: &LHS, new_value: JsType) -> Option<()> {
        match lhs {
            LHS::Local(v) | LHS::NonLocal(NonLocalId(v)) => {
                if self.multiple_writes.contains(v) {
                    return None;
                } else {
                    self.variables.insert(*v, new_value.clone());
                    Some(())
                }
            }
            LHS::Global(_) => None, // TODO
            LHS::Member(base, member) => {
                let old_base = self.get_lhs(base)?;
                let string_key = match member {
                    ObjectKey::NormalKey(string_key) => string_key.clone(),
                    ObjectKey::Computed(varname) => {
                        self.get_lhs(&LHS::Local(*varname))?.to_string()?
                    }
                    _ => return None,
                };

                let old_value = match old_base {
                    JsType::Object | JsType::Array => {
                        return Some(()); // No-op
                    }
                    JsType::TheObject(o) | JsType::TheFunction(_, o) => o.get(&string_key),
                    JsType::TheArray(a) => {
                        let as_num = string_key.parse::<usize>().ok()?;
                        a.get(as_num)
                    }
                    _ => return None,
                };

                if old_value == Some(&new_value) {
                    return Some(()); // Nothing was changed
                }
                // TODO mutations of the environment
                return None;

                let mut new_t = old_base.clone();
                match &mut new_t {
                    JsType::TheObject(ref mut o) | JsType::TheFunction(_, ref mut o) => {
                        o.insert(string_key, new_value.clone());
                    }
                    JsType::TheArray(ref mut a) => {
                        let as_num = string_key.parse::<usize>().ok()?;
                        a[as_num] = new_value.clone();
                    }
                    _ => unreachable!(),
                };

                self.set_lhs(base, new_t)
            }
        }
    }

    pub fn delete_lhs(&mut self, lhs: &LHS) -> Option<JsType> {
        None // TODO deleting object props, etc
    }

    pub fn get_argument(&self, n: usize) -> Option<&JsType> {
        self.arguments.nth(n)
    }
    pub fn get_spread_argument(&self, n: usize) -> Option<&[JsType]> {
        self.arguments.spread_from(n)
    }

    pub(crate) fn start_branch(&mut self) {
        self.variables.fork();
    }
    pub(crate) fn end_branch(&mut self) -> BTreeMap<usize, JsType> {
        self.variables.unfork()
    }

    pub(crate) fn merge_branch_mutations(
        &mut self,
        mut a_vars: BTreeMap<usize, JsType>,
        mut b_vars: BTreeMap<usize, JsType>,
    ) {
        let changed_vars: BTreeSet<_> = a_vars.keys().chain(b_vars.keys()).cloned().collect();

        for changed in changed_vars {
            let to_insert = match (a_vars.remove(&changed), b_vars.remove(&changed)) {
                (Some(a), Some(b)) => a.union(&b),
                (Some(either), None) | (None, Some(either)) => {
                    match self.variables.get_mut(&changed) {
                        Some(entry) => entry.union(&either),
                        None => either,
                    }
                }
                _ => unreachable!(),
            };

            self.variables.insert(changed, to_insert);
        }
    }

    pub(crate) fn start_function(
        &mut self,
        is_canonical: bool,
        func_id: FunctionId,
        args: JsArgs,
    ) -> JsArgs {
        if self.non_canon {
            panic!("forbid_calls");
        }

        if is_canonical {
            self.cached_functions
                .entry(func_id)
                .or_default()
                .start_caching(&args);
        } else {
            self.non_canon = true;
            self.variables.fork();
        }

        std::mem::replace(&mut self.arguments, args)
    }

    pub(crate) fn end_function(
        &mut self,
        is_canonical: bool,
        func_id: FunctionId,
        old_args: JsArgs,
        result: Option<JsCompletion>,
    ) {
        let in_fn_args = std::mem::replace(&mut self.arguments, old_args);
        if is_canonical {
            self.cached_functions
                .get_mut(&func_id)
                .expect("unbalanced start_function() and end_function()")
                .end_caching(in_fn_args, result);
        } else {
            self.non_canon = false;
            self.variables.unfork();
        }
    }

    fn is_simple_function<'a>(&'a self, the_function: FunctionId) -> bool {
        let func = self.module.and_then(|md| md.functions.get(&the_function));

        if let Some(func) = func {
            func.blocks.iter().all(|(_, block)| {
                matches!(
                    block.exit,
                    BasicBlockExit::ExitFn(ExitType::Return, _)
                        | BasicBlockExit::Debugger
                        | BasicBlockExit::Fallthrough
                ) && block
                    .instructions
                    .iter()
                    .all(|(_, ins)| ins.can_be_reordered())
            })
        } else {
            false
        }
    }

    fn eval_tiny_function(
        &mut self,
        the_function: FunctionId,
        args: JsArgs,
    ) -> Option<JsCompletion> {
        if self.disable_tinyfuncs || !self.is_simple_function(the_function) {
            return None;
        }

        let cached = self.cached_functions.entry(the_function).or_default();
        if !cached.can_cache() || args == JsArgs::Unknown {
            return None;
        }

        let simple_function = self
            .module?
            .functions
            .get(&the_function)
            .expect("ensured in is_simple_function()");

        let result = interpret_function(self, simple_function, args.clone(), false);

        let ret = result.as_ref().and_then(|r| r.as_return().cloned());

        Some(JsCompletion::Normal(ret?))
    }

    pub(crate) fn get_function_return(
        &mut self,
        the_function: FunctionId,
        args: JsArgs,
    ) -> Option<JsCompletion> {
        if self.non_canon {
            return None;
        }

        if args != JsArgs::Unknown {
            if let Some(known_ret) = self
                .cached_functions
                .get(&the_function)
                .and_then(|cache| cache.get_cached_for_args(&args))
            {
                // With known args, we can attempt to get a cached result for these specific args
                return match known_ret {
                    Some(JsCompletion::Return(ret)) => Some(JsCompletion::Normal(ret.clone())),
                    _ => None,
                };
            } else if let Some(tiny_eval) = self.eval_tiny_function(the_function, args.clone()) {
                return Some(tiny_eval);
            }
        }

        Some(JsCompletion::Normal(
            self.cached_functions
                .get(&the_function)?
                .get_cached_for_args(&args)?
                .clone()?
                .into_return()?,
        ))
    }

    /// Coalesce this context into an index of all variables and their values
    pub(crate) fn into_all_variables(mut self) -> CowMap<usize, JsType> {
        assert_eq!(
            self.variables.is_root(),
            true,
            "this must be called outside of an interpretation phase"
        );

        std::mem::take(&mut self.variables)
    }
}

const CACHED_VARIANTS_LIMIT: usize = 10;

#[derive(Debug, Default, PartialEq)]
pub struct CachedFunction {
    caching: BTreeSet<JsArgs>,
    cached: BTreeMap<JsArgs, Option<JsCompletion>>,
}

impl CachedFunction {
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

    fn end_caching(&mut self, args: JsArgs, result: Option<JsCompletion>) {
        let was_cached = self.caching.remove(&args);
        assert!(was_cached);
        self.cached.insert(args, result);
    }
}

#[cfg(test)]
mod tests {
    use crate::testutils::*;

    use super::*;

    use super::super::interpret_module;

    /*
    fn get_known_args(ctx: &InterpretCtx) -> JsArgs {
        let func_result = ctx.function_returns.get(&FunctionId(1));

        // the function is marked as unknown because we didn't evaluate any calls to it
        func_result.unwrap().unwrap()
    }

    #[test]
    fn wrap_up_module_nocalls() {
        let m = parse_instructions_module(vec![
            "@0: { $0 = undefined exit = return $0 }",
            "@0: { $1 = undefined exit = return $1 }",
        ]);
        let mut ctx = InterpretCtx::from_module(&m);

        interpret_module(&mut ctx, &m);

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

        // the function arguments are known
        insta::assert_debug_snapshot!(get_known_args(&ctx).as_known().unwrap(), @"[]");
    }
    */
}
