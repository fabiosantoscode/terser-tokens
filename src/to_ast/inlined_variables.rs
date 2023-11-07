use std::collections::BTreeMap;

use crate::basic_blocks::{BasicBlockInstruction, BasicBlockModule, LHS};

/// Returns a map of variable indices to instructions that can be inlined.
/// Inlining is when we take advantage of nested expressions to
/// reduce the amount of variables that need to be emitted.
pub fn get_inlined_variables(
    module: &BasicBlockModule,
    variable_write_count: &BTreeMap<usize, u32>,
    variable_use_count: &BTreeMap<usize, u32>,
) -> BTreeMap<usize, BasicBlockInstruction> {
    let mut ctx = GetInlinedVariablesCtx::new(variable_write_count, variable_use_count);

    for (_id, block_group) in module.iter() {
        // variables used once that are reorderable. Wherever they were defined, they can be inlined.
        ctx.pure_candidates = block_group
            .iter_all_instructions()
            .filter(|&(_blk, ins_id, ins)| ctx.is_single_use(ins_id) && ins.can_be_reordered())
            .map(|(_blk, ins_id, ins)| (ins_id, ins))
            .collect();

        for (_, block) in block_group.iter() {
            ctx.non_reorderable_candidates = vec![];

            for (var_idx, instruction) in block.iter() {
                let used_vars = match instruction {
                    BasicBlockInstruction::IncrDecr(_, _)
                    | BasicBlockInstruction::IncrDecrPostfix(_, _) => vec![],
                    BasicBlockInstruction::Write(LHS::Local(_), written_val) => {
                        vec![*written_val]
                    }
                    _ => instruction.used_vars(),
                };

                // In reverse, check if we can inline an argument in here.
                mark_deps(&mut ctx, used_vars);

                if ctx.is_single_use(var_idx) {
                    // We can inline this variable later. Is it reorderable or not?
                    if !instruction.can_be_reordered() {
                        ctx.non_reorderable_candidates.push((var_idx, instruction));
                    }
                }
            }

            mark_deps(&mut ctx, block.exit.used_vars());
        }
    }

    ctx.inlined_variables
}

fn mark_deps(ctx: &mut GetInlinedVariablesCtx<'_>, dependencies: Vec<usize>) {
    for used_var in dependencies.into_iter().rev() {
        if let Some(candidate) = ctx.pop_reorderable_candidate(used_var) {
            if ctx.force_mark_inlineable(used_var, candidate) {
                continue;
            } else {
                break; // inlining a phi node? Nah let's leave
            }
        }

        if let Some(&(cand_idx, cand)) = ctx.non_reorderable_candidates.last() {
            if cand_idx == used_var && ctx.force_mark_inlineable(cand_idx, cand) {
                ctx.non_reorderable_candidates.pop();
                continue;
            }
        }
    }
}

pub struct GetInlinedVariablesCtx<'blkmod> {
    pub pure_candidates: BTreeMap<usize, &'blkmod BasicBlockInstruction>,
    pub non_reorderable_candidates: Vec<(usize, &'blkmod BasicBlockInstruction)>,
    /// from count_variable_uses()
    pub variable_use_count: &'blkmod BTreeMap<usize, u32>,
    pub variable_write_count: &'blkmod BTreeMap<usize, u32>,
    /// Our final result
    pub inlined_variables: BTreeMap<usize, BasicBlockInstruction>,
}

impl<'blkmod> GetInlinedVariablesCtx<'blkmod> {
    fn new(variable_write_count: &'blkmod BTreeMap<usize, u32>, variable_use_count: &'blkmod BTreeMap<usize, u32>) -> Self {
        Self {
            pure_candidates: Default::default(),
            non_reorderable_candidates: Default::default(),
            variable_write_count,
            variable_use_count,
            inlined_variables: Default::default(),
        }
    }

    fn is_single_use(&self, var_idx: usize) -> bool {
        self.variable_use_count.get(&var_idx) == Some(&1) && self.variable_write_count.get(&var_idx) == Some(&1)
    }

    fn pop_reorderable_candidate(
        &mut self,
        var_idx: usize,
    ) -> Option<&'blkmod BasicBlockInstruction> {
        self.pure_candidates.remove(&var_idx)
    }

    fn force_mark_inlineable(&mut self, index: usize, ins: &BasicBlockInstruction) -> bool {
        if matches!(
            ins,
            BasicBlockInstruction::ArgumentRead(_) | BasicBlockInstruction::ArgumentRest(_)
        ) {
            return false;
        }

        // Can inline here!
        self.inlined_variables.insert(index, ins.clone(/* TODO */));
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{analyze::{count_variable_uses, count_variable_writes}, testutils::*};

    fn test_inlined_vars(module: &BasicBlockModule) -> BTreeMap<usize, BasicBlockInstruction> {
        get_inlined_variables(module, &count_variable_writes(module), &count_variable_uses(module))
    }

    #[test]
    fn test_inline_vars() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                $1 = 2
                exit = jump @1
            }
            @1: {
                $2 = $0 + $1
                $3 = 3
                $4 = $1 + $3
                exit = return $2
            }
            ",
        ]);

        let inlined_vars = test_inlined_vars(&module);

        insta::assert_debug_snapshot!(inlined_vars, @r###"
        {
            0: 1,
            2: $0 + $1,
            3: 3,
        }
        "###);
    }

    #[test]
    fn test_adjacent_mutation() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                $1 = call $0()
                $2 = $1
                exit = return $2
            }",
        ]);

        let inlined_vars = test_inlined_vars(&module);

        insta::assert_debug_snapshot!(inlined_vars, @r###"
        {
            0: 1,
            1: call $0(),
            2: $1,
        }
        "###);
    }

    #[test]
    fn test_adjacent_mutation_2() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                $1 = write_non_local $$99 $0
                $2 = write_non_local $$99 $1
                exit = return $2
            }",
        ]);

        let inlined_vars = test_inlined_vars(&module);

        insta::assert_debug_snapshot!(inlined_vars, @r###"
        {
            0: 1,
            1: write_non_local $$99 $0,
            2: write_non_local $$99 $1,
        }
        "###);
    }

    #[test]
    fn test_adjacent_mutation_3() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                $1 = 4
                $2 = write_non_local $$99 $0
                $3 = write_non_local $$99 $1
                $4 = call $2($3)
                exit = return $4
            }",
        ]);

        let inlined_vars = test_inlined_vars(&module);

        insta::assert_debug_snapshot!(inlined_vars, @r###"
        {
            0: 1,
            1: 4,
            2: write_non_local $$99 $0,
            3: write_non_local $$99 $1,
            4: call $2($3),
        }
        "###);
    }

    #[test]
    fn test_adjacent_mutation_4() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                $1 = 2
                $2 = write_non_local $$99 $1
                $3 = 4
                $4 = write_non_local $$99 $0
                $5 = write_non_local $$99 $3
                $6 = call $4($5)
                exit = return $6
            }",
        ]);

        let inlined_vars = test_inlined_vars(&module);

        insta::assert_debug_snapshot!(inlined_vars, @r###"
        {
            0: 1,
            1: 2,
            3: 4,
            4: write_non_local $$99 $0,
            5: write_non_local $$99 $3,
            6: call $4($5),
        }
        "###);
    }

    #[test]
    fn test_multifunc() {
        let module = parse_instructions_module(vec![
            "@0: {
                $8 = FunctionId(1)
                $11 = FunctionId(3)
                $13 = call $11()
                $15 = call $8()
                $16 = $13 + $15
                exit = return $16
            }",
        ]);

        let inlined_vars = test_inlined_vars(&module);

        insta::assert_debug_snapshot!(inlined_vars, @r###"
        {
            8: FunctionId(1),
            11: FunctionId(3),
            13: call $11(),
            15: call $8(),
            16: $13 + $15,
        }
        "###);
    }

    #[test]
    fn test_multifunc_out_of_order() {
        let module = parse_instructions_module(vec![
            "@0: {
                $8 = FunctionId(1)
                $11 = FunctionId(3)
                $13 = call $11()
                $15 = call $8()
                $16 = $15 + $13
                exit = return $16
            }",
        ]);

        let inlined_vars = test_inlined_vars(&module);

        insta::assert_debug_snapshot!(inlined_vars, @r###"
        {
            8: FunctionId(1),
            11: FunctionId(3),
            15: call $8(),
            16: $15 + $13,
        }
        "###);
    }

    #[test]
    fn test_phied() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = 10
                $1 = 123
                exit = jump @1
            }
            @1: {
                exit = cond $1 ? @2..@4 : @4..@6
            }
            @2: {
                $2 = 456
                exit = jump @3
            }
            @3: {
                exit = jump @6
            }
            @4: {
                $3 = 789
                exit = jump @5
            }
            @5: {
                exit = jump @6
            }
            @6: {
                $4 = either($2, $3)
                $5 = $4
                $6 = 1
                $7 = $5 + $6
                $8 = undefined
                exit = return $8
            }",
        ]);

        let inlined_vars = test_inlined_vars(&module);

        insta::assert_debug_snapshot!(inlined_vars, @r###"
        {
            1: 123,
            5: $4,
            6: 1,
            8: undefined,
        }
        "###);
    }
}
