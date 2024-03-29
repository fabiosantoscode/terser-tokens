use std::collections::{BTreeMap, BTreeSet};

use crate::basic_blocks::{Instruction, StructuredClassMember, StructuredFlow, StructuredModule};

pub fn generate_phi_nodes(module: &mut StructuredModule) {
    let mut ctx = PhiGenerationCtx::new();

    for (_func_id, block_group) in module.iter_mut() {
        ctx.enter_conditional();

        let blocks = std::mem::take(&mut block_group.blocks);
        let blocks = generate_phi_nodes_inner(&mut ctx, blocks);

        ctx.leave_conditional();
        assert_eq!(ctx.conditionals.len(), 1);

        block_group.blocks = blocks;
    }
}

struct PhiGenerationCtx {
    conditionals: Vec<BTreeMap<usize, Vec<usize>>>,
    created_names: BTreeSet<usize>,
}
impl PhiGenerationCtx {
    fn new() -> Self {
        Self {
            conditionals: vec![BTreeMap::new()],
            created_names: BTreeSet::new(),
        }
    }
    fn make_name(&mut self, mut orig: usize) -> usize {
        while !self.created_names.insert(orig) {
            orig += 1;
        }
        orig
    }
    fn read_name_cond(&mut self, varname: usize) -> Option<usize> {
        self.conditionals
            .iter()
            .rev()
            .find_map(|cond| cond.get(&varname))
            .map(|phis| phis.last().unwrap().clone())
    }
    fn read_name(&mut self, varname: usize) -> usize {
        self.read_name_cond(varname)
            .unwrap_or_else(|| panic!("variable ${varname} is yet to defined"))
    }
    fn write_name(&mut self, varname: usize, value: usize) {
        self.conditionals
            .last_mut()
            .unwrap()
            .entry(varname)
            .or_insert_with(|| vec![])
            .push(value);
    }
    fn enter_conditional(&mut self) {
        self.conditionals.push(match self.conditionals.last() {
            Some(cond) => cond.clone(),
            _ => BTreeMap::new(),
        });
    }

    fn leave_conditional(&mut self) -> Vec<StructuredFlow> {
        let to_phi = self
            .conditionals
            .pop()
            .expect("unbalanced conditional")
            .into_iter()
            .filter(|(_, v)| v.len() > 1);

        let mut phi_instructions = vec![];

        for (varname, phies) in to_phi {
            let phi = Instruction::Phi(phies);

            let name = self.make_name(varname);
            phi_instructions.push(StructuredFlow::Instruction(name, phi));
            self.write_name(varname, name);
        }

        phi_instructions
    }
}

fn generate_phi_nodes_inner(
    ctx: &mut PhiGenerationCtx,
    as_recursive: Vec<StructuredFlow>,
) -> Vec<StructuredFlow> {
    let mut out_recursive = vec![];

    for item in as_recursive {
        match item {
            StructuredFlow::Instruction(varname, mut ins) => {
                for used_var in ins.used_vars_mut() {
                    *used_var = ctx.read_name(*used_var);
                }

                let new_varname = ctx.make_name(varname);
                ctx.write_name(varname, new_varname);

                out_recursive.push(StructuredFlow::Instruction(new_varname, ins));
            }
            StructuredFlow::Block(brk, contents) => {
                out_recursive.push(StructuredFlow::Block(
                    brk,
                    generate_phi_nodes_inner(ctx, contents),
                ));
            }
            StructuredFlow::Return(exit, exit_val) => {
                out_recursive.push(StructuredFlow::Return(exit, ctx.read_name(exit_val)));
            }
            // Conditional branches
            StructuredFlow::Cond(brk, cond_var, cons, alt) => {
                let cond_var = ctx.read_name(cond_var);

                ctx.enter_conditional();

                let cons = generate_phi_nodes_inner(ctx, cons);
                let alt = generate_phi_nodes_inner(ctx, alt);

                let phi_block = ctx.leave_conditional();

                out_recursive.push(StructuredFlow::Cond(brk, cond_var, cons, alt));
                out_recursive.extend(phi_block.into_iter());
            }
            StructuredFlow::LogicalCond(kind, left, cond_on, right, then_take) => {
                let left = generate_phi_nodes_inner(ctx, left);
                let cond_on = ctx.read_name(cond_on);

                ctx.enter_conditional();

                let right = generate_phi_nodes_inner(ctx, right);
                let then_take = ctx.read_name(then_take);

                let phi_block = ctx.leave_conditional();

                out_recursive.push(StructuredFlow::LogicalCond(
                    kind, left, cond_on, right, then_take,
                ));
                out_recursive.extend(phi_block.into_iter());
            }
            StructuredFlow::Switch(brk, expression, mut cases) => {
                let expression = ctx.read_name(expression);

                let mut conditionals_to_pop = 1;
                ctx.enter_conditional();

                for case in cases.iter_mut() {
                    conditionals_to_pop += 1;
                    ctx.enter_conditional();

                    if let Some((exprs, varname)) = &mut case.condition {
                        *exprs = generate_phi_nodes_inner(ctx, std::mem::take(exprs));
                        *varname = ctx.read_name(*varname);
                    }
                    case.body = generate_phi_nodes_inner(ctx, std::mem::take(&mut case.body));
                }

                let phi_block = (0..conditionals_to_pop)
                    .flat_map(|_| ctx.leave_conditional())
                    .collect::<Vec<_>>();

                out_recursive.push(StructuredFlow::Switch(brk, expression, cases));
                out_recursive.extend(phi_block);
            }
            StructuredFlow::Loop(brk, contents) => {
                let (phi_block, contents) = generate_phi_nodes_loops(ctx, contents);

                out_recursive.push(StructuredFlow::Loop(brk, contents));
                out_recursive.extend(phi_block);
            }
            StructuredFlow::ForInOfLoop(brk, looped_var, kind, contents) => {
                // ForInOf will perform a read at the start
                let looped_var = ctx.read_name(looped_var);

                let (phi_block, contents) = generate_phi_nodes_loops(ctx, contents);

                out_recursive.push(StructuredFlow::ForInOfLoop(brk, looped_var, kind, contents));
                out_recursive.extend(phi_block);
            }
            StructuredFlow::TryCatch(brk, body, catch, fin) => {
                let body = generate_phi_nodes_inner(ctx, body);

                ctx.enter_conditional();
                let catch = generate_phi_nodes_inner(ctx, catch);
                let phi_block = ctx.leave_conditional();

                let fin = generate_phi_nodes_inner(ctx, fin);
                let fin = phi_block.into_iter().chain(fin).collect();

                out_recursive.push(StructuredFlow::TryCatch(brk, body, catch, fin));
            }
            StructuredFlow::Class(class_var, members) => {
                let class_var = ctx.read_name(class_var);

                let members = members
                    .into_iter()
                    .map(|member| match member {
                        StructuredClassMember::Property(code, mut prop) => {
                            let code = generate_phi_nodes_inner(ctx, code);
                            for used in prop.used_vars_mut() {
                                *used = ctx.read_name(*used);
                            }
                            StructuredClassMember::Property(code, prop)
                        }
                        StructuredClassMember::Constructor(fn_id) => {
                            StructuredClassMember::Constructor(fn_id)
                        }
                        StructuredClassMember::StaticBlock(contents) => {
                            let contents = generate_phi_nodes_inner(ctx, contents);
                            StructuredClassMember::StaticBlock(contents)
                        }
                    })
                    .collect();

                out_recursive.push(StructuredFlow::Class(class_var, members));
            }
            // Identity - don't need to do anything with these, because they don't read or write vars
            StructuredFlow::Debugger | StructuredFlow::Break(_) | StructuredFlow::Continue(_) => {
                out_recursive.push(item)
            }
        }
    }

    out_recursive
}

fn generate_phi_nodes_loops(
    ctx: &mut PhiGenerationCtx,
    mut contents: Vec<StructuredFlow>,
) -> (Vec<StructuredFlow>, Vec<StructuredFlow>) {
    // We may re-use variables coming back to the top of the loop
    ctx.enter_conditional();
    let vars_used_and_defined_in_loop = get_loop_reentry_vars(&mut contents);
    let mut loop_top_phis = vec![];
    for canonical_name in vars_used_and_defined_in_loop {
        if let Some(current_name) = ctx.read_name_cond(canonical_name) {
            let new_name = ctx.make_name(canonical_name);
            loop_top_phis.push((canonical_name, current_name, new_name));
            ctx.write_name(canonical_name, new_name);
        }
    }

    let contents = generate_phi_nodes_inner(ctx, contents);

    // Top-phi
    let mut loop_top_phi = vec![];
    for (canonical_name, name_before_loop, name_in_loop) in loop_top_phis {
        let phi = Instruction::Phi(vec![name_before_loop, ctx.read_name(canonical_name)]);
        loop_top_phi.push(StructuredFlow::Instruction(name_in_loop, phi));
    }

    let contents = loop_top_phi.into_iter().chain(contents).collect();

    let phi_block = ctx.leave_conditional();

    (phi_block, contents)
}

fn get_loop_reentry_vars(contents: &Vec<StructuredFlow>) -> BTreeSet<usize> {
    let mut vars_defined_in_loop = BTreeSet::new();
    for block in contents.iter().flat_map(|child| child.nested_iter()) {
        if let StructuredFlow::Instruction(varname, _ins) = block {
            // This will be a re-entry var if we also see it defined in the loop
            vars_defined_in_loop.insert(*varname);
        }
    }

    let mut loop_vars_used_in_loop = BTreeSet::new();
    for block in contents.iter().flat_map(|child| child.nested_iter()) {
        let mut seen_defs = BTreeSet::new();
        if let StructuredFlow::Instruction(varname, ins) = block {
            // Push this into phi unconditionally
            for used_var in ins.used_vars() {
                if vars_defined_in_loop.contains(&used_var) && !seen_defs.contains(&used_var) {
                    loop_vars_used_in_loop.insert(used_var);
                }
            }

            seen_defs.insert(*varname);
        }

        for used_var in block.used_vars() {
            if vars_defined_in_loop.contains(&used_var) && !seen_defs.contains(&used_var) {
                loop_vars_used_in_loop.insert(used_var);
            }
        }
    }

    loop_vars_used_in_loop
}

pub fn remove_phi_module(module: &mut StructuredModule) {
    for (_, flow) in module.iter_mut() {
        let phies_to_final_name: BTreeMap<usize, usize> = flow
            .blocks
            .iter()
            .flat_map(|block| collect_phi(block))
            .collect();
        for flow in flow.blocks.iter_mut() {
            remove_phi_recursive(flow, &phies_to_final_name);
        }
    }
}

#[cfg(test)]
fn remove_phi(group: &mut StructuredFlow) {
    let phies_to_final_name: BTreeMap<usize, usize> = collect_phi(group);

    remove_phi_recursive(group, &phies_to_final_name);
}

fn collect_phi(group: &StructuredFlow) -> BTreeMap<usize, usize> {
    // Create pools (sets) of variables that have a phi connection between them
    // and merge them as we go.
    // Each of these pools will be mapped into a single variable, and all phis
    // replaced with the mapped variable

    let mut related_pools_of_phis: Vec<BTreeSet<usize>> = vec![];
    for (varname, ins) in group.iter_all_instructions() {
        let (from, to) = match ins {
            Instruction::Phi(alternatives) => (varname, alternatives),
            _ => continue,
        };
        // If either "from" or "to" are in any pool, add everything to that pool
        // However, if they are in different pools, merge the pools

        let pools = related_pools_of_phis
            .iter()
            .enumerate()
            .flat_map(|(idx, pool)| {
                if pool.contains(&from) || to.iter().any(|to| pool.contains(to)) {
                    Some(idx)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        match &pools[..] {
            [] => {
                let mut new_pool = BTreeSet::from_iter(to.iter().copied());
                new_pool.insert(from);
                related_pools_of_phis.push(new_pool);
            }
            [one_pool] => {
                let pool = related_pools_of_phis.get_mut(*one_pool).unwrap();
                pool.insert(from);
                pool.extend(to.iter().copied());
            }
            many_pools => {
                let mut merged = many_pools
                    .iter()
                    .rev()
                    .flat_map(|idx| related_pools_of_phis.remove(*idx))
                    .collect::<BTreeSet<_>>();

                merged.insert(from);
                merged.extend(to.iter().copied());

                related_pools_of_phis.push(merged);
            }
        }
    }

    // Finally, take any element out of each pool and make it the "final" name
    // All the other elements in the pool will be remapped to this final name
    let phies_to_final_name = related_pools_of_phis
        .into_iter()
        .flat_map(|pool| {
            let mut pool = pool.into_iter();
            if let Some(first) = pool.next() {
                Some(pool.map(move |x| (x, first)))
            } else {
                None
            }
        })
        .flatten()
        .collect();

    phies_to_final_name
}

fn remove_phi_recursive(flow: &mut StructuredFlow, phies_to_final_name: &BTreeMap<usize, usize>) {
    flow.retain_blocks_mut(&mut |child| match child {
        StructuredFlow::Instruction(varname, ins) => {
            if let Some(final_name) = phies_to_final_name.get(&varname) {
                *varname = *final_name;
            }

            for used_var in ins.used_vars_mut() {
                if let Some(final_name) = phies_to_final_name.get(used_var) {
                    *used_var = *final_name;
                }
            }

            match ins {
                Instruction::Phi(_) => false,
                Instruction::Ref(itself) if itself == varname => false,
                _ => true,
            }
        }
        child => {
            for var in child.used_vars_mut() {
                if let Some(final_name) = phies_to_final_name.get(var) {
                    *var = *final_name;
                }
            }

            true
        }
    });
}

#[cfg(test)]
mod tests {
    use crate::testutils::*;

    use super::*;

    #[test]
    fn test_generate_phi() {
        let mut module = parse_test_module(vec![
            "{
                $0 = 1
                $1 = 1
                if ($0) {
                    $1 = 2
                } else {
                    $1 = 3
                }
                Return $1
            }",
        ]);

        generate_phi_nodes(&mut module);

        insta::assert_debug_snapshot!(module.top_level_stats(), @r###"
        {
            $0 = 1
            $1 = 1
            if ($0) {
                $2 = 2
            } else {
                $3 = 3
            }
            $4 = either($1, $2, $3)
            Return $4
        }
        "###);
    }

    #[test]
    fn test_generate_phi_2() {
        let mut module = parse_test_module(vec![
            "{
                $0 = 999
                $1 = 1
                if ($1) {
                    $2 = 2
                    $3 = $2
                } else {
                    $4 = 3
                }
                $5 = either($0, $2)
                $6 = either($3, $4)
                $7 = $5
                Return $7
            }",
        ]);

        remove_phi_module(&mut module);

        generate_phi_nodes(&mut module);

        insta::assert_debug_snapshot!(module.top_level_stats(), @r###"
        {
            $0 = 999
            $1 = 1
            if ($1) {
                $2 = 2
                $3 = $2
            } else {
                $4 = 3
            }
            $5 = either($0, $2)
            $6 = either($3, $4)
            $7 = $5
            Return $7
        }
        "###);
    }

    #[test]
    fn test_remove_phi_1() {
        let mut blocks = parse_test_flow(
            r###"
            {
                $0 = 777
                $1 = $0
                $2 = either($0, $1)
                Return $2
            }
            "###,
        );

        remove_phi(&mut blocks);

        insta::assert_debug_snapshot!(blocks,
        @r###"
        {
            $0 = 777
            Return $0
        }
        "###
        );
    }

    #[test]
    fn test_remove_phi_2() {
        let mut blocks = parse_test_flow(
            r###"
            {
                $0 = 999
                $1 = 1
                if ($1) {
                    $2 = 2
                    $3 = $2
                } else {
                    $4 = 3
                }
                $5 = either($0, $2)
                $6 = either($3, $4)
                $7 = $5
                Return $7
            }
            "###,
        );

        remove_phi(&mut blocks);

        insta::assert_debug_snapshot!(blocks,
        @r###"
        {
            $0 = 999
            $1 = 1
            if ($1) {
                $0 = 2
                $3 = $0
            } else {
                $3 = 3
            }
            $7 = $0
            Return $7
        }
        "###
        );
    }

    #[test]
    fn test_remove_phi_3() {
        let mut blocks = parse_test_module(vec![
            r###"
            {
                $0 = 1
                $1 = $0
                $2 = 1
                $3 = $1 == $2
                if ($3) {
                    $4 = $0
                    $5 = 1
                    $6 = $4 == $5
                    if ($6) {
                        $7 = $0
                        $8 = 2000
                        $9 = $7 + $8
                        $10 = $9
                    } else {
                        $11 = 3
                        $12 = $11
                    }
                    $13 = either($0, $9, $11)
                    $14 = $13
                    $15 = 1000
                    $16 = $14 + $15
                    $17 = $16
                } else {
                    $18 = 3
                    $19 = $18
                }
                $20 = either($13, $16, $18)
                $21 = $20
                Return $21
            }
            "###,
        ]);

        remove_phi_module(&mut blocks);

        insta::assert_debug_snapshot!(blocks.take_top_level_stats(),
        @r###"
        {
            $0 = 1
            $1 = $0
            $2 = 1
            $3 = $1 == $2
            if ($3) {
                $4 = $0
                $5 = 1
                $6 = $4 == $5
                if ($6) {
                    $7 = $0
                    $8 = 2000
                    $0 = $7 + $8
                    $10 = $0
                } else {
                    $0 = 3
                    $12 = $0
                }
                $14 = $0
                $15 = 1000
                $0 = $14 + $15
                $17 = $0
            } else {
                $0 = 3
                $19 = $0
            }
            $21 = $0
            Return $21
        }
        "###
        );
    }

    #[test]
    fn test_redo_phi() {
        let mut blocks = parse_test_module(vec![
            r###"
            {
                try {
                    $0 = 777
                } catch {
                    $1 = either($0, $2, $3)
                    $2 = 888
                } finally {
                    $3 = either($0, $1, $2)
                }
                $4 = $3
                Return $4
            }
            "###,
        ]);

        remove_phi_module(&mut blocks);
        generate_phi_nodes(&mut blocks);

        insta::assert_debug_snapshot!(blocks.top_level_stats(),
        @r###"
        {
            try {
                $0 = 777
            } catch {
                $1 = 888
            } finally {
                $2 = either($0, $1)
            }
            $4 = $2
            Return $4
        }
        "###
        );
    }

    #[test]
    fn test_redo_phi_loop() {
        let mut blocks = parse_test_module(vec![
            r###"
            {
                $0 = 0
                loop (@1) {
                    $1 = 1
                    $2 = $0
                    $0 = $2 + $1
                    Continue (@1)
                }
                $3 = undefined
                Return $3
            }
            "###,
        ]);

        remove_phi_module(&mut blocks);
        generate_phi_nodes(&mut blocks);

        insta::assert_debug_snapshot!(blocks.top_level_stats(),
        @r###"
        {
            $0 = 0
            loop (@1) {
                $1 = either($0, $4)
                $2 = 1
                $3 = $1
                $4 = $3 + $2
                Continue (@1)
            }
            $5 = either($0, $1, $4)
            $6 = undefined
            Return $6
        }
        "###
        );
    }

    #[test]
    fn test_redo_phi_loop_multiblock() {
        let mut blocks = parse_test_module(vec![
            r###"
            {
                $0 = 0
                loop (@1) {
                    $1 = 1
                    $2 = $0
                    $0 = $2 + $1
                    Continue (@1)
                }
                $3 = undefined
                Return $3
            }
            "###,
        ]);

        remove_phi_module(&mut blocks);
        generate_phi_nodes(&mut blocks);

        insta::assert_debug_snapshot!(blocks.top_level_stats(),
        @r###"
        {
            $0 = 0
            loop (@1) {
                $1 = either($0, $4)
                $2 = 1
                $3 = $1
                $4 = $3 + $2
                Continue (@1)
            }
            $5 = either($0, $1, $4)
            $6 = undefined
            Return $6
        }
        "###
        );
    }
}
