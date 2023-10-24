use std::collections::{BTreeMap, BTreeSet};

use crate::{
    basic_blocks::{
        BasicBlock, BasicBlockGroup, BasicBlockInstruction, BasicBlockModule, StructuredFlow,
    },
    block_ops::{block_group_to_structured_flow, normalize_basic_blocks_tree, normalize_varnames},
};

pub fn generate_phi_nodes(module: &mut BasicBlockModule) {
    let unique_name = module
        .iter_all_instructions()
        .map(|(_, _, varname, _)| varname)
        .max()
        .unwrap_or(0)
        + 1;

    let mut ctx = PhiGenerationCtx::new(unique_name);

    for (_func_id, block_group) in module.iter_mut() {
        ctx.enter_conditional();

        let taken_blocks = std::mem::take(&mut block_group.blocks);

        let as_recursive = block_group_to_structured_flow(taken_blocks);
        let as_recursive = generate_phi_nodes_inner(&mut ctx, vec![as_recursive]);

        ctx.leave_conditional();
        assert_eq!(ctx.conditionals.len(), 1);

        block_group.blocks = normalize_basic_blocks_tree(as_recursive);
    }

    normalize_varnames(module);
}

struct PhiGenerationCtx {
    unique_name: usize,
    conditionals: Vec<BTreeMap<usize, Vec<usize>>>,
}
impl PhiGenerationCtx {
    fn new(unique_name: usize) -> Self {
        Self {
            unique_name,
            conditionals: vec![BTreeMap::new()],
        }
    }
    fn make_name(&mut self) -> usize {
        let name = self.unique_name;
        self.unique_name += 1;
        name
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

    fn insert_phi_nodes(
        &mut self,
        phi_instructions: Vec<(usize, BasicBlockInstruction)>,
    ) -> Option<StructuredFlow> {
        if phi_instructions.len() > 0 {
            Some(StructuredFlow::BasicBlock(phi_instructions))
        } else {
            None
        }
    }

    fn leave_conditional(&mut self) -> Option<StructuredFlow> {
        let to_phi = self
            .conditionals
            .pop()
            .expect("unbalanced conditional")
            .into_iter()
            .filter(|(_, v)| v.len() > 1);

        let mut phi_instructions = vec![];

        for (varname, phies) in to_phi {
            let phi = BasicBlockInstruction::Phi(phies);

            let name = self.make_name();
            phi_instructions.push((name, phi));
            self.write_name(varname, name);
        }

        self.insert_phi_nodes(phi_instructions)
    }
}

fn generate_phi_nodes_inner(
    ctx: &mut PhiGenerationCtx,
    as_recursive: Vec<StructuredFlow>,
) -> Vec<StructuredFlow> {
    let mut out_recursive = vec![];

    for item in as_recursive {
        match item {
            StructuredFlow::BasicBlock(mut instructions) => {
                for (varname, ins) in instructions.iter_mut() {
                    for used_var in ins.used_vars_mut() {
                        *used_var = ctx.read_name(*used_var);
                    }

                    let new_varname = ctx.make_name();
                    ctx.write_name(*varname, new_varname);
                    *varname = new_varname;
                }

                out_recursive.push(StructuredFlow::BasicBlock(instructions));
            }
            StructuredFlow::Block(contents) => {
                out_recursive.extend(generate_phi_nodes_inner(ctx, contents));
            }
            StructuredFlow::Return(exit, exit_val) => {
                out_recursive.push(StructuredFlow::Return(
                    exit,
                    Some(ctx.read_name(exit_val.unwrap())),
                ));
            }
            // Conditional branches
            StructuredFlow::Branch(cond, cond_var, cons, alt) => {
                ctx.enter_conditional();

                let cons = generate_phi_nodes_inner(ctx, cons);
                let alt = generate_phi_nodes_inner(ctx, alt);

                let phi_block = ctx.leave_conditional();

                out_recursive.push(StructuredFlow::Branch(
                    cond,
                    ctx.read_name(cond_var),
                    cons,
                    alt,
                ));
                out_recursive.extend(phi_block.into_iter());
            }
            StructuredFlow::Loop(brk, contents) => {
                let (phi_block, contents) = generate_phi_nodes_loops(ctx, contents);

                out_recursive.push(StructuredFlow::Loop(brk, contents));
                out_recursive.extend(phi_block.into_iter());
            }
            StructuredFlow::ForInOfLoop(brk, looped_var, kind, contents) => {
                // ForInOf will perform a read at the start
                let looped_var = ctx.read_name(looped_var);

                let (phi_block, contents) = generate_phi_nodes_loops(ctx, contents);

                out_recursive.push(StructuredFlow::ForInOfLoop(brk, looped_var, kind, contents));
                out_recursive.extend(phi_block.into_iter());
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
            // Identity - don't need to do anything with these, because they don't read or write vars
            StructuredFlow::Break(_) | StructuredFlow::Continue(_) => out_recursive.push(item),
        }
    }

    out_recursive
}

fn generate_phi_nodes_loops(
    ctx: &mut PhiGenerationCtx,
    mut contents: Vec<StructuredFlow>,
) -> (Option<StructuredFlow>, Vec<StructuredFlow>) {
    // We may re-use variables coming back to the top of the loop
    ctx.enter_conditional();
    let vars_used_and_defined_in_loop = get_loop_reentry_vars(&mut contents);
    let mut loop_top_phis = vec![];
    for canonical_name in vars_used_and_defined_in_loop {
        if let Some(current_name) = ctx.read_name_cond(canonical_name) {
            let new_name = ctx.make_name();
            loop_top_phis.push((canonical_name, current_name, new_name));
            ctx.write_name(canonical_name, new_name);
        }
    }

    let contents = generate_phi_nodes_inner(ctx, contents);

    // Top-phi
    let mut phi_instructions = vec![];
    for (canonical_name, name_before_loop, name_in_loop) in loop_top_phis {
        let phi = BasicBlockInstruction::Phi(vec![name_before_loop, ctx.read_name(canonical_name)]);
        phi_instructions.push((name_in_loop, phi));
    }

    let loop_top_phi = ctx.insert_phi_nodes(phi_instructions);
    let contents = loop_top_phi.into_iter().chain(contents).collect();

    let phi_block = ctx.leave_conditional();

    (phi_block, contents)
}

fn get_loop_reentry_vars(contents: &Vec<StructuredFlow>) -> BTreeSet<usize> {
    let mut vars_defined_in_loop = BTreeSet::new();
    for block in contents.iter().flat_map(|child| child.nested_iter()) {
        if let StructuredFlow::BasicBlock(ins) = block {
            for (varname, _ins) in ins.iter() {
                // This will be a re-entry var if we also see it defined in the loop
                vars_defined_in_loop.insert(*varname);
            }
        }
    }

    let mut loop_vars_used_in_loop = BTreeSet::new();
    for block in contents.iter().flat_map(|child| child.nested_iter()) {
        let mut seen_defs = BTreeSet::new();
        if let StructuredFlow::BasicBlock(ins) = block {
            for (varname, ins) in ins.iter() {
                // Push this into phi unconditionally
                for used_var in ins.used_vars() {
                    if vars_defined_in_loop.contains(&used_var) && !seen_defs.contains(&used_var) {
                        loop_vars_used_in_loop.insert(used_var);
                    }
                }

                seen_defs.insert(*varname);
            }
        } else if let Some(used_var) = block.control_flow_var() {
            if vars_defined_in_loop.contains(&used_var) && !seen_defs.contains(&used_var) {
                loop_vars_used_in_loop.insert(used_var);
            }
        }
    }

    loop_vars_used_in_loop
}

pub fn remove_phi(group: &mut BasicBlockGroup) {
    let mut phies_to_final_name: BTreeMap<usize, usize> = collect_phi(group);

    for (_, block) in group.iter_mut() {
        remove_phi_inner(block, &mut phies_to_final_name);
    }
}

pub fn remove_phi_module(module: &mut BasicBlockModule) {
    for (_, group) in module.iter_mut() {
        remove_phi(group);
    }
}

fn collect_phi(group: &BasicBlockGroup) -> BTreeMap<usize, usize> {
    let mut phies_to_final_name: BTreeMap<usize, usize> = Default::default();
    let mut final_names: BTreeSet<usize> = Default::default();

    for (_, varname, ins) in group.iter_all_instructions() {
        if let BasicBlockInstruction::Phi(alternatives) = ins {
            let indirect_name = alternatives
                .iter()
                .find(|alt_var| final_names.contains(alt_var));

            match indirect_name {
                Some(indirect_name) => {
                    phies_to_final_name.insert(varname, *indirect_name);

                    for phi in alternatives {
                        phies_to_final_name.insert(*phi, *indirect_name);
                        final_names.insert(varname);
                    }
                }
                None => {
                    for phi in alternatives {
                        phies_to_final_name.insert(*phi, varname);
                        final_names.insert(varname);
                    }
                }
            }
        }
    }

    phies_to_final_name
}

fn remove_phi_inner(block: &mut BasicBlock, phies_to_final_name: &mut BTreeMap<usize, usize>) {
    block.instructions.retain(|(_, ins)| match ins {
        BasicBlockInstruction::Phi(_) => false,
        _ => true,
    });

    for (varname, ins) in block.instructions.iter_mut() {
        if let Some(final_name) = phies_to_final_name.get(&varname) {
            *varname = *final_name;
        }

        for used_var in ins.used_vars_mut() {
            if let Some(final_name) = phies_to_final_name.get(used_var) {
                *used_var = *final_name;
            }
        }
    }

    for x in block.exit.used_vars_mut() {
        if let Some(final_name) = phies_to_final_name.get(x) {
            *x = *final_name;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::testutils::*;

    use super::*;

    #[test]
    fn test_generate_phi() {
        let mut module = parse_instructions_module(vec![
            "@0: {
                $0 = 1
                $1 = 1
                exit = cond $0 ? @1..@1 : @2..@2
            }
            @1: {
                $1 = 2
                exit = jump @3
            }
            @2: {
                $1 = 3
                exit = jump @3
            }
            @3: {
                exit = return $1
            }",
        ]);

        generate_phi_nodes(&mut module);

        insta::assert_debug_snapshot!(module.top_level_stats(), @r###"
        @0: {
            $0 = 1
            $1 = 1
            exit = cond $0 ? @1..@1 : @2..@2
        }
        @1: {
            $2 = 2
            exit = jump @3
        }
        @2: {
            $3 = 3
            exit = jump @3
        }
        @3: {
            $4 = either($1, $2, $3)
            exit = return $4
        }
        "###);
    }

    #[test]
    fn test_generate_phi_2() {
        let mut module = parse_instructions_module(vec![
            "@0: {
                $0 = 999
                $1 = 1
                exit = cond $1 ? @1..@1 : @2..@2
            }
            @1: {
                $2 = 2
                $3 = $2
                exit = jump @3
            }
            @2: {
                $4 = 3
                exit = jump @3
            }
            @3: {
                $5 = either($0, $2)
                $6 = either($3, $4)
                $7 = $5
                exit = return $7
            }",
        ]);

        remove_phi_module(&mut module);

        generate_phi_nodes(&mut module);

        insta::assert_debug_snapshot!(module.top_level_stats(), @r###"
        @0: {
            $0 = 999
            $1 = 1
            exit = cond $1 ? @1..@1 : @2..@2
        }
        @1: {
            $2 = 2
            $3 = $2
            exit = jump @3
        }
        @2: {
            $4 = 3
            exit = jump @3
        }
        @3: {
            $5 = either($0, $2)
            $6 = either($3, $4)
            $7 = $5
            exit = return $7
        }
        "###);
    }

    #[test]
    fn test_remove_phi_1() {
        let mut blocks = parse_instructions(
            r###"
            @0: {
                $0 = 777
                $1 = $0
                $2 = either($0, $1)
                exit = return $2
            }
            "###,
        );

        remove_phi(&mut blocks);

        insta::assert_debug_snapshot!(blocks,
        @r###"
        @0: {
            $2 = 777
            $2 = $2
            exit = return $2
        }
        "###
        );
    }

    #[test]
    fn test_remove_phi_2() {
        let mut blocks = parse_instructions(
            r###"
            @0: {
                $0 = 999
                exit = jump @1
            }
            @1: {
                $1 = 1
                exit = cond $1 ? @2..@3 : @3..@4
            }
            @2: {
                $2 = 2
                $3 = $2
                exit = jump @4
            }
            @3: {
                $4 = 3
                exit = jump @4
            }
            @4: {
                $5 = either($0, $2)
                $6 = either($3, $4)
                $7 = $5
                exit = return $7
            }
            "###,
        );

        remove_phi(&mut blocks);

        insta::assert_debug_snapshot!(blocks,
        @r###"
        @0: {
            $5 = 999
            exit = jump @1
        }
        @1: {
            $1 = 1
            exit = cond $1 ? @2..@3 : @3..@4
        }
        @2: {
            $5 = 2
            $6 = $5
            exit = jump @4
        }
        @3: {
            $6 = 3
            exit = jump @4
        }
        @4: {
            $7 = $5
            exit = return $7
        }
        "###
        );
    }

    #[test]
    fn test_remove_phi_3() {
        let mut blocks = parse_instructions(
            r###"
            @0: {
                $0 = 1
                $1 = $0
                $2 = 1
                $3 = $1 == $2
                exit = jump @1
            }
            @1: {
                exit = cond $3 ? @2..@9 : @10..@11
            }
            @2: {
                $4 = $0
                $5 = 1
                $6 = $4 == $5
                exit = jump @3
            }
            @3: {
                exit = cond $6 ? @4..@5 : @6..@7
            }
            @4: {
                $7 = $0
                $8 = 2000
                $9 = $7 + $8
                $10 = $9
                exit = jump @5
            }
            @5: {
                exit = jump @8
            }
            @6: {
                $11 = 3
                $12 = $11
                exit = jump @7
            }
            @7: {
                exit = jump @8
            }
            @8: {
                $13 = either($0, $9, $11)
                $14 = $13
                $15 = 1000
                $16 = $14 + $15
                $17 = $16
                exit = jump @9
            }
            @9: {
                exit = jump @12
            }
            @10: {
                $18 = 3
                $19 = $18
                exit = jump @11
            }
            @11: {
                exit = jump @12
            }
            @12: {
                $20 = either($13, $16, $18)
                $21 = $20
                exit = return $21
            }
            "###,
        );

        remove_phi(&mut blocks);

        insta::assert_debug_snapshot!(blocks,
        @r###"
        @0: {
            $13 = 1
            $1 = $13
            $2 = 1
            $3 = $1 == $2
            exit = jump @1
        }
        @1: {
            exit = cond $3 ? @2..@9 : @10..@11
        }
        @2: {
            $4 = $13
            $5 = 1
            $6 = $4 == $5
            exit = jump @3
        }
        @3: {
            exit = cond $6 ? @4..@5 : @6..@7
        }
        @4: {
            $7 = $13
            $8 = 2000
            $13 = $7 + $8
            $10 = $13
            exit = jump @5
        }
        @5: {
            exit = jump @8
        }
        @6: {
            $13 = 3
            $12 = $13
            exit = jump @7
        }
        @7: {
            exit = jump @8
        }
        @8: {
            $14 = $13
            $15 = 1000
            $13 = $14 + $15
            $17 = $13
            exit = jump @9
        }
        @9: {
            exit = jump @12
        }
        @10: {
            $13 = 3
            $19 = $13
            exit = jump @11
        }
        @11: {
            exit = jump @12
        }
        @12: {
            $21 = $13
            exit = return $21
        }
        "###
        );
    }

    #[test]
    fn test_redo_phi() {
        let mut blocks = parse_instructions_module(vec![
            r###"
            @0: {
                exit = jump @1
            }
            @1: {
                exit = try @2 catch @4 finally @6 after @7
            }
            @2: {
                $0 = 777
                exit = jump @3
            }
            @3: {
                exit = error ? jump @4 : jump @5
            }
            @4: {
                $1 = either($0, $2, $3)
                $2 = 888
                exit = jump @5
            }
            @5: {
                exit = finally @6 after @7
            }
            @6: {
                $3 = either($0, $1, $2)
                exit = jump @7
            }
            @7: {
                exit = end finally after @8
            }
            @8: {
                $4 = $3
                exit = return $4
            }
            "###,
        ]);

        remove_phi_module(&mut blocks);
        generate_phi_nodes(&mut blocks);

        insta::assert_debug_snapshot!(blocks.top_level_stats(),
        @r###"
        @0: {
            exit = try @1 catch @2 finally @3 after @3
        }
        @1: {
            $0 = 777
            exit = error ? jump @2 : jump @3
        }
        @2: {
            $1 = 888
            exit = finally @3 after @3
        }
        @3: {
            $2 = either($0, $1)
            exit = end finally after @4
        }
        @4: {
            $3 = $2
            exit = return $3
        }
        "###
        );
    }

    #[test]
    fn test_redo_phi_loop() {
        let mut blocks = parse_instructions_module(vec![
            r###"
            @0: {
                $0 = 0
                exit = loop @1..@1
            }
            @1: {
                $1 = 1
                $2 = $0
                $0 = $2 + $1
                exit = continue @1
            }
            @2: {
                $3 = undefined
                exit = return $3
            }
            "###,
        ]);

        remove_phi_module(&mut blocks);
        generate_phi_nodes(&mut blocks);

        insta::assert_debug_snapshot!(blocks.top_level_stats(),
        @r###"
        @0: {
            $0 = 0
            exit = loop @1..@1
        }
        @1: {
            $1 = either($0, $4)
            $2 = 1
            $3 = $1
            $4 = $3 + $2
            exit = continue @1
        }
        @2: {
            $5 = either($0, $1, $4)
            $6 = undefined
            exit = return $6
        }
        "###
        );
    }

    #[test]
    fn test_redo_phi_loop_multiblock() {
        let mut blocks = parse_instructions_module(vec![
            r###"
            @0: {
                $0 = 0
                exit = loop @1..@2
            }
            @1: {
                $1 = 1
                $2 = $0
                exit = jump @2
            }
            @2: {
                $0 = $2 + $1
                exit = continue @1
            }
            @3: {
                $3 = undefined
                exit = return $3
            }
            "###,
        ]);

        remove_phi_module(&mut blocks);
        generate_phi_nodes(&mut blocks);

        insta::assert_debug_snapshot!(blocks.top_level_stats(),
        @r###"
        @0: {
            $0 = 0
            exit = loop @1..@1
        }
        @1: {
            $1 = either($0, $4)
            $2 = 1
            $3 = $1
            $4 = $3 + $2
            exit = continue @1
        }
        @2: {
            $5 = either($0, $1, $4)
            $6 = undefined
            exit = return $6
        }
        "###
        );
    }
}
