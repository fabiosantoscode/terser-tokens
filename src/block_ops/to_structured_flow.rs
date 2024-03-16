use std::{collections::BTreeMap, fmt::Debug};

use crate::basic_blocks::{
    BasicBlock, BasicBlockExit, BreakableId, StructuredClassMember, StructuredFlow,
    StructuredSwitchCase,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct BreakAndRange(pub BreakableId, pub usize, pub usize);

struct Ctx {
    containing_syntax: Vec<BreakAndRange>,
    last_breakable_id: usize,
}

impl Ctx {
    pub fn new() -> Self {
        Ctx {
            containing_syntax: Default::default(),
            last_breakable_id: 1,
        }
    }

    pub fn push_within<T, Fnc>(&mut self, syn: BreakAndRange, func: Fnc) -> T
    where
        Fnc: FnOnce(&mut Ctx) -> T,
    {
        self.containing_syntax.push(syn);
        let ret = func(self);
        self.containing_syntax.pop();
        ret
    }

    /// Given a break target, find the enclosing loop or labelled block
    pub fn break_index(&self, target: usize) -> BreakableId {
        *self
            .containing_syntax
            .iter()
            .find_map(
                |BreakAndRange(item, _start, end)| {
                    if end + 1 == target {
                        Some(item)
                    } else {
                        None
                    }
                },
            )
            .expect(&format!("break @{target} without matching to a container"))
    }

    /// Given a continue target, find the enclosing loop
    pub fn continue_index(&self, target: usize) -> BreakableId {
        *self
            .containing_syntax
            .iter()
            .find_map(
                |BreakAndRange(item, start, _end)| {
                    if *start == target {
                        Some(item)
                    } else {
                        None
                    }
                },
            )
            .expect(&format!(
                "continue @{target} without matching to a container"
            ))
    }

    pub fn get_breakable_id(&mut self) -> BreakableId {
        self.last_breakable_id += 1;
        BreakableId(Some(self.last_breakable_id))
    }
}

pub fn block_group_to_structured_flow(mut func: BTreeMap<usize, BasicBlock>) -> StructuredFlow {
    let mut ctx = Ctx::new();
    let (first, last) = (
        *(func.first_key_value().expect("no blocks").0),
        *(func.last_key_value().expect("no blocks").0),
    );
    let tree = StructuredFlow::Block(do_tree_chunk(&mut ctx, &mut func, first, last));

    tree.simplify()
}

fn do_tree_chunk(
    ctx: &mut Ctx,
    func: &mut BTreeMap<usize, BasicBlock>,
    start_blk: usize,
    end_blk: usize,
) -> Vec<StructuredFlow> {
    if end_blk < start_blk {
        return vec![];
    }

    let first_block = func.range_mut(start_blk..=end_blk).into_iter().next();

    match first_block {
        Some((blk_id, block)) => {
            let mut rest = blk_id + 1;

            let mut blocks = vec![StructuredFlow::BasicBlock(std::mem::take(
                &mut block.instructions,
            ))];

            match block.exit {
                BasicBlockExit::Jump(to) => rest = to,
                BasicBlockExit::Debugger(to) => {
                    rest = to;
                    blocks.push(StructuredFlow::Debugger)
                }
                BasicBlockExit::Break(tgt) => {
                    blocks.push(StructuredFlow::Break(ctx.break_index(tgt)))
                }
                BasicBlockExit::Continue(tgt) => {
                    blocks.push(StructuredFlow::Continue(ctx.continue_index(tgt)))
                }
                BasicBlockExit::Cond(cond, cons_start, cons_end, alt_start, alt_end) => {
                    rest = alt_end + 1;

                    let brk_id = ctx.get_breakable_id();
                    ctx.push_within(BreakAndRange(brk_id, cons_start, alt_end), |ctx| {
                        blocks.push(StructuredFlow::Branch(
                            brk_id,
                            cond,
                            do_tree_chunk(ctx, func, cons_start, cons_end),
                            do_tree_chunk(ctx, func, alt_start, alt_end),
                        ))
                    })
                }
                BasicBlockExit::Loop(start, end) => {
                    rest = end + 1;

                    let loop_brk_id = ctx.get_breakable_id();
                    ctx.push_within(BreakAndRange(loop_brk_id, start, end), |ctx| {
                        blocks.push(StructuredFlow::Loop(
                            loop_brk_id,
                            do_tree_chunk(ctx, func, start, end),
                        ))
                    })
                }
                BasicBlockExit::SwitchStart(exp, start, end) => {
                    rest = end + 1;

                    let switch_brk_id = ctx.get_breakable_id();
                    ctx.push_within(BreakAndRange(switch_brk_id, start, end), |ctx| {
                        let switch_contents = do_switch(ctx, func, start, end);
                        blocks.push(StructuredFlow::Switch(switch_brk_id, exp, switch_contents));
                    })
                }
                BasicBlockExit::SwitchCaseExpression(_, _, _)
                | BasicBlockExit::SwitchEnd(_)
                | BasicBlockExit::SwitchCase(_, _, _, _) => {
                    unreachable!("invalid switch case structure")
                }
                BasicBlockExit::ForInOfLoop(looped_var, loop_type, start, end) => {
                    rest = end + 1;

                    let loop_brk_id = ctx.get_breakable_id();
                    ctx.push_within(BreakAndRange(loop_brk_id, start, end), |ctx| {
                        blocks.push(StructuredFlow::ForInOfLoop(
                            loop_brk_id,
                            looped_var,
                            loop_type,
                            do_tree_chunk(ctx, func, start, end),
                        ))
                    })
                }
                BasicBlockExit::ExitFn(ref exit_type, yielded_val) => {
                    blocks.push(StructuredFlow::Return(exit_type.clone(), yielded_val))
                }
                BasicBlockExit::SetTryAndCatch(
                    try_block,
                    catch_block,
                    finally_block,
                    end_finally,
                ) => {
                    rest = end_finally + 1;

                    let brk_id = ctx.get_breakable_id();
                    ctx.push_within(BreakAndRange(brk_id, try_block, end_finally), |ctx| {
                        blocks.push(StructuredFlow::TryCatch(
                            brk_id,
                            do_tree_chunk(ctx, func, try_block, catch_block - 1),
                            do_tree_chunk(ctx, func, catch_block, finally_block - 1),
                            do_tree_chunk(ctx, func, finally_block, end_finally),
                        ))
                    })
                }
                BasicBlockExit::PopCatch(catch, _) => rest = catch,
                BasicBlockExit::PopFinally(finally, _) => rest = finally,
                BasicBlockExit::EndFinally(after) => rest = after,
                BasicBlockExit::ClassStart(class_var, start, end) => {
                    rest = end + 1;

                    blocks.push(StructuredFlow::Class(
                        class_var,
                        do_class_members(ctx, func, start, end, vec![]),
                    ));
                }
                BasicBlockExit::ClassPopStaticBlock(to) => {
                    rest = to;
                }
                BasicBlockExit::ClassProperty(_, _) | BasicBlockExit::ClassConstructor(_, _) => {
                    // Pop back to do_class_members
                    return blocks;
                }
                BasicBlockExit::ClassPushStaticBlock(_, _) | BasicBlockExit::ClassEnd(_) => {
                    assert_eq!(
                        blocks.iter().fold(0, |acc, x| acc + x.count_instructions()),
                        0,
                        "class block should be empty"
                    );
                    return blocks;
                }
            };

            blocks.extend(do_tree_chunk(ctx, func, rest, end_blk));

            blocks
        }
        None => vec![],
    }
}

fn do_class_members(
    ctx: &mut Ctx,
    func: &mut BTreeMap<usize, BasicBlock>,
    start_blk: usize,
    end_blk: usize,
    mut preceding_code: Vec<StructuredFlow>,
) -> Vec<StructuredClassMember> {
    if end_blk < start_blk {
        return vec![];
    }

    let first_member = func.range_mut(start_blk..=end_blk).into_iter().next();

    match first_member {
        Some((_blk_id, block)) => {
            let rest;

            let mut blocks = vec![];

            match block.exit {
                BasicBlockExit::ClassProperty(ref prop, after) => {
                    rest = after;

                    let preceding_code = std::mem::take(&mut preceding_code);

                    blocks.push(StructuredClassMember::Property(
                        preceding_code,
                        prop.clone(),
                    ));
                }
                BasicBlockExit::ClassConstructor(fn_id, after) => {
                    rest = after;

                    blocks.push(StructuredClassMember::Constructor(fn_id));
                }
                BasicBlockExit::ClassPushStaticBlock(start, end) => {
                    rest = end + 1;

                    blocks.push(StructuredClassMember::StaticBlock(do_tree_chunk(
                        ctx, func, start, end,
                    )));
                }
                BasicBlockExit::ClassEnd(to) => {
                    rest = to;
                }
                BasicBlockExit::ClassPopStaticBlock(_) => {
                    unreachable!("pop static block should be handled by do_tree_chunk")
                }
                _ => {
                    let index_of_next_prop =
                        func.range(start_blk..=end_blk)
                            .into_iter()
                            .find_map(|(id, block)| match block.exit {
                                BasicBlockExit::ClassProperty(_, _) => Some(*id),
                                BasicBlockExit::ClassConstructor(_, _) => Some(*id),
                                BasicBlockExit::ClassPushStaticBlock(_, _) => Some(*id),
                                BasicBlockExit::ClassEnd(_) => Some(*id),
                                _ => None,
                            });

                    if let Some(next_prop) = index_of_next_prop {
                        rest = next_prop;

                        preceding_code.extend(do_tree_chunk(ctx, func, start_blk, next_prop - 1));
                    } else {
                        todo!("maybe we found end")
                    }
                }
            };

            blocks.extend(do_class_members(ctx, func, rest, end_blk, preceding_code));

            blocks
        }
        None => vec![],
    }
}

fn do_switch(
    ctx: &mut Ctx,
    func: &mut BTreeMap<usize, BasicBlock>,
    start_blk: usize,
    end_blk: usize,
) -> Vec<StructuredSwitchCase> {
    let mut switch_cases = vec![];

    let mut rest = start_blk;

    loop {
        let block = func.get(&rest).unwrap();

        let mut condition_exp = None;

        let block = match block.exit {
            BasicBlockExit::SwitchCaseExpression(start, end, next) => {
                rest = next;

                condition_exp =
                    Some(do_tree_chunk(ctx, func, start, end)).filter(|x| !x.is_empty());

                func.get(&rest).unwrap()
            }
            BasicBlockExit::SwitchEnd(next) => {
                assert_eq!(rest, end_blk, "invalid switch end");
                assert!(next > rest);
                return switch_cases;
            }
            _ => block,
        };

        if let BasicBlockExit::SwitchCase(exp, start, end, next) = block.exit {
            rest = next;

            match (exp, condition_exp) {
                (Some(exp), condition_exp) => {
                    switch_cases.push(StructuredSwitchCase {
                        condition: Some((condition_exp.unwrap_or_default(), exp)),
                        body: do_tree_chunk(ctx, func, start, end),
                    });
                }
                (None, None) => {
                    switch_cases.push(StructuredSwitchCase {
                        condition: None,
                        body: do_tree_chunk(ctx, func, start, end),
                    });
                }
                _ => panic!("invalid switch case structure"),
            };
        } else {
            panic!("invalid switch case structure. Found: {:?}", block.exit)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testutils::*;

    #[test]
    fn basic_flow() {
        let func = parse_instructions(
            r###"
            @0: {
                $0 = 123
                exit = jump @1
            }
            @1: {
                $1 = 456
                exit = return $1
            }
            "###,
        );

        insta::assert_debug_snapshot!(block_group_to_structured_flow(func.blocks), @r###"
        {
            $0 = 123
            $1 = 456
            Return $1
        }
        "###);
    }

    #[test]
    fn basic_if() {
        let func = parse_instructions(
            r###"
            @0: {
                $0 = 123
                exit = cond $0 ? @1..@1 : @2..@2
            }
            @1: {
                $1 = 456
                exit = jump @3
            }
            @2: {
                $2 = 789
                exit = jump @3
            }
            @3: {
                $3 = either($1, $2)
                $4 = undefined
                exit = return $4
            }
            "###,
        );

        insta::assert_debug_snapshot!(block_group_to_structured_flow(func.blocks), @r###"
        {
            $0 = 123
            if ($0) {
                $1 = 456
            } else {
                $2 = 789
            }
            $3 = either($1, $2)
            $4 = undefined
            Return $4
        }
        "###);
    }

    #[test]
    fn basic_if_2() {
        let func = parse_instructions(
            r###"
            @0: {
                exit = jump @1
            }
            @1: {
                $0 = 123
                exit = cond $0 ? @2..@2 : @3..@3
            }
            @2: {
                $1 = 456
                exit = jump @4
            }
            @3: {
                $2 = 789
                exit = jump @4
            }
            @4: {
                $3 = either($1, $2)
                $4 = undefined
                exit = return $4
            }
        "###,
        );

        insta::assert_debug_snapshot!(block_group_to_structured_flow(func.blocks), @r###"
        {
            $0 = 123
            if ($0) {
                $1 = 456
            } else {
                $2 = 789
            }
            $3 = either($1, $2)
            $4 = undefined
            Return $4
        }
        "###);
    }

    #[test]
    fn basic_while_1() {
        let func = parse_instructions(
            r###"
            @0: {
                $0 = 123
                exit = jump @1
            }
            @1: {
                exit = jump @2
            }
            @2: {
                exit = loop @3..@9
            }
            @3: {
                $1 = 123
                exit = cond $1 ? @4..@8 : @9..@9
            }
            @4: {
                $2 = 456
                exit = jump @5
            }
            @5: {
                exit = jump @6
            }
            @6: {
                exit = jump @7
            }
            @7: {
                exit = jump @8
            }
            @8: {
                exit = continue @3
            }
            @9: {
                exit = break @10
            }
            @10: {
                exit = jump @11
            }
            @11: {
                exit = jump @12
            }
            @12: {
                exit = jump @13
            }
            @13: {
                $3 = undefined
                exit = return $3
            }
            "###,
        );

        insta::assert_debug_snapshot!(block_group_to_structured_flow(func.blocks), @r###"
        {
            $0 = 123
            loop (@2) {
                $1 = 123
                if ($1) {
                    $2 = 456
                    Continue (@2)
                } else {
                
                    Break (@2)
                }
            }
            $3 = undefined
            Return $3
        }
        "###);
    }

    #[test]
    fn basic_while_2() {
        let func = parse_instructions(
            r###"
            @0: {
                exit = jump @1
            }
            @1: {
                exit = loop @2..@15
            }
            @2: {
                $0 = 123
                exit = cond $0 ? @3..@14 : @15..@15
            }
            @3: {
                exit = jump @4
            }
            @4: {
                exit = loop @5..@11
            }
            @5: {
                $1 = 456
                exit = cond $1 ? @6..@10 : @11..@11
            }
            @6: {
                exit = jump @7
            }
            @7: {
                exit = break @16
            }
            @8: {
                exit = jump @9
            }
            @9: {
                exit = jump @10
            }
            @10: {
                exit = continue @5
            }
            @11: {
                exit = break @12
            }
            @12: {
                exit = jump @13
            }
            @13: {
                exit = jump @14
            }
            @14: {
                exit = continue @2
            }
            @15: {
                exit = break @16
            }
            @16: {
                exit = jump @17
            }
            @17: {
                $2 = undefined
                exit = return $2
            }
            "###,
        );

        insta::assert_debug_snapshot!(block_group_to_structured_flow(func.blocks), @r###"
        {

            loop (@2) {
                $0 = 123
                if ($0) {
                
                    loop (@4) {
                        $1 = 456
                        if ($1) {
                        
                            Break (@2)
                        
                            Continue (@4)
                        } else {
                        
                            Break (@4)
                        }
                    }
                
                    Continue (@2)
                } else {
                
                    Break (@2)
                }
            }
            $2 = undefined
            Return $2
        }
        "###);
    }

    #[test]
    fn mk_stats() {
        let func = parse_instructions(
            r###"
            @0: {
                $0 = 123
                $1 = 123
                $2 = 123
                $3 = 123
                exit = jump @1
            }
            @1: {
                $4 = 123
                exit = cond $4 ? @2..@2 : @3..@3
            }
            @2: {
                $5 = 456
                exit = jump @4
            }
            @3: {
                $6 = 789
                exit = jump @4
            }
            @4: {
                $7 = either($5, $6)
                $8 = undefined
                exit = return $8
            }
            "###,
        );

        let stats = block_group_to_structured_flow(func.blocks);
        insta::assert_debug_snapshot!(stats, @r###"
        {
            $0 = 123
            $1 = 123
            $2 = 123
            $3 = 123
            $4 = 123
            if ($4) {
                $5 = 456
            } else {
                $6 = 789
            }
            $7 = either($5, $6)
            $8 = undefined
            Return $8
        }
        "###);
    }

    #[test]
    fn an_array() {
        let func = parse_instructions(
            r###"
            @0: {
                $0 = 1
                $1 = 2
                exit = cond $1 ? @1..@1 : @2..@2
            }
            @1: {
                $2 = 3
                exit = jump @3
            }
            @2: {
                $3 = 4
                exit = jump @3
            }
            @3: {
                $4 = either($2, $3)
                $5 = 3
                $6 = [$0, $4, , ...$5,]
                $7 = undefined
                exit = return $7
            }
            "###,
        );

        let stats = block_group_to_structured_flow(func.blocks);
        insta::assert_debug_snapshot!(stats, @r###"
        {
            $0 = 1
            $1 = 2
            if ($1) {
                $2 = 3
            } else {
                $3 = 4
            }
            $4 = either($2, $3)
            $5 = 3
            $6 = [$0, $4, , ...$5]
            $7 = undefined
            Return $7
        }
        "###);
    }

    #[test]
    fn mk_stats_2() {
        let func = parse_instructions(
            r###"
            @0: {
                $0 = 123
                exit = cond $0 ? @1..@5 : @6..@6
            }
            @1: {
                $1 = 456
                exit = cond $1 ? @2..@2 : @3..@4
            }
            @2: {
                $2 = 7
                exit = jump @4
            }
            @3: {
                $3 = 8
                exit = jump @4
            }
            @4: {
                exit = jump @5
            }
            @5: {
                $4 = either($2, $3)
                exit = jump @7
            }
            @6: {
                $5 = 9
                exit = jump @7
            }
            @7: {
                $6 = either($4, $5)
                $7 = undefined
                exit = return $7
            }
            "###,
        );

        let stats = block_group_to_structured_flow(func.blocks);
        insta::assert_debug_snapshot!(stats, @r###"
        {
            $0 = 123
            if ($0) {
                $1 = 456
                if ($1) {
                    $2 = 7
                } else {
                    $3 = 8
                }
                $4 = either($2, $3)
            } else {
                $5 = 9
            }
            $6 = either($4, $5)
            $7 = undefined
            Return $7
        }
        "###);
    }

    #[test]
    fn mk_stats_3() {
        let func = parse_instructions(
            r###"
            @0: {
                $0 = 123
                exit = jump @1
            }
            @1: {
                exit = cond $0 ? @2..@2 : @3..@3
            }
            @2: {
                $1 = 345
                exit = jump @3
            }
            @3: {
                exit = jump @4
            }
            @4: {
                $2 = 10
                $3 = 1
                exit = jump @5
            }
            @5: {
                exit = cond $3 ? @6..@6 : @7..@7
            }
            @6: {
                $4 = 2
                exit = jump @7
            }
            @7: {
                exit = jump @8
            }
            @8: {
                $5 = undefined
                exit = return $5
            }
            "###,
        );

        let stats = block_group_to_structured_flow(func.blocks);
        insta::assert_debug_snapshot!(stats, @r###"
        {
            $0 = 123
            if ($0) {
                $1 = 345
            } else {
            
            }
            $2 = 10
            $3 = 1
            if ($3) {
                $4 = 2
            } else {
            
            }
            $5 = undefined
            Return $5
        }
        "###);
    }

    #[test]
    fn mk_trycatch() {
        let func = parse_instructions(
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
                exit = error ? jump @4 : jump @6
            }
            @4: {
                $1 = caught_error()
                $2 = 888
                exit = jump @5
            }
            @5: {
                exit = finally @6 after @7
            }
            @6: {
                exit = jump @7
            }
            @7: {
                exit = end finally after @8
            }
            @8: {
                $3 = 999
                $4 = undefined
                exit = return $4
            }
            "###,
        );

        let stats = block_group_to_structured_flow(func.blocks);
        insta::assert_debug_snapshot!(stats, @r###"
        {

            try (@2) {
                $0 = 777
            } catch {
                $1 = caught_error()
                $2 = 888
            } finally {
            
            }
            $3 = 999
            $4 = undefined
            Return $4
        }
        "###);
    }

    #[test]
    fn mk_trycatch_2() {
        let func = parse_instructions(
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
                exit = error ? jump @4 : jump @6
            }
            @4: {
                $1 = caught_error()
                $2 = 888
                exit = jump @5
            }
            @5: {
                exit = finally @6 after @7
            }
            @6: {
                $3 = 999
                exit = jump @7
            }
            @7: {
                exit = end finally after @8
            }
            @8: {
                $4 = 111
                $5 = undefined
                exit = return $5
            }
            "###,
        );

        let stats = block_group_to_structured_flow(func.blocks);
        insta::assert_debug_snapshot!(stats, @r###"
        {

            try (@2) {
                $0 = 777
            } catch {
                $1 = caught_error()
                $2 = 888
            } finally {
                $3 = 999
            }
            $4 = 111
            $5 = undefined
            Return $5
        }
        "###);
    }

    #[test]
    fn mk_trycatch_edgecases() {
        let func = parse_instructions(
            r###"
            @0: {
                exit = jump @1
            }
            @1: {
                exit = try @2 catch @8 finally @10 after @11
            }
            @2: {
                $0 = 111
                exit = jump @3
            }
            @3: {
                exit = cond $0 ? @4..@5 : @6..@6
            }
            @4: {
                $1 = 222
                exit = jump @5
            }
            @5: {
                exit = jump @6
            }
            @6: {
                exit = jump @7
            }
            @7: {
                exit = error ? jump @8 : jump @10
            }
            @8: {
                exit = jump @9
            }
            @9: {
                exit = finally @10 after @12
            }
            @10: {
                exit = jump @11
            }
            @11: {
                exit = end finally after @12
            }
            @12: {
                $2 = 111
                $3 = undefined
                exit = return $3
            }
            "###,
        );

        let stats = block_group_to_structured_flow(func.blocks);
        insta::assert_debug_snapshot!(stats, @r###"
        {

            try (@2) {
                $0 = 111
                if ($0) {
                    $1 = 222
                } else {
                
                }
            
            } catch {
            
            } finally {
            
            }
            $2 = 111
            $3 = undefined
            Return $3
        }
        "###);
    }

    #[test]
    fn mk_sparse() {
        let func = parse_instructions(
            r###"
            @0: {
                $0 = 1
                $1 = $0
                $2 = 1
                $3 = $1 == $2
                exit = jump @1
            }
            @1: {
                $4 = $0
                $5 = 1
                $6 = $4 == $5
                exit = jump @2
            }
            @2: {
                $7 = $0
                $8 = 2000
                $9 = $7 + $8
                $10 = $9
                exit = jump @4
            }
            @4: {
                $13 = either($0, $9)
                $14 = $13
                $15 = 1000
                $16 = $14 + $15
                $17 = $16
                exit = jump @6
            }
            @6: {
                $20 = either($13, $16)
                $21 = $20
                exit = return $21
            }
            "###,
        );

        let stats = block_group_to_structured_flow(func.blocks);
        insta::assert_debug_snapshot!(stats, @r###"
        {
            $0 = 1
            $1 = $0
            $2 = 1
            $3 = $1 == $2
            $4 = $0
            $5 = 1
            $6 = $4 == $5
            $7 = $0
            $8 = 2000
            $9 = $7 + $8
            $10 = $9
            $13 = either($0, $9)
            $14 = $13
            $15 = 1000
            $16 = $14 + $15
            $17 = $16
            $20 = either($13, $16)
            $21 = $20
            Return $21
        }
        "###);
    }
}
