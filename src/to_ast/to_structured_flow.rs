use std::cell::RefCell;
use std::fmt::Debug;

use super::{BreakableId, StructuredFlow};
use crate::basic_blocks::{BasicBlockExit, BasicBlockGroup};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct BreakAndRange(pub BreakableId, pub usize, pub usize);

struct Ctx {
    containing_syntax: RefCell<Vec<BreakAndRange>>,
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
        self.containing_syntax.borrow_mut().push(syn);
        let ret = func(self);
        self.containing_syntax.borrow_mut().pop();
        ret
    }

    /// Given a break target, find the enclosing loop or labelled block
    pub fn break_index(&self, target: usize) -> BreakableId {
        let containing_syntax = self.containing_syntax.borrow();

        let brk_id = containing_syntax
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
            .expect(&format!("break @{target} without matching to a container"));

        *brk_id
    }

    /// Given a continue target, find the enclosing loop
    pub fn continue_index(&self, target: usize) -> BreakableId {
        let containing_syntax = self.containing_syntax.borrow();

        let brk_id = containing_syntax
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
            .expect(&format!("break @{target} without matching to a container"));

        *brk_id
    }

    pub fn get_breakable_id(&mut self) -> BreakableId {
        self.last_breakable_id += 1;
        BreakableId(Some(self.last_breakable_id))
    }
}

pub fn do_tree(func: &BasicBlockGroup) -> StructuredFlow {
    fn do_tree_chunk(
        ctx: &mut Ctx,
        func: &BasicBlockGroup,
        start_blk: usize,
        end_blk: usize,
    ) -> Vec<StructuredFlow> {
        if end_blk < start_blk {
            return vec![];
        }

        let first_block = func.blocks.range(start_blk..=end_blk).into_iter().next();

        match first_block {
            Some((blk_id, block)) => {
                let mut rest = blk_id + 1;

                let mut blocks = vec![StructuredFlow::BasicBlock(*blk_id)];

                match block.exit {
                    BasicBlockExit::Jump(to) => rest = to,
                    BasicBlockExit::Break(tgt) => {
                        blocks.extend(vec![StructuredFlow::Break(ctx.break_index(tgt))])
                    }
                    BasicBlockExit::Continue(tgt) => {
                        blocks.extend(vec![StructuredFlow::Continue(ctx.continue_index(tgt))])
                    }
                    BasicBlockExit::Cond(cond, cons_start, cons_end, alt_start, alt_end) => {
                        rest = alt_end + 1;

                        let brk_id = ctx.get_breakable_id();
                        ctx.push_within(BreakAndRange(brk_id, cons_start, alt_end), |ctx| {
                            blocks.extend(vec![StructuredFlow::Branch(
                                brk_id,
                                cond,
                                do_tree_chunk(ctx, func, cons_start, cons_end),
                                do_tree_chunk(ctx, func, alt_start, alt_end),
                            )])
                        })
                    }
                    BasicBlockExit::Loop(start, end) => {
                        rest = end + 1;

                        let loop_brk_id = ctx.get_breakable_id();
                        ctx.push_within(BreakAndRange(loop_brk_id, start, end), |ctx| {
                            blocks.extend(vec![StructuredFlow::Loop(
                                loop_brk_id,
                                do_tree_chunk(ctx, func, start, end),
                            )])
                        })
                    }
                    BasicBlockExit::ExitFn(ref exit_type, yielded_val) => {
                        blocks.extend(vec![StructuredFlow::Return(
                            exit_type.clone(),
                            Some(yielded_val),
                        )])
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
                            blocks.extend(vec![StructuredFlow::TryCatch(
                                brk_id,
                                do_tree_chunk(ctx, func, try_block, catch_block - 1),
                                do_tree_chunk(ctx, func, catch_block, finally_block - 1),
                                do_tree_chunk(ctx, func, finally_block, end_finally),
                            )])
                        })
                    }
                    BasicBlockExit::PopCatch(catch, _) => rest = catch,
                    BasicBlockExit::PopFinally(finally, _) => rest = finally,
                    BasicBlockExit::EndFinally(after) => rest = after,
                };

                blocks.extend(do_tree_chunk(ctx, func, rest, end_blk));

                blocks
            }
            None => vec![],
        }
    }

    let mut ctx = Ctx::new();
    let (first, last) = func.get_block_range();
    let tree = StructuredFlow::Block(do_tree_chunk(&mut ctx, &func, first, last));

    tree.simplify()
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

        insta::assert_debug_snapshot!(do_tree(&func), @r###"
        Block(
            [BasicBlockRef(0), BasicBlockRef(1), Return]
        )
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

        insta::assert_debug_snapshot!(do_tree(&func), @r###"
        Block(
            [BasicBlockRef(0), Branch  ($0) {
                [BasicBlockRef(1)]
                [BasicBlockRef(2)]
            }, BasicBlockRef(3), Return]
        )
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

        insta::assert_debug_snapshot!(do_tree(&func), @r###"
        Block(
            [BasicBlockRef(0), BasicBlockRef(1), Branch  ($0) {
                [BasicBlockRef(2)]
                [BasicBlockRef(3)]
            }, BasicBlockRef(4), Return]
        )
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

        insta::assert_debug_snapshot!(do_tree(&func), @r###"
        Block(
            [BasicBlockRef(0), BasicBlockRef(1), BasicBlockRef(2), Loop #2(
                [BasicBlockRef(3), Branch  ($1) {
                    [BasicBlockRef(4), BasicBlockRef(5), BasicBlockRef(6), BasicBlockRef(7), BasicBlockRef(8), Continue #2]
                    [BasicBlockRef(9), Break #2]
                }]
            ), BasicBlockRef(10), BasicBlockRef(11), BasicBlockRef(12), BasicBlockRef(13), Return]
        )
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

        insta::assert_debug_snapshot!(do_tree(&func), @r###"
        Block(
            [BasicBlockRef(0), BasicBlockRef(1), Loop #2(
                [BasicBlockRef(2), Branch  ($0) {
                    [BasicBlockRef(3), BasicBlockRef(4), Loop #4(
                        [BasicBlockRef(5), Branch  ($1) {
                            [BasicBlockRef(6), BasicBlockRef(7), Break #2, BasicBlockRef(8), BasicBlockRef(9), BasicBlockRef(10), Continue #4]
                            [BasicBlockRef(11), Break #4]
                        }]
                    ), BasicBlockRef(12), BasicBlockRef(13), BasicBlockRef(14), Continue #2]
                    [BasicBlockRef(15), Break #2]
                }]
            ), BasicBlockRef(16), BasicBlockRef(17), Return]
        )
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

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [BasicBlockRef(0), BasicBlockRef(1), Branch  ($4) {
                [BasicBlockRef(2)]
                [BasicBlockRef(3)]
            }, BasicBlockRef(4), Return]
        )
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

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [BasicBlockRef(0), Branch  ($1) {
                [BasicBlockRef(1)]
                [BasicBlockRef(2)]
            }, BasicBlockRef(3), Return]
        )
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

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [BasicBlockRef(0), Branch  ($0) {
                [BasicBlockRef(1), Branch  ($1) {
                    [BasicBlockRef(2)]
                    [BasicBlockRef(3), BasicBlockRef(4)]
                }, BasicBlockRef(5)]
                [BasicBlockRef(6)]
            }, BasicBlockRef(7), Return]
        )
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

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [BasicBlockRef(0), BasicBlockRef(1), Branch  ($0) {
                [BasicBlockRef(2)]
                [BasicBlockRef(3)]
            }, BasicBlockRef(4), BasicBlockRef(5), Branch  ($3) {
                [BasicBlockRef(6)]
                [BasicBlockRef(7)]
            }, BasicBlockRef(8), Return]
        )
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

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [BasicBlockRef(0), BasicBlockRef(1), TryCatch #2(
                [BasicBlockRef(2), BasicBlockRef(3)]
                [BasicBlockRef(4), BasicBlockRef(5)]
                [BasicBlockRef(6), BasicBlockRef(7)]
            ), BasicBlockRef(8), Return]
        )
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

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [BasicBlockRef(0), BasicBlockRef(1), TryCatch #2(
                [BasicBlockRef(2), BasicBlockRef(3)]
                [BasicBlockRef(4), BasicBlockRef(5)]
                [BasicBlockRef(6), BasicBlockRef(7)]
            ), BasicBlockRef(8), Return]
        )
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

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [BasicBlockRef(0), BasicBlockRef(1), TryCatch #2(
                [BasicBlockRef(2), BasicBlockRef(3), Branch  ($0) {
                    [BasicBlockRef(4), BasicBlockRef(5)]
                    [BasicBlockRef(6)]
                }, BasicBlockRef(7)]
                [BasicBlockRef(8), BasicBlockRef(9)]
                [BasicBlockRef(10), BasicBlockRef(11)]
            ), BasicBlockRef(12), Return]
        )
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

        let stats = do_tree(&func);
        insta::assert_debug_snapshot!(stats, @r###"
        Block(
            [BasicBlockRef(0), BasicBlockRef(1), BasicBlockRef(2), BasicBlockRef(4), BasicBlockRef(6), Return]
        )
        "###);
    }
}
