use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::ops::RangeInclusive;
use std::rc::Rc;

use deep_bind::contextual;

use crate::basic_blocks::{BasicBlockExit, BasicBlockGroup, ExitType};

// https://dl.acm.org/doi/pdf/10.1145/3547621
// This paper really unlocked this project.
// It explains how one can turn basic blocks into a structured AST.
// It's focused on WASM, but I've heard JS also has "if", "break" and "loops"
// So it's probably helpful here too!
//
// What follows is a translation of this paper into Rust.

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default)]
pub struct BreakableId(pub Option<usize>);

impl std::fmt::Display for BreakableId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(brk) = self.0 {
            write!(f, "#{}", brk)?;
        }

        Ok(())
    }
}

#[derive(Clone)]
pub enum StructuredFlow {
    Block(Vec<StructuredFlow>),
    Loop(BreakableId, Vec<StructuredFlow>),
    Branch(BreakableId, usize, Vec<StructuredFlow>, Vec<StructuredFlow>),
    TryCatch(
        BreakableId,
        Vec<StructuredFlow>,
        Vec<StructuredFlow>,
        Vec<StructuredFlow>,
    ),
    Break(BreakableId),
    Continue(BreakableId),
    Return(ExitType, Option<usize>),
    BasicBlock(usize),
}

struct Ctx {
    pub containing_syntax: RefCell<Vec<(BreakableId, RangeInclusive<usize>, bool)>>,
}

impl Ctx {
    pub fn new() -> Self {
        Ctx {
            containing_syntax: Default::default(),
        }
    }

    pub fn push_within<T, Fnc>(
        &self,
        syn: BreakableId,
        range: RangeInclusive<usize>,
        is_loop: bool,
        func: Fnc,
    ) -> T
    where
        Fnc: FnOnce() -> T,
    {
        self.containing_syntax
            .borrow_mut()
            .push((syn, range, is_loop));
        let ret = func();
        self.containing_syntax.borrow_mut().pop();
        ret
    }

    ///
    pub fn break_index(&self, target: usize) -> BreakableId {
        let containing_syntax = self.containing_syntax.borrow();

        let brk_id = containing_syntax
            .iter()
            .find_map(|(item, range, _is_loop)| {
                if range.end() + 1 == target {
                    Some(item)
                } else {
                    None
                }
            })
            .expect(&format!("break @{target} without matching to a container"));

        *brk_id
    }

    ///
    pub fn continue_index(&self, target: usize) -> BreakableId {
        let containing_syntax = self.containing_syntax.borrow();

        let brk_id = containing_syntax
            .iter()
            .find_map(|(item, range, _is_loop)| {
                if *range.start() == target {
                    Some(item)
                } else {
                    None
                }
            })
            .expect(&format!("break @{target} without matching to a container"));

        *brk_id
    }
}

contextual!(Context(CONTEXT_2): Option<Rc<Ctx>> = None);
contextual!(BreakableIdCounter(BREAKABLE_ID): usize = 1);

fn context() -> Rc<Ctx> {
    Context::clone().unwrap()
}

fn get_breakable_id() -> BreakableId {
    BREAKABLE_ID.with(|id| {
        *id.borrow_mut() += 1;
        BreakableId(Some(*id.borrow()))
    })
}

pub fn do_tree(func: &BasicBlockGroup) -> StructuredFlow {
    fn do_tree_chunk(
        func: &BasicBlockGroup,
        start_blk: usize,
        end_blk: usize,
    ) -> Vec<StructuredFlow> {
        match &func.blocks[start_blk..=end_blk] {
            [block, ..] => {
                let mut rest = start_blk + 1;

                let mut blocks = vec![StructuredFlow::BasicBlock(start_blk)];

                match block.exit {
                    BasicBlockExit::Break(tgt) => {
                        blocks.extend(vec![StructuredFlow::Break(context().break_index(tgt))])
                    }
                    BasicBlockExit::Continue(tgt) => blocks.extend(vec![StructuredFlow::Continue(
                        context().continue_index(tgt),
                    )]),
                    BasicBlockExit::Cond(cond, cons_start, cons_end, alt_start, alt_end) => {
                        rest = alt_end + 1;

                        let brk_id = get_breakable_id();
                        context().push_within(brk_id, cons_start..=alt_end, false, || {
                            blocks.extend(vec![StructuredFlow::Branch(
                                brk_id,
                                cond,
                                do_tree_chunk(func, cons_start, cons_end),
                                do_tree_chunk(func, alt_start, alt_end),
                            )])
                        })
                    }
                    BasicBlockExit::Loop(start, end) => {
                        rest = end + 1;

                        let loop_brk_id = get_breakable_id();
                        context().push_within(loop_brk_id, start..=end, true, || {
                            blocks.extend(vec![StructuredFlow::Loop(
                                loop_brk_id,
                                do_tree_chunk(func, start, end),
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

                        let brk_id = get_breakable_id();
                        context().push_within(brk_id, try_block..=end_finally, false, || {
                            blocks.extend(vec![StructuredFlow::TryCatch(
                                brk_id,
                                do_tree_chunk(func, try_block, catch_block - 1),
                                do_tree_chunk(func, catch_block, finally_block - 1),
                                do_tree_chunk(func, finally_block, end_finally),
                            )])
                        })
                    }
                    _ => blocks.extend(vec![]),
                };

                blocks.extend(do_tree_chunk(func, rest, end_blk));

                blocks
            }
            [] => vec![],
        }
    }

    BreakableIdCounter::replace_within(1, || {
        Context::replace_within(Some(Rc::new(Ctx::new())), || {
            let tree = StructuredFlow::Block(do_tree_chunk(&func, 0, func.blocks.len() - 1));

            tree.simplify()
        })
    })
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
}

// For printing out these trees

impl StructuredFlow {
    fn str_head(&self) -> String {
        match self {
            StructuredFlow::Branch(_, _, _, _) => "Branch".to_string(),
            StructuredFlow::Break(_) => "Break".to_string(),
            StructuredFlow::Continue(_) => "Continue".to_string(),
            StructuredFlow::Loop(_, _) => "Loop".to_string(),
            StructuredFlow::Block(_) => "Block".to_string(),
            StructuredFlow::Return(_, _) => "Return".to_string(),
            StructuredFlow::BasicBlock(_) => "BasicBlockRef".to_string(),
            StructuredFlow::TryCatch(_, _, _, _) => "TryCatch".to_string(),
        }
    }
    fn simplify(self) -> Self {
        let break_targets = self.get_all_break_targets();
        let mut flat = self.flatten();
        flat.remove_unused_break_ids(&break_targets);
        flat
    }
    fn flatten(self) -> StructuredFlow {
        let map = fix_fn::fix_fn!(|map, items: Vec<StructuredFlow>| -> Vec<StructuredFlow> {
            items
                .into_iter()
                .flat_map(|item| match item {
                    StructuredFlow::Block(items) => map(items),
                    _ => vec![item.flatten()],
                })
                .collect::<Vec<_>>()
        });

        match self {
            StructuredFlow::Block(items) => {
                let items = map(items);
                if items.len() == 1 {
                    items.into_iter().next().unwrap()
                } else {
                    StructuredFlow::Block(items)
                }
            }
            StructuredFlow::Branch(id, cond, cons, alt) => {
                StructuredFlow::Branch(id, cond, map(cons), map(alt))
            }
            StructuredFlow::TryCatch(id, try_, catch, finally) => {
                StructuredFlow::TryCatch(id, map(try_), map(catch), map(finally))
            }
            StructuredFlow::Loop(id, items) => StructuredFlow::Loop(id, map(items)),
            no_children => no_children,
        }
    }
    fn remove_unused_break_ids(&mut self, used_break_targets: &HashSet<BreakableId>) {
        if let Some(id) = self.breakable_id() {
            if !used_break_targets.contains(&id) {
                self.remove_break_id();
            }
        }

        for children in self.children_mut().iter_mut() {
            for child in children.iter_mut() {
                child.remove_unused_break_ids(used_break_targets);
            }
        }
    }

    fn children(&self) -> Vec<&Vec<StructuredFlow>> {
        match self {
            StructuredFlow::Branch(_id, _x /* who cares */, y, z) => vec![y, z],
            StructuredFlow::Break(_) => vec![],
            StructuredFlow::Continue(_) => vec![],
            StructuredFlow::Loop(_, x) => vec![x],
            StructuredFlow::Block(x) => vec![x],
            StructuredFlow::Return(_, _) => vec![],
            StructuredFlow::BasicBlock(_) => vec![],
            StructuredFlow::TryCatch(_, t, v, fin) => {
                vec![t, v, fin]
            }
        }
    }

    fn children_mut(&mut self) -> Vec<&mut Vec<StructuredFlow>> {
        match self {
            StructuredFlow::Branch(_id, _x /* who cares */, y, z) => vec![y, z],
            StructuredFlow::Break(_) => vec![],
            StructuredFlow::Continue(_) => vec![],
            StructuredFlow::Loop(_, x) => vec![x],
            StructuredFlow::Block(x) => vec![x],
            StructuredFlow::Return(_, _) => vec![],
            StructuredFlow::BasicBlock(_) => vec![],
            StructuredFlow::TryCatch(_, t, v, fin) => {
                vec![t, v, fin]
            }
        }
    }

    fn get_all_break_targets(&self) -> HashSet<BreakableId> {
        let mut ret = HashSet::new();
        if let Some(breakable_id) = self.breaks_to_id() {
            ret.insert(breakable_id);
        }

        for child in self.children() {
            for child in child {
                for t in child.get_all_break_targets() {
                    ret.insert(t);
                }
            }
        }

        ret
    }

    fn breaks_to_id(&self) -> Option<BreakableId> {
        match self {
            StructuredFlow::Break(id) | StructuredFlow::Continue(id) => Some(*id),
            _ => None,
        }
    }
    fn remove_break_id(&mut self) {
        match self {
            StructuredFlow::Branch(id, _, _, _) | StructuredFlow::Loop(id, _) => {
                *id = BreakableId(None)
            }
            _ => {}
        }
    }

    fn breakable_id(&self) -> Option<BreakableId> {
        match self {
            StructuredFlow::Branch(id, _, _, _)
            | StructuredFlow::Break(id)
            | StructuredFlow::Continue(id)
            | StructuredFlow::Loop(id, _)
            | StructuredFlow::TryCatch(id, _, _, _) => Some(*id),
            _ => None,
        }
    }

    fn index_for_formatting(&self) -> Option<usize> {
        match self {
            StructuredFlow::Branch(_, _, _, _) => None,
            StructuredFlow::Break(x) | StructuredFlow::Continue(x) => x.0,
            StructuredFlow::Loop(_, _) => None,
            StructuredFlow::Block(_) => None,
            StructuredFlow::Return(_, _) => None,
            StructuredFlow::BasicBlock(x) => Some(*x),
            StructuredFlow::TryCatch(_, _, _, _) => None,
        }
    }
}

impl Debug for StructuredFlow {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let indent_str_lines = |s: &str| {
            let lines = s.lines();
            let indented_lines = lines.map(|line| format!("    {}", line));
            indented_lines.collect::<Vec<String>>().join("\n")
        };

        write!(f, "{}", self.str_head())?;
        if let Some(breakable_idx) = self.breakable_id() {
            write!(f, " {}", breakable_idx)?;
        }

        match self {
            StructuredFlow::Continue(_) | StructuredFlow::Break(_) => return Ok(()),
            _ => {}
        };

        if let StructuredFlow::Branch(_, var, cons, alt) = self {
            let cons = format!("{:?}", cons);
            let alt = format!("{:?}", alt);

            return write!(
                f,
                " (${}) {{\n{}\n{}\n}}",
                var,
                indent_str_lines(&cons),
                indent_str_lines(&alt)
            );
        } else if let Some(index) = self.index_for_formatting() {
            write!(f, "({})", index)
        } else {
            let children = self.children();
            if children.len() > 0 {
                let lines = children
                    .iter()
                    .map(|child| indent_str_lines(&format!("{:?}", child)))
                    .collect::<Vec<String>>()
                    .join("\n");

                write!(f, "(\n{}\n)", lines)?;
            }

            Ok(())
        }
    }
}
