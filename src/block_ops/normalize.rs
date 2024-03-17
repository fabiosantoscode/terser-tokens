use std::collections::BTreeMap;
use std::usize::MAX;

use crate::{
    basic_blocks::{
        BasicBlock, BasicBlockExit, BasicBlockInstruction, BreakableId, StructuredClassMember,
        StructuredFlow,
    },
    block_ops::block_group_to_structured_flow,
};

pub fn normalize_basic_blocks(blocks: BTreeMap<usize, BasicBlock>) -> BTreeMap<usize, BasicBlock> {
    let recursive = block_group_to_structured_flow(blocks);

    normalize_basic_blocks_tree(vec![recursive])
}

pub fn normalize_basic_blocks_tree(recursive: Vec<StructuredFlow>) -> BTreeMap<usize, BasicBlock> {
    let mut ctx = FoldBlocksCtx::default();
    fold_blocks(&mut ctx, recursive);

    ctx.out_blocks
}

pub fn normalize_basic_blocks_tree_at_block_index(
    recursive: Vec<StructuredFlow>,
    idx: usize,
) -> BTreeMap<usize, BasicBlock> {
    let mut ctx = FoldBlocksCtx::default();
    ctx.block_index = idx;
    fold_blocks(&mut ctx, recursive);

    ctx.out_blocks
}

fn forward_jump_marker(block: Option<&mut BasicBlock>) -> Option<&mut usize> {
    if let Some(block) = block {
        match &mut block.exit {
            BasicBlockExit::Break(mrk) if *mrk == MAX => Some(mrk),
            _ => None,
        }
    } else {
        None
    }
}

fn ensure_forward_jump_marker(ctx: &mut FoldBlocksCtx) -> usize {
    let last_block = ctx.out_blocks.get_mut(&(ctx.block_index - 1));

    let has_forward_jump_marker = matches!(
        last_block,
        Some(BasicBlock {
            exit: BasicBlockExit::Fallthrough,
            ..
        })
    ) || forward_jump_marker(last_block).is_some();

    if !has_forward_jump_marker {
        ctx.push_block(BasicBlock {
            instructions: vec![],
            exit: BasicBlockExit::Fallthrough,
        });
    }

    ctx.block_index()
}

fn resolve_forward_jumps(ctx: &mut FoldBlocksCtx) {
    let jump_target = ctx.next_block();
    for label in ctx.to_label.drain(..) {
        let m = ctx.out_blocks.get_mut(&label);
        if let Some(marker) = forward_jump_marker(m) {
            *marker = jump_target;
        }
    }
}

fn fold_basic_blocks(
    inp: Vec<StructuredFlow>,
) -> Vec<(
    Vec<Vec<(usize, BasicBlockInstruction)>>,
    Option<StructuredFlow>,
)> {
    let mut out = vec![];
    let mut leftover_instructions = Vec::with_capacity(inp.len());
    for item in inp {
        match item {
            StructuredFlow::BasicBlock(instructions) => leftover_instructions.push(instructions),
            item if item.is_structured_flow_empty() => {}
            _ => {
                if leftover_instructions.len() > 0 {
                    out.push((leftover_instructions, Some(item)));
                    leftover_instructions = vec![];
                } else {
                    out.push((vec![], Some(item)));
                }
            }
        }
    }
    if leftover_instructions.len() > 0 {
        out.push((leftover_instructions, None));
    }
    out
}

#[derive(Default)]
struct FoldBlocksCtx {
    to_label: Vec<usize>,
    block_index: usize,
    out_blocks: BTreeMap<usize, BasicBlock>,
    jump_targets: BTreeMap<BreakableId, (usize, Vec<usize>)>,
}

impl FoldBlocksCtx {
    fn push_block(&mut self, block: BasicBlock) {
        self.out_blocks.insert(self.block_index, block);
        self.block_index += 1;
    }

    fn block_index(&self) -> usize {
        self.block_index - 1
    }

    fn next_block(&self) -> usize {
        self.block_index
    }

    fn block_after_next(&self) -> usize {
        self.block_index + 1
    }

    fn set_exit(&mut self, at: usize, exit: BasicBlockExit) {
        let blk = self.out_blocks.get_mut(&at).unwrap();
        blk.exit = exit;
    }
}

fn fold_blocks(ctx: &mut FoldBlocksCtx, as_tree: Vec<StructuredFlow>) -> usize {
    for (preceding_bbs, item) in fold_basic_blocks(as_tree) {
        resolve_forward_jumps(ctx);

        let instructions = preceding_bbs.into_iter().flatten().collect::<Vec<_>>();

        let item = match item {
            Some(item) => item,
            None => {
                // Will not stand on its own
                ctx.push_block(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Fallthrough,
                });
                continue;
            }
        };

        match item {
            // THE HUMBLE BASIC BLOCK
            StructuredFlow::BasicBlock(_) => {
                unreachable!("handled above")
            }
            // EXITS
            StructuredFlow::Break(brk) => {
                let new_tgt = ctx.block_index() + 1;
                let (_, jump_target) = ctx.jump_targets.get_mut(&brk).unwrap();

                jump_target.push(new_tgt);
                ctx.push_block(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Break(MAX),
                });
                break;
            }
            StructuredFlow::Continue(brk) => {
                let (continue_to, _) = ctx.jump_targets.get(&brk).unwrap();

                ctx.push_block(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Continue(*continue_to),
                });
                break;
            }
            StructuredFlow::Return(exit_type, var_idx) => {
                ctx.push_block(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::ExitFn(exit_type, var_idx),
                });
                break;
            }
            // SETS OF BLOCKS (will recurse 1+ times)
            StructuredFlow::Block(blocks_within) => {
                if instructions.len() > 0 {
                    ctx.push_block(BasicBlock {
                        instructions,
                        exit: BasicBlockExit::Fallthrough,
                    });
                }
                fold_blocks(ctx, blocks_within);
            }
            StructuredFlow::Cond(brk, cond_var, cons, alt) => {
                if brk.0.is_some() {
                    ctx.jump_targets
                        .insert(brk, (ctx.block_after_next(), vec![]));
                }

                ctx.push_block(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Cond(cond_var, MAX, MAX, MAX, MAX),
                });
                let cond = ctx.block_index();

                let cons = fold_blocks(ctx, cons);
                let alt = fold_blocks(ctx, alt);

                ctx.set_exit(
                    cond,
                    BasicBlockExit::Cond(cond_var, cond + 1, cons, cons + 1, alt),
                );

                if let Some((_, broken_from)) = ctx.jump_targets.remove(&brk) {
                    ctx.to_label.extend(broken_from);
                }
            }
            StructuredFlow::Switch(brk, expression, cases) => {
                if brk.0.is_some() {
                    ctx.jump_targets
                        .insert(brk, (ctx.block_after_next(), vec![]));
                }

                ctx.push_block(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Switch(expression, MAX, MAX),
                });
                let switch_start = ctx.block_index();

                for case in cases {
                    match case.condition {
                        Some((insx, condvar)) => {
                            ctx.push_block(BasicBlock {
                                instructions: vec![],
                                exit: BasicBlockExit::SwitchCaseExpression(MAX, MAX),
                            });
                            let case_expr_start = ctx.block_index();
                            let case_expr_end = fold_blocks(ctx, insx);

                            ctx.push_block(BasicBlock {
                                instructions: vec![],
                                exit: BasicBlockExit::SwitchCase(Some(condvar), MAX, MAX),
                            });
                            let case_start = ctx.block_index();

                            let case_end = fold_blocks(ctx, case.body);

                            ctx.set_exit(
                                case_expr_start,
                                BasicBlockExit::SwitchCaseExpression(
                                    case_expr_start + 1,
                                    case_expr_end,
                                ),
                            );
                            ctx.set_exit(
                                case_start,
                                BasicBlockExit::SwitchCase(Some(condvar), case_start + 1, case_end),
                            );
                        }
                        None => {
                            ctx.push_block(BasicBlock {
                                instructions: vec![],
                                exit: BasicBlockExit::SwitchCase(None, MAX, MAX),
                            });
                            let case_start = ctx.block_index();

                            let case_end = fold_blocks(ctx, case.body);

                            ctx.set_exit(
                                case_start,
                                BasicBlockExit::SwitchCase(None, case_start + 1, case_end),
                            );
                        }
                    }
                }

                ctx.push_block(BasicBlock {
                    instructions: vec![],
                    exit: BasicBlockExit::SwitchEnd,
                });
                let switch_end = ctx.block_index();

                ctx.set_exit(
                    switch_start,
                    BasicBlockExit::Switch(expression, switch_start + 1, switch_end),
                );

                if let Some((_, broken_from)) = ctx.jump_targets.remove(&brk) {
                    ctx.to_label.extend(broken_from);
                }
            }
            StructuredFlow::Loop(brk, body) => {
                if brk.0.is_some() {
                    ctx.jump_targets
                        .insert(brk, (ctx.block_after_next(), vec![]));
                }

                ctx.push_block(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Loop(MAX, MAX),
                });
                let head = ctx.block_index();

                let body = fold_blocks(ctx, body);

                ctx.set_exit(head, BasicBlockExit::Loop(head + 1, body));

                if let Some((_, broken_from)) = ctx.jump_targets.remove(&brk) {
                    ctx.to_label.extend(broken_from);
                }
            }
            StructuredFlow::ForInOfLoop(brk, looped_var, loop_kind, body) => {
                if brk.0.is_some() {
                    ctx.jump_targets
                        .insert(brk, (ctx.block_after_next(), vec![]));
                }

                ctx.push_block(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::ForInOfLoop(looped_var, loop_kind, MAX, MAX),
                });
                let head = ctx.block_index();

                let body = fold_blocks(ctx, body);

                ctx.set_exit(
                    head,
                    BasicBlockExit::ForInOfLoop(looped_var, loop_kind, head + 1, body),
                );

                if let Some((_, broken_from)) = ctx.jump_targets.remove(&brk) {
                    ctx.to_label.extend(broken_from);
                }
            }
            StructuredFlow::TryCatch(brk, body, catch, finally) => {
                if brk.0.is_some() {
                    ctx.jump_targets
                        .insert(brk, (ctx.block_after_next(), vec![]));
                }

                ctx.push_block(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::SetTryAndCatch(MAX, MAX, MAX, MAX),
                });
                let head = ctx.block_index();

                fold_blocks(ctx, body);
                let body = ensure_forward_jump_marker(ctx);

                fold_blocks(ctx, catch);
                let catch = ensure_forward_jump_marker(ctx);

                fold_blocks(ctx, finally);
                let finally = ensure_forward_jump_marker(ctx);

                ctx.set_exit(
                    head,
                    BasicBlockExit::SetTryAndCatch(head + 1, body + 1, catch + 1, finally),
                );
                ctx.set_exit(body, BasicBlockExit::PopCatch(body + 1, catch + 1));
                ctx.set_exit(catch, BasicBlockExit::PopFinally(catch + 1, finally));

                if let Some((_, broken_from)) = ctx.jump_targets.remove(&brk) {
                    ctx.to_label.extend(broken_from);
                }
            }
            StructuredFlow::Class(class_var, members) => {
                // TODO push/pop jump_targets? It's invalid to `break` out of a static block
                let head = ctx.next_block();

                ctx.push_block(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::ClassStart(class_var, head + 1, MAX),
                });

                for member in members.into_iter() {
                    match member {
                        StructuredClassMember::Property(block, prop) => {
                            fold_blocks(ctx, block);

                            ctx.push_block(BasicBlock {
                                instructions: vec![],
                                exit: BasicBlockExit::ClassProperty(prop.clone()),
                            });
                        }
                        StructuredClassMember::Constructor(func_id) => {
                            ctx.push_block(BasicBlock {
                                instructions: vec![],
                                exit: BasicBlockExit::ClassConstructor(func_id),
                            });
                        }
                        StructuredClassMember::StaticBlock(block) => {
                            ctx.push_block(BasicBlock {
                                instructions: vec![],
                                exit: BasicBlockExit::ClassStaticBlock(MAX, MAX),
                            });

                            let sb_start = ctx.block_index();

                            let sb_end = fold_blocks(ctx, block);

                            ctx.set_exit(
                                sb_start,
                                BasicBlockExit::ClassStaticBlock(sb_start + 1, sb_end + 1),
                            );
                        }
                    }
                }

                ctx.push_block(BasicBlock {
                    instructions: vec![],
                    exit: BasicBlockExit::ClassEnd,
                });
                let end = ctx.block_index();

                ctx.set_exit(head, BasicBlockExit::ClassStart(class_var, head + 1, end));
            }
            StructuredFlow::Debugger => {
                ctx.push_block(BasicBlock {
                    instructions,
                    exit: BasicBlockExit::Debugger,
                });
            }
        }
    }

    ctx.block_index()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_blocks::BasicBlockGroup;
    use crate::testutils::*;

    fn test_normalize(instructions: &str) -> BasicBlockGroup {
        let group = parse_instructions(instructions);
        let blocks = normalize_basic_blocks(group.blocks);

        let blocks_again = normalize_basic_blocks(blocks.clone());
        assert_eq!(
            blocks, blocks_again,
            "normalizing twice should be idempotent"
        );

        BasicBlockGroup {
            blocks,
            ..Default::default()
        }
    }

    #[test]
    fn test_normalize_if() {
        let group = test_normalize(
            "@0: {
                $0 = 1
                exit = cond $0 ? @1..@1 : @2..@2
            }
            @1: {
                $1 = 2
                exit = jump @3
            }
            @2: {
                $2 = 3
                exit = jump @3
            }
            @3: {
                $3 = 4
                exit = return $3
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            $0 = 1
            exit = cond $0 ? @1..@1 : @2..@2
        }
        @1: {
            $1 = 2
        }
        @2: {
            $2 = 3
        }
        @3: {
            $3 = 4
            exit = return $3
        }
        "###);
    }

    #[test]
    fn test_normalize_if_empty() {
        let group = test_normalize(
            "@0: {
                $0 = 1
                exit = cond $0 ? @1..@1 : @2..@2
            }
            @1: {
                exit = jump @3
            }
            @2: {
                exit = jump @3
            }
            @3: {
                $1 = 4
                exit = return $1
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            $0 = 1
            $1 = 4
            exit = return $1
        }
        "###);
    }

    #[test]
    fn test_normalize_trycatch() {
        let group = test_normalize(
            "@0: {
                exit = jump @1
            }
            @1: {
                exit = try @2 catch @4 finally @6..@7
            }
            @2: {
                $0 = 777
                exit = jump @3
            }
            @3: {
                exit = catch @4..@6
            }
            @4: {
                $1 = either($0, $2, $3)
                $2 = 888
                exit = jump @5
            }
            @5: {
                $3 = either($0, $1, $2)
                exit = finally @6..@7
            }
            @6: {
                exit = jump @7
            }
            @7: {
            }
            @8: {
                $4 = $3
                exit = return $4
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            exit = try @1 catch @2 finally @3..@3
        }
        @1: {
            $0 = 777
            exit = catch @2..@3
        }
        @2: {
            $1 = either($0, $2, $3)
            $2 = 888
            $3 = either($0, $1, $2)
            exit = finally @3..@3
        }
        @3: {
        }
        @4: {
            $4 = $3
            exit = return $4
        }
        "###);
    }

    #[test]
    fn test_normalize_trycatch_empty() {
        let group = test_normalize(
            "@0: {
                exit = jump @1
            }
            @1: {
                exit = try @2 catch @4 finally @6..@7
            }
            @2: {
                exit = jump @3
            }
            @3: {
                exit = catch @4..@6
            }
            @4: {
                $1 = either($2, $3)
                $2 = 888
                exit = jump @5
            }
            @5: {
                $3 = either($1, $2)
                exit = finally @6..@7
            }
            @6: {
                exit = jump @7
            }
            @7: {
            }
            @8: {
                $4 = $3
                exit = return $4
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            $4 = $3
            exit = return $4
        }
        "###);
    }

    #[test]
    fn test_normalize_loop() {
        let group = test_normalize(
            "@0: {
                exit = loop @1..@2
            }
            @1: {
                $1 = 2
                exit = break @3
            }
            @2: {
                $2 = 3
                exit = jump @3
            }
            @3: {
                $3 = 4
                exit = return $3
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            exit = loop @1..@1
        }
        @1: {
            $1 = 2
            exit = break @2
        }
        @2: {
            $3 = 4
            exit = return $3
        }
        "###);
    }

    #[test]
    fn test_normalize_loop_break() {
        let group = test_normalize(
            "@0: {
                exit = jump @1
            }
            @1: {
                exit = loop @2..@9
            }
            @2: {
                $0 = 123
                exit = cond $0 ? @3..@8 : @9..@9
            }
            @3: {
                exit = jump @4
            }
            @4: {
                exit = jump @11
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
                exit = jump @2
            }
            @9: {
                exit = jump @10
            }
            @10: {
                exit = jump @11
            }
            @11: {
                exit = jump @12
            }
            @12: {
                $1 = undefined
                exit = return $1
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            exit = loop @1..@1
        }
        @1: {
            $0 = 123
        }
        @2: {
            $1 = undefined
            exit = return $1
        }
        "###);
    }

    #[test]
    fn test_normalize_loop_break_2() {
        let group = test_normalize(
            "@0: {
                exit = jump @1
            }
            @1: {
                exit = loop @2..@8
            }
            @2: {
                $0 = 123
                exit = cond $0 ? @3..@7 : @8..@8
            }
            @3: {
                exit = jump @4
            }
            @4: {
                exit = break @9
            }
            @5: {
                exit = jump @6
            }
            @6: {
                exit = jump @7
            }
            @7: {
                exit = continue @2
            }
            @8: {
                exit = break @9
            }
            @9: {
                exit = jump @10
            }
            @10: {
                $1 = undefined
                exit = return $1
            }",
        );
        insta::assert_debug_snapshot!(group, @r###"
        @0: {
            exit = loop @1..@3
        }
        @1: {
            $0 = 123
            exit = cond $0 ? @2..@2 : @3..@3
        }
        @2: {
            exit = break @4
        }
        @3: {
            exit = break @4
        }
        @4: {
            $1 = undefined
            exit = return $1
        }
        "###);
    }
}
