use super::{BasicBlock, BasicBlockGroup, BasicBlockInstruction, BasicBlockModule, FunctionId};

impl BasicBlockModule {
    pub fn iter_all_instructions<'a>(&'a self) -> ModuleInstructionIterator<'a> {
        ModuleInstructionIterator {
            blockgroup_iter: CurIterator::from_iterator(self.functions.iter()),
            instruction_iter: None,
        }
    }

    pub fn iter_all_blocks<'a>(&'a self) -> ModuleBlockIterator<'a> {
        ModuleBlockIterator {
            blockgroup_iter: CurIterator::from_iterator(self.functions.iter()),
            block_iter: None,
        }
    }
}

impl BasicBlockGroup {
    pub fn iter_all_instructions<'a>(&'a self) -> BasicBlockGroupInstructionIterator<'a> {
        BasicBlockGroupInstructionIterator {
            block_iter: CurIterator::from_iterator(self.iter().enumerate()),
            instruction_iter: None,
        }
    }
}

/// An "iterator" that remembers the last thing that went through it. next() is called automatically at the beginning so that cur() doesn't start with None.
struct CurIterator<T, TIter>
where
    TIter: Iterator<Item = T>,
{
    iterator: TIter,
    current: Option<T>,
}

impl<T, TIter> CurIterator<T, TIter>
where
    TIter: Iterator<Item = T>,
    T: Copy,
{
    pub fn from_iterator(mut iterator: TIter) -> CurIterator<T, TIter> {
        let current = iterator.next();
        CurIterator { iterator, current }
    }

    fn cur(&mut self) -> Option<T> {
        self.current
    }

    fn advance(&mut self) {
        self.current = self.iterator.next();
    }
}

pub struct BasicBlockGroupInstructionIterator<'a> {
    block_iter: CurIterator<
        (usize, &'a BasicBlock),
        core::iter::Enumerate<core::slice::Iter<'a, BasicBlock>>,
    >,
    instruction_iter: Option<std::slice::Iter<'a, (usize, BasicBlockInstruction)>>,
}

impl<'a> Iterator for BasicBlockGroupInstructionIterator<'a> {
    type Item = (usize, usize, &'a BasicBlockInstruction);

    fn next(&mut self) -> Option<Self::Item> {
        let ((block_index, _), (ins_id, instruction)) = nested_iter_advance(
            &mut self.block_iter,
            &mut self.instruction_iter,
            &mut |(_, current_block): (usize, &BasicBlock)| current_block.instructions.iter(),
        )?;

        Some((block_index, *ins_id, instruction))
    }
}

pub struct ModuleBlockIterator<'a> {
    blockgroup_iter: CurIterator<
        (&'a FunctionId, &'a BasicBlockGroup),
        std::collections::btree_map::Iter<'a, FunctionId, BasicBlockGroup>,
    >,
    block_iter: Option<core::iter::Enumerate<core::slice::Iter<'a, BasicBlock>>>,
}

impl<'a> Iterator for ModuleBlockIterator<'a> {
    type Item = (FunctionId, usize, &'a BasicBlock);

    fn next(&mut self) -> Option<Self::Item> {
        let ((function_id, _), (block_id, block)) = nested_iter_advance(
            &mut self.blockgroup_iter,
            &mut self.block_iter,
            &mut |(_, current_blockgroup): (&FunctionId, &BasicBlockGroup)| {
                current_blockgroup.iter().enumerate()
            },
        )?;

        Some((*function_id, block_id, block))
    }
}

pub struct ModuleInstructionIterator<'a> {
    blockgroup_iter: CurIterator<
        (&'a FunctionId, &'a BasicBlockGroup),
        std::collections::btree_map::Iter<'a, FunctionId, BasicBlockGroup>,
    >,
    instruction_iter: Option<BasicBlockGroupInstructionIterator<'a>>,
}

impl<'a> Iterator for ModuleInstructionIterator<'a> {
    type Item = (FunctionId, usize, usize, &'a BasicBlockInstruction);

    fn next(&mut self) -> Option<Self::Item> {
        let ((function_id, _), (block_id, ins_id, instruction)) = nested_iter_advance(
            &mut self.blockgroup_iter,
            &mut self.instruction_iter,
            &mut |(_, current_blockgroup): (&FunctionId, &BasicBlockGroup)| {
                current_blockgroup.iter_all_instructions()
            },
        )?;

        Some((*function_id, block_id, ins_id, instruction))
    }
}

fn nested_iter_advance<OuterT, OuterTIter, InnerT, InnerTIter, CreateInnerIter>(
    outer_iter: &mut CurIterator<OuterT, OuterTIter>,
    mut maybe_inner_iter: &mut Option<InnerTIter>,
    create_inner_iter: &mut CreateInnerIter,
) -> Option<(OuterT, InnerT)>
where
    OuterTIter: Iterator<Item = OuterT>,
    InnerTIter: Iterator<Item = InnerT>,
    OuterT: Copy,
    InnerT: Copy,
    CreateInnerIter: FnMut(OuterT) -> InnerTIter,
{
    loop {
        let Some(outer_item) = outer_iter.cur() else { return None };

        let inner_iter = match &mut maybe_inner_iter {
            Some(inner_iter) => inner_iter,
            None => {
                *maybe_inner_iter = Some(create_inner_iter(outer_item));
                maybe_inner_iter.as_mut().unwrap()
            }
        };

        match inner_iter.next() {
            Some(inner_item) => return Some((outer_item, inner_item)),
            None => {
                *maybe_inner_iter = None;
                outer_iter.advance();
                continue;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::testutils::*;

    use super::*;

    #[test]
    fn test_cur_iterator() {
        let v = vec![1, 2];
        let inner_it = v.iter();

        let mut it = CurIterator::from_iterator(inner_it);

        assert_eq!(it.cur(), Some(&1));
        assert_eq!(it.advance(), ());
        assert_eq!(it.cur(), Some(&2));
        assert_eq!(it.advance(), ());

        assert_eq!(it.cur(), None);
        assert_eq!(it.advance(), ());
        assert_eq!(it.cur(), None);
    }

    #[test]
    fn test_blockgroup_instruction_iterator() {
        let blockgroup = parse_instructions(
            "@0: {
                $0 = $0
                $1 = $1
                exit = return $2
            }
            @1: {
                $2 = $12
                $3 = $13
                exit = return $2
            }",
        );

        let collected: Vec<_> = blockgroup
            .iter_all_instructions()
            .map(|(block, i, ins)| format!("@{}> ${} = {:?}", block, i, ins))
            .collect();

        insta::assert_debug_snapshot!(collected, @r###"
        [
            "@0> $0 = $0",
            "@0> $1 = $1",
            "@1> $2 = $12",
            "@1> $3 = $13",
        ]
        "###);
    }

    #[test]
    fn test_module_instruction_iterator() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = $0
                $1 = $1
                exit = return $2
            }
            @1: {
                $2 = $12
                $3 = $13
                exit = return $2
            }",
            "@0: {
                $4 = $4
                exit = return $2
            }",
        ]);

        let collected: Vec<_> = module
            .iter_all_instructions()
            .map(|(function, block, i, ins)| {
                format!("func {} > @{}> ${} = {:?}", function.0, block, i, ins)
            })
            .collect();

        insta::assert_debug_snapshot!(collected, @r###"
        [
            "func 0 > @0> $0 = $0",
            "func 0 > @0> $1 = $1",
            "func 0 > @1> $2 = $12",
            "func 0 > @1> $3 = $13",
            "func 1 > @0> $4 = $4",
        ]
        "###);
    }

    #[test]
    fn test_module_block_iterator() {
        let module = parse_instructions_module(vec![
            "@0: {
                $0 = $0
                $1 = $1
                exit = return $2
            }
            @1: {
                $2 = $12
                $3 = $13
                exit = return $2
            }",
            "@0: {
                $4 = $4
                exit = return $2
            }",
        ]);

        let collected: Vec<_> = module
            .iter_all_blocks()
            .map(|(function, block, _)| format!("func {} > @{}", function.0, block))
            .collect();

        insta::assert_debug_snapshot!(collected, @r###"
        [
            "func 0 > @0",
            "func 0 > @1",
            "func 1 > @0",
        ]
        "###);
    }
}
