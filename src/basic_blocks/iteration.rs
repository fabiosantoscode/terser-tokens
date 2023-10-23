use super::{
    BasicBlock, BasicBlockGroup, BasicBlockInstruction, BasicBlockModule, FunctionId,
    StructuredFlow,
};

impl BasicBlockModule {
    pub fn iter_all_instructions<'a>(
        &'a self,
    ) -> impl Iterator<Item = (FunctionId, usize, usize, &'a BasicBlockInstruction)> {
        self.iter().flat_map(|(func_id, block_group)| {
            block_group.iter().flat_map(move |(block_id, block)| {
                block
                    .iter()
                    .map(move |(varname, ins)| (func_id, block_id, varname, ins))
            })
        })
    }

    pub fn iter_all_instructions_mut<'a>(
        &'a mut self,
    ) -> impl Iterator<Item = (FunctionId, usize, usize, &'a mut BasicBlockInstruction)> {
        self.iter_mut().flat_map(|(func_id, block_group)| {
            block_group.iter_mut().flat_map(move |(block_id, block)| {
                block
                    .iter_mut()
                    .map(move |(varname, ins)| (func_id, block_id, varname, ins))
            })
        })
    }

    pub fn iter_all_blocks<'a>(
        &'a self,
    ) -> impl Iterator<Item = (FunctionId, usize, &'a BasicBlock)> {
        self.iter().flat_map(|(function_id, block_group)| {
            block_group
                .iter()
                .map(move |(block_id, block)| (function_id, block_id, block))
        })
    }
}

impl BasicBlockGroup {
    pub fn iter_all_instructions<'a>(
        &'a self,
    ) -> impl Iterator<Item = (usize, usize, &'a BasicBlockInstruction)> {
        self.iter().flat_map(move |(block_id, block)| {
            block
                .iter()
                .map(move |(varname, ins)| (block_id, varname, ins))
        })
    }

    pub fn iter_all_instructions_mut<'a>(
        &'a mut self,
    ) -> impl Iterator<Item = (usize, usize, &'a mut BasicBlockInstruction)> {
        self.iter_mut().flat_map(move |(block_id, block)| {
            block
                .iter_mut()
                .map(move |(varname, ins)| (block_id, varname, ins))
        })
    }
}

impl StructuredFlow {
    pub fn nested_iter<'a>(&'a self) -> impl Iterator<Item = &'a StructuredFlow> {
        StructuredFlowIter { stack: vec![self] }
    }

    pub fn iter_all_blocks<'a>(
        &'a self,
    ) -> impl Iterator<Item = &'a Vec<(usize, BasicBlockInstruction)>> {
        self.nested_iter().flat_map(|flow| match flow {
            StructuredFlow::BasicBlock(block) => Some(block),
            _ => None,
        })
    }

    pub fn iter_all_instructions<'a>(
        &'a self,
    ) -> impl Iterator<Item = (usize, &'a BasicBlockInstruction)> {
        self.iter_all_blocks()
            .flat_map(|block| block.iter().map(|(varname, ins)| (*varname, ins)))
    }
}

struct StructuredFlowIter<'a> {
    stack: Vec<&'a StructuredFlow>,
}
impl<'a> Iterator for StructuredFlowIter<'a> {
    type Item = &'a StructuredFlow;
    fn next(&mut self) -> Option<&'a StructuredFlow> {
        let next = self.stack.pop()?;
        self.stack.extend(next.children().iter().flatten().rev());
        Some(next)
    }
}

#[cfg(test)]
mod tests {
    use crate::testutils::*;

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
