use super::{FunctionId, Instruction, StructuredFlow, StructuredFunction, StructuredModule};

impl StructuredFlow {
    pub fn nested_iter<'a>(&'a self) -> impl Iterator<Item = &'a StructuredFlow> {
        StructuredFlowIter { stack: vec![self] }
    }

    pub fn iter_all_instructions<'a>(&'a self) -> impl Iterator<Item = (usize, &'a Instruction)> {
        self.nested_iter().flat_map(|block| match block {
            StructuredFlow::Instruction(varname, ins) => Some((*varname, ins)),
            _ => None,
        })
    }

    pub fn iter_all_flows<'a>(&'a self) -> impl Iterator<Item = &'a StructuredFlow> {
        self.nested_iter()
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

impl StructuredFunction {
    pub fn iter_all_instructions<'a>(&'a self) -> impl Iterator<Item = (usize, &'a Instruction)> {
        self.blocks
            .iter()
            .flat_map(|block| block.iter_all_instructions())
    }

    pub fn iter_all_flows<'a>(&'a self) -> impl Iterator<Item = &'a StructuredFlow> {
        self.blocks.iter().flat_map(|block| block.iter_all_flows())
    }
}

impl StructuredModule {
    pub fn iter_all_instructions<'a>(
        &'a self,
    ) -> impl Iterator<Item = (FunctionId, usize, &'a Instruction)> {
        self.iter().flat_map(|(func_id, func)| {
            func.iter_all_instructions()
                .map(move |(varname, ins)| (func_id, varname, ins))
        })
    }

    pub fn iter_all_flows<'a>(&'a self) -> impl Iterator<Item = (FunctionId, &'a StructuredFlow)> {
        self.iter()
            .flat_map(|(func_id, func)| func.iter_all_flows().map(move |flow| (func_id, flow)))
    }
}

#[cfg(test)]
mod tests {
    use crate::testutils::*;

    #[test]
    fn test_module_instruction_iterator() {
        let module = parse_test_module(vec![
            "{
                $0 = $0
                $1 = $1
                Return $2
                $2 = $12
                $3 = $13
                Return $2
            }",
            "{
                $4 = $4
                Return $2
            }",
        ]);

        let collected: Vec<_> = module
            .iter_all_instructions()
            .map(|(function, i, ins)| format!("func {} > ${} = {:?}", function.0, i, ins))
            .collect();

        insta::assert_debug_snapshot!(collected, @r###"
        [
            "func 0 > $0 = $0",
            "func 0 > $1 = $1",
            "func 0 > $2 = $12",
            "func 0 > $3 = $13",
            "func 1 > $4 = $4",
        ]
        "###);
    }
}
