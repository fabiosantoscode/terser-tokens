use super::{FunctionId, StructuredFlow};

#[derive(Clone)]
pub struct StructuredFunction {
    pub id: FunctionId,
    pub blocks: Vec<StructuredFlow>,
    pub environment: BasicBlockEnvironment,
}

#[derive(Default, Clone, Debug)]
pub enum BasicBlockEnvironment {
    #[default]
    Module,
    /// (is_generator, is_async)
    Function(bool, bool),
}

impl StructuredFunction {
    pub fn for_each_flow_mut<F>(&mut self, mut cb: F) -> ()
    where
        F: FnMut(&mut StructuredFlow),
    {
        for flow in self.blocks.iter_mut() {
            flow.for_each_flow_mut(|blk| cb(blk));
        }
    }
}

impl BasicBlockEnvironment {
    pub fn unwrap_function(&self) -> (bool, bool) {
        match self {
            BasicBlockEnvironment::Function(is_generator, is_async) => (*is_generator, *is_async),
            _ => panic!("not a function"),
        }
    }
}
