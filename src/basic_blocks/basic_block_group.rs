use std::{fmt::Debug, iter::Enumerate};

use super::basic_block::{self, BasicBlock};

#[derive(Default)]
pub struct BasicBlockGroup {
    pub asts: Vec<BasicBlock>,
}

impl BasicBlockGroup {
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    pub fn from_asts(asts: Vec<BasicBlock>) -> Self {
        Self { asts }
    }

    pub fn iter<'a>(&'a self) -> Enumerate<std::slice::Iter<'_, basic_block::BasicBlock>> {
        self.asts.iter().enumerate()
    }
}

impl Debug for BasicBlockGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (k, v) in self.iter() {
            writeln!(f, "@{}: {:?}", k, v)?;
        }
        Ok(())
    }
}
