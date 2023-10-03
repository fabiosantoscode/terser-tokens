use std::collections::HashSet;
use std::fmt::{Debug, Formatter};

use crate::basic_blocks::ExitType;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Default, PartialOrd, Ord)]
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
    pub fn simplify(self) -> Self {
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

    pub fn children(&self) -> Vec<&Vec<StructuredFlow>> {
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
