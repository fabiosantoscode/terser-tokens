use std::collections::{BTreeMap, HashSet};
use std::fmt::{Debug, Formatter};

use crate::basic_blocks::{ClassProperty, FunctionId, Instruction};

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

#[derive(Clone, PartialEq)]
pub enum StructuredFlow {
    Block(BreakableId, Vec<StructuredFlow>),
    Loop(BreakableId, Vec<StructuredFlow>),
    ForInOfLoop(BreakableId, usize, ForInOfKind, Vec<StructuredFlow>),
    Cond(BreakableId, usize, Vec<StructuredFlow>, Vec<StructuredFlow>),
    /// (breakable_id, condition, structured_switch_cases)
    Switch(BreakableId, usize, Vec<StructuredSwitchCase>),
    TryCatch(
        BreakableId,
        Vec<StructuredFlow>,
        Vec<StructuredFlow>,
        Vec<StructuredFlow>,
    ),
    Break(BreakableId),
    Continue(BreakableId),
    Return(ExitType, usize),
    Instruction(usize, Instruction),
    /// (class_var, class_members)
    Class(usize, Vec<StructuredClassMember>),
    Debugger,
}

/// A "thing" inside a class
#[derive(Clone, Debug, PartialEq)]
pub enum StructuredClassMember {
    Constructor(FunctionId),
    Property(Vec<StructuredFlow>, ClassProperty),
    StaticBlock(Vec<StructuredFlow>),
}
impl StructuredClassMember {
    pub(crate) fn flows_mut(&mut self) -> Vec<&mut Vec<StructuredFlow>> {
        match self {
            StructuredClassMember::Constructor(_) => {
                vec![]
            }
            StructuredClassMember::Property(v, _) => {
                vec![v]
            }
            StructuredClassMember::StaticBlock(v) => {
                vec![v]
            }
        }
    }
}

impl Default for StructuredFlow {
    fn default() -> Self {
        StructuredFlow::Block(BreakableId(None), vec![])
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct StructuredSwitchCase {
    pub condition: Option<(Vec<StructuredFlow>, usize)>,
    pub body: Vec<StructuredFlow>,
}

#[repr(u8)]
#[derive(Clone, Debug, PartialEq, Copy)]
pub enum ExitType {
    Return,
    Throw,
}

#[repr(u8)]
#[derive(Clone, PartialEq, Copy)]
pub enum ForInOfKind {
    ForIn,
    ForOf,
    ForAwaitOf,
}

impl StructuredFlow {
    #[cfg(test)]
    fn str_head(&self) -> String {
        match self {
            StructuredFlow::Cond(_, _, _, _) => "Cond".to_string(),
            StructuredFlow::Switch(_, _, _) => "Switch".to_string(),
            StructuredFlow::Break(_) => "Break".to_string(),
            StructuredFlow::Continue(_) => "Continue".to_string(),
            StructuredFlow::Loop(_, _) => "Loop".to_string(),
            StructuredFlow::ForInOfLoop(_, _, _, _) => "ForInOfLoop".to_string(),
            StructuredFlow::Block(_, _) => "Block".to_string(),
            StructuredFlow::Return(_, _) => "Return".to_string(),
            StructuredFlow::Instruction(_, _) => "Instruction".to_string(),
            StructuredFlow::TryCatch(_, _, _, _) => "TryCatch".to_string(),
            StructuredFlow::Class(_, _) => "Class".to_string(),
            StructuredFlow::Debugger => "Debugger".to_string(),
        }
    }
    pub fn from_vec(items: Vec<StructuredFlow>) -> StructuredFlow {
        match items.len() {
            1 => match items.into_iter().next().unwrap() {
                StructuredFlow::Block(BreakableId(None), items) => Self::from_vec(items),
                item => item,
            },
            _ => StructuredFlow::Block(BreakableId(None), items),
        }
    }
    pub fn simplify(mut self) -> Self {
        let break_targets = self.get_all_break_targets();
        self.remove_unused_break_ids(&break_targets);
        Self::from_vec(self.simplify_inner())
    }
    pub fn simplify_vec(mut items: Vec<StructuredFlow>) -> Vec<StructuredFlow> {
        let break_targets = items
            .iter()
            .flat_map(|item| item.get_all_break_targets())
            .collect();
        items
            .iter_mut()
            .for_each(|item| item.remove_unused_break_ids(&break_targets));

        Self::simplify_inner_vec(items)
    }
    fn simplify_inner(self) -> Vec<StructuredFlow> {
        use StructuredFlow::*;

        let map = |items: Vec<StructuredFlow>| -> Vec<StructuredFlow> {
            StructuredFlow::simplify_inner_vec(items)
        };

        match self {
            Block(brk, items) => {
                let items = map(items);
                if items.len() == 0 {
                    vec![]
                } else if brk.0 == None {
                    items
                } else {
                    vec![Block(brk, items)]
                }
            }
            Cond(id, cond, cons, alt) => vec![Cond(id, cond, map(cons), map(alt))],
            TryCatch(id, try_, catch, finally) => {
                vec![TryCatch(id, map(try_), map(catch), map(finally))]
            }
            Loop(id, items) => vec![Loop(id, map(items))],
            Class(class_var, items) => vec![Class(
                class_var,
                items
                    .into_iter()
                    .map(|class_item| match class_item {
                        StructuredClassMember::StaticBlock(items) => {
                            StructuredClassMember::StaticBlock(map(items))
                        }
                        no_children => no_children,
                    })
                    .collect(),
            )],
            no_children => vec![no_children],
        }
    }
    fn simplify_inner_vec(items: Vec<StructuredFlow>) -> Vec<StructuredFlow> {
        use StructuredFlow::*;

        items
            .into_iter()
            .flat_map(|item| item.simplify_inner())
            .fold(vec![], |mut acc, item| {
                let prev = acc.last_mut();
                match (prev, item) {
                    (Some(Block(BreakableId(None), prev)), Block(BreakableId(None), items)) => {
                        prev.extend(items.into_iter());
                    }
                    (_, Block(BreakableId(None), items)) => {
                        acc.extend(items);
                    }
                    (_, item) => {
                        acc.push(item);
                    }
                }

                acc
            })
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

    pub fn children(&self) -> Vec<Vec<&StructuredFlow>> {
        match self {
            StructuredFlow::Cond(_id, _x /* who cares */, y, z) => {
                vec![y.iter().collect(), z.iter().collect()]
            }
            StructuredFlow::Switch(_, _, cases) => cases
                .iter()
                .flat_map(|case| match &case.condition {
                    Some((cond, _)) => vec![cond.iter().collect(), case.body.iter().collect()],
                    None => vec![case.body.iter().collect()],
                })
                .collect(),
            StructuredFlow::Break(_) => vec![],
            StructuredFlow::Continue(_) => vec![],
            StructuredFlow::Loop(_, body) => vec![body.iter().collect()],
            StructuredFlow::ForInOfLoop(_, _, _, body) => vec![body.iter().collect()],
            StructuredFlow::Block(_, body) => vec![body.iter().collect()],
            StructuredFlow::Return(_, _) => vec![],
            StructuredFlow::Instruction(_, _) => vec![],
            StructuredFlow::TryCatch(_, t, v, fin) => {
                vec![t.iter().collect(), v.iter().collect(), fin.iter().collect()]
            }
            StructuredFlow::Class(_, items) => {
                vec![items
                    .iter()
                    .flat_map(|item| match item {
                        StructuredClassMember::StaticBlock(items) => Some(items.iter()),
                        StructuredClassMember::Property(children, _) => Some(children.iter()),
                        StructuredClassMember::Constructor(_) => None,
                    })
                    .flatten()
                    .collect()]
            }
            StructuredFlow::Debugger => vec![],
        }
    }

    pub fn children_mut(&mut self) -> Vec<&mut Vec<StructuredFlow>> {
        match self {
            StructuredFlow::Cond(_id, _x /* who cares */, y, z) => {
                vec![y, z]
            }
            StructuredFlow::Switch(_id, _var, cases) => cases
                .iter_mut()
                .flat_map(|case| match &mut case.condition {
                    Some((insx, _var)) => vec![insx, &mut case.body],
                    None => vec![&mut case.body],
                })
                .collect(),
            StructuredFlow::Break(_) => vec![],
            StructuredFlow::Continue(_) => vec![],
            StructuredFlow::Loop(_, body) => vec![body],
            StructuredFlow::ForInOfLoop(_, _, _, body) => vec![body],
            StructuredFlow::Block(_, body) => vec![body],
            StructuredFlow::Return(_, _) => vec![],
            StructuredFlow::Instruction(_, _) => vec![],
            StructuredFlow::TryCatch(_, t, v, fin) => {
                vec![t, v, fin]
            }
            StructuredFlow::Class(_, items) => items
                .iter_mut()
                .flat_map(|item| match item {
                    StructuredClassMember::StaticBlock(items) => Some(items),
                    StructuredClassMember::Property(children, _) => Some(children),
                    StructuredClassMember::Constructor(_) => None,
                })
                .collect(),
            StructuredFlow::Debugger => vec![],
        }
    }

    /// Does this flow leave the current vector of structuredflows?
    pub(crate) fn aborts(&self) -> bool {
        match self {
            StructuredFlow::Block(_, _)
            | StructuredFlow::Loop(_, _)
            | StructuredFlow::ForInOfLoop(_, _, _, _)
            | StructuredFlow::Cond(_, _, _, _)
            | StructuredFlow::Switch(_, _, _)
            | StructuredFlow::TryCatch(_, _, _, _)
            | StructuredFlow::Instruction(_, _)
            | StructuredFlow::Class(_, _)
            | StructuredFlow::Debugger => false,
            StructuredFlow::Break(_)
            | StructuredFlow::Continue(_)
            | StructuredFlow::Return(_, _) => true,
        }
    }

    pub fn retain_instructions<DoRetain>(&mut self, do_retain: &mut DoRetain) -> ()
    where
        DoRetain: FnMut((usize, &Instruction)) -> bool,
    {
        match self {
            StructuredFlow::Instruction(varname, ins) => {
                if !do_retain((*varname, ins)) {
                    *self = Default::default();
                }
            }
            _ => {
                for children in self.children_mut().iter_mut() {
                    children.retain_mut(|child| match child {
                        StructuredFlow::Instruction(varname, ins) => do_retain((*varname, ins)),
                        _ => {
                            child.retain_instructions(do_retain);
                            true
                        }
                    })
                }
            }
        }
    }

    pub fn retain_instructions_mut<DoRetain>(&mut self, do_retain: &mut DoRetain) -> ()
    where
        DoRetain: FnMut((usize, &mut Instruction)) -> bool,
    {
        match self {
            StructuredFlow::Instruction(varname, ins) => {
                if !do_retain((*varname, ins)) {
                    *self = Default::default();
                }
            }
            _ => {
                for children in self.children_mut().iter_mut() {
                    children.retain_mut(|child| match child {
                        StructuredFlow::Instruction(varname, ins) => do_retain((*varname, ins)),
                        _ => {
                            child.retain_instructions_mut(do_retain);
                            true
                        }
                    })
                }
            }
        }
    }

    pub fn retain_blocks_mut<DoRetain>(&mut self, do_retain: &mut DoRetain) -> bool
    where
        DoRetain: FnMut(&mut StructuredFlow) -> bool,
    {
        if !do_retain(self) {
            *self = Default::default();
            false
        } else {
            for children in self.children_mut().iter_mut() {
                Self::retain_blocks_vec_mut(children, do_retain);
            }
            true
        }
    }

    pub fn retain_blocks_vec_mut<DoRetain>(
        children: &mut Vec<StructuredFlow>,
        do_retain: &mut DoRetain,
    ) where
        DoRetain: FnMut(&mut StructuredFlow) -> bool,
    {
        children.retain_mut(|child| child.retain_blocks_mut(do_retain));
    }

    fn get_all_break_targets(&self) -> HashSet<BreakableId> {
        self.iter_all_flows()
            .flat_map(|flow| flow.breaks_to_id())
            .collect()
    }

    pub fn count_all_break_targets(&self) -> BTreeMap<BreakableId, usize> {
        self.iter_all_flows()
            .flat_map(|flow| flow.breaks_to_id())
            .fold(BTreeMap::new(), |mut map, id| {
                *map.entry(id).or_insert(0) += 1;
                map
            })
    }

    pub fn breaks_to_id(&self) -> Option<BreakableId> {
        match self {
            StructuredFlow::Break(id) | StructuredFlow::Continue(id) => Some(*id),
            _ => None,
        }
    }
    fn remove_break_id(&mut self) {
        match self {
            StructuredFlow::Cond(id, _, _, _)
            | StructuredFlow::ForInOfLoop(id, _, _, _)
            | StructuredFlow::Loop(id, _)
            | StructuredFlow::Block(id, _)
            | StructuredFlow::Switch(id, _, _)
            | StructuredFlow::TryCatch(id, _, _, _) => *id = BreakableId(None),
            StructuredFlow::Break(_)
            | StructuredFlow::Continue(_)
            | StructuredFlow::Return(_, _)
            | StructuredFlow::Instruction(_, _)
            | StructuredFlow::Class(_, _)
            | StructuredFlow::Debugger => {}
        }
    }

    fn breakable_id(&self) -> Option<BreakableId> {
        match self {
            StructuredFlow::Block(id, _)
            | StructuredFlow::ForInOfLoop(id, _, _, _)
            | StructuredFlow::Cond(id, _, _, _)
            | StructuredFlow::Switch(id, _, _)
            | StructuredFlow::Break(id)
            | StructuredFlow::Continue(id)
            | StructuredFlow::Loop(id, _)
            | StructuredFlow::TryCatch(id, _, _, _) => Some(*id),
            StructuredFlow::Return(_, _)
            | StructuredFlow::Instruction(_, _)
            | StructuredFlow::Class(_, _)
            | StructuredFlow::Debugger => None,
        }
    }

    /// Retrieves what variables this block reads, not including instructions.
    /// Careful: classes and switch statements will return vars that may be defined inside them
    pub(crate) fn used_vars(&self) -> Vec<usize> {
        match self {
            StructuredFlow::Cond(_, x, _, _) => vec![*x],
            StructuredFlow::Switch(_, exp, cases) => {
                let mut vars = vec![*exp];
                for case in cases {
                    if let Some((_insx, condvar)) = &case.condition {
                        vars.push(*condvar);
                    }
                }
                vars
            }
            StructuredFlow::Break(_) => vec![],
            StructuredFlow::Continue(_) => vec![],
            StructuredFlow::Loop(_, _) => vec![],
            StructuredFlow::ForInOfLoop(_, loop_var, _, _) => vec![*loop_var],
            StructuredFlow::Block(_, _) => vec![],
            StructuredFlow::Return(_, ret_val) => vec![*ret_val],
            StructuredFlow::Instruction(_, _) => vec![],
            StructuredFlow::TryCatch(_, _, _, _) => vec![],
            StructuredFlow::Class(class_var, members) => {
                let mut vars = vec![*class_var];
                for member in members {
                    match member {
                        StructuredClassMember::Property(_, prop) => {
                            vars.extend(prop.used_vars());
                        }
                        StructuredClassMember::StaticBlock(_)
                        | StructuredClassMember::Constructor(_) => {}
                    }
                }
                vars
            }
            StructuredFlow::Debugger => vec![],
        }
    }

    pub(crate) fn used_vars_mut(&mut self) -> Vec<&mut usize> {
        match self {
            StructuredFlow::Cond(_, x, _, _) => vec![x],
            StructuredFlow::Switch(_, exp, cases) => {
                let mut vars = vec![exp];
                for case in cases {
                    if let Some((_insx, condvar)) = &mut case.condition {
                        vars.push(condvar);
                    }
                }
                vars
            }
            StructuredFlow::Break(_) => vec![],
            StructuredFlow::Continue(_) => vec![],
            StructuredFlow::Loop(_, _) => vec![],
            StructuredFlow::ForInOfLoop(_, loop_var, _, _) => vec![loop_var],
            StructuredFlow::Block(_, _) => vec![],
            StructuredFlow::Return(_, ret_val) => vec![ret_val],
            StructuredFlow::Instruction(_, _) => vec![],
            StructuredFlow::TryCatch(_, _, _, _) => vec![],
            StructuredFlow::Class(class_var, members) => {
                let mut vars = vec![class_var];
                for member in members {
                    match member {
                        StructuredClassMember::Property(_, prop) => {
                            vars.extend(prop.used_vars_mut());
                        }
                        StructuredClassMember::StaticBlock(_)
                        | StructuredClassMember::Constructor(_) => {}
                    }
                }
                vars
            }
            StructuredFlow::Debugger => vec![],
        }
    }

    pub fn for_each_flow_mut<F>(&mut self, mut cb: F) -> ()
    where
        F: FnMut(&mut StructuredFlow),
    {
        let mut stack = vec![self];

        loop {
            if let Some(flow) = stack.pop() {
                cb(flow);
                stack.extend(flow.children_mut().into_iter().flatten().rev());
            } else {
                break;
            }
        }
    }
}

impl StructuredFlow {
    fn _all(items: &Vec<StructuredFlow>) -> bool {
        items.iter().all(|x| x.is_flow_empty())
    }

    pub fn is_flow_empty_vec(items: &Vec<StructuredFlow>) -> bool {
        Self::_all(items)
    }

    pub fn is_flow_empty(&self) -> bool {
        match self {
            StructuredFlow::Block(_, items) => Self::_all(items),
            StructuredFlow::Loop(_, body) => Self::_all(body),
            StructuredFlow::Cond(_, _, cons, alt) => Self::_all(cons) && Self::_all(alt),
            StructuredFlow::TryCatch(_, body, _, fin) => Self::_all(body) && Self::_all(fin),
            StructuredFlow::Instruction(..) => false,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::testutils::*;

    #[test]
    fn test_structured_flow_iter() {
        let structured = parse_test_flow(
            "{
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
        }",
        );

        let flat = structured
            .nested_iter()
            .enumerate()
            .map(|(i, s)| match s.children().len() {
                0 => format!("{}: {:?}", i, s),
                _ => format!("{}: {}", i, s.str_head()),
            })
            .collect::<Vec<_>>()
            .join("\n");

        insta::assert_snapshot!(flat, @r###"
        0: Block
        1: $0 = 123

        2: Cond
        3: $1 = 456

        4: Cond
        5: $2 = 7

        6: $3 = 8

        7: $4 = either($2, $3)

        8: $5 = 9

        9: $6 = either($4, $5)

        10: $7 = undefined

        11: Return $7
        "###);
    }
}
