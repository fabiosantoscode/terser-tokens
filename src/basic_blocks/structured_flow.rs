use std::collections::HashSet;
use std::fmt::{Debug, Formatter};

use crate::basic_blocks::{BasicBlockInstruction, ClassProperty, ExitType, FunctionId};

use super::ForInOfKind;

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
    Instruction(usize, BasicBlockInstruction),
    /// (class_var, class_members)
    Class(usize, Vec<StructuredClassMember>),
    Debugger,
}

/// A "thing" inside a class
#[derive(Clone, Debug)]
pub enum StructuredClassMember {
    Constructor(FunctionId),
    Property(Vec<StructuredFlow>, ClassProperty),
    StaticBlock(Vec<StructuredFlow>),
}

impl Default for StructuredFlow {
    fn default() -> Self {
        StructuredFlow::Block(BreakableId(None), vec![])
    }
}

#[derive(Clone, Debug, Default)]
pub struct StructuredSwitchCase {
    pub condition: Option<(Vec<StructuredFlow>, usize)>,
    pub body: Vec<StructuredFlow>,
}

// For printing out these trees

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
            1 => items.into_iter().next().unwrap(),
            _ => StructuredFlow::Block(BreakableId(None), items),
        }
    }
    pub fn simplify(self) -> Self {
        let break_targets = self.get_all_break_targets();
        let mut flat = self.flatten();
        flat.remove_unused_break_ids(&break_targets);
        flat
    }
    pub fn simplify_vec(items: Vec<StructuredFlow>) -> Vec<StructuredFlow> {
        let break_targets = items
            .iter()
            .flat_map(|item| item.get_all_break_targets())
            .collect();

        Self::flatten_vec(
            items
                .into_iter()
                .map(|item| {
                    let mut item = item.flatten();
                    item.remove_unused_break_ids(&break_targets);
                    item
                })
                .collect(),
        )
    }
    pub fn flatten_vec(items: Vec<StructuredFlow>) -> Vec<StructuredFlow> {
        use StructuredFlow::*;

        items
            .into_iter()
            .flat_map(|item| match item {
                Block(BreakableId(None), items) => StructuredFlow::simplify_vec(items),
                _ => vec![item.flatten()],
            })
            .fold(vec![], |mut acc, item| {
                let prev = acc.last_mut();
                match (prev, item) {
                    (Some(Block(BreakableId(None), prev)), Block(BreakableId(None), items)) => {
                        prev.extend(items.into_iter());
                    }
                    (_, item) => {
                        acc.push(item);
                    }
                }

                acc
            })
    }
    fn flatten(self) -> StructuredFlow {
        use StructuredFlow::*;

        let map = |items: Vec<StructuredFlow>| -> Vec<StructuredFlow> {
            StructuredFlow::simplify_vec(items)
        };

        match self {
            Block(brk, items) => {
                let items = map(items);
                if items.len() == 1 && brk.0 == None {
                    items.into_iter().next().unwrap()
                } else {
                    Block(brk, items)
                }
            }
            Cond(id, cond, cons, alt) => Cond(id, cond, map(cons), map(alt)),
            TryCatch(id, try_, catch, finally) => TryCatch(id, map(try_), map(catch), map(finally)),
            Loop(id, items) => Loop(id, map(items)),
            Class(class_var, items) => Class(
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
            ),
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

    pub fn count_instructions(&self) -> usize {
        match self {
            StructuredFlow::Instruction(_, _) => 1,
            _ => self
                .children()
                .into_iter()
                .flat_map(|children| children.into_iter())
                .map(|child| child.count_instructions())
                .sum(),
        }
    }

    pub fn retain_instructions<DoRetain>(&mut self, do_retain: &mut DoRetain) -> ()
    where
        DoRetain: FnMut((usize, &BasicBlockInstruction)) -> bool,
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
            StructuredFlow::Cond(id, _, _, _) | StructuredFlow::Loop(id, _) => {
                *id = BreakableId(None)
            }
            _ => {}
        }
    }

    fn breakable_id(&self) -> Option<BreakableId> {
        match self {
            StructuredFlow::Block(id, _)
            | StructuredFlow::Cond(id, _, _, _)
            | StructuredFlow::Break(id)
            | StructuredFlow::Continue(id)
            | StructuredFlow::Loop(id, _)
            | StructuredFlow::TryCatch(id, _, _, _) => Some(*id),
            _ => None,
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
}

impl Debug for StructuredFlow {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let indent_str_lines = |s: &str| {
            let lines = s.lines();
            let indented_lines = lines.map(|line| format!("    {}", line));
            indented_lines.collect::<Vec<String>>().join("\n")
        };

        let print_brk = |brk: &BreakableId| match brk.0 {
            Some(x) => format!(" (@{})", x),
            None => String::new(),
        };

        let print_vec_no_eol = |f: &mut Formatter<'_>, items: &Vec<StructuredFlow>| {
            write!(f, "{{\n")?;
            for item in items {
                writeln!(f, "{}", indent_str_lines(&format!("{:?}", item)))?;
            }
            write!(f, "}}")
        };
        let print_vec = |f: &mut Formatter<'_>, items: &Vec<StructuredFlow>| {
            print_vec_no_eol(f, items)?;
            write!(f, "\n")
        };

        match self {
            StructuredFlow::Block(brk, items) => {
                if let Some(_) = brk.0 {
                    write!(f, "block{} {{", print_brk(brk))?;
                    print_vec(f, items)?;
                    write!(f, "}}\n")?;
                }
                print_vec(f, items)
            }
            StructuredFlow::Loop(brk, body) => {
                write!(f, "loop{} ", print_brk(brk))?;
                print_vec(f, body)
            }
            StructuredFlow::ForInOfLoop(brk, loop_var, kind, body) => {
                match kind {
                    ForInOfKind::ForIn => {
                        write!(f, "for in{} (${}) ", print_brk(brk), loop_var)?;
                    }
                    ForInOfKind::ForOf => {
                        write!(f, "for of{} (${}) ", print_brk(brk), loop_var)?;
                    }
                    ForInOfKind::ForAwaitOf => {
                        write!(f, "for await of{} (${}) ", print_brk(brk), loop_var)?;
                    }
                }
                print_vec(f, body)
            }
            StructuredFlow::Cond(brk, cond, cons, alt) => {
                write!(f, "if{} (${}) ", print_brk(brk), cond)?;
                print_vec_no_eol(f, cons)?;
                write!(f, " else ")?;
                print_vec(f, alt)
            }
            StructuredFlow::Switch(brk, expression, cases) => {
                write!(f, "switch{} (${}) ", print_brk(brk), expression)?;
                for case in cases {
                    match &case.condition {
                        Some((instructions, test_var)) => {
                            if instructions.len() > 0 {
                                print_vec_no_eol(f, &instructions)?;
                            }
                            write!(f, "case (${}) ", test_var)?;
                            print_vec_no_eol(f, &case.body)?;
                        }
                        None => {
                            write!(f, "default: ")?;
                            print_vec_no_eol(f, &case.body)?;
                        }
                    }
                }

                Ok(())
            }
            StructuredFlow::TryCatch(brk, body, catch, fin) => {
                write!(f, "try{} ", print_brk(brk))?;
                print_vec_no_eol(f, body)?;
                write!(f, " catch ")?;
                print_vec_no_eol(f, catch)?;
                write!(f, " finally ")?;
                print_vec(f, fin)
            }
            StructuredFlow::Class(class_var, body) => {
                write!(f, "class ${class_var} {{\n")?;
                for item in body {
                    writeln!(f, "{}", indent_str_lines(&format!("{:?}", item)))?;
                }
                write!(f, "}}\n")
            }
            StructuredFlow::Break(brk) => writeln!(f, "Break{}", print_brk(brk)),
            StructuredFlow::Continue(brk) => writeln!(f, "Continue{}", print_brk(brk)),
            StructuredFlow::Return(exit, ret) => {
                writeln!(f, "{:?} ${}", exit, ret)
            }
            StructuredFlow::Instruction(var, ins) => {
                let mut buf = String::new();
                buf.push_str(&format!("${} = {:?}\n", var, ins));
                write!(f, "{}", &buf)
            }
            StructuredFlow::Debugger => writeln!(f, "Debugger"),
        }
    }
}

impl StructuredFlow {
    fn _all(items: &Vec<StructuredFlow>) -> bool {
        items.iter().all(|x| x.is_structured_flow_empty())
    }

    pub fn is_structured_flow_vec_empty(items: &Vec<StructuredFlow>) -> bool {
        Self::_all(items)
    }

    pub fn is_structured_flow_empty(&self) -> bool {
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
        let structured = parse_structured_flow(
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
