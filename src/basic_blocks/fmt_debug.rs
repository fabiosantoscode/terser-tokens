use std::fmt::{Debug, Error, Formatter};

use crate::basic_blocks::{ForInOfKind, ObjectMember, ObjectProp};

use super::{
    ArrayElement, ArrayPatternPiece, BasicBlock, BasicBlockEnvironment, BasicBlockExit,
    BasicBlockGroup, BasicBlockInstruction, BasicBlockModule, ExitType, FunctionId, NonLocalId,
    ObjectPatternPiece,
};

impl Debug for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{\n")?;
        for (id, node) in self.iter() {
            write!(f, "    ${} = {:?}\n", id, node)?;
        }
        write!(f, "    exit = {:?}\n", &self.exit)?;
        write!(f, "}}")
    }
}

impl Debug for BasicBlockInstruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BasicBlockInstruction::LitNumber(num) => {
                write!(f, "{}", num)
            }
            BasicBlockInstruction::LitBool(b) => {
                write!(f, "{:?}", b)
            }
            BasicBlockInstruction::LitString(s) => {
                write!(f, "{:?}", s)
            }
            BasicBlockInstruction::UnaryOp(op, operand) => {
                write!(f, "{}${}", op, operand)
            }
            BasicBlockInstruction::BinOp(op, l, r) => {
                write!(f, "${} {} ${}", l, op, r)
            }
            BasicBlockInstruction::Ref(id) => {
                write!(f, "${}", id)
            }
            BasicBlockInstruction::Undefined => {
                write!(f, "undefined")
            }
            BasicBlockInstruction::Null => {
                write!(f, "null")
            }
            BasicBlockInstruction::This => {
                write!(f, "this")
            }
            BasicBlockInstruction::TypeOf(var) => {
                write!(f, "typeof ${}", var)
            }
            BasicBlockInstruction::TypeOfGlobal(var) => {
                write!(f, "typeof global {:?}", var)
            }
            BasicBlockInstruction::Array(elements) => {
                write!(
                    f,
                    "[{}]",
                    elements
                        .iter()
                        .map(|e| match e {
                            ArrayElement::Hole => format!(","),
                            ArrayElement::Item(id) => format!("${},", id),
                            ArrayElement::Spread(id) => format!("...${},", id),
                        })
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            BasicBlockInstruction::Object(proto, props) => {
                write!(
                    f,
                    "{{{}}}",
                    proto
                        .iter()
                        .map(|proto| { format!("__proto__: ${}", proto) })
                        .chain(props.iter().map(|e| match e {
                            ObjectProp::KeyValue(key, value) => format!("{}: ${}", key, value),
                            ObjectProp::Computed(key, value) => format!("[${}]: ${}", key, value),
                            ObjectProp::Spread(spread_obj) => format!("...{}", spread_obj),
                        }))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            BasicBlockInstruction::Member(base, member) => match member {
                ObjectMember::KeyValue(member) => write!(f, "${base}.{member}"),
                ObjectMember::Private(member) => write!(f, "${base}.#{member}"),
                ObjectMember::Computed(member) => write!(f, "${base}[${member}]"),
            },
            BasicBlockInstruction::MemberSet(base, member, value) => match member {
                ObjectMember::KeyValue(member) => write!(f, "${base}.{member} = ${value}"),
                ObjectMember::Private(member) => write!(f, "${base}.#{member} = ${value}"),
                ObjectMember::Computed(member) => write!(f, "${base}[${member}] = ${value}"),
            },
            BasicBlockInstruction::ArrayPattern(input, items) => write!(
                f,
                "pack ${} [{}]",
                input,
                items
                    .iter()
                    .map(|item| {
                        match item {
                            ArrayPatternPiece::Item => format!("_"),
                            ArrayPatternPiece::Spread => format!("_..."),
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            BasicBlockInstruction::ObjectPattern(input, props) => write!(
                f,
                "pack ${} {{{}}}",
                input,
                props
                    .iter()
                    .map(|prop| {
                        match prop {
                            ObjectPatternPiece::TakeKey(key) => format!("{}: _", key),
                            ObjectPatternPiece::TakeComputedKey(comp) => format!("[${}]: _", comp),
                            ObjectPatternPiece::Spread => format!("..._"),
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            BasicBlockInstruction::PatternUnpack(pattern, index) => {
                write!(f, "unpack ${}[{}]", pattern, index)
            }
            BasicBlockInstruction::Function(id) => {
                write!(f, "{:?}", id)
            }
            BasicBlockInstruction::Call(callee, args) => {
                write!(
                    f,
                    "call ${}({})",
                    callee,
                    args.iter()
                        .map(|a| format!("${}", a))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            BasicBlockInstruction::ArgumentRead(idx) => {
                write!(f, "arguments[{}]", idx)
            }
            BasicBlockInstruction::ArgumentRest(idx) => {
                write!(f, "arguments[{}...]", idx)
            }

            BasicBlockInstruction::ReadNonLocal(id) => {
                write!(f, "read_non_local $${}", id.0)
            }
            BasicBlockInstruction::WriteNonLocal(id, val) => {
                write!(f, "write_non_local $${} ${}", id.0, val)
            }

            BasicBlockInstruction::ReadGlobal(name) => {
                write!(f, "global {:?}", name)
            }
            BasicBlockInstruction::WriteGlobal(name, val) => {
                write!(f, "global {:?} = ${}", name, val)
            }

            BasicBlockInstruction::TempExit(exit_type, arg) => {
                write!(f, "{:?} ${}", exit_type, arg)
            }

            BasicBlockInstruction::Phi(vars) => {
                write!(
                    f,
                    "either({})",
                    vars.iter()
                        .map(|v| format!("${}", v))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            BasicBlockInstruction::CaughtError => {
                write!(f, "caught_error()")
            }
            BasicBlockInstruction::ForInOfValue => {
                write!(f, "for_in_of_value()")
            }
        }
    }
}

impl Debug for BasicBlockExit {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BasicBlockExit::Cond(cond, cons, cons_end, alt, alt_end) => {
                write!(
                    f,
                    "cond ${} ? @{}..@{} : @{}..@{}",
                    cond, cons, cons_end, alt, alt_end
                )
            }
            BasicBlockExit::Loop(start, end) => {
                write!(f, "loop @{}..@{}", start, end)
            }
            BasicBlockExit::ForInOfLoop(loop_var, kind, start, end) => match kind {
                ForInOfKind::ForIn => {
                    write!(f, "for in ${} @{}..@{}", loop_var, start, end)
                }
                ForInOfKind::ForOf => {
                    write!(f, "for of ${} @{}..@{}", loop_var, start, end)
                }
                ForInOfKind::ForAwaitOf => {
                    write!(f, "for await of ${} @{}..@{}", loop_var, start, end)
                }
            },
            BasicBlockExit::Jump(to) => {
                write!(f, "jump @{}", to)
            }
            BasicBlockExit::Break(to) => {
                write!(f, "break @{}", to)
            }
            BasicBlockExit::Continue(to) => {
                write!(f, "continue @{}", to)
            }
            BasicBlockExit::ExitFn(exit_type, val) => match exit_type {
                ExitType::Return => {
                    write!(f, "return ${}", val)
                }
                ExitType::Throw => {
                    write!(f, "throw ${}", val)
                }
            },
            BasicBlockExit::SetTryAndCatch(try_block, catch_block, finally_block, after_block) => {
                write!(
                    f,
                    "try @{} catch @{} finally @{} after @{}",
                    try_block, catch_block, finally_block, after_block
                )
            }
            BasicBlockExit::PopCatch(catch_block, finally_or_after) => {
                write!(
                    f,
                    "error ? jump @{} : jump @{}",
                    catch_block, finally_or_after
                )
            }
            BasicBlockExit::PopFinally(finally_block, after_block) => {
                write!(f, "finally @{} after @{}", finally_block, after_block)
            }
            BasicBlockExit::EndFinally(after_block) => {
                write!(f, "end finally after @{}", after_block)
            }
        }
    }
}

impl Debug for BasicBlockModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let BasicBlockModule {
            summary,
            functions,
            imports,
            exports,
        } = self;

        let functions: Vec<_> = functions.iter().map(|(_, v)| v).collect();

        let (top_level_stats, functions) = functions.split_first().unwrap();

        let mut d = f.debug_struct("BasicBlockModule");

        d.field("summary", &summary);
        d.field("top_level_stats", &top_level_stats);

        if !functions.is_empty() {
            d.field("functions", &functions);
        }
        if !imports.is_empty() {
            d.field("imports", &self.imports);
        }
        if !exports.is_empty() {
            d.field("exports", &self.exports);
        }

        d.finish()
    }
}

impl Debug for FunctionId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "FunctionId({})", self.0)
    }
}

impl Debug for NonLocalId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "NonLocalId({})", self.0)
    }
}

impl Debug for BasicBlockGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.environment {
            BasicBlockEnvironment::Module => {}
            BasicBlockEnvironment::Function(is_generator, is_async) => {
                match (is_generator, is_async) {
                    (false, false) => writeln!(f, "function():")?,
                    (true, false) => writeln!(f, "function*():")?,
                    (false, true) => writeln!(f, "async function():")?,
                    (true, true) => writeln!(f, "async function*():")?,
                }
            }
        }
        for (i, v) in self.iter() {
            if i > 0 {
                write!(f, "\n")?;
            }
            write!(f, "@{}: {:?}", i, v)?;
        }
        Ok(())
    }
}
