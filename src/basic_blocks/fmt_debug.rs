use std::fmt::{Debug, Error, Formatter};

use super::{
    ArrayElement, BasicBlock, BasicBlockEnvironmentType, BasicBlockExit, BasicBlockGroup,
    BasicBlockInstruction, BasicBlockModule, ExitType, FunctionId, NonLocalId,
};

impl Debug for BasicBlock {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{\n")?;
        for (id, node) in &self.instructions {
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
            BasicBlockInstruction::BinOp(op, l, r) => {
                write!(f, "${} {} ${}", l, op, r)
            }
            BasicBlockInstruction::Ref(id) => {
                write!(f, "${}", id)
            }
            BasicBlockInstruction::Undefined => {
                write!(f, "undefined")
            }
            BasicBlockInstruction::This => {
                write!(f, "this")
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
        match self.environment.env_type {
            BasicBlockEnvironmentType::Module => {}
            BasicBlockEnvironmentType::Function(_argc) => writeln!(f, "function():")?,
        }
        for (i, v) in self.iter().enumerate() {
            if i > 0 {
                write!(f, "\n")?;
            }
            write!(f, "@{}: {:?}", i, v)?;
        }
        Ok(())
    }
}
