use std::fmt::{Debug, Error, Formatter};

use crate::basic_blocks::{ForInOfKind, MethodKind, ObjectKey, ObjectProperty, ObjectValue};

use super::{
    ArrayElement, ArrayPatternPiece, BasicBlock, BasicBlockEnvironment, BasicBlockExit,
    BasicBlockGroup, BasicBlockInstruction, BasicBlockModule, ExitType, FunctionId, NonLocalId,
    ObjectPatternPiece, LHS,
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

impl Debug for LHS {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LHS::Local(var) => write!(f, "${}", var),
            LHS::NonLocal(nonloc) => write!(f, "$${}", nonloc.0),
            LHS::Global(str) => {
                write!(f, "globalThis.{}", str)
            }
            LHS::Member(base, member) => {
                write!(f, "{:?}{:?}", base, member)
            }
        }
    }
}

impl Debug for ObjectKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectKey::NormalKey(member) => write!(f, ".{}", member),
            ObjectKey::Private(member) => write!(f, ".#{}", member),
            ObjectKey::Computed(member) => write!(f, "[${}]", member),
        }
    }
}

impl Debug for ObjectValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectValue::Property(varname) => write!(f, "${:?}", varname),
            ObjectValue::Method(kind, fn_id) => match kind {
                MethodKind::Method => write!(f, "method {:?}", fn_id),
                MethodKind::Getter => write!(f, "getter {:?}", fn_id),
                MethodKind::Setter => write!(f, "setter {:?}", fn_id),
            },
        }
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
            BasicBlockInstruction::IncrDecr(lhs, op) => {
                write!(f, "{}{:?}", op.op_string(), lhs)
            }
            BasicBlockInstruction::IncrDecrPostfix(lhs, op) => {
                write!(f, "{:?}{}", lhs, op.op_string())
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
                            ArrayElement::Hole => format!(""),
                            ArrayElement::Item(id) => format!("${}", id),
                            ArrayElement::Spread(id) => format!("...${}", id),
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            BasicBlockInstruction::Object(proto, props) => {
                let keys = proto
                    .iter()
                    .map(|proto| format!("__proto__: ${}", proto))
                    .chain(props.iter().map(|e| match e {
                        ObjectProperty::KeyValue(key, value) => {
                            let key = match key {
                                ObjectKey::NormalKey(key) => format!("{}", key),
                                ObjectKey::Computed(key) => format!("[${}]", key),
                                ObjectKey::Private(_) => unreachable!(),
                            };
                            format!("{}: {:?}", key, value)
                        }
                        ObjectProperty::Spread(spread_obj) => format!("...${}", spread_obj),
                    }))
                    .collect::<Vec<_>>();

                write!(f, "{{{}}}", keys.join(", "))
            }
            BasicBlockInstruction::Super => {
                write!(f, "super")
            }
            BasicBlockInstruction::CreateClass(optional_extends) => {
                write!(
                    f,
                    "class{}",
                    optional_extends
                        .as_ref()
                        .map(|extends| format!(" extends ${}", extends))
                        .unwrap_or("".to_string())
                )
            }
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
            BasicBlockInstruction::New(constructor, args) => {
                write!(
                    f,
                    "new ${}({})",
                    constructor,
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

            BasicBlockInstruction::Read(lhs) => match lhs {
                LHS::Local(var) => write!(f, "read_local ${:?}", var),
                LHS::NonLocal(nonlocal) => {
                    write!(f, "read_non_local $${:?}", nonlocal.0)
                }
                LHS::Global(glob_name) => write!(f, "global {:?}", glob_name),
                LHS::Member(base, member) => write!(f, "{:?}{:?}", base, member),
            },
            BasicBlockInstruction::Write(lhs, val) => match lhs {
                LHS::Local(var) => write!(f, "write_local ${:?} ${}", var, val),
                LHS::NonLocal(nonlocal) => {
                    write!(f, "write_non_local $${:?} ${}", nonlocal.0, val)
                }
                LHS::Global(glob_name) => write!(f, "global {:?} = ${}", glob_name, val),
                LHS::Member(base, member) => write!(f, "{:?}{:?} = ${}", base, member, val),
            },

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
            BasicBlockExit::ClassStart(class_var, start, end) => {
                write!(f, "class ${} @{}..@{}", class_var, start, end)
            }
            BasicBlockExit::ClassConstructor(fn_id, next) => {
                write!(f, "class constructor {:?} after @{}", fn_id, next)
            }
            BasicBlockExit::ClassProperty(prop, next) => {
                write!(f, "class property {:?} after @{}", prop, next)
            }
            BasicBlockExit::ClassPushStaticBlock(start, end) => {
                write!(f, "class static block @{}..@{}", start, end)
            }
            BasicBlockExit::ClassPopStaticBlock(next) => {
                write!(f, "end static block after @{}", next)
            }
            BasicBlockExit::ClassEnd(next) => {
                write!(f, "class end after @{}", next)
            }
            BasicBlockExit::Debugger(next) => {
                write!(f, "debugger and jump @{}", next)
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

        let (top_level_stats, functions) = functions
            .split_first()
            .expect("there must be a first element in a module's list of functions");

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
