use std::fmt::{Debug, Error, Formatter};

use crate::basic_blocks::{ForInOfKind, MethodKind, ObjectKey, ObjectProperty, ObjectValue};

use super::{
    ArrayElement, ArrayPatternPiece, BasicBlockEnvironment, BreakableId, FunctionId, Instruction,
    LogicalCondKind, NonLocalId, ObjectPatternPiece, StructuredFlow, StructuredFunction,
    StructuredModule, LHS,
};

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

impl Debug for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::LitNumber(num) => {
                write!(f, "{}", num)
            }
            Instruction::LitBigInt(big_int) => {
                write!(f, "{}n", big_int)
            }
            Instruction::LitBool(b) => {
                write!(f, "{:?}", b)
            }
            Instruction::LitString(s) => {
                write!(f, "{:?}", s)
            }
            Instruction::LitRegExp(re, flags) => {
                write!(f, "/{}/{}", re, flags)
            }
            Instruction::TemplateString(tag, contents) => {
                if let Some(tag) = tag {
                    write!(f, "${}`", tag)?;
                } else {
                    write!(f, "`")?;
                }

                for (str, expr) in contents {
                    write!(f, "{}", str)?;
                    if let Some(expr) = expr {
                        write!(f, "${{${}}}", expr);
                    }
                }

                write!(f, "`")
            }
            Instruction::UnaryOp(op, operand) => {
                write!(f, "{}${}", op, operand)
            }
            Instruction::BinOp(op, l, r) => {
                write!(f, "${} {} ${}", l, op, r)
            }
            Instruction::PrivateIn(priv_name, expr) => {
                write!(f, "#{} in ${}", priv_name, expr)
            }
            Instruction::IncrDecr(lhs, op) => {
                write!(f, "{}{:?}", op.op_string(), lhs)
            }
            Instruction::IncrDecrPostfix(lhs, op) => {
                write!(f, "{:?}{}", lhs, op.op_string())
            }
            Instruction::Ref(id) => {
                write!(f, "${}", id)
            }
            Instruction::Undefined => {
                write!(f, "undefined")
            }
            Instruction::Null => {
                write!(f, "null")
            }
            Instruction::This => {
                write!(f, "this")
            }
            Instruction::TypeOf(var) => {
                write!(f, "typeof ${}", var)
            }
            Instruction::TypeOfGlobal(var) => {
                write!(f, "typeof global {:?}", var)
            }
            Instruction::Array(elements) => {
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
            Instruction::Object(proto, props) => {
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
            Instruction::Super => {
                write!(f, "super")
            }
            Instruction::CreateClass(optional_extends) => {
                write!(
                    f,
                    "class{}",
                    optional_extends
                        .as_ref()
                        .map(|extends| format!(" extends ${}", extends))
                        .unwrap_or("".to_string())
                )
            }
            Instruction::ArrayPattern(input, items) => write!(
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
            Instruction::ObjectPattern(input, props) => write!(
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
            Instruction::PatternUnpack(pattern, index) => {
                write!(f, "unpack ${}[{}]", pattern, index)
            }
            Instruction::Function(id) => {
                write!(f, "{:?}", id)
            }
            Instruction::Call(callee, args) => {
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
            Instruction::New(constructor, args) => {
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
            Instruction::ArgumentRead(idx) => {
                write!(f, "arguments[{}]", idx)
            }
            Instruction::ArgumentRest(idx) => {
                write!(f, "arguments[{}...]", idx)
            }

            Instruction::Read(lhs) => match lhs {
                LHS::Local(var) => write!(f, "read_local ${:?}", var),
                LHS::NonLocal(nonlocal) => {
                    write!(f, "read_non_local $${:?}", nonlocal.0)
                }
                LHS::Global(glob_name) => write!(f, "global {:?}", glob_name),
                LHS::Member(base, member) => write!(f, "{:?}{:?}", base, member),
            },
            Instruction::Write(lhs, val) => match lhs {
                LHS::Local(var) => write!(f, "write_local ${:?} ${}", var, val),
                LHS::NonLocal(nonlocal) => {
                    write!(f, "write_non_local $${:?} ${}", nonlocal.0, val)
                }
                LHS::Global(glob_name) => write!(f, "global {:?} = ${}", glob_name, val),
                LHS::Member(base, member) => write!(f, "{:?}{:?} = ${}", base, member, val),
            },
            Instruction::Delete(lhs) => {
                write!(f, "delete {:?}", lhs)
            }

            Instruction::TempExit(exit_type, arg) => {
                write!(f, "{:?} ${}", exit_type, arg)
            }

            Instruction::Phi(vars) => {
                write!(
                    f,
                    "either({})",
                    vars.iter()
                        .map(|v| format!("${}", v))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Instruction::CaughtError => {
                write!(f, "caught_error()")
            }
            Instruction::ForInOfValue => {
                write!(f, "for_in_of_value()")
            }
        }
    }
}

impl Debug for StructuredModule {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let StructuredModule {
            summary,
            functions,
            imports,
            exports,
        } = self;

        let functions: Vec<_> = functions.iter().map(|(_, v)| v).collect();

        let (top_level_stats, functions) = functions
            .split_first()
            .expect("there must be a first element in a module's list of functions");

        let mut d = f.debug_struct("StructuredModule");

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

impl Debug for StructuredFunction {
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
        for (i, b) in self.blocks.iter().enumerate() {
            let s = format!("{:?}", b);
            let ends_in_newline = s.ends_with('\n');
            write!(f, "{}", s)?;
            if i < self.blocks.len() - 1 && !ends_in_newline {
                write!(f, "\n")?;
            }
        }
        Ok(())
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
            StructuredFlow::LogicalCond(kind, before, cond_on, after, then_take) => {
                write!(f, "(")?;
                print_vec_no_eol(f, before)?;
                write!(
                    f,
                    ", ${}) {} ",
                    cond_on,
                    match kind {
                        LogicalCondKind::And => "&&",
                        LogicalCondKind::Or => "||",
                        LogicalCondKind::NullishCoalescing => "??",
                    }
                )?;
                write!(f, "(")?;
                print_vec_no_eol(f, after)?;
                write!(f, ", ${})", then_take)
            }
            StructuredFlow::Switch(brk, expression, cases) => {
                writeln!(f, "switch{} (${}) {{", print_brk(brk), expression)?;

                for case in cases {
                    match &case.condition {
                        Some((instructions, test_var)) => {
                            if instructions.len() > 0 {
                                print_vec(f, &instructions)?;
                            }
                            writeln!(f, "case (${}) ", test_var)?;
                            print_vec(f, &case.body)?;
                        }
                        None => {
                            writeln!(f, "default: ")?;
                            print_vec(f, &case.body)?;
                        }
                    }
                }
                write!(f, "}}\n")?;

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
