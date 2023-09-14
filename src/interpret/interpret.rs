use ordered_float::NotNan;

use crate::basic_blocks::{ArrayElement, BasicBlockInstruction};

use super::{interpret_function, InterpretCompletion, InterpretCtx, JsType};

pub fn interpret(
    ctx: &mut InterpretCtx,
    instruction: &BasicBlockInstruction,
) -> Option<InterpretCompletion> {
    let normal_completion = match instruction {
        BasicBlockInstruction::LitNumber(n) => JsType::new_number(*n),
        BasicBlockInstruction::LitBool(b) => JsType::TheBoolean(*b),
        BasicBlockInstruction::Ref(var_idx) => ctx.get_variable(*var_idx)?.clone(),
        BasicBlockInstruction::BinOp(op, l, r) => {
            if let (Some(JsType::TheNumber(l)), Some(JsType::TheNumber(r))) =
                (ctx.get_variable(*l), ctx.get_variable(*r))
            {
                interp_float_binops((*l).into(), (*r).into(), op)?
            } else if ctx.get_variable(*l) == Some(&JsType::Number)
                && ctx.get_variable(*r) == Some(&JsType::Number)
            {
                use swc_ecma_ast::BinaryOp::*;
                match op {
                    BitAnd | BitOr | BitXor | RShift | LShift | ZeroFillRShift | Add | Sub
                    | Mul | Div | Mod => JsType::Number,
                    _ => return None,
                }
            } else {
                return None;
            }
        }
        BasicBlockInstruction::Undefined => JsType::Undefined,
        BasicBlockInstruction::This => None?, // TODO: grab from context?
        BasicBlockInstruction::CaughtError => None?, // TODO: grab from context?
        BasicBlockInstruction::Array(elements) => {
            for element in elements {
                if let ArrayElement::Spread(_) | ArrayElement::Hole = element {
                    return None;
                }
            }

            JsType::Array
        }
        BasicBlockInstruction::TempExit(_, _) => None?, // TODO: yield, await
        BasicBlockInstruction::Phi(alternatives) => {
            let mut ts: Vec<&JsType> = Vec::new();

            for alt in alternatives {
                ts.push(ctx.get_variable(*alt)?);
            }

            JsType::union_all(ts.into_iter())
        }
        BasicBlockInstruction::Function(id) => JsType::TheFunction(*id),
        BasicBlockInstruction::Call(callee, args) => {
            let the_function = ctx.get_variable(*callee)?.as_function_id()?;
            let func = ctx.get_function(the_function)?.clone(/* TODO */);
            let args = args
                .iter()
                .map(|arg| ctx.get_variable(*arg).cloned())
                .collect::<Option<Vec<_>>>();

            interpret_function(ctx, &func, args)?.as_return()?
        }
        BasicBlockInstruction::ArgumentRead(n) => ctx.get_argument(*n)?.clone(),
        BasicBlockInstruction::ArgumentRest(_) => None?, // TODO: grab from context?
        BasicBlockInstruction::ReadNonLocal(_) => None?, // TODO: grab from context?
        BasicBlockInstruction::WriteNonLocal(_, _) => None?, // TODO: grab from context?
    };

    Some(InterpretCompletion::Normal(normal_completion))
}

fn interp_float_binops(l: f64, r: f64, op: &swc_ecma_ast::BinaryOp) -> Option<JsType> {
    let f = |n: f64| NotNan::new(n).ok();
    let i = |n: f64| n as u64;

    use swc_ecma_ast::BinaryOp::*;

    Some(JsType::new_number(match op {
        // Float ops
        Add => l + r,
        Sub => l - r,
        Mul => l * r,
        Div => l / r,
        Mod => (l as u64 % r as u64) as f64,
        Exp => l.powf(r),
        // Comparison ops
        EqEq | EqEqEq => return Some(JsType::TheBoolean(f(l)? == f(r)?)),
        NotEq | NotEqEq => return Some(JsType::TheBoolean(f(l)? != f(r)?)),
        Lt => return Some(JsType::TheBoolean(f(l)? < f(r)?)),
        LtEq => return Some(JsType::TheBoolean(f(l)? <= f(r)?)),
        Gt => return Some(JsType::TheBoolean(f(l)? > f(r)?)),
        GtEq => return Some(JsType::TheBoolean(f(l)? >= f(r)?)),
        // Bit ops
        BitAnd => (i(l) & i(r)) as f64,
        BitOr => (i(l) | i(r)) as f64,
        BitXor => (i(l) ^ i(r)) as f64,
        RShift => (i(l) >> i(r)) as f64,
        LShift => (i(l) << i(r)) as f64,
        ZeroFillRShift => (i(l) >> i(r)) as f64,
        _ => unreachable!(),
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{interpret::JsType, testutils::*};

    fn test_interp(source: &str) -> Option<InterpretCompletion> {
        let mut ctx = InterpretCtx::new();
        ctx.assign_variable(1, JsType::new_number(1.0));
        ctx.assign_variable(2, JsType::new_number(2.0));
        let instructions = parse_instructions(&format!(
            "@0: {{
                $0 = {source}
                exit = return $0
            }}"
        ));
        let ins = instructions.iter_all_instructions().next().unwrap().2;
        interpret(&mut ctx, ins)
    }

    fn test_interp_normal(source: &str) -> JsType {
        match test_interp(source) {
            Some(InterpretCompletion::Normal(t)) => t,
            comp => panic!("expected normal completion, got {:?}", comp),
        }
    }

    fn test_interp_unknown(source: &str) -> bool {
        test_interp(source).is_none()
    }

    #[test]
    fn test_number() {
        insta::assert_debug_snapshot!(test_interp_normal("1"), @"TheNumber(1)");
    }

    #[test]
    fn test_ref() {
        insta::assert_debug_snapshot!(test_interp_normal("$1"), @"TheNumber(1)");
    }

    #[test]
    fn test_binop() {
        insta::assert_debug_snapshot!(test_interp_normal("$1 + $2"), @"TheNumber(3)");
    }

    #[test]
    fn test_undefined() {
        insta::assert_debug_snapshot!(test_interp_normal("undefined"), @r###"
            Undefined
        "###);
    }

    #[test]
    fn test_this() {
        assert!(test_interp_unknown("this"));
    }

    #[test]
    fn test_array() {
        insta::assert_debug_snapshot!(test_interp_normal("[]"), @"Array");
        insta::assert_debug_snapshot!(test_interp_normal("[$1]"), @"Array");
        insta::assert_debug_snapshot!(test_interp_unknown("[$1, ...$2]"), @"true");
    }

    #[test]
    fn test_phi() {
        insta::assert_debug_snapshot!(test_interp_normal("either($1)"), @"TheNumber(1)");
        insta::assert_debug_snapshot!(test_interp_normal("either($1, $2)"), @"Number");
    }

    #[test]
    fn test_function() {
        insta::assert_debug_snapshot!(test_interp_normal("FunctionId(1)"), @"TheFunction(1)");
    }
}
