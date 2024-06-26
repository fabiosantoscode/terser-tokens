use num_bigint::BigInt;
use ordered_float::NotNan;

use super::{InterpretCtx, JsCompletion, JsType};

pub fn interp_math_ops(
    ctx: &mut InterpretCtx,
    l: usize,
    r: usize,
    op: &swc_ecma_ast::BinaryOp,
) -> Option<JsCompletion> {
    use swc_ecma_ast::BinaryOp::*;
    let l = ctx.get_variable(l)?;
    let r = ctx.get_variable(r)?;

    if let Some(comparison_res) = interp_comparisons(op, l, r) {
        return Some(comparison_res);
    }
    let t = match (l, r) {
        (JsType::TheNumber(l), JsType::TheNumber(r)) => {
            interp_float_binops((*l).into(), (*r).into(), op)?
        }
        (JsType::TheBigInt(l), JsType::TheBigInt(r)) => interp_big_int_binops(l, r, op)?,
        (JsType::Number, JsType::Number) => match op {
            BitAnd | BitOr | BitXor | RShift | LShift | ZeroFillRShift | Add | Sub | Mul | Div
            | Mod => JsType::Number,
            _ => return None,
        },
        (JsType::BigInt, JsType::BigInt) => match op {
            BitAnd | BitOr | BitXor | RShift | LShift | Add | Sub | Mul => JsType::BigInt,
            ZeroFillRShift | Mod | Div => return None, // may throw
            _ => return None,
        },
        (JsType::TheString(l), JsType::TheString(r)) => match op {
            Add => JsType::TheString(format!("{}{}", l, r)),
            _ => return None,
        },
        (JsType::String, JsType::String) => match op {
            Add => JsType::String,
            _ => return None,
        },
        _ => return None,
    };

    Some(JsCompletion::Normal(t))
}

fn interp_float_binops(l: f64, r: f64, op: &swc_ecma_ast::BinaryOp) -> Option<JsType> {
    let f = |n: f64| NotNan::new(n).ok();
    let i = |n: f64| n as i32;

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
        RShift => None?,
        LShift => None?,
        ZeroFillRShift => None?,
        In => None?,
        InstanceOf => None?,
        _ => unreachable!(),
    }))
}

fn interp_big_int_binops(l: &BigInt, r: &BigInt, op: &swc_ecma_ast::BinaryOp) -> Option<JsType> {
    let u = |n: &BigInt| -> Option<u32> { n.try_into().ok() };

    use swc_ecma_ast::BinaryOp::*;

    Some(JsType::TheBigInt(match op {
        // Float ops
        Add => l + r,
        Sub => l - r,
        Mul => l * r,
        Div => l / r,
        Mod => l % r,
        Exp => l.pow(u(r)?),
        // Comparison ops
        EqEq | EqEqEq => return Some(JsType::TheBoolean(l == r)),
        NotEq | NotEqEq => return Some(JsType::TheBoolean(l != r)),
        Lt => return Some(JsType::TheBoolean(l < r)),
        LtEq => return Some(JsType::TheBoolean(l <= r)),
        Gt => return Some(JsType::TheBoolean(l > r)),
        GtEq => return Some(JsType::TheBoolean(l >= r)),
        // Bit ops
        BitAnd => l & r,
        BitOr => l | r,
        BitXor => l ^ r,
        RShift => None?,
        LShift => None?,
        ZeroFillRShift => None?,
        In => None?,
        InstanceOf => None?,
        _ => unreachable!(),
    }))
}

fn interp_comparisons(op: &swc_ecma_ast::BinaryOp, l: &JsType, r: &JsType) -> Option<JsCompletion> {
    use swc_ecma_ast::BinaryOp::*;

    let is_strict = match op {
        EqEq | NotEq => false,
        EqEqEq | NotEqEq => true,
        _ => return None,
    };
    let can_compare_type = |t: &JsType| match t {
        JsType::TheBoolean(_)
        | JsType::TheNumber(_)
        | JsType::TheString(_)
        | JsType::Undefined
        | JsType::Null => true,
        _ => false,
    };

    let is_equal = match (l, r) {
        (JsType::Undefined | JsType::Null, JsType::Undefined | JsType::Null) => is_strict,
        _ => {
            if can_compare_type(l) && can_compare_type(r) && is_strict {
                l == r
            } else {
                return None;
            }
        }
    };

    match op {
        EqEq | EqEqEq => Some(JsCompletion::Normal(JsType::TheBoolean(is_equal == true))),
        NotEq | NotEqEq => Some(JsCompletion::Normal(JsType::TheBoolean(is_equal == false))),
        _ => unreachable!(),
    }
}
