use std::collections::{BTreeMap, HashSet};

use ordered_float::NotNan;

use crate::basic_blocks::{
    ArrayElement, ArrayPatternPiece, BasicBlockInstruction, ObjectPatternPiece, ObjectProp,
};

use super::{interpret_function, InterpretCtx, JsCompletion, JsType};

static ARRAY_MAX_ELEMENTS: usize = 100;

pub fn interpret(
    ctx: &mut InterpretCtx,
    instruction: &BasicBlockInstruction,
) -> Option<JsCompletion> {
    let normal_completion = match instruction {
        BasicBlockInstruction::LitNumber(n) => JsType::new_number(*n),
        BasicBlockInstruction::LitBool(b) => JsType::TheBoolean(*b),
        BasicBlockInstruction::LitString(s) => JsType::TheString(s.clone()),
        BasicBlockInstruction::Ref(var_idx) => ctx.get_variable(*var_idx)?.clone(),
        BasicBlockInstruction::UnaryOp(op, operand) => {
            let operand = ctx.get_variable(*operand)?;

            match op {
                swc_ecma_ast::UnaryOp::Minus => match operand.to_numeric() {
                    Some(num) => JsType::new_number(-num),
                    None => JsType::Number,
                },
                swc_ecma_ast::UnaryOp::Plus => match operand.to_numeric() {
                    Some(num) => JsType::new_number(num),
                    None => JsType::Number,
                },
                swc_ecma_ast::UnaryOp::Bang => match operand.to_boolean() {
                    Some(b) => JsType::TheBoolean(!b),
                    None => JsType::Boolean,
                },
                swc_ecma_ast::UnaryOp::Tilde => JsType::Number,
                swc_ecma_ast::UnaryOp::Void
                | swc_ecma_ast::UnaryOp::TypeOf
                | swc_ecma_ast::UnaryOp::Delete => unreachable!(),
            }
        }
        BasicBlockInstruction::BinOp(op, l, r) => {
            use swc_ecma_ast::BinaryOp::*;
            let l = ctx.get_variable(*l)?;
            let r = ctx.get_variable(*r)?;

            if let Some(comparison_res) = interp_comparisons(op, l, r) {
                return Some(comparison_res);
            }
            match (l, r) {
                (JsType::TheNumber(l), JsType::TheNumber(r)) => {
                    interp_float_binops((*l).into(), (*r).into(), op)?
                }
                (JsType::Number, JsType::Number) => match op {
                    BitAnd | BitOr | BitXor | RShift | LShift | ZeroFillRShift | Add | Sub
                    | Mul | Div | Mod => JsType::Number,
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
            }
        }
        BasicBlockInstruction::IncrDecr(lhs, incr) => match ctx.get_lhs(lhs)? {
            JsType::Number => JsType::Number,
            JsType::TheNumber(n) => {
                let n = JsType::new_number(n.into_inner() + incr.as_float_incr());
                ctx.set_lhs(lhs, &n)?;
                n
            }
            _ => return None,
        },
        BasicBlockInstruction::IncrDecrPostfix(lhs, incr) => match ctx.get_lhs(lhs)? {
            JsType::Number => JsType::Number,
            JsType::TheNumber(old_n) => {
                let old_n = *old_n;

                let n = JsType::new_number(old_n.into_inner() + incr.as_float_incr());
                ctx.set_lhs(lhs, &n)?;
                JsType::TheNumber(old_n)
            }
            _ => return None,
        },
        BasicBlockInstruction::Undefined => JsType::Undefined,
        BasicBlockInstruction::Null => JsType::Null,
        BasicBlockInstruction::This => None?, // TODO: grab from context?
        BasicBlockInstruction::TypeOf(t) => ctx.get_variable(*t)?.typeof_string(),
        BasicBlockInstruction::TypeOfGlobal(_) => JsType::String,
        BasicBlockInstruction::ForInOfValue => None?, // TODO: grab from context?
        BasicBlockInstruction::CaughtError => None?,  // TODO: grab from context?
        BasicBlockInstruction::Array(elements) => {
            let plain_items = elements
                .iter()
                .all(|elem| matches!(elem, ArrayElement::Item(_)));

            if !plain_items {
                return None;
            }

            if elements.len() < ARRAY_MAX_ELEMENTS {
                let elements = elements
                    .iter()
                    .map(|elem| elem.as_item())
                    .collect::<Option<Vec<_>>>()
                    .and_then(|vars| ctx.get_variables(vars));

                match elements {
                    Some(elements) => JsType::TheArray(elements),
                    None => JsType::Array,
                }
            } else {
                JsType::Array
            }
        }
        BasicBlockInstruction::Object(proto, props) => {
            if proto.is_some() || props.len() > ARRAY_MAX_ELEMENTS {
                JsType::Object
            } else {
                let mut out_props = BTreeMap::new();
                for prop in props.iter() {
                    match prop {
                        ObjectProp::KeyValue(key, value) => {
                            let value = ctx.get_variable(*value)?.clone();
                            out_props.insert(key.clone(), value);
                        }
                        ObjectProp::Computed(key_varname, value) => {
                            if let Some(key) = ctx.get_variable(*key_varname)?.to_string() {
                                let value = ctx.get_variable(*value)?.clone();
                                out_props.insert(key, value);
                            } else {
                                return Some(JsCompletion::Normal(JsType::Object));
                            }
                        }
                        ObjectProp::Spread(_) => todo!(),
                    }
                }

                if out_props.len() > ARRAY_MAX_ELEMENTS {
                    JsType::Object
                } else {
                    JsType::TheObject(out_props)
                }
            }
        }
        BasicBlockInstruction::ArrayPattern(from_arr, pieces) => {
            let from_arr = ctx.get_variable(*from_arr)?.as_array()?;

            let mut pattern_contents: Vec<JsType> = Vec::new();

            for (i, item) in pieces.iter().enumerate() {
                match item {
                    ArrayPatternPiece::Item => {
                        let item_or_undef = from_arr.get(i).cloned().unwrap_or(JsType::Undefined);
                        pattern_contents.push(item_or_undef);
                    }
                    ArrayPatternPiece::Spread => todo!(),
                }
            }

            JsType::Pattern(pattern_contents)
        }
        BasicBlockInstruction::ObjectPattern(inp_obj, pattern_props) => {
            let obj = ctx.get_variable(*inp_obj)?.as_object()?;

            let mut pattern_contents: Vec<JsType> = Vec::new();

            let mut seen_keys = if pattern_props
                .iter()
                .any(|prop| prop == &ObjectPatternPiece::Spread)
            {
                Some(HashSet::new())
            } else {
                None
            };

            for (i, item) in pattern_props.iter().enumerate() {
                match item {
                    ObjectPatternPiece::TakeKey(key) => {
                        if let Some(ref mut keys_set) = seen_keys {
                            keys_set.insert(key.clone());
                        }

                        let item_or_undef = obj.get(key).cloned().unwrap_or(JsType::Undefined);
                        pattern_contents.push(item_or_undef);
                    }
                    ObjectPatternPiece::TakeComputedKey(key) => {
                        if let Some(key) = ctx.get_variable(*key)?.to_string() {
                            if let Some(ref mut keys_set) = seen_keys {
                                keys_set.insert(key.clone());
                            }

                            let item_or_undef = obj.get(&key).cloned().unwrap_or(JsType::Undefined);
                            pattern_contents.push(item_or_undef);
                        } else {
                            return Some(JsCompletion::Normal(JsType::String));
                        };
                    }
                    ObjectPatternPiece::Spread => {
                        // Spread must be last
                        if i != pattern_props.len() - 1 {
                            return None;
                        }

                        let except_keys = seen_keys.as_ref().unwrap();

                        let spread_obj = JsType::TheObject(
                            obj.iter()
                                .filter(|(key, _)| !except_keys.contains(*key))
                                .map(|(key, value)| (key.clone(), value.clone()))
                                .collect(),
                        );

                        pattern_contents.push(spread_obj);
                    }
                }
            }

            JsType::Pattern(pattern_contents)
        }
        BasicBlockInstruction::PatternUnpack(pat, index) => {
            let Some(JsType::Pattern(contents)) = ctx.get_variable(*pat) else {
                return None;
            };

            contents.get(*index).cloned()?
        }
        BasicBlockInstruction::TempExit(_, _) => None?, // TODO: yield, await
        BasicBlockInstruction::Function(id) => JsType::TheFunction(*id),
        BasicBlockInstruction::Call(callee, args) => {
            let the_function = ctx.get_variable(*callee)?.as_function_id()?;
            let func = ctx.get_function(the_function)?.clone(/* TODO */);
            let args = args.iter().cloned().collect::<Vec<_>>();
            let args = ctx.get_variables(args).into();

            interpret_function(ctx, &func, args)?.into_return()?
        }
        BasicBlockInstruction::ArgumentRead(n) => ctx.get_argument(*n)?.clone(),
        BasicBlockInstruction::ArgumentRest(n) => match ctx.get_spread_argument(*n) {
            Some(rest) => JsType::TheArray(rest.into()),
            None => JsType::Array,
        },
        BasicBlockInstruction::Read(lhs) => ctx.get_lhs(lhs)?.clone(),
        BasicBlockInstruction::Write(lhs, new_val) => {
            let val = ctx.get_variable(*new_val)?.clone();
            ctx.set_lhs(lhs, &val)?;
            val
        }
    };

    Some(JsCompletion::Normal(normal_completion))
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

fn interp_comparisons(op: &swc_ecma_ast::BinaryOp, l: &JsType, r: &JsType) -> Option<JsCompletion> {
    use swc_ecma_ast::BinaryOp::*;

    let can_compare_type = |t: &JsType| match t {
        JsType::TheBoolean(_) | JsType::TheNumber(_) | JsType::TheString(_) | JsType::Undefined => {
            true
        }
        _ => false,
    };

    let is_equal = if can_compare_type(l) && can_compare_type(r) && matches!(op, EqEqEq | NotEqEq) {
        l == r
    } else {
        match (l, r) {
            (JsType::Undefined, _) | (_, JsType::Undefined) => false,

            _ => return None,
        }
    };

    match op {
        EqEq | EqEqEq => Some(JsCompletion::Normal(JsType::TheBoolean(is_equal == true))),
        NotEq | NotEqEq => Some(JsCompletion::Normal(JsType::TheBoolean(is_equal == true))),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        basic_blocks::FunctionId,
        interpret::{interpret_block_group, JsType},
        testutils::*,
    };

    fn test_interp_block(source: &str) -> Option<JsCompletion> {
        let mut ctx = InterpretCtx::new();
        ctx.start_function(
            FunctionId(0),
            Some(vec![0.0.into(), 1.0.into(), 2.0.into()]).into(),
        );
        ctx.assign_variable(1, JsType::new_number(1.0));
        ctx.assign_variable(2, JsType::new_number(2.0));
        ctx.assign_variable(123, JsType::Number);
        let instructions = parse_instructions(source);
        interpret_block_group(&mut ctx, &instructions)
    }

    fn test_interp(source: &str) -> Option<JsCompletion> {
        test_interp_block(&format!(
            "@0: {{
                $0 = {source}
                exit = return $0
            }}"
        ))
    }

    fn test_interp_normal(source: &str) -> JsType {
        match test_interp(source) {
            Some(JsCompletion::Return(t)) => t,
            comp => panic!("expected normal completion, got {:?}", comp),
        }
    }

    fn test_interp_js(source: &str) -> JsType {
        let b_group = test_basic_blocks(source);
        let mut ctx = InterpretCtx::new();
        ctx.start_function(
            FunctionId(0),
            Some(vec![0.0.into(), 1.0.into(), 2.0.into()]).into(),
        );
        ctx.assign_variable(1, JsType::new_number(1.0));
        ctx.assign_variable(2, JsType::new_number(2.0));
        interpret_block_group(&mut ctx, &b_group)
            .unwrap()
            .as_return()
            .unwrap()
            .clone()
    }

    fn test_interp_unknown(source: &str) -> bool {
        test_interp(source).is_none()
    }

    #[test]
    fn interp_number() {
        insta::assert_debug_snapshot!(test_interp_normal("1"), @"TheNumber(1)");
    }

    #[test]
    fn interp_ref() {
        insta::assert_debug_snapshot!(test_interp_normal("$1"), @"TheNumber(1)");
    }

    #[test]
    fn interp_binop() {
        insta::assert_debug_snapshot!(test_interp_normal("$1 + $2"), @"TheNumber(3)");
    }

    #[test]
    fn interp_undefined() {
        insta::assert_debug_snapshot!(test_interp_normal("undefined"), @r###"
            Undefined
        "###);
    }

    #[test]
    fn interp_cond_mutations_1() {
        let num = test_interp_block(
            "@0: {
                $0 = 5
                exit = cond $123 ? @1..@1 : @2..@2 }
            @1: {
                $1 = $0++
                exit = jump @3 }
            @2: {
                $2 = --$0
                exit = jump @3 }
            @3: {
                exit = return $0 }",
        )
        .unwrap()
        .into_return()
        .unwrap();
        insta::assert_debug_snapshot!(num, @"Number"); // We don't know if 4 or 6
    }

    #[test]
    fn interp_cond_mutations_2() {
        let num = test_interp_block(
            "@0: {
                $0 = [$2]
                exit = cond $123 ? @1..@1 : @2..@2 }
            @1: {
                $1 = 0
                $2 = $0[$1]++
                exit = jump @3 }
            @2: {
                $3 = 0
                $4 = --$0[$3]
                exit = jump @3 }
            @3: {
                $5 = 0
                $6 = $0[$5]
                exit = return $6 }",
        )
        .unwrap()
        .into_return()
        .unwrap();
        insta::assert_debug_snapshot!(num, @"Number"); // We don't know if 1 or 3
    }

    #[test]
    fn interp_this() {
        assert!(test_interp_unknown("this"));
    }

    #[test]
    fn interp_array() {
        insta::assert_debug_snapshot!(test_interp_normal("[]"), @"TheArray([])");
        insta::assert_debug_snapshot!(test_interp_normal("[$1]"), @"TheArray([TheNumber(1)])");
        insta::assert_debug_snapshot!(test_interp_unknown("[$1, ...$2]"), @"true");
    }

    #[test]
    fn interp_arguments() {
        insta::assert_debug_snapshot!(test_interp_normal("arguments[0]"), @"TheNumber(0)");
        insta::assert_debug_snapshot!(test_interp_normal("arguments[1...]"), @"TheArray([TheNumber(1), TheNumber(2)])");
    }

    #[test]
    fn interp_phi() {
        insta::assert_debug_snapshot!(test_interp_normal("either($1)"), @"TheNumber(1)");
        insta::assert_debug_snapshot!(test_interp_normal("either($1, $2)"), @"Number");
    }

    #[test]
    fn interp_function() {
        insta::assert_debug_snapshot!(test_interp_normal("FunctionId(1)"), @"TheFunction(1)");
    }

    #[test]
    fn interp_pattern() {
        insta::assert_debug_snapshot!(test_interp_js("
            let [a, [,,b]] = [1, [0,0,2]];
            return [b, a];
        "), @"TheArray([TheNumber(2), TheNumber(1)])");
        insta::assert_debug_snapshot!(test_interp_js("
            let [a = 123] = [];
            return a;
        "), @"TheNumber(123)");
        insta::assert_debug_snapshot!(test_interp_js("
            let [a = 999, b = 456] = [123];
            return [a, b];
        "), @"TheArray([TheNumber(123), TheNumber(456)])");
    }

    #[test]
    fn interp_obj_pattern() {
        insta::assert_debug_snapshot!(test_interp_js("
            let { a = 123 } = {};
            return a;
        "), @"TheNumber(123)");
        insta::assert_debug_snapshot!(test_interp_js("
            let { a = 1, [1]: b, ...c } = { 1: 2, xrest: 3 };
            return [a, b, c];
        "), @"TheArray([TheNumber(1), TheNumber(2), TheObject({\"xrest\": TheNumber(3)})])");
    }

    #[test]
    fn interp_typeof() {
        insta::assert_debug_snapshot!(test_interp_js("return typeof undefined"), @"TheString(\"undefined\")");
        insta::assert_debug_snapshot!(test_interp_js("return typeof {}"), @"TheString(\"object\")");
        insta::assert_debug_snapshot!(test_interp_js("return typeof []"), @"TheString(\"object\")");
        insta::assert_debug_snapshot!(test_interp_js("return typeof function(){}"), @"TheString(\"function\")");
        insta::assert_debug_snapshot!(test_interp_js("return typeof missingVar"), @"String");
    }

    #[test]
    fn interp_incr() {
        let num = test_interp_js("let a = 1; return a++;");
        insta::assert_debug_snapshot!(num, @"TheNumber(1)");
        let num = test_interp_js("let a = 2; return --a;");
        insta::assert_debug_snapshot!(num, @"TheNumber(1)");
        let num = test_interp_js("let a = 2; a--; return a++;");
        insta::assert_debug_snapshot!(num, @"TheNumber(1)");
        let num = test_interp_js("let a = 1; a++; return --a;");
        insta::assert_debug_snapshot!(num, @"TheNumber(1)");
    }

    #[test]
    fn interp_lhs() {
        let obj = test_interp_js("let o = {a: 1}; o.a++; return o;");
        insta::assert_debug_snapshot!(obj, @"TheObject({\"a\": TheNumber(2)})");
        let obj = test_interp_js("let o = {a: 1}; o.a++; return o.a;");
        insta::assert_debug_snapshot!(obj, @"TheNumber(2)");
    }

    /*
    #[test]
    fn interp_nonlocal() {
        let obj = test_interp_js("
            let a = 1;
            function f() {
                let b = a + 1;
                return b;
            }
            return f();
        ");
        insta::assert_debug_snapshot!(obj, @"TheNumber(2)");
    } */
}
