use std::collections::{BTreeMap, HashSet};

use crate::basic_blocks::{
    ArrayElement, ArrayPatternPiece, BasicBlockInstruction, ObjectPatternPiece, ObjectProperty,
};

use super::{interp_math_ops, InterpretCtx, JsArgs, JsCompletion, JsType};

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
            return interp_math_ops(ctx, *l, *r, op);
        }
        BasicBlockInstruction::IncrDecr(lhs, incr) => match ctx.get_lhs(lhs)? {
            JsType::Number => JsType::Number,
            JsType::TheNumber(n) => {
                let n = JsType::new_number(n.into_inner() + incr.as_float_incr());
                ctx.set_lhs(lhs, n.clone())?;
                n
            }
            _ => return None,
        },
        BasicBlockInstruction::IncrDecrPostfix(lhs, incr) => match ctx.get_lhs(lhs)? {
            JsType::Number => JsType::Number,
            JsType::TheNumber(old_n) => {
                let old_n = *old_n;

                let n = JsType::new_number(old_n.into_inner() + incr.as_float_incr());
                ctx.set_lhs(lhs, n)?;
                JsType::TheNumber(old_n)
            }
            _ => return None,
        },
        BasicBlockInstruction::Undefined => JsType::Undefined,
        BasicBlockInstruction::Null => JsType::Null,
        BasicBlockInstruction::This => JsType::Any, // TODO: grab from context?
        BasicBlockInstruction::TypeOf(t) => ctx.get_variable(*t)?.typeof_string(),
        BasicBlockInstruction::TypeOfGlobal(_) => JsType::String,
        BasicBlockInstruction::ForInOfValue => JsType::Any, // TODO: grab from context?
        BasicBlockInstruction::CaughtError => JsType::Any,  // TODO: grab from context?
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
                    .and_then(|vars| ctx.get_variables(&vars));

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
                    match &prop {
                        ObjectProperty::KeyValue(key, value) => {
                            let key = match key {
                                crate::basic_blocks::ObjectKey::Computed(expr) => {
                                    if let Some(key) = ctx.get_variable(*expr)?.to_string() {
                                        key
                                    } else {
                                        return Some(JsCompletion::Normal(JsType::Object));
                                    }
                                }
                                crate::basic_blocks::ObjectKey::NormalKey(key) => key.clone(),
                                crate::basic_blocks::ObjectKey::Private(_) => {
                                    unreachable!("objects cannot have private props")
                                }
                            };

                            let value = match value {
                                crate::basic_blocks::ObjectValue::Property(prop) => {
                                    ctx.get_variable(*prop)?
                                }
                                crate::basic_blocks::ObjectValue::Method(_, _) => {
                                    return Some(JsCompletion::Normal(JsType::Object))
                                }
                            };

                            out_props.insert(key, value.clone());
                        }
                        ObjectProperty::Spread(spread) => {
                            let spread = ctx.get_variable(*spread)?.as_object()?;

                            for (key, value) in spread {
                                out_props.insert(key.clone(), value.clone());
                            }
                        }
                    }
                }

                if out_props.len() > ARRAY_MAX_ELEMENTS {
                    JsType::Object
                } else {
                    JsType::TheObject(out_props)
                }
            }
        }
        BasicBlockInstruction::Super => JsType::Any, // TODO: grab from context?
        BasicBlockInstruction::CreateClass(_) => JsType::Any, // TODO: class type
        BasicBlockInstruction::ArrayPattern(from_arr, pieces) => {
            let from_arr = ctx.get_variable(*from_arr)?.as_array()?;

            let mut pattern_contents: Vec<JsType> = Vec::new();

            let mut i = 0;
            for item in pieces {
                match item {
                    ArrayPatternPiece::Item => {
                        let item_or_undef = from_arr.get(i).cloned().unwrap_or(JsType::Undefined);
                        i += 1;

                        pattern_contents.push(item_or_undef);
                    }
                    ArrayPatternPiece::Spread => {
                        let rest = from_arr.iter().skip(i).cloned().collect::<Vec<_>>();
                        i += rest.len();

                        pattern_contents.push(JsType::TheArray(rest));
                    }
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
        BasicBlockInstruction::Phi(alternatives) => {
            let types = alternatives
                .iter()
                // Some variables will be missing if we eliminate a branch, so we use flat_map
                .flat_map(|alt| ctx.get_variable(*alt));
            JsType::union_all(types)?
        }
        BasicBlockInstruction::Function(id) => JsType::TheFunction(*id, Default::default()),
        BasicBlockInstruction::Call(callee, args) => {
            let the_function = ctx.get_variable(*callee)?.as_function_id()?;
            let args = JsArgs::from(ctx.get_variables(args));
            return ctx.get_function_return(the_function, args);
        }
        BasicBlockInstruction::New(_constructor, _args) => {
            return None;
        }
        BasicBlockInstruction::ArgumentRead(n) => ctx.get_argument(*n)?.clone(),
        BasicBlockInstruction::ArgumentRest(n) => match ctx.get_spread_argument(*n) {
            Some(rest) => JsType::TheArray(rest.into()),
            None => JsType::Array,
        },
        BasicBlockInstruction::Read(lhs) => ctx.get_lhs(lhs)?.clone(),
        BasicBlockInstruction::Write(lhs, new_val) => {
            let val = ctx.get_variable(*new_val)?.clone();
            ctx.set_lhs(lhs, val.clone())?;
            val
        }
    };

    Some(JsCompletion::Normal(normal_completion))
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        basic_blocks::FunctionId,
        interpret::{interpret_block_group, interpret_module, JsType},
        testutils::*,
    };

    fn test_interp_block(source: &str) -> Option<JsCompletion> {
        let module = parse_instructions_module(vec![source]);
        let mut ctx = InterpretCtx::from_module(&module);
        ctx.start_function(
            true,
            FunctionId(0),
            Some(vec![0.0.into(), 1.0.into(), 2.0.into()]).into(),
        );
        ctx.assign_variable(1, JsType::new_number(1.0));
        ctx.assign_variable(2, JsType::new_number(2.0));
        ctx.assign_variable(123, JsType::Number);
        interpret_block_group(&mut ctx, &module.get_function(FunctionId(0)).unwrap())
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

    fn test_interp_unknown(source: &str) -> bool {
        test_interp(source).is_none()
    }

    fn test_interp_js(source: &str) -> Option<JsCompletion> {
        let b_group = test_basic_blocks_module(source);
        let mut ctx = InterpretCtx::from_module(&b_group);
        interpret_module(&mut ctx, &b_group)
    }

    fn test_interp_js_normal(source: &str) -> JsType {
        match test_interp_js(source) {
            Some(JsCompletion::Return(t)) => t,
            comp => panic!("expected normal completion, got {:?}", comp),
        }
    }

    fn test_interp_js_unknown(source: &str) -> bool {
        test_interp_js(source).is_none()
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
        );
        insta::assert_debug_snapshot!(num, @"None"); // Mutations are not supported
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
        );
        insta::assert_debug_snapshot!(num, @"None"); // Mutations are not supported
    }

    #[test]
    fn interp_this() {
        assert_eq!(test_interp_normal("this"), JsType::Any);
    }

    #[test]
    fn interp_array() {
        insta::assert_debug_snapshot!(test_interp_normal("[]"), @"TheArray([])");
        insta::assert_debug_snapshot!(test_interp_normal("[$1]"), @"TheArray([TheNumber(1)])");
        insta::assert_debug_snapshot!(test_interp_unknown("[$1, ...$2]"), @"true");
    }

    #[test]
    fn interp_object() {
        insta::assert_debug_snapshot!(test_interp_normal("{ }"), @"TheObject({})");
        insta::assert_debug_snapshot!(test_interp_normal("{a: $1}"), @"TheObject({\"a\": TheNumber(1)})");
        insta::assert_debug_snapshot!(test_interp_unknown("{a: $1, ...$2}"), @"true");
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
    fn interp_arguments() {
        insta::assert_debug_snapshot!(test_interp_normal("arguments[0]"), @"TheNumber(0)");
        insta::assert_debug_snapshot!(test_interp_normal("arguments[1...]"), @"TheArray([TheNumber(1), TheNumber(2)])");
    }

    #[test]
    fn interp_functions() {
        let obj = test_interp_js_normal(
            "return (function(a, b) {
                return a + b;
            })(1, 2);",
        );
        insta::assert_debug_snapshot!(obj, @"TheNumber(3)");

        let obj = test_interp_js_normal(
            "let x = function(a, b) {
                return a + b;
            };
            return x(1, 2) + x(3, 4);",
        );
        insta::assert_debug_snapshot!(obj, @"TheNumber(10)");

        /*
        let obj = test_interp_js_normal(
            "let x = function() {
                return x.foo;
            };
            x.foo = 1;
            return x();",
        );
        insta::assert_debug_snapshot!(obj, @"TheNumber(1)");
        */

        let obj = test_interp_js_unknown(
            "let x = function() {};
            return x === x;",
        );
        assert!(obj);
    }

    #[test]
    fn interp_pattern() {
        insta::assert_debug_snapshot!(test_interp_js_normal("
            let [a, [,,b]] = [1, [0,0,2]];
            return [b, a];
        "), @"TheArray([TheNumber(2), TheNumber(1)])");
        insta::assert_debug_snapshot!(test_interp_js_normal("
            let [a = 123] = [];
            return a;
        "), @"TheNumber(123)");
        insta::assert_debug_snapshot!(test_interp_js_normal("
            let [a = 999, b = 456] = [123];
            return [a, b];
        "), @"TheArray([TheNumber(123), TheNumber(456)])");
        insta::assert_debug_snapshot!(test_interp_js_normal("
            let [a, ...b] = [123, 456, 789];
            return [a, b];
        "), @"TheArray([TheNumber(123), TheArray([TheNumber(456), TheNumber(789)])])");
    }

    #[test]
    fn interp_obj_pattern() {
        insta::assert_debug_snapshot!(test_interp_js_normal("
            let { a = 123 } = {};
            return a;
        "), @"TheNumber(123)");
        insta::assert_debug_snapshot!(test_interp_js_normal("
            let { a = 1, [-1 + 2]: b, ...c } = { 1: 2, xrest: 3 };
            return [a, b, c];
        "), @"TheArray([TheNumber(1), TheNumber(2), TheObject({\"xrest\": TheNumber(3)})])");
    }

    #[test]
    fn interp_typeof() {
        insta::assert_debug_snapshot!(test_interp_js_normal("return typeof undefined"), @"TheString(\"undefined\")");
        insta::assert_debug_snapshot!(test_interp_js_normal("return typeof {}"), @"TheString(\"object\")");
        insta::assert_debug_snapshot!(test_interp_js_normal("return typeof []"), @"TheString(\"object\")");
        insta::assert_debug_snapshot!(test_interp_js_normal("return typeof function(){}"), @"TheString(\"function\")");
        insta::assert_debug_snapshot!(test_interp_js_normal("return typeof missingVar"), @"String");
    }

    #[test]
    fn interp_incr() {
        let num = test_interp_js_normal("let a = 1; return a++;");
        insta::assert_debug_snapshot!(num, @"TheNumber(1)");
        let num = test_interp_js_normal("let a = 2; return --a;");
        insta::assert_debug_snapshot!(num, @"TheNumber(1)");
        /*
        let num = test_interp_js_normal("let a = 2; a--; return a++;");
        insta::assert_debug_snapshot!(num, @"TheNumber(1)");
        let num = test_interp_js_normal("let a = 1; a++; return --a;");
        insta::assert_debug_snapshot!(num, @"TheNumber(1)");
        */
    }

    #[test]
    fn interp_lhs() {
        let obj = test_interp_js_unknown("let o = {a: 1}; o.a++; return o;");
        assert!(obj);
        let obj = test_interp_js_unknown("let o = {a: 1}; o.a++; return o.a;");
        assert!(obj);
        let obj = test_interp_js_unknown(
            "let o = {['prop']: {}};
            o.prop.a = 1;
            return [o.prop, o.prop.a];",
        );
        assert!(obj);
    }

    #[test]
    fn interp_nonlocal() {
        /*
        let obj = test_interp_js(
            "let a = 100;
            function f() {
                let b = a + 1;
                return b;
            }
            return f();",
        );
        insta::assert_debug_snapshot!(obj, @"TheNumber(101)");
        */
    }
}
