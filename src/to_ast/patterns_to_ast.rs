use swc_ecma_ast::{
    ArrayPat, ComputedPropName, Ident, KeyValuePatProp, ObjectPat, ObjectPatProp, Pat, PropName,
    RestPat, Stmt,
};

use crate::basic_blocks::{ArrayPatternPiece, BasicBlockInstruction, ObjectPatternPiece};

use super::{build_binding_identifier, build_var_decl, ref_or_inlined_expr, ToAstContext};

/// ObjectPattern and ArrayPattern are not expressions, so we need to emit them as statements.
pub fn pattern_to_statement(
    ctx: &mut ToAstContext<'_>,
    instruction: &BasicBlockInstruction,
    variable: usize,
) -> Option<Stmt> {
    match instruction {
        BasicBlockInstruction::ObjectPattern(base, items) => {
            // Creates a statement itself
            let variables = ctx.create_pattern(variable, items.len());
            let props = items
                .iter()
                .zip(variables.into_iter())
                .map(|(piece, variable)| match piece {
                    ObjectPatternPiece::TakeKey(key_name) => {
                        ObjectPatProp::KeyValue(KeyValuePatProp {
                            key: PropName::Ident(Ident::new(
                                key_name.clone().into(),
                                Default::default(),
                            )),
                            value: Box::new(build_binding_identifier(&variable.to_string())),
                        })
                    }
                    ObjectPatternPiece::TakeComputedKey(key_variable) => {
                        ObjectPatProp::KeyValue(KeyValuePatProp {
                            key: PropName::Computed(ComputedPropName {
                                span: Default::default(),
                                expr: Box::new(ref_or_inlined_expr(ctx, *key_variable)),
                            }),
                            value: Box::new(build_binding_identifier(&variable.to_string())),
                        })
                    }
                    ObjectPatternPiece::Spread => ObjectPatProp::Rest(RestPat {
                        span: Default::default(),
                        dot3_token: Default::default(),
                        arg: Box::new(build_binding_identifier(&variable.to_string())),
                        type_ann: None,
                    }),
                })
                .collect();

            let pat = Pat::Object(ObjectPat {
                span: Default::default(),
                props,
                optional: false,
                type_ann: None,
            });

            Some(build_var_decl(pat, ref_or_inlined_expr(ctx, *base)))
        }
        BasicBlockInstruction::ArrayPattern(base, items) => {
            // Creates a statement itself
            let variables = ctx.create_pattern(variable, items.len());
            let elems = items
                .iter()
                .zip(variables.into_iter())
                .map(|(element, variable)| match element {
                    ArrayPatternPiece::Item => {
                        Some(build_binding_identifier(&variable.to_string()))
                    }
                    ArrayPatternPiece::Spread => Some(Pat::Rest(RestPat {
                        arg: Box::new(build_binding_identifier(&variable.to_string())),
                        dot3_token: Default::default(),
                        span: Default::default(),
                        type_ann: None,
                    })),
                })
                .collect();

            let pat = Pat::Array(ArrayPat {
                elems,
                optional: false,
                type_ann: None,
                span: Default::default(),
            });

            Some(build_var_decl(pat, ref_or_inlined_expr(ctx, *base)))
        }
        _ => return None,
    }
}

#[cfg(test)]
mod tests {
    use crate::{testutils::*, to_ast::module_to_ast};

    #[test]
    fn to_object_patterns() {
        let block_group = test_basic_blocks_module(
            "var b, obj = { x: { a: b, c } };
            var { x: { a, c = b } = {}, ...rest } = obj;
            return c;",
        );

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        var a = undefined;
        var { x: b, ...c } = {
            x: {
                a: a,
                c: undefined
            }
        };
        var d = b;
        if (d === undefined) {
            var e = {};
        } else {
            e = d;
        }
        var { a: f, c: g } = e;
        var h = g;
        if (h === undefined) {
            var i = a;
        } else {
            i = h;
        }
        return i;
        "###);
    }

    #[test]
    fn to_array_patterns() {
        let block_group = test_basic_blocks_module(
            "var b, arr = [b, c];
            var [a, c = b, ...rest] = arr;
            return [b, a, c, ...rest];",
        );

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        var a = undefined;
        var [b, c, ...d] = [
            a,
            undefined
        ];
        var e = c;
        if (e === undefined) {
            var f = a;
        } else {
            f = e;
        }
        return [
            a,
            b,
            f,
            ...d
        ];
        "###);
    }

    #[test]
    fn to_function_arg_patterns() {
        let obj = test_basic_blocks_module(
            "var func = ({ x, y = 123, ...z }) => {return [x, y, z];};
            func()",
        );

        let tree = module_to_ast(obj);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        (function(a) {
            var { x: b, y: c, ...d } = a;
            var e = c;
            if (e === undefined) {
                var f = 123;
            } else {
                f = e;
            }
            return [
                b,
                f,
                d
            ];
        })();
        "###);

        let obj = test_basic_blocks_module(
            "var func = function ([x, y = [], ...z]) {return [x, y, z];};
            func()",
        );

        let tree = module_to_ast(obj);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        (function(a) {
            var [b, c, ...d] = a;
            var e = c;
            if (e === undefined) {
                var f = [];
            } else {
                f = e;
            }
            return [
                b,
                f,
                d
            ];
        })();
        "###);

        let obj = test_basic_blocks_module(
            "var func = function (x = 2, ...y = []) {return [x, y];};
            func()",
        );

        let tree = module_to_ast(obj);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        (function(a, ...b) {
            if (a === undefined) {
                var c = 2;
            } else {
                c = a;
            }
            if (b === undefined) {
                var d = [];
            } else {
                d = b;
            }
            return [
                c,
                d
            ];
        })();
        "###);
    }

    #[test]
    fn to_object_patterns_computed_key() {
        let block_group = test_basic_blocks_module(
            "var obj = { x: 1 };
            var { ['x']: a } = obj;
            a;",
        );

        let tree = module_to_ast(block_group);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        var { ["x"]: a } = {
            x: 1
        };
        var b = a;
        "###);
    }

    #[test]
    fn to_function_default_args_scope() {
        let obj = test_basic_blocks_module(
            "((x, setFn = () => (x = 2)) => {
                // TODO adding 'var x' here makes it return 1 instead, since the x in the setFn is a different x
                setFn(); return x;
            })(1)",
        );

        let tree = module_to_ast(obj);
        insta::assert_snapshot!(module_to_string(&tree), @r###"
        (function(a, b) {
            var c = undefined;
            c = a;
            if (b === undefined) {
                var e = (function() {
                    var d = 2;
                    c = d;
                    return d;
                });
            } else {
                e = b;
            }
            e();
            return c;
        })(1);
        "###);
    }
}
