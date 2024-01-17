use super::{
    build_privatename, build_propname_str_or_ident, function_to_ast, ref_or_inlined_expr,
    statements_forced_to_expr_ast, to_statements, ToAstContext,
};
use crate::basic_blocks::{ObjectKey, ObjectValue, StructuredClassMember};
use swc_ecma_ast::{
    BlockStmt, Class, ClassDecl, ClassMember, ClassMethod, ClassProp, ComputedPropName,
    Constructor, Decl, Expr, Function, Ident, ParamOrTsParamProp, PrivateMethod, PrivateName,
    PrivateProp, PropName, StaticBlock, Stmt,
};

/// Called when we see the CreateClass instruction
pub fn class_to_ast_prepare(ctx: &mut ToAstContext, varname: usize, extends: Option<usize>) {
    ctx.register_class(varname, extends);
    ctx.create_varname_for(varname);
}

pub fn class_to_ast(
    ctx: &mut ToAstContext,
    varname: usize,
    members: &Vec<StructuredClassMember>,
) -> Vec<Stmt> {
    let super_class: Option<Box<Expr>> = ctx.take_class(varname).map(|super_var| {
        let expr = ref_or_inlined_expr(ctx, super_var);
        Box::new(expr)
    });

    let mut body: Vec<ClassMember> = Vec::with_capacity(members.len());

    for member in members {
        let member_ast = match member {
            StructuredClassMember::Property(block, prop) => {
                match prop.value {
                    ObjectValue::Property(value) => {
                        let value_expr =
                            |ctx: &mut ToAstContext<'_>, needs_forced_statement: bool| {
                                if !needs_forced_statement {
                                    ref_or_inlined_expr(ctx, value)
                                } else {
                                    statements_forced_to_expr_ast(ctx, block, value)
                                }
                            };

                        let class_prop = |key: PropName, value: Expr| {
                            ClassMember::ClassProp(ClassProp {
                                key,
                                value: Some(Box::new(value)), // TODO empty props should be None here, not Some(undefined)
                                is_static: prop.is_static,
                                decorators: vec![],
                                accessibility: None,
                                is_abstract: false,
                                definite: false,

                                // TS
                                type_ann: None,
                                is_optional: false,
                                is_override: false,
                                readonly: false,
                                declare: false,

                                span: Default::default(),
                            })
                        };
                        let class_private_prop = |key: PrivateName, value: Expr| {
                            ClassMember::PrivateProp(PrivateProp {
                                key,
                                value: Some(Box::new(value)),

                                is_static: prop.is_static,
                                decorators: vec![],
                                accessibility: None,
                                definite: false,

                                // TS
                                type_ann: None,
                                is_optional: false,
                                is_override: false,
                                readonly: false,

                                span: Default::default(),
                            })
                        };

                        match &prop.key {
                            ObjectKey::Computed(comp_key) => {
                                let expr = statements_forced_to_expr_ast(ctx, block, *comp_key);
                                let prop_name = PropName::Computed(ComputedPropName {
                                    span: Default::default(),
                                    expr: Box::new(expr),
                                });

                                class_prop(prop_name, value_expr(ctx, false))
                            }
                            ObjectKey::NormalKey(key) => {
                                let prop_name = build_propname_str_or_ident(&key);

                                class_prop(prop_name, value_expr(ctx, true))
                            }
                            ObjectKey::Private(name) => {
                                let priv_name = build_privatename(name);

                                class_private_prop(priv_name, value_expr(ctx, true))
                            }
                        }
                    }
                    ObjectValue::Method(kind, fn_id) => {
                        let function = ctx.module.take_function(fn_id).unwrap();
                        let function = function_to_ast(ctx, function);

                        let class_method = |key: PropName, function: Function| {
                            ClassMember::Method(ClassMethod {
                                key,
                                function: Box::new(function),
                                kind: kind.into(),

                                is_static: prop.is_static,
                                is_abstract: false,
                                is_optional: false,
                                is_override: false,
                                accessibility: None,
                                span: Default::default(),
                            })
                        };
                        let private_class_method = |key: PrivateName, function: Function| {
                            ClassMember::PrivateMethod(PrivateMethod {
                                key,
                                function: Box::new(function),
                                kind: kind.into(),

                                is_static: prop.is_static,
                                is_abstract: false,
                                is_optional: false,
                                is_override: false,
                                accessibility: None,
                                span: Default::default(),
                            })
                        };

                        match &prop.key {
                            ObjectKey::Computed(comp_key) => {
                                let expr = statements_forced_to_expr_ast(ctx, block, *comp_key);
                                let prop_name = PropName::Computed(ComputedPropName {
                                    span: Default::default(),
                                    expr: Box::new(expr),
                                });

                                class_method(prop_name, function)
                            }
                            ObjectKey::NormalKey(key) => {
                                let prop_name = build_propname_str_or_ident(&key);

                                class_method(prop_name, function)
                            }
                            ObjectKey::Private(name) => {
                                let priv_name = build_privatename(name);

                                private_class_method(priv_name, function)
                            }
                        }
                    }
                }
            }
            StructuredClassMember::Constructor(fn_id) => {
                let function = ctx.module.take_function(*fn_id).unwrap();
                let function = function_to_ast(ctx, function);

                ClassMember::Constructor(Constructor {
                    key: PropName::Ident(Ident::new("constructor".into(), Default::default())),
                    params: function
                        .params
                        .into_iter()
                        .map(|param| ParamOrTsParamProp::Param(param))
                        .collect(),

                    body: function.body,

                    accessibility: None,
                    is_optional: false,
                    span: Default::default(),
                })
            }
            StructuredClassMember::StaticBlock(stmts) => {
                let blk = StaticBlock {
                    span: Default::default(),
                    body: BlockStmt {
                        span: Default::default(),
                        stmts: stmts
                            .iter()
                            .flat_map(|stat| to_statements(ctx, stat))
                            .collect(),
                    },
                };

                ClassMember::StaticBlock(blk)
            }
        };

        body.push(member_ast);
    }

    let ident = Ident::new(ctx.get_varname_for(varname).into(), Default::default());
    let class = Class {
        body,
        super_class,
        decorators: vec![], // no decorator support right now
        is_abstract: false,
        span: Default::default(),
        type_params: None,
        super_type_params: None,
        implements: vec![],
    };

    vec![Stmt::Decl(Decl::Class(ClassDecl {
        declare: false,
        ident,
        class: Box::new(class),
    }))]
}

#[cfg(test)]
mod tests {
    use super::super::module_to_ast;
    use crate::testutils::*;

    fn test_to_ast(source: &str) -> String {
        let blocks = test_basic_blocks_module(source);

        let ast = module_to_ast(blocks);

        module_to_string(&ast)
    }

    #[test]
    fn class_to_ast_props() {
        let out = test_to_ast(
            "var a_var = 1
            class A {
                prop = a_var;
                [1 + 1] = 2;
                static prop = 1;
                static [1 + 1] = 2;
            }",
        );

        insta::assert_display_snapshot!(out, @"
            class a {
                \"prop\" = 1;
                [1 + 1] = 2;
                static \"prop\" = 1;
                static [1 + 1] = 2;
            }
            return undefined;
        ");
    }

    #[test]
    fn class_to_ast_computed_prop_and_val() {
        let out = test_to_ast(
            "class A {
                [0 + 1] = 2 + 3;
            }",
        );
        insta::assert_display_snapshot!(out, @"
            class a {
                [0 + 1] = 2 + 3;
            }
            return undefined;
        ");

        let out = test_to_ast(
            "var a;
            class A {
                [(a = 0) + 1] = (a++) + 3;
            }",
        );
        insta::assert_display_snapshot!(out, @"
            class a {
                [(b = 0, b + 1)] = b++ + 3;
            }
            return undefined;
            var b;
        ");
    }

    #[test]
    fn class_to_ast_static_blocks() {
        let out = test_to_ast(
            "var a = 1;
            class A {
                static {
                    a = 2;
                }
            }
            return a",
        );

        insta::assert_display_snapshot!(out, @"
            class a {
                static{
                    var b = 2;
                }
            }
            return b;
        ");
    }
}
