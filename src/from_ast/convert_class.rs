use swc_ecma_ast::{Class, ClassMember, StaticBlock};

use super::{
    block_to_basic_blocks, convert_object_propname_tmp, expr_to_basic_blocks, FromAstCtx,
    FunctionLike,
};
use crate::{
    basic_blocks::{
        BasicBlockInstruction, ClassProperty, MethodKind, ObjectKey, ObjectValue,
        StructuredClassMember, StructuredFlow,
    },
    from_ast::function_to_basic_blocks_tmp,
};

/// Convert a class to basic blocks.
pub fn class_to_basic_blocks_tmp(
    ctx: &mut FromAstCtx,
    class: &Class,
    optional_name: Option<String>,
) -> Result<(Vec<StructuredFlow>, usize), String> {
    let mut before_class = vec![];

    let extends = if let Some(super_class) = &class.super_class {
        let (flow, var) = expr_to_basic_blocks(ctx, &super_class)?;
        before_class.extend(flow);

        Some(var)
    } else {
        None
    };

    let (flow, created_class) = ctx.push_instruction(BasicBlockInstruction::CreateClass(extends));
    before_class.extend(flow);

    if let Some(optional_name) = optional_name {
        let (flow, _) = ctx.declare_name(&optional_name, created_class);
        before_class.extend(flow);
    }

    let mut members = vec![];
    for member in &class.body {
        match member {
            ClassMember::Empty(_) => continue,
            ClassMember::ClassProp(class_prop) => {
                let mut prop_flow = vec![];

                let (flow, key) = convert_object_propname_tmp(ctx, &class_prop.key)?;
                prop_flow.extend(flow);

                let (flow, value) = match &class_prop.value {
                    Some(value) => expr_to_basic_blocks(ctx, &*value)?,
                    None => ctx.push_instruction(BasicBlockInstruction::Undefined),
                };
                prop_flow.extend(flow);

                members.push(StructuredClassMember::Property(
                    prop_flow,
                    ClassProperty {
                        is_static: class_prop.is_static,
                        key,
                        value: ObjectValue::Property(value),
                    },
                ));

                for _ in class_prop.decorators.iter() {
                    todo!("decorators")
                }
            }
            ClassMember::PrivateProp(class_prop) => {
                let mut prop_flow = vec![];

                let (flow, value) = match &class_prop.value {
                    Some(value) => expr_to_basic_blocks(ctx, &*value)?,
                    None => ctx.push_instruction(BasicBlockInstruction::Undefined),
                };
                prop_flow.extend(flow);

                members.push(StructuredClassMember::Property(
                    prop_flow,
                    ClassProperty {
                        is_static: class_prop.is_static,
                        key: ObjectKey::Private(class_prop.key.id.sym.to_string()),
                        value: ObjectValue::Property(value),
                    },
                ));

                for _ in class_prop.decorators.iter() {
                    todo!("decorators")
                }
            }
            ClassMember::StaticBlock(StaticBlock { body, .. }) => {
                let body_flow = block_to_basic_blocks(ctx, body.stmts.iter())?;

                members.push(StructuredClassMember::StaticBlock(body_flow));
            }
            ClassMember::Method(method) => {
                let mut prop_flow = vec![];

                let (flow, key) = convert_object_propname_tmp(ctx, &method.key)?;
                prop_flow.extend(flow);

                let (flow, _, fn_id) =
                    function_to_basic_blocks_tmp(ctx, FunctionLike::ClassMethod(&method), None)?;
                assert!(StructuredFlow::is_structured_flow_vec_empty(&flow));

                members.push(StructuredClassMember::Property(
                    prop_flow,
                    ClassProperty {
                        is_static: method.is_static,
                        key,
                        value: ObjectValue::Method(MethodKind::from(method.kind), fn_id),
                    },
                ));
            }
            ClassMember::PrivateMethod(method) => {
                let key = ObjectKey::Private(method.key.id.sym.to_string());

                let (flow, _fn_varname, fn_id) =
                    function_to_basic_blocks_tmp(ctx, FunctionLike::PrivateMethod(&method), None)?;
                assert!(StructuredFlow::is_structured_flow_vec_empty(&flow));

                members.push(StructuredClassMember::Property(
                    vec![],
                    ClassProperty {
                        is_static: method.is_static,
                        key,
                        value: ObjectValue::Method(MethodKind::from(method.kind), fn_id),
                    },
                ));
            }
            ClassMember::Constructor(method) => {
                let (flow, _fn_varname, fn_id) = function_to_basic_blocks_tmp(
                    ctx,
                    FunctionLike::ClassConstructor(&method),
                    None,
                )?;

                assert!(StructuredFlow::is_structured_flow_vec_empty(&flow));

                members.push(StructuredClassMember::Constructor(fn_id));
            }
            ClassMember::TsIndexSignature(_) => unimplemented!("TypeScript AST nodes"),
            ClassMember::AutoAccessor(_) => todo!("Class auto accessors"),
        }
    }

    Ok((
        vec![
            StructuredFlow::from_vec(before_class),
            StructuredFlow::Class(created_class, members),
        ],
        created_class,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::basic_blocks::BasicBlockGroup;
    use crate::swc_parse::swc_parse;

    fn conv_class(src: &str) -> BasicBlockGroup {
        let mut ctx = FromAstCtx::new();
        let func = swc_parse(src);
        let decl = func.body[0]
            .clone()
            .expect_stmt()
            .expect_decl()
            .expect_class();

        let (cls, _) =
            class_to_basic_blocks_tmp(&mut ctx, &*decl.class, Some(decl.ident.sym.to_string()))
                .unwrap();

        return ctx
            .wrap_up_module(Default::default(), cls)
            .take_top_level_stats();
    }

    #[test]
    fn conv_class_empty() {
        let func = conv_class("class Foo {}");
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = class
            exit = class $0 @1..@1
        }
        @1: {
            exit = class end
        }
        "###);
    }

    #[test]
    fn conv_class_prop() {
        let func = conv_class(
            "class Foo {
                prop1 = 1;
                static prop2 = 2;
                ['prop3'] = 3;
                static ['prop4'] = 4;
                123 = 5;
            }",
        );
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = class
            exit = class $0 @1..@11
        }
        @1: {
            $1 = 1
        }
        @2: {
            exit = class property ClassProperty { is_static: false, key: .prop1, value: $1 }
        }
        @3: {
            $2 = 2
        }
        @4: {
            exit = class property ClassProperty { is_static: true, key: .prop2, value: $2 }
        }
        @5: {
            $3 = "prop3"
            $4 = 3
        }
        @6: {
            exit = class property ClassProperty { is_static: false, key: [$3], value: $4 }
        }
        @7: {
            $5 = "prop4"
            $6 = 4
        }
        @8: {
            exit = class property ClassProperty { is_static: true, key: [$5], value: $6 }
        }
        @9: {
            $7 = 5
        }
        @10: {
            exit = class property ClassProperty { is_static: false, key: .123, value: $7 }
        }
        @11: {
            exit = class end
        }
        "###);
    }

    #[test]
    fn conv_class_static_block() {
        let func = conv_class(
            "class Foo {
                static {
                    var x = 1;
                }
            }",
        );
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = class
            exit = class $0 @1..@3
        }
        @1: {
            exit = class static block @2..@3
        }
        @2: {
            $1 = 1
        }
        @3: {
            exit = class end
        }
        "###);
    }

    #[test]
    fn conv_class_constructor() {
        let func = conv_class(
            "class Foo {
                constructor() {
                    var x = 1;
                }
            }",
        );
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = class
            exit = class $0 @1..@2
        }
        @1: {
            exit = class constructor FunctionId(1)
        }
        @2: {
            exit = class end
        }
        "###);
    }

    #[test]
    fn conv_class_getset() {
        let func = conv_class(
            "class Foo {
                get prop() { return 1; }
                set prop(x) { }
            }",
        );
        insta::assert_debug_snapshot!(func, @r###"
        @0: {
            $0 = class
            exit = class $0 @1..@5
        }
        @1: {
        }
        @2: {
            exit = class property ClassProperty { is_static: false, key: .prop, value: getter FunctionId(1) }
        }
        @3: {
        }
        @4: {
            exit = class property ClassProperty { is_static: false, key: .prop, value: setter FunctionId(2) }
        }
        @5: {
            exit = class end
        }
        "###);
    }
}
