use swc_ecma_ast::{Class, ClassMember, StaticBlock};

use super::{
    block_to_basic_blocks, convert_object_propname, expr_to_basic_blocks, function_to_basic_blocks,
    FromAstCtx, FunctionLike,
};
use crate::{
    basic_blocks::{
        BasicBlockInstruction, ClassProperty, MethodKind, ObjectKey, ObjectValue,
        StructuredClassMember, StructuredFlow,
    },
    block_ops::normalize_basic_blocks_tree_at_block_index,
};

/// Convert a class to basic blocks.
pub fn class_to_basic_blocks(
    ctx: &mut FromAstCtx,
    class: &Class,
    optional_name: Option<String>,
) -> Result<usize, String> {
    let extends = class
        .super_class
        .as_ref()
        .map(|extends| expr_to_basic_blocks(ctx, &*extends));

    let created_class = ctx.push_instruction(BasicBlockInstruction::CreateClass(extends));

    if let Some(optional_name) = optional_name {
        ctx.declare_name(&optional_name, created_class);
    }

    ctx.wrap_up_block();

    let members = ctx.with_temporary_instructions(|ctx| -> Result<_, String> {
        let mut members = vec![];

        for member in &class.body {
            match member {
                ClassMember::Empty(_) => continue,
                ClassMember::ClassProp(class_prop) => {
                    let key = convert_object_propname(ctx, &class_prop.key);

                    let value = match &class_prop.value {
                        Some(value) => expr_to_basic_blocks(ctx, &*value),
                        None => ctx.push_instruction(BasicBlockInstruction::Undefined),
                    };

                    let value_flow = ctx.snip_into_structured_flow();

                    members.push(StructuredClassMember::Property(
                        vec![value_flow],
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
                ClassMember::PrivateProp(prop) => {
                    let value = match &prop.value {
                        Some(value) => expr_to_basic_blocks(ctx, &*value),
                        None => ctx.push_instruction(BasicBlockInstruction::Undefined),
                    };

                    let value_flow = ctx.snip_into_structured_flow();

                    members.push(StructuredClassMember::Property(
                        vec![value_flow],
                        ClassProperty {
                            is_static: prop.is_static,
                            key: ObjectKey::Private(prop.key.id.sym.to_string()),
                            value: ObjectValue::Property(value),
                        },
                    ));

                    for _ in prop.decorators.iter() {
                        todo!("decorators")
                    }
                }
                ClassMember::StaticBlock(StaticBlock { body, .. }) => {
                    block_to_basic_blocks(ctx, &body.stmts)?;

                    let body_flow = ctx.snip_into_structured_flow();

                    members.push(StructuredClassMember::StaticBlock(vec![body_flow]));
                }
                ClassMember::Method(method) => {
                    let key = convert_object_propname(ctx, &method.key);

                    let (_fn_varname, fn_id) =
                        function_to_basic_blocks(ctx, FunctionLike::ClassMethod(&method), None)?;

                    let method_flow = ctx.snip_into_structured_flow();

                    members.push(StructuredClassMember::Property(
                        vec![method_flow],
                        ClassProperty {
                            is_static: method.is_static,
                            key,
                            value: ObjectValue::Method(MethodKind::from(method.kind), fn_id),
                        },
                    ));
                }
                ClassMember::PrivateMethod(method) => {
                    let key = ObjectKey::Private(method.key.id.sym.to_string());

                    let (_fn_varname, fn_id) =
                        function_to_basic_blocks(ctx, FunctionLike::PrivateMethod(&method), None)?;

                    let method_flow = ctx.snip_into_structured_flow();
                    assert!(method_flow.is_structured_flow_empty());

                    members.push(StructuredClassMember::Property(
                        vec![method_flow],
                        ClassProperty {
                            is_static: method.is_static,
                            key,
                            value: ObjectValue::Method(MethodKind::from(method.kind), fn_id),
                        },
                    ));
                }
                ClassMember::Constructor(method) => {
                    let (_fn_varname, fn_id) = function_to_basic_blocks(
                        ctx,
                        FunctionLike::ClassConstructor(&method),
                        None,
                    )?;

                    let constructor_flow = ctx.snip_into_structured_flow();
                    assert!(constructor_flow.is_structured_flow_empty());

                    members.push(StructuredClassMember::Constructor(fn_id));
                }
                ClassMember::TsIndexSignature(_) => unimplemented!("TypeScript AST nodes"),
                ClassMember::AutoAccessor(_) => todo!("Class auto accessors"),
            }
        }

        let no_leftovers = ctx.snip_into_structured_flow();
        assert!(no_leftovers.is_structured_flow_empty());

        Ok(members)
    })?;

    let class = normalize_basic_blocks_tree_at_block_index(
        vec![StructuredFlow::Class(created_class, members)],
        ctx.current_block_index() + 1,
    );

    ctx.inject_blocks(class);

    ctx.wrap_up_block();

    Ok(created_class)
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

        class_to_basic_blocks(&mut ctx, &*decl.class, Some(decl.ident.sym.to_string())).unwrap();

        ctx.wrap_up_module(Default::default())
            .take_top_level_stats()
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
        @2: {
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
        @12: {
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
        @4: {
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
        @3: {
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
        @6: {
        }
        "###);
    }
}
