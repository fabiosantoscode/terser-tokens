use swc_ecma_ast::{Class, ClassMember, StaticBlock};

use super::{
    block_to_basic_blocks, convert_object_propname, expr_to_basic_blocks, function_to_basic_blocks,
    FromAstCtx, FunctionLike,
};
use crate::basic_blocks::{
    BasicBlockExit, BasicBlockInstruction, ClassProperty, MethodKind, ObjectKey, ObjectValue,
};

/// Convert a class to basic blocks.
pub fn class_to_basic_blocks(
    ctx: &mut FromAstCtx,
    class: &Class,
    optional_name: Option<String>,
) -> Result<usize, String> {
    let head = ctx.wrap_up_block();

    let extends = class
        .super_class
        .as_ref()
        .map(|extends| expr_to_basic_blocks(ctx, &*extends));

    let created_class = ctx.push_instruction(BasicBlockInstruction::CreateClass(extends));

    let class_start = ctx.wrap_up_block();

    if let Some(optional_name) = optional_name {
        ctx.declare_name(&optional_name, created_class);
    }

    let class_end = match class.body.len() {
        0 => class_start,
        _ => {
            for member in &class.body {
                match member {
                    ClassMember::Empty(_) => continue,
                    ClassMember::ClassProp(class_prop) => {
                        ctx.wrap_up_block();

                        let key = convert_object_propname(ctx, &class_prop.key);

                        let value = match &class_prop.value {
                            Some(value) => expr_to_basic_blocks(ctx, &*value),
                            None => ctx.push_instruction(BasicBlockInstruction::Undefined),
                        };

                        let prop = ctx.wrap_up_block();

                        ctx.set_exit(
                            prop,
                            BasicBlockExit::ClassProperty(
                                ClassProperty {
                                    is_static: class_prop.is_static,
                                    key,
                                    value: ObjectValue::Property(value),
                                },
                                prop + 1,
                            ),
                        );

                        for _ in class_prop.decorators.iter() {
                            todo!()
                        }
                    }
                    ClassMember::PrivateProp(prop) => {
                        ctx.wrap_up_block();

                        let value = match &prop.value {
                            Some(value) => expr_to_basic_blocks(ctx, &*value),
                            None => ctx.push_instruction(BasicBlockInstruction::Undefined),
                        };

                        let prop_idx = ctx.wrap_up_block();

                        ctx.set_exit(
                            prop_idx,
                            BasicBlockExit::ClassProperty(
                                ClassProperty {
                                    is_static: prop.is_static,
                                    key: ObjectKey::Private(prop.key.id.sym.to_string()),
                                    value: ObjectValue::Property(value),
                                },
                                prop_idx + 1,
                            ),
                        );

                        for _ in prop.decorators.iter() {
                            todo!()
                        }
                    }
                    ClassMember::StaticBlock(StaticBlock { body, .. }) => {
                        ctx.wrap_up_block();

                        let start = ctx.wrap_up_block();

                        ctx.wrap_up_block();

                        block_to_basic_blocks(ctx, &body.stmts)?;

                        let end = ctx.wrap_up_block();
                        ctx.set_exit(start, BasicBlockExit::ClassPushStaticBlock(start + 1, end));

                        let after = ctx.wrap_up_block();

                        ctx.set_exit(end, BasicBlockExit::ClassPopStaticBlock(after))
                    }
                    ClassMember::Method(method) => {
                        ctx.wrap_up_block();

                        let key = convert_object_propname(ctx, &method.key);

                        let (_fn_varname, fn_id) = function_to_basic_blocks(
                            ctx,
                            FunctionLike::ClassMethod(&method),
                            None,
                        )?;

                        // the above function pushes no instructions, but we stay on the safe side
                        let value = ctx.wrap_up_block();

                        let after = ctx.wrap_up_block();

                        ctx.set_exit(
                            value,
                            BasicBlockExit::ClassProperty(
                                ClassProperty {
                                    is_static: method.is_static,
                                    key,
                                    value: ObjectValue::Method(
                                        MethodKind::from(method.kind),
                                        fn_id,
                                    ),
                                },
                                after,
                            ),
                        );
                    }
                    ClassMember::PrivateMethod(method) => {
                        ctx.wrap_up_block();

                        let key = ObjectKey::Private(method.key.id.sym.to_string());

                        let (_fn_varname, fn_id) = function_to_basic_blocks(
                            ctx,
                            FunctionLike::PrivateMethod(&method),
                            None,
                        )?;

                        // the above function pushes no instructions, but we stay on the safe side
                        let value = ctx.wrap_up_block();

                        let after = ctx.wrap_up_block();

                        ctx.set_exit(
                            value,
                            BasicBlockExit::ClassProperty(
                                ClassProperty {
                                    is_static: method.is_static,
                                    key,
                                    value: ObjectValue::Method(
                                        MethodKind::from(method.kind),
                                        fn_id,
                                    ),
                                },
                                after,
                            ),
                        );
                    }
                    ClassMember::Constructor(method) => {
                        ctx.wrap_up_block();

                        let (_fn_varname, fn_id) = function_to_basic_blocks(
                            ctx,
                            FunctionLike::ClassConstructor(&method),
                            None,
                        )?;

                        // the above function pushes no instructions, but we stay on the safe side
                        let value = ctx.wrap_up_block();

                        let after = ctx.wrap_up_block();

                        ctx.set_exit(value, BasicBlockExit::ClassConstructor(fn_id, after));
                    }
                    ClassMember::TsIndexSignature(_) => unimplemented!("TypeScript AST nodes"),
                    ClassMember::AutoAccessor(_) => todo!("Class auto accessors"),
                    
                }
            }
            ctx.wrap_up_block()
        }
    };

    let after = ctx.wrap_up_block();

    ctx.set_exit(
        head,
        BasicBlockExit::ClassStart(created_class, class_start, class_end),
    );
    ctx.set_exit(class_end, BasicBlockExit::ClassEnd(after));

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
            exit = class end after @2
        }
        @2: {
            $1 = undefined
            exit = return $1
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
            exit = jump @2
        }
        @2: {
            exit = class property ClassProperty { is_static: false, key: .prop1, value: $1 } after @3
        }
        @3: {
            $2 = 2
            exit = jump @4
        }
        @4: {
            exit = class property ClassProperty { is_static: true, key: .prop2, value: $2 } after @5
        }
        @5: {
            $3 = "prop3"
            $4 = 3
            exit = jump @6
        }
        @6: {
            exit = class property ClassProperty { is_static: false, key: [$3], value: $4 } after @7
        }
        @7: {
            $5 = "prop4"
            $6 = 4
            exit = jump @8
        }
        @8: {
            exit = class property ClassProperty { is_static: true, key: [$5], value: $6 } after @9
        }
        @9: {
            $7 = 5
            exit = jump @10
        }
        @10: {
            exit = class property ClassProperty { is_static: false, key: .123, value: $7 } after @11
        }
        @11: {
            exit = class end after @12
        }
        @12: {
            $8 = undefined
            exit = return $8
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
            exit = jump @4
        }
        @3: {
            exit = class end after @4
        }
        @4: {
            $2 = undefined
            exit = return $2
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
            exit = class constructor FunctionId(1) after @2
        }
        @2: {
            exit = class end after @3
        }
        @3: {
            $1 = undefined
            exit = return $1
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
            exit = jump @2
        }
        @2: {
            exit = class property ClassProperty { is_static: false, key: .prop, value: getter FunctionId(1) } after @3
        }
        @3: {
            exit = jump @4
        }
        @4: {
            exit = class property ClassProperty { is_static: false, key: .prop, value: setter FunctionId(2) } after @5
        }
        @5: {
            exit = class end after @6
        }
        @6: {
            $1 = undefined
            exit = return $1
        }
        "###);
    }
}
