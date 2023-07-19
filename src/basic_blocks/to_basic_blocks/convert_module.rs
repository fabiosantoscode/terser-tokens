use super::super::basic_block_module::{BasicBlockModule, ModuleSummary};
use super::convert::statements_to_basic_blocks;
use super::convert_context::ConvertContext;

use swc_ecma_ast::{Module, ModuleItem, Stmt};

pub fn module_to_basic_blocks(filename: &str, module: &Module) -> Result<BasicBlockModule, String> {
    let mut ctx = ConvertContext::new();
    let summary = get_module_summary(&mut ctx, filename, &module);

    let top_level_stats: Vec<&Stmt> = module
        .body
        .iter()
        .map(|item| match item {
            ModuleItem::Stmt(stmt) => stmt,
            _ => todo!("module item: {:?}", item),
        })
        .collect();
    let top_level_stats = statements_to_basic_blocks(&mut ctx, &top_level_stats);

    Ok(BasicBlockModule {
        summary,
        top_level_stats,
        functions: ctx.functions,
    })
}

pub fn get_module_summary(
    _ctx: &mut ConvertContext,
    filename: &str,
    _module: &swc_ecma_ast::Module,
) -> ModuleSummary {
    ModuleSummary {
        filename: filename.into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::swc_parse::swc_parse;

    #[test]
    fn test_basic_blocks_module() {
        let module = module_to_basic_blocks("index.js", &swc_parse("1 + 1"));
        insta::assert_debug_snapshot!(module, @r###"
        Ok(
            BasicBlockModule {
                summary: ModuleSummary {
                    filename: "index.js",
                },
                top_level_stats: @0: {
                    $0 = 1
                    $1 = 1
                    $2 = $0 + $1
                    $3 = undefined
                    exit = return $3
                }
                ,
                functions: [],
            },
        )
        "###);
    }

    #[test]
    fn a_function() {
        let module = module_to_basic_blocks(
            "index.js",
            &swc_parse(
                "var foo = function() {
                    var bar = function () {
                        return 2
                    }
                    return bar()
                }",
            ),
        );
        insta::assert_debug_snapshot!(module, @r###"
        Ok(
            BasicBlockModule {
                summary: ModuleSummary {
                    filename: "index.js",
                },
                top_level_stats: @0: {
                    $0 = FunctionId(0)
                    $1 = undefined
                    exit = return $1
                }
                ,
                functions: [
                    @0: {
                        $0 = FunctionId(1)
                        $1 = $0
                        $2 = call $1()
                        exit = return $2
                    }
                    ,
                    @0: {
                        $0 = 2
                        exit = return $0
                    }
                    ,
                ],
            },
        )
        "###);
    }
}
