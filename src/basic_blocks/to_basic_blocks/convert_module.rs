use super::super::basic_block_module::{BasicBlockModule, ModuleSummary};
use super::convert::statements_to_basic_blocks;

use swc_ecma_ast::{Module, ModuleItem, Stmt};

pub struct ConvertModuleCtx {}
impl ConvertModuleCtx {}

pub fn module_to_basic_blocks(
    ctx: &mut ConvertModuleCtx,
    filename: &str,
    module: &Module,
) -> Result<BasicBlockModule, String> {
    let summary = get_module_summary(ctx, filename, &module);

    let top_level_stats: Vec<&Stmt> = module
        .body
        .iter()
        .map(|item| match item {
            ModuleItem::Stmt(stmt) => stmt,
            _ => todo!("module item: {:?}", item),
        })
        .collect();
    let top_level_stats = statements_to_basic_blocks(&top_level_stats);

    Ok(BasicBlockModule {
        summary,
        top_level_stats,
    })
}

pub fn get_module_summary(
    _ctx: &mut ConvertModuleCtx,
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
        let mut ctx = ConvertModuleCtx {};
        let module = module_to_basic_blocks(&mut ctx, "index.js", &swc_parse("1 + 1"));
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
            },
        )
        "###);
    }
}
