use swc_ecma_ast::{
    ExportSpecifier, ImportSpecifier, Module, ModuleDecl, ModuleExportName, ModuleItem, Stmt,
};

use super::{statements_to_basic_blocks, FromAstCtx};
use crate::basic_blocks::{BasicBlockModule, Export, Import, ModuleSummary};

pub fn module_to_basic_blocks(filename: &str, module: &Module) -> Result<BasicBlockModule, String> {
    let mut ctx = FromAstCtx::new();
    let summary = find_importexport(&mut ctx, filename, &module);

    let top_level_stats: Vec<&Stmt> = module
        .body
        .iter()
        .flat_map(|item| match item {
            ModuleItem::Stmt(stmt) => Some(stmt),
            ModuleItem::ModuleDecl(_) => None,
        })
        .collect();
    let top_level_stats = statements_to_basic_blocks(&mut ctx, &top_level_stats);

    Ok(BasicBlockModule {
        summary,
        top_level_stats,
        functions: ctx.functions,
        imports: ctx.imports,
        exports: ctx.exports,
    })
}

fn find_importexport(
    ctx: &mut FromAstCtx,
    filename: &str,
    module: &swc_ecma_ast::Module,
) -> ModuleSummary {
    fn export_name_to_str(n: &ModuleExportName) -> String {
        match n {
            ModuleExportName::Str(s) => s.value.to_string(),
            ModuleExportName::Ident(i) => i.sym.to_string(),
        }
    }
    fn remote_and_local(exp: &Option<ModuleExportName>, local: String) -> (String, String) {
        (
            match exp {
                Some(n) => export_name_to_str(n),
                None => local.clone(),
            },
            local,
        )
    }

    // Imports
    module.body.iter().for_each(|body_item| match body_item {
        ModuleItem::ModuleDecl(ModuleDecl::Import(import_decl)) => {
            for spec in &import_decl.specifiers {
                match spec {
                    ImportSpecifier::Named(named) => {
                        let (imp, local) =
                            remote_and_local(&named.imported, named.local.to_string());

                        ctx.register_import(Import::Name(imp, local));
                    }
                    ImportSpecifier::Default(d) => {
                        ctx.register_import(Import::Default(d.local.sym.to_string()));
                    }
                    ImportSpecifier::Namespace(n) => {
                        ctx.register_import(Import::Star(n.local.sym.to_string()));
                    }
                }
            }
        }
        _ => {}
    });

    // Exports
    module.body.iter().for_each(|body_item| match body_item {
        ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(_)) => todo!(),
        ModuleItem::ModuleDecl(ModuleDecl::ExportNamed(named)) => {
            for specifier in &named.specifiers {
                match specifier {
                    ExportSpecifier::Named(n) => {
                        let (imp, local) =
                            remote_and_local(&n.exported, export_name_to_str(&n.orig));

                        ctx.register_export(Export::Name(imp, local));
                    }
                    _ => todo!(),
                }
            }
        }
        ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultDecl(_)) => todo!(),
        ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultExpr(_)) => todo!(),
        ModuleItem::ModuleDecl(ModuleDecl::ExportAll(_)) => todo!(),
        _ => {}
    });

    ModuleSummary {
        filename: filename.into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{basic_blocks::FunctionId, swc_parse::swc_parse};

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
        )
        .unwrap();
        insta::assert_debug_snapshot!(module, @r###"
        BasicBlockModule {
            summary: ModuleSummary {
                filename: "index.js",
            },
            top_level_stats: @0: {
                $6 = FunctionId(0)
                $7 = undefined
                exit = return $7
            }
            ,
            functions: [
                function():
                @0: {
                    $2 = FunctionId(1)
                    $3 = $2
                    $4 = call $3()
                    exit = return $4
                }
                ,
                function():
                @0: {
                    $0 = 2
                    exit = return $0
                }
                ,
            ],
        }
        "###);
    }

    #[test]
    fn a_closure() {
        let module = module_to_basic_blocks(
            "index.js",
            &swc_parse(
                "var foo = function() {
                    var bar = 1
                    return function() {
                        return bar
                    }
                }",
            ),
        )
        .unwrap();
        insta::assert_debug_snapshot!(module.get_function(FunctionId(0)).unwrap(), @r###"
        function(nonlocals=[NonLocalId(0)]):
        @0: {
            $0 = 1
            $3 = FunctionId(1)
            exit = return $3
        }
        "###);
        insta::assert_debug_snapshot!(module.get_function(FunctionId(1)).unwrap(), @r###"
        function(nonlocals=[NonLocalId(0)]):
        @0: {
            $1 = read_non_local $$0
            exit = return $1
        }
        "###);
    }

    #[test]
    fn imports() {
        let module = module_to_basic_blocks(
            "index.js",
            &swc_parse(
                "import { foo } from 'bar'
                export { foo }",
            ),
        );
        insta::assert_debug_snapshot!(module, @r###"
        Ok(
            BasicBlockModule {
                summary: ModuleSummary {
                    filename: "index.js",
                },
                top_level_stats: @0: {
                    $0 = undefined
                    exit = return $0
                }
                ,
                imports: [
                    Name(
                        "foo#0",
                        "foo#0",
                    ),
                ],
                exports: [
                    Name(
                        "foo",
                        "foo",
                    ),
                ],
            },
        )
        "###);
    }
}
