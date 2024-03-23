use swc_ecma_ast::{Decl, FnDecl, Stmt};

use crate::basic_blocks::{Instruction, StructuredFlow};

use super::{function_to_basic_blocks, stat_to_basic_blocks, FromAstCtx, FunctionLike};

/// Turn a block into basic blocks, but operates on StructuredFlow
pub fn block_to_basic_blocks<'a, Stats>(
    ctx: &mut FromAstCtx,
    stmts: Stats,
) -> Result<Vec<StructuredFlow>, String>
where
    Stats: Iterator<Item = &'a Stmt>,
{
    let mut fn_decls = Vec::new();
    let mut non_fn_decls = Vec::new();

    let mut out_flow = Vec::with_capacity(non_fn_decls.len());

    for stat in stmts {
        match get_fn_decl(stat) {
            Some(fn_decl) => {
                fn_decls.push(fn_decl);
            }
            _ => non_fn_decls.push(stat),
        }
    }

    for fn_decl in fn_decls.iter() {
        let varname = ctx.get_var_index(); // get a name for a future Function() instruction

        let (flow, varname, fn_id) =
            function_to_basic_blocks(ctx, FunctionLike::FnDecl(fn_decl), Some(varname))?;

        assert_eq!(flow.len(), 0);
        out_flow.push(StructuredFlow::Instruction(
            varname,
            Instruction::Function(fn_id),
        ));
        let (flow, _) = ctx.declare_name(&fn_decl.ident.sym.to_string(), varname);
        out_flow.extend(flow);
    }

    for stat in non_fn_decls {
        out_flow.extend(stat_to_basic_blocks(ctx, stat)?);
    }

    Ok(out_flow)
}

fn get_fn_decl(stat: &Stmt) -> Option<&FnDecl> {
    match stat {
        Stmt::Decl(Decl::Fn(fn_decl)) => Some(fn_decl),
        Stmt::Labeled(labeled) => get_fn_decl(&labeled.body),
        _ => None,
    }
}
