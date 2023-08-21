use swc_ecma_ast::{Decl, FnDecl, Stmt};

use crate::basic_blocks::BasicBlockInstruction;

use super::{function_to_basic_blocks, stat_to_basic_blocks, FromAstCtx, FunctionLike};

/// Convert block to basic blocks. This will deal with block scopes as well.
pub fn block_to_basic_blocks(ctx: &mut FromAstCtx, stmts: &Vec<Stmt>) -> Result<(), String> {
    block_to_basic_blocks_inner(ctx, stmts.iter())
}

/// Same as `block_to_basic_blocks` but takes a vec of statement refs.
pub fn block_statrefs_to_basic_blocks(
    ctx: &mut FromAstCtx,
    stmts: Vec<&Stmt>,
) -> Result<(), String> {
    block_to_basic_blocks_inner(ctx, stmts.into_iter())
}

fn block_to_basic_blocks_inner<'a, Stats>(ctx: &mut FromAstCtx, stmts: Stats) -> Result<(), String>
where
    Stats: Iterator<Item = &'a Stmt>,
{
    let mut fn_decls = Vec::new();
    let mut non_fn_decls = Vec::new();

    for stat in stmts {
        match get_fn_decl(stat) {
            Some(fn_decl) => {
                let varname = ctx.bump_var_index(); // get a name for a future Function() instruction
                ctx.assign_name(&fn_decl.ident.sym.to_string(), varname);
                fn_decls.push((varname, fn_decl));
            }
            _ => non_fn_decls.push(stat),
        }
    }

    for (varname, fn_decl) in fn_decls.iter() {
        let func_id = function_to_basic_blocks(ctx, FunctionLike::FnDecl(fn_decl))?;
        ctx.arbitrarily_set_id(*varname, BasicBlockInstruction::Ref(func_id));
    }

    for stat in non_fn_decls {
        stat_to_basic_blocks(ctx, stat)
    }
    ctx.wrap_up_block();

    Ok(())
}

fn get_fn_decl(stat: &Stmt) -> Option<&FnDecl> {
    match stat {
        Stmt::Decl(Decl::Fn(fn_decl)) => Some(fn_decl),
        Stmt::Labeled(labeled) => get_fn_decl(&labeled.body),
        _ => None,
    }
}
