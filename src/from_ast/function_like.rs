use swc_ecma_ast::{ArrowExpr, FnDecl, FnExpr, Pat, Stmt};

#[derive(Clone)]
pub enum FunctionLike<'a> {
    FnDecl(&'a FnDecl),
    FnExpr(&'a FnExpr),
    ArrowExpr(&'a ArrowExpr),
}

impl<'a> FunctionLike<'a> {
    pub fn function_length(&self) -> usize {
        match self {
            FunctionLike::FnDecl(FnDecl { function, .. })
            | FunctionLike::FnExpr(FnExpr { function, .. }) => function
                .params
                .iter()
                .filter(|param| match param.pat {
                    swc_ecma_ast::Pat::Ident(_) => true,
                    swc_ecma_ast::Pat::Rest(_) => false,
                    _ => todo!("non-ident function param"),
                })
                .count(),
            _ => todo!(),
        }
    }

    pub fn get_params(&self) -> Vec<&Pat> {
        match self {
            FunctionLike::FnDecl(FnDecl { function, .. })
            | FunctionLike::FnExpr(FnExpr { function, .. }) => {
                function.params.iter().map(|param| &param.pat).collect()
            }
            _ => todo!(),
        }
    }

    pub(crate) fn get_statements(&'a self) -> Vec<&'a Stmt> {
        match self {
            FunctionLike::FnDecl(FnDecl { function, .. })
            | FunctionLike::FnExpr(FnExpr { function, .. }) => function
                .body
                .as_ref()
                .expect("function body is empty")
                .stmts
                .iter()
                .collect(),
            _ => todo!(),
        }
    }
}
