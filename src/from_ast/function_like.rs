use swc_ecma_ast::{ArrowExpr, BlockStmt, BlockStmtOrExpr, Expr, FnDecl, FnExpr, Pat};

#[derive(Clone)]
pub enum FunctionLike<'a> {
    FnDecl(&'a FnDecl),
    FnExpr(&'a FnExpr),
    ArrowExpr(&'a ArrowExpr),
}

pub enum FuncBlockOrRetExpr<'a> {
    Block(&'a BlockStmt),
    RetExpr(&'a Expr),
}

impl<'a> FunctionLike<'a> {
    pub fn function_length(&self) -> usize {
        let param_counts = |pat: &Pat| match pat {
            Pat::Rest(_) | Pat::Assign(_) => false,
            _ => true,
        };
        match self {
            FunctionLike::FnDecl(FnDecl { function, .. })
            | FunctionLike::FnExpr(FnExpr { function, .. }) => function
                .params
                .iter()
                .filter(|param| param_counts(&param.pat))
                .count(),
            FunctionLike::ArrowExpr(ArrowExpr { params, .. }) => {
                params.iter().filter(|pat| param_counts(pat)).count()
            }
        }
    }

    pub fn get_params(&self) -> Vec<&Pat> {
        match self {
            FunctionLike::FnDecl(FnDecl { function, .. })
            | FunctionLike::FnExpr(FnExpr { function, .. }) => {
                function.params.iter().map(|param| &param.pat).collect()
            }
            FunctionLike::ArrowExpr(ArrowExpr { params, .. }) => params.iter().collect(),
        }
    }

    pub(crate) fn get_body(&'a self) -> FuncBlockOrRetExpr<'a> {
        match self {
            FunctionLike::FnDecl(FnDecl { function, .. })
            | FunctionLike::FnExpr(FnExpr { function, .. }) => {
                FuncBlockOrRetExpr::Block(function.body.as_ref().expect("function body is empty"))
            }
            FunctionLike::ArrowExpr(ArrowExpr { body, .. }) => match body.as_ref() {
                BlockStmtOrExpr::BlockStmt(block) => FuncBlockOrRetExpr::Block(block),
                BlockStmtOrExpr::Expr(expr) => FuncBlockOrRetExpr::RetExpr(&**expr),
            },
        }
    }
}
