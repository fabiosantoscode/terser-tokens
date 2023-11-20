use swc_ecma_ast::{
    ArrowExpr, BlockStmt, BlockStmtOrExpr, ClassMethod, Expr, FnDecl, FnExpr, Pat, PrivateMethod,
};

#[derive(Clone)]
pub enum FunctionLike<'a> {
    ClassMethod(&'a ClassMethod),
    PrivateMethod(&'a PrivateMethod),
    FnDecl(&'a FnDecl),
    FnExpr(&'a FnExpr),
    ArrowExpr(&'a ArrowExpr),
}

pub enum FuncBlockOrRetExpr<'a> {
    Block(&'a BlockStmt),
    RetExpr(&'a Expr),
}

impl<'a> FunctionLike<'a> {
    pub fn get_params(&self) -> Vec<&Pat> {
        match self {
            FunctionLike::FnDecl(FnDecl { function, .. })
            | FunctionLike::FnExpr(FnExpr { function, .. })
            | FunctionLike::ClassMethod(ClassMethod { function, .. })
            | FunctionLike::PrivateMethod(PrivateMethod { function, .. }) => {
                function.params.iter().map(|param| &param.pat).collect()
            }
            FunctionLike::ArrowExpr(ArrowExpr { params, .. }) => params.iter().collect(),
        }
    }

    pub(crate) fn get_body(&'a self) -> FuncBlockOrRetExpr<'a> {
        match self {
            FunctionLike::FnDecl(FnDecl { function, .. })
            | FunctionLike::FnExpr(FnExpr { function, .. })
            | FunctionLike::ClassMethod(ClassMethod { function, .. })
            | FunctionLike::PrivateMethod(PrivateMethod { function, .. }) => {
                FuncBlockOrRetExpr::Block(function.body.as_ref().expect("function body is empty"))
            }
            FunctionLike::ArrowExpr(ArrowExpr { body, .. }) => match body.as_ref() {
                BlockStmtOrExpr::BlockStmt(block) => FuncBlockOrRetExpr::Block(block),
                BlockStmtOrExpr::Expr(expr) => FuncBlockOrRetExpr::RetExpr(&**expr),
            },
        }
    }
}
