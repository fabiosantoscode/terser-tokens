use swc_ecma_ast::{
    AssignExpr, AssignOp, BindingIdent, Decl, Expr, ExprStmt, Ident, Pat, PatOrExpr, Stmt, VarDecl,
    VarDeclKind, VarDeclarator,
};

pub fn build_identifier(i: String) -> Expr {
    Expr::Ident(Ident::new(i.into(), Default::default()))
}

pub fn build_identifier_str(i: &str) -> Expr {
    Expr::Ident(Ident::new(i.into(), Default::default()))
}

pub fn build_binding_identifier(i: &str) -> Pat {
    Pat::Ident(BindingIdent {
        id: Ident::new(i.into(), Default::default()),
        type_ann: None,
    })
}

pub fn build_var_decl(name: Pat, init: Expr) -> Stmt {
    Stmt::Decl(Decl::Var(Box::new(VarDecl {
        span: Default::default(),
        kind: VarDeclKind::Var,
        declare: false,
        decls: vec![VarDeclarator {
            span: Default::default(),
            name,
            init: Some(Box::new(init)),
            definite: false,
        }],
    })))
}

pub fn build_block(stmts: Vec<Stmt>) -> Stmt {
    Stmt::Block(swc_ecma_ast::BlockStmt {
        span: Default::default(),
        stmts,
    })
}

pub fn build_var_assign(varname: &str, value: Expr) -> Stmt {
    Stmt::Expr(ExprStmt {
        span: Default::default(),
        expr: Box::new(Expr::Assign(AssignExpr {
            span: Default::default(),
            op: AssignOp::Assign,
            left: PatOrExpr::Pat(Box::new(build_binding_identifier(&varname))),
            right: Box::new(value),
        })),
    })
}
