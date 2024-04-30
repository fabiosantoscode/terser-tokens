use swc_ecma_ast::{
    ArrowExpr, AssignExpr, AssignOp, BindingIdent, BlockStmt, BlockStmtOrExpr, CallExpr, Callee,
    Decl, Expr, ExprStmt, Ident, Param, ParenExpr, Pat, PatOrExpr, PrivateName, PropName,
    ReturnStmt, Stmt, VarDecl, VarDeclKind, VarDeclarator,
};

use crate::basic_blocks::identifier_needs_quotes;

pub fn build_parens(expr: Expr) -> Expr {
    Expr::Paren(ParenExpr {
        span: Default::default(),
        expr: Box::new(expr),
    })
}

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

pub fn build_multivar_decl(names: Vec<String>) -> Option<Stmt> {
    match names.len() {
        0 => None,
        _ => Some(Stmt::Decl(Decl::Var(Box::new(VarDecl {
            span: Default::default(),
            kind: VarDeclKind::Var,
            declare: false,
            decls: names
                .into_iter()
                .map(|name| VarDeclarator {
                    span: Default::default(),
                    name: build_binding_identifier(&name),
                    init: None,
                    definite: false,
                })
                .collect(),
        })))),
    }
}

pub fn build_empty_var_decl(name: Pat) -> VarDecl {
    VarDecl {
        span: Default::default(),
        kind: VarDeclKind::Var,
        declare: false,
        decls: vec![VarDeclarator {
            span: Default::default(),
            name,
            init: None,
            definite: false,
        }],
    }
}

pub fn build_block(stmts: Vec<Stmt>) -> Stmt {
    Stmt::Block(swc_ecma_ast::BlockStmt {
        span: Default::default(),
        stmts,
    })
}

pub fn build_propname_str_or_ident(key: &str) -> PropName {
    if identifier_needs_quotes(key) {
        PropName::Str(key.into())
    } else {
        PropName::Ident(Ident::new(key.into(), Default::default()))
    }
}

pub fn build_privatename(key: &str) -> PrivateName {
    PrivateName {
        span: Default::default(),
        id: Ident::new(key.into(), Default::default()),
    }
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

pub fn build_ident_param(i: &str) -> Param {
    Param {
        span: Default::default(),
        decorators: Default::default(),
        pat: build_binding_identifier(i),
    }
}

pub fn build_iife(mut stmts: Vec<Stmt>, ret: Expr) -> Expr {
    stmts.push(Stmt::Return(ReturnStmt {
        span: Default::default(),
        arg: Some(Box::new(ret)),
    }));

    let body = BlockStmtOrExpr::BlockStmt(BlockStmt {
        span: Default::default(),
        stmts,
    });
    let callee = Expr::Arrow(ArrowExpr {
        span: Default::default(),
        params: vec![],
        body: Box::new(body),
        is_async: false,
        is_generator: false,
        type_params: None,
        return_type: None,
    });
    let callee = Expr::Paren(ParenExpr {
        span: Default::default(),
        expr: Box::new(callee),
    });
    let callee = Callee::Expr(Box::new(callee));
    Expr::Call(CallExpr {
        span: Default::default(),
        callee,
        args: vec![],
        type_args: None,
    })
}
