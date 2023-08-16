use fix_fn::fix_fn;
use swc_ecma_ast::{
    AwaitExpr, BlockStmtOrExpr, Callee, CondExpr, Decl, Expr, ExprOrSpread, IfStmt, Lit, Module,
    ModuleItem, Param, Pat, PatOrExpr, Stmt, VarDeclKind, YieldExpr,
};

use crate::scope::{ScopeTree, ScopeTreeHandle};

use super::FunctionLike;

#[derive(Debug)]
pub struct NonLocalInfo {
    pub nonlocals: Vec<String>,
    pub funscoped: Vec<String>,
}

/// given a list of statements, which vars here are used in scopes within?
pub fn find_nonlocals(func: FunctionLike) -> NonLocalInfo {
    let mut ctx = NonLocalsContext {
        scopes: ScopeTree::new(),
        depth: 0,
        found_nonlocals: vec![],
        deferred_fns: vec![],
        found_funscoped: vec![],
    };
    ctx.do_fn(func);
    ctx.undefer_all();

    NonLocalInfo {
        nonlocals: ctx.found_nonlocals,
        funscoped: ctx.found_funscoped,
    }
}

/// given a list of statements, which vars here are used in scopes within?
pub fn find_module_nonlocals(module: &Module) -> NonLocalInfo {
    let mut ctx = NonLocalsContext {
        scopes: ScopeTree::new(),
        depth: 0,
        found_nonlocals: vec![],
        deferred_fns: vec![],
        found_funscoped: vec![],
    };
    ctx.do_module(module);
    ctx.undefer_all();

    NonLocalInfo {
        nonlocals: ctx.found_nonlocals,
        funscoped: ctx.found_funscoped,
    }
}

struct NonLocalsContext<'a> {
    pub scopes: ScopeTree<()>,
    pub depth: u32,
    pub found_nonlocals: Vec<String>,
    pub deferred_fns: Vec<(FunctionLike<'a>, ScopeTreeHandle)>,
    pub found_funscoped: Vec<String>,
}

impl<'a> NonLocalsContext<'a> {
    fn assign_name(&mut self, name: String, funscoped: bool) {
        if funscoped && self.depth == 0 && !self.found_funscoped.contains(&name) {
            self.found_funscoped.push(name.clone());
        }
        self.scopes.insert(name.clone(), ());
    }

    fn read_name(&mut self, name: String) {
        let root_scope = ScopeTreeHandle(0);

        if !self
            .scopes
            .same_function_as(self.scopes.current_scope, root_scope)
        {
            if let Some(scope_of_name) = self.scopes.lookup_scope_of(&name) {
                if self.scopes.same_function_as(scope_of_name, root_scope) {
                    if !self.found_nonlocals.contains(&name) {
                        self.found_nonlocals.push(name);
                    }
                }
            }
        }
    }

    fn insert_params(&mut self, params: &[Param]) {
        let insert_pat = fix_fn!(|insert_pat, ctx: &mut NonLocalsContext, pat: &Pat| -> () {
            if let Pat::Ident(id) = &pat {
                ctx.assign_name(id.sym.to_string(), true);
            } else if let Pat::Assign(assign) = &pat {
                insert_pat(ctx, &assign.left);
            } else if let Pat::Rest(rest) = &pat {
                insert_pat(ctx, &rest.arg);
            } else {
                todo!("pattern params")
            }
        });
        params.iter().for_each(|param| {
            insert_pat(self, &param.pat);
        });
    }
    fn insert_pats(&mut self, pats: &[Pat], funscoped: bool) {
        pats.iter().for_each(|pat| {
            if let Pat::Ident(id) = &pat {
                self.assign_name(id.sym.to_string(), funscoped);
            } else {
                todo!("pattern params")
            }
        });
    }

    pub fn defer(&mut self, expr: FunctionLike<'a>) {
        self.deferred_fns.push((expr, self.scopes.current_scope));
    }

    pub fn undefer(&mut self, (expr, scope): (FunctionLike<'a>, ScopeTreeHandle)) {
        let save_scope = self.scopes.current_scope;
        self.scopes.go_to_scope(scope);
        self.scopes.go_into_function_scope();
        self.do_fn(expr);
        self.scopes.go_to_scope(save_scope);
    }

    pub fn do_fn(&mut self, expr: FunctionLike<'a>) {
        match expr {
            FunctionLike::FnExpr(function) => {
                if let Some(name) = function.ident.as_ref() {
                    self.assign_name(name.sym.to_string(), true);
                }
                self.insert_params(&function.function.params);
                block_nonlocals(self, &function.function.body.as_ref().unwrap().stmts);
            }
            FunctionLike::ArrowExpr(arrow) => {
                self.insert_pats(&arrow.params, true);
                match arrow.body.as_ref() {
                    BlockStmtOrExpr::BlockStmt(b) => block_nonlocals(self, &b.stmts),
                    BlockStmtOrExpr::Expr(e) => expr_nonlocals(self, &e),
                }
            }
            FunctionLike::FnDecl(fn_decl) => {
                // TODO does the function name exist differently inside the function, or is it the same as the symbol defined outside?
                self.assign_name(fn_decl.ident.sym.to_string(), true);
                // TODO hoisted fn decls: self.push_fndecl(fn_decl);
                self.insert_params(&fn_decl.function.params);
                block_nonlocals(self, &fn_decl.function.body.as_ref().unwrap().stmts);
            }
        }
    }

    pub fn do_module(&mut self, module: &'a Module) {
        module.body.iter().for_each(|item| match item {
            ModuleItem::Stmt(stmt) => {
                stat_hoist(self, stmt);
            }
            _ => {}
        });
        module.body.iter().for_each(|item| match item {
            ModuleItem::Stmt(stmt) => {
                stat_nonlocals(self, stmt);
            }
            ModuleItem::ModuleDecl(_) => {
                // TODO
            }
        });
    }

    pub fn undefer_all(&mut self) {
        self.depth += 1;
        while let Some(deferred) = self.deferred_fns.pop() {
            self.undefer(deferred)
        }
    }
}

fn block_nonlocals<'a>(ctx: &mut NonLocalsContext<'a>, stat: &'a [Stmt]) {
    ctx.scopes.go_into_block_scope();
    for stat in stat {
        stat_hoist(ctx, stat);
    }
    for stat in stat {
        stat_nonlocals(ctx, stat);
    }
    ctx.scopes.leave_scope();
}

fn stat_hoist(ctx: &mut NonLocalsContext<'_>, stat: &Stmt) {
    match stat {
        Stmt::Decl(Decl::Fn(fn_decl)) => {
            ctx.assign_name(fn_decl.ident.sym.to_string(), false);
        }
        Stmt::Labeled(labeled) => {
            stat_hoist(ctx, &labeled.body);
        }
        _ => {}
    }
}

fn stat_nonlocals<'a>(ctx: &mut NonLocalsContext<'a>, stat: &'a Stmt) {
    match stat {
        Stmt::Expr(expr) => {
            expr_nonlocals(ctx, expr.expr.as_ref());
        }
        Stmt::Decl(Decl::Var(var)) => {
            for decl in &var.decls {
                let Pat::Ident(ident) = &decl.name else {todo!()};
                expr_nonlocals(ctx, decl.init.as_ref().unwrap());
                ctx.assign_name(ident.sym.to_string(), var.kind == VarDeclKind::Var);
            }
        }
        Stmt::Decl(Decl::Fn(fn_decl)) => {
            ctx.defer(FunctionLike::FnDecl(fn_decl));
        }
        Stmt::DoWhile(_) => todo!(),
        Stmt::For(_) => todo!(),
        Stmt::ForIn(_) => todo!(),
        Stmt::ForOf(_) => todo!(),
        Stmt::While(whil) => {
            expr_nonlocals(ctx, &whil.test);
            stat_nonlocals(ctx, &whil.body);
        }
        Stmt::If(IfStmt {
            test, cons, alt, ..
        }) => {
            expr_nonlocals(ctx, test.as_ref());
            stat_nonlocals(ctx, cons.as_ref());
            if let Some(alt) = alt {
                stat_nonlocals(ctx, alt);
            }
        }
        Stmt::Block(block) => block_nonlocals(ctx, &block.stmts),
        Stmt::Break(_) => {}
        Stmt::Continue(_cont) => todo!("ctx.register_continue(cont.label)"),
        Stmt::Labeled(stat) => stat_nonlocals(ctx, stat.body.as_ref()),
        Stmt::Debugger(_) => todo!(),
        Stmt::With(_) => todo!(),
        Stmt::Switch(_) => todo!(),
        Stmt::Throw(thr) => {
            expr_nonlocals(ctx, &thr.arg);
        }
        Stmt::Return(ret) => {
            if let Some(ret_arg) = ret.arg.as_ref() {
                expr_nonlocals(ctx, ret_arg);
            }
        }
        Stmt::Try(ref stmt) => {
            block_nonlocals(ctx, &stmt.block.stmts);

            if let Some(ref handler) = stmt.handler {
                if let Some(p) = &handler.param {
                    ctx.assign_name(p.clone().ident().unwrap(/* TODO */).sym.to_string(), false);
                }
                block_nonlocals(ctx, &handler.body.stmts);
            }

            if let Some(ref finalizer) = stmt.finalizer {
                block_nonlocals(ctx, &finalizer.stmts);
            }
        }
        Stmt::Empty(_) => {}
        _ => {
            todo!("statements_to_ssa: stat_to_ssa: {:?} not implemented", stat)
        }
    }
}

fn expr_nonlocals<'a>(ctx: &mut NonLocalsContext<'a>, exp: &'a Expr) {
    match exp {
        // WRITES
        Expr::Assign(assign) => match &assign.left {
            PatOrExpr::Pat(e) => match e.as_ref() {
                Pat::Ident(ident) => ctx.read_name(ident.id.sym.to_string()),
                _ => todo!(),
            },
            _ => todo!(),
        },
        // READS
        Expr::Ident(ident) => {
            ctx.read_name(ident.sym.to_string());
        }
        // PUSH CLOSURES FOR THE FUTURE. IN THE FUTURE, DO THEM
        Expr::Fn(fn_expr) => ctx.defer(FunctionLike::FnExpr(fn_expr)),
        Expr::Arrow(arrow) => ctx.defer(FunctionLike::ArrowExpr(arrow)),
        // TERMINALS
        Expr::Lit(Lit::Num(_)) | Expr::This(_) => {}
        // VISIT BELOW
        Expr::Bin(bin) => {
            expr_nonlocals(ctx, &bin.left);
            expr_nonlocals(ctx, &bin.right);
        }
        Expr::Paren(paren) => {
            expr_nonlocals(ctx, &paren.expr);
        }
        Expr::Seq(seq) => {
            for expr in &seq.exprs {
                expr_nonlocals(ctx, expr);
            }
        }
        Expr::Cond(CondExpr {
            test, cons, alt, ..
        }) => {
            expr_nonlocals(ctx, &test);
            expr_nonlocals(ctx, &cons);
            expr_nonlocals(ctx, &alt);
        }
        Expr::Array(array_lit) => {
            for elem in &array_lit.elems {
                if let Some(ExprOrSpread { expr, .. }) = elem {
                    expr_nonlocals(ctx, expr);
                }
            }
        }
        Expr::Object(_) => todo!(),
        Expr::Unary(_) => todo!(),
        Expr::Update(_) => todo!(),
        Expr::Member(member) => {
            expr_nonlocals(ctx, &member.obj);
        }
        Expr::SuperProp(_) => todo!(),
        Expr::Call(call) => {
            match call.callee {
                Callee::Expr(ref expr) => expr_nonlocals(ctx, expr),
                _ => todo!("non-expr callees (super, import)"),
            };

            for arg in &call.args {
                match arg.spread {
                    Some(_) => todo!("spread args"),
                    None => expr_nonlocals(ctx, arg.expr.as_ref()),
                }
            }
        }
        Expr::New(_) => todo!(),
        Expr::Tpl(_) => todo!(),
        Expr::TaggedTpl(_) => todo!(),
        Expr::Class(_) => todo!(),
        Expr::MetaProp(_) => todo!(),
        Expr::Yield(YieldExpr { arg, .. }) => {
            if let Some(arg) = arg {
                expr_nonlocals(ctx, arg);
            }
        }
        Expr::Await(AwaitExpr { arg, .. }) => {
            expr_nonlocals(ctx, arg);
        }
        Expr::OptChain(_) => todo!(),
        Expr::PrivateName(_) => todo!("handle this in the binary op and member op"),
        Expr::Invalid(_) => unreachable!("Expr::Invalid from SWC should be impossible"),
        Expr::JSXMember(_)
        | Expr::JSXNamespacedName(_)
        | Expr::JSXEmpty(_)
        | Expr::JSXElement(_)
        | Expr::JSXFragment(_) => unreachable!("Expr::JSX from SWC should be impossible"),
        Expr::TsTypeAssertion(_)
        | Expr::TsConstAssertion(_)
        | Expr::TsNonNull(_)
        | Expr::TsAs(_)
        | Expr::TsInstantiation(_)
        | Expr::TsSatisfies(_) => unreachable!("Expr::Ts from SWC should be impossible"),
        _ => {
            todo!("statements_to_ssa: expr_to_ssa: {:?} not implemented", exp)
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::swc_parse::swc_parse;

    #[test]
    fn test_find_nonlocals() {
        let func = swc_parse(
            "
            function x(x_arg, ...x_rest) {
                let a = 1;
                let absent_inside = 0;
                let overwritten = 0;
                let overwritten_in_block = 0;
                function y() {
                    let b = 2;
                    function z() {
                        let c = 3;
                        let overwritten = 0;
                        {
                            let overwritten_in_block = 0;
                        }
                        return x_arg + a + b + c + defined_after + overwritten + overwritten_in_block + x_rest.length;
                    }
                    return z();
                }
                let defined_after = 0;
                return y();
            }
        ",
        );
        let func = func.body[0]
            .clone()
            .expect_stmt()
            .expect_decl()
            .expect_fn_decl();

        insta::assert_debug_snapshot!(find_nonlocals(FunctionLike::FnDecl(&func)), @r###"
        NonLocalInfo {
            nonlocals: [
                "x_arg",
                "a",
                "defined_after",
                "overwritten_in_block",
                "x_rest",
            ],
            funscoped: [
                "x",
                "x_arg",
                "x_rest",
            ],
        }
        "###);
    }

    #[test]
    fn test_find_nonlocals_module() {
        let module = swc_parse(
            "var x = 1;
            var a = 0;
            var x = function a() {
                return x + a;
            }",
        );

        insta::assert_debug_snapshot!(find_module_nonlocals(&module), @r###"
        NonLocalInfo {
            nonlocals: [
                "x",
            ],
            funscoped: [
                "x",
                "a",
            ],
        }
        "###);
    }

    #[test]
    fn test_find_nonlocals_module_w() {
        let module = swc_parse(
            "var outer = 1
            var bar = function bar() { outer = 9 }",
        );

        insta::assert_debug_snapshot!(find_module_nonlocals(&module), @r###"
        NonLocalInfo {
            nonlocals: [
                "outer",
            ],
            funscoped: [
                "outer",
                "bar",
            ],
        }
        "###);
    }
}
