use swc_ecma_ast::{
    AwaitExpr, Callee, Class, ClassMember, CondExpr, Decl, DoWhileStmt, Expr, ExprOrSpread,
    ForHead, ForInStmt, ForOfStmt, ForStmt, GetterProp, IfStmt, MemberProp, MethodProp, Module,
    ModuleItem, ObjectLit, ObjectPatProp, Pat, PatOrExpr, Prop, PropName, PropOrSpread, SetterProp,
    Stmt, SuperProp, VarDecl, VarDeclKind, VarDeclOrExpr, WhileStmt, YieldExpr,
};

use crate::scope::{ScopeTree, ScopeTreeHandle};

use super::{FuncBlockOrRetExpr, FunctionLike};

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
    pub scopes: ScopeTree<String, ()>,
    pub depth: u32,
    pub found_nonlocals: Vec<String>,
    pub deferred_fns: Vec<(FunctionLike<'a>, ScopeTreeHandle)>,
    pub found_funscoped: Vec<String>,
}

impl<'a> NonLocalsContext<'a> {
    fn declare_name(&mut self, name: String, funscoped: bool) {
        if funscoped && self.depth == 0 && !self.found_funscoped.contains(&name) {
            self.found_funscoped.push(name.clone());
        }
        if funscoped {
            self.scopes.insert_at_function(name.clone(), ());
        } else {
            self.scopes.insert(name.clone(), ());
        }
    }

    fn use_name(&mut self, name: String) {
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

    fn insert_params(&mut self, params: Vec<&'a Pat>) {
        for pat in params {
            pat_nonlocals(self, &pat, PatType::FunArg);
        }
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
                    self.declare_name(name.sym.to_string(), true);
                }
            }
            FunctionLike::FnDecl(fn_decl) => {
                // TODO does the function name exist differently inside the function, or is it the same as the symbol defined outside?
                self.declare_name(fn_decl.ident.sym.to_string(), true);
                // TODO hoisted fn decls: self.push_fndecl(fn_decl);
            }
            _ => {}
        };

        self.insert_params(expr.get_params());

        match expr.get_body() {
            FuncBlockOrRetExpr::Block(block) => {
                block_nonlocals(self, &block.stmts);
            }
            FuncBlockOrRetExpr::RetExpr(ret_expr) => {
                expr_nonlocals(self, &ret_expr);
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
            ctx.declare_name(fn_decl.ident.sym.to_string(), false);
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
        Stmt::Decl(Decl::Var(var)) => vardecl_nonlocals(ctx, var),
        Stmt::Decl(Decl::Fn(fn_decl)) => {
            ctx.defer(FunctionLike::FnDecl(fn_decl));
        }
        Stmt::Decl(Decl::Class(class)) => {
            class_nonlocals(ctx, class.class.as_ref());
        }
        Stmt::ForIn(ForInStmt {
            left, right, body, ..
        })
        | Stmt::ForOf(ForOfStmt {
            left, right, body, ..
        }) => {
            match left {
                ForHead::VarDecl(var) => vardecl_nonlocals(ctx, var),
                ForHead::Pat(pat) => pat_nonlocals(ctx, pat, PatType::ForInOf),
                ForHead::UsingDecl(_) => todo!(),
            }
            expr_nonlocals(ctx, right);
            stat_nonlocals(ctx, body);
        }
        Stmt::For(ForStmt {
            init,
            test,
            update,
            body,
            ..
        }) => {
            match init {
                Some(VarDeclOrExpr::Expr(exp)) => expr_nonlocals(ctx, exp),
                Some(VarDeclOrExpr::VarDecl(var)) => vardecl_nonlocals(ctx, var),
                _ => {}
            };
            match test {
                Some(expr) => expr_nonlocals(ctx, expr),
                _ => {}
            };
            match update {
                Some(expr) => expr_nonlocals(ctx, expr),
                _ => {}
            }
            stat_nonlocals(ctx, body);
        }
        Stmt::DoWhile(DoWhileStmt { test, body, .. })
        | Stmt::While(WhileStmt { test, body, .. }) => {
            expr_nonlocals(ctx, test);
            stat_nonlocals(ctx, body);
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
        Stmt::Continue(_) => {}
        Stmt::Labeled(stat) => stat_nonlocals(ctx, stat.body.as_ref()),
        Stmt::Debugger(_) => {}
        Stmt::With(_) => todo!("with statement"),
        Stmt::Switch(switch) => {
            expr_nonlocals(ctx, &switch.discriminant);
            for case in &switch.cases {
                if let Some(test) = &case.test {
                    expr_nonlocals(ctx, test);
                }
                block_nonlocals(ctx, &case.cons);
            }
        }
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
                    pat_nonlocals(ctx, p, PatType::DeclareErr);
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

fn vardecl_nonlocals<'a>(ctx: &mut NonLocalsContext<'a>, var: &'a Box<VarDecl>) {
    for decl in &var.decls {
        if let Some(ref init) = decl.init {
            expr_nonlocals(ctx, init)
        }
        pat_nonlocals(
            ctx,
            &decl.name,
            match var.kind {
                VarDeclKind::Var => PatType::DeclareVar,
                VarDeclKind::Let | VarDeclKind::Const => PatType::DeclareLet,
            },
        );
    }
}

#[derive(Copy, Clone)]
enum PatType {
    Assign,
    DeclareVar,
    DeclareLet,
    DeclareErr,
    FunArg,
    ForInOf,
}

fn pat_nonlocals<'a>(ctx: &mut NonLocalsContext<'a>, pat_expr: &'a Pat, pat_type: PatType) {
    match &pat_expr {
        Pat::Ident(ident) => {
            let name = ident.id.sym.to_string();
            match pat_type {
                PatType::Assign | PatType::ForInOf => ctx.use_name(name),
                PatType::DeclareVar | PatType::FunArg => ctx.declare_name(name, true),
                PatType::DeclareLet | PatType::DeclareErr => ctx.declare_name(name, false),
            }
        }
        Pat::Array(rx) => rx.elems.iter().for_each(|elem| match elem {
            Some(elem) => pat_nonlocals(ctx, elem, pat_type),
            None => (),
        }),
        Pat::Rest(rest) => pat_nonlocals(ctx, rest.arg.as_ref(), pat_type),
        Pat::Object(object_pat) => {
            object_pat.props.iter().for_each(|prop| {
                match prop {
                    ObjectPatProp::KeyValue(pat_prop) => {
                        match &pat_prop.key {
                            PropName::Computed(computed) => {
                                expr_nonlocals(ctx, &computed.expr);
                            }
                            PropName::Ident(_)
                            | PropName::Str(_)
                            | PropName::Num(_)
                            | PropName::BigInt(_) => {} // nothing inside
                        }
                        pat_nonlocals(ctx, &pat_prop.value, pat_type);
                    }
                    ObjectPatProp::Assign(pat_prop) => {
                        if let Some(value) = &pat_prop.value {
                            expr_nonlocals(ctx, value);
                        }
                        let name = pat_prop.key.sym.to_string();
                        match pat_type {
                            PatType::Assign | PatType::ForInOf => ctx.use_name(name),
                            _ => ctx.declare_name(name, matches!(pat_type, PatType::DeclareVar)),
                        };
                    }
                    ObjectPatProp::Rest(pat_prop) => {
                        pat_nonlocals(ctx, pat_prop.arg.as_ref(), pat_type)
                    }
                }
            })
        }
        Pat::Assign(assign_pat) => {
            expr_nonlocals(ctx, &assign_pat.right);
            pat_nonlocals(ctx, &assign_pat.left, pat_type);
        }
        Pat::Invalid(_) => unreachable!(),
        Pat::Expr(assignee_expr) => {
            expr_nonlocals(ctx, assignee_expr.as_ref());
        }
    }
}

fn expr_nonlocals<'a>(ctx: &mut NonLocalsContext<'a>, exp: &'a Expr) {
    match exp {
        // WRITES
        Expr::Assign(assign) => {
            match &assign.left {
                PatOrExpr::Pat(pattern) => pat_nonlocals(ctx, pattern.as_ref(), PatType::Assign),
                PatOrExpr::Expr(expr) => expr_nonlocals(ctx, expr.as_ref()),
            }
            expr_nonlocals(ctx, &assign.right)
        }
        // READS
        Expr::Ident(ident) => {
            ctx.use_name(ident.sym.to_string());
        }
        // PUSH CLOSURES FOR THE FUTURE. IN THE FUTURE, DO THEM
        Expr::Fn(fn_expr) => ctx.defer(FunctionLike::FnExpr(fn_expr)),
        Expr::Arrow(arrow) => ctx.defer(FunctionLike::ArrowExpr(arrow)),
        // TERMINALS
        Expr::Lit(_) | Expr::This(_) => {}
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
        Expr::Object(ObjectLit { props, .. }) => {
            for prop in props {
                match prop {
                    PropOrSpread::Spread(spread) => {
                        expr_nonlocals(ctx, &spread.expr);
                    }
                    PropOrSpread::Prop(prop) => match prop.as_ref() {
                        Prop::Shorthand(ident) => ctx.use_name(ident.sym.to_string()),
                        Prop::KeyValue(kv) => {
                            if let PropName::Computed(expr) = &kv.key {
                                expr_nonlocals(ctx, &expr.expr)
                            }
                            expr_nonlocals(ctx, &kv.value)
                        }
                        Prop::Getter(GetterProp { key, .. })
                        | Prop::Setter(SetterProp { key, .. })
                        | Prop::Method(MethodProp { key, .. }) => {
                            if let PropName::Computed(expr) = &key {
                                expr_nonlocals(ctx, &expr.expr)
                            }
                        }
                        Prop::Assign(_) => unreachable!(),
                    },
                }
            }
        }
        Expr::Unary(unary_expr) => expr_nonlocals(ctx, &unary_expr.arg),
        Expr::Update(update_expr) => expr_nonlocals(ctx, &update_expr.arg),
        Expr::Member(member) => {
            expr_nonlocals(ctx, &member.obj);

            if let MemberProp::Computed(computed_prop) = &member.prop {
                expr_nonlocals(ctx, &computed_prop.expr)
            }
        }
        Expr::SuperProp(sp) => match &sp.prop {
            SuperProp::Ident(ident) => ctx.use_name(ident.sym.to_string()),
            SuperProp::Computed(expr) => expr_nonlocals(ctx, &expr.expr),
        },
        Expr::Call(call) => {
            match call.callee {
                Callee::Expr(ref expr) => expr_nonlocals(ctx, expr),
                Callee::Super(_) | Callee::Import(_) => {}
            };

            for arg in &call.args {
                expr_nonlocals(ctx, arg.expr.as_ref())
            }
        }
        Expr::New(new_expr) => {
            expr_nonlocals(ctx, &new_expr.callee);

            if let Some(args) = &new_expr.args {
                for arg in args {
                    expr_nonlocals(ctx, arg.expr.as_ref())
                }
            }
        }
        Expr::Tpl(tpl) => {
            for expr in &tpl.exprs {
                expr_nonlocals(ctx, expr)
            }
        }
        Expr::TaggedTpl(ttpl) => {
            expr_nonlocals(ctx, &ttpl.tag);
            for expr in &ttpl.tpl.exprs {
                expr_nonlocals(ctx, expr)
            }
        }
        Expr::Class(class) => {
            class_nonlocals(ctx, class.class.as_ref());
        }
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
    };
}

fn class_nonlocals<'a>(ctx: &mut NonLocalsContext<'a>, class: &'a Class) {
    if let Some(extends) = &class.super_class {
        expr_nonlocals(ctx, &*extends);
    }

    for member in &class.body {
        match member {
            ClassMember::Constructor(constructor) => {
                ctx.defer(FunctionLike::ClassConstructor(&constructor));
            }
            ClassMember::Method(method) => {
                if let PropName::Computed(ident) = &method.key {
                    expr_nonlocals(ctx, &ident.expr);
                }
                for dec in &method.function.decorators {
                    expr_nonlocals(ctx, &dec.expr);
                }
                ctx.defer(FunctionLike::ClassMethod(&method));
            }
            ClassMember::PrivateMethod(method) => {
                for dec in &method.function.decorators {
                    expr_nonlocals(ctx, &dec.expr);
                }
                ctx.defer(FunctionLike::PrivateMethod(&method));
            }
            ClassMember::ClassProp(class_prop) => {
                if let PropName::Computed(ident) = &class_prop.key {
                    expr_nonlocals(ctx, &ident.expr);
                }
                if let Some(value) = &class_prop.value {
                    expr_nonlocals(ctx, &value);
                }
                for dec in &class_prop.decorators {
                    expr_nonlocals(ctx, &dec.expr);
                }
            }
            ClassMember::PrivateProp(private_prop) => {
                if let Some(value) = &private_prop.value {
                    expr_nonlocals(ctx, &value);
                }
                for dec in &private_prop.decorators {
                    expr_nonlocals(ctx, &dec.expr);
                }
            }
            ClassMember::StaticBlock(static_block) => {
                for stat in static_block.body.stmts.iter() {
                    stat_nonlocals(ctx, stat);
                }
            }
            ClassMember::Empty(_) => continue,
            ClassMember::AutoAccessor(_) => todo!("auto accessors"),
            ClassMember::TsIndexSignature(_) => unreachable!("TS index signatures"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::swc_parse::swc_parse;

    #[test]
    fn test_find_nonlocals() {
        let func = swc_parse(
            "function x(x_arg, ...x_rest) {
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
            }",
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
