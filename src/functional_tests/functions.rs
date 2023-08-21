use super::run_checks;

#[test]
fn calling_functions() {
    let res = run_checks(
        "let fn = (x) => x + 1;
        return fn(1);",
    );
    insta::assert_display_snapshot!(res, @"2");

    let res = run_checks(
        "let fn = (x) => { return x + 1 };
        return fn(1);",
    );
    insta::assert_display_snapshot!(res, @"2");

    let res = run_checks(
        "let fn = function(x) { return x + 1 };
        return fn(1);",
    );
    insta::assert_display_snapshot!(res, @"2");

    let res = run_checks(
        "function fn(x) { return x + 1 };
        return fn(1);",
    );
    insta::assert_display_snapshot!(res, @"2");
}

#[test]
fn hoisting() {
    let res = run_checks(
        "return fn(1);
        function fn(x) { return x + 1 }",
    );
    insta::assert_display_snapshot!(res, @"2");

    let res = run_checks(
        "return fn(1);
        function fn(x) { return x + y() }
        function y() { return 100 }",
    );
    insta::assert_display_snapshot!(res, @"101");

    let res = run_checks(
        "y = function () { return 1 };
        return fn(1);
        function fn(x) { return x + y() }
        function y() { return 99999 }",
    );
    insta::assert_display_snapshot!(res, @"2");
}

#[test]
fn recursion() {
    let res = run_checks(
        "var fn = function (x) { return x ? fn(0) : x }
        return fn(1)",
    );
    insta::assert_display_snapshot!(res, @"0");

    let res = run_checks(
        "var fn = function fn(x) { return x ? fn(0) : x }
        return fn(1)",
    );
    insta::assert_display_snapshot!(res, @"0");
}

#[test]
fn dual_binding() {
    let res = run_checks(
        "var fn_expr = function fn_expr_inner(x) {
            fn_expr_inner = fn_expr;
            return x ? fn_expr_inner(0) : x
        }
        return fn_expr(1)",
    );
    insta::assert_display_snapshot!(res, @"0");
    let res = run_checks(
        "var fn_expr = function fn_expr_inner(x) {
            var fn_expr_inner = fn_expr;
            return x ? fn_expr_inner(0) : x
        }
        return fn_expr(1)",
    );
    insta::assert_display_snapshot!(res, @"0");
}
