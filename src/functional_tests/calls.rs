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
}
