use super::run_checks;

#[test]
fn global_references() {
    let res = run_checks("return typeof Object;");
    insta::assert_display_snapshot!(res, @r###""function""###);

    let res = run_checks("return Number('1234');");
    insta::assert_display_snapshot!(res, @"1234");

    let res = run_checks(
        "if (typeof foo === 'undefined') {
            globalThis.foo = 0
            foo = 1
            if (typeof foo === 'number') {
                return foo
            }
        }",
    );
    insta::assert_display_snapshot!(res, @"1");
}
