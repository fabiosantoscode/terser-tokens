use super::run_checks;

#[test]
fn functional_switch() {
    let res = run_checks(
        "switch (true) {
            case false: console.log('before')
            default: console.log('default')
            case false: console.log('after')
        }",
    );
    insta::assert_display_snapshot!(res, @"
    default
    after
    ");

    let res = run_checks(
        "switch (true) {
            case false: console.log('before')
            case false: console.log('after')
            default: console.log('default')
        }",
    );
    insta::assert_display_snapshot!(res, @"default");

    let res = run_checks(
        "switch (true) {
            case true: console.log('before')
            default: console.log('default')
            case false: console.log('after')
        }",
    );
    insta::assert_display_snapshot!(res, @"
    before
    default
    after
    ");

    let res = run_checks(
        "switch (true) {
            case false: console.log('before')
            default: console.log('default')
            case true: console.log('after')
        }",
    );
    insta::assert_display_snapshot!(res, @"after");
}

#[test]
fn functional_switch_sides() {
    let res = run_checks(
        "switch ((console.log('before_expr'), true)) {
            case (console.log('before_test'), false): console.log('before')
            default: console.log('default')
            case false: console.log('after')
        }",
    );
    insta::assert_display_snapshot!(res, @r###"
    before_expr
    before_test
    default
    after
    "###);
}
