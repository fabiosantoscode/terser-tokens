use super::run_checks;

#[test]
fn global_references() {
    let res = run_checks("return typeof Object;");
    insta::assert_display_snapshot!(res, @"function");

    let res = run_checks("return Number('1234');");
    insta::assert_display_snapshot!(res, @"1234");
}
