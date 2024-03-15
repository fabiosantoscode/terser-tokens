use super::run_checks;

#[test]
fn functional_closures() {
    let res = run_checks("
        let a = 100;
        let f = function (b) {
            return a + b;
        }
        return f(1);
    ");
    insta::assert_display_snapshot!(res, @"101");
}
