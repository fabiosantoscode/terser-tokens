use super::run_checks;

#[test]
fn functional_test_loop() {
    let res = run_checks(
        r###"
        var x = 1;
        while (x < 10) { x = x + 1; }
        return x;
        "###,
    );
    insta::assert_display_snapshot!(res, @"10");
}
