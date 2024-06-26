use super::run_checks;

#[test]
fn functional_ops() {
    let res = run_checks("return 1 - 2");
    insta::assert_display_snapshot!(res, @"-1");
    let res = run_checks("return 1 + 2");
    insta::assert_display_snapshot!(res, @"3");
    let res = run_checks("return 1 * 2");
    insta::assert_display_snapshot!(res, @"2");
    let res = run_checks("return 1 / 2");
    insta::assert_display_snapshot!(res, @"0.5");
    let res = run_checks("return id('a') + (1 + id(2))");
    insta::assert_display_snapshot!(res, @"a3");

    let res = run_checks("return 0b1010 & 0b1100");
    insta::assert_display_snapshot!(res, @"8");
    let res = run_checks("return 0b1010 | 0b1100");
    insta::assert_display_snapshot!(res, @"14");
    let res = run_checks("return 0b1010 ^ 0b1100");
    insta::assert_display_snapshot!(res, @"6");
    let res = run_checks("return 0b1010 >> 2");
    insta::assert_display_snapshot!(res, @"2");
    let res = run_checks("return 0b1010 << 2");
    insta::assert_display_snapshot!(res, @"40");
    let res = run_checks("return 0b1010 >>> 2");
    insta::assert_display_snapshot!(res, @"2");
}

#[test]
fn conditional_ops() {
    let res = run_checks("return 0 || 1");
    insta::assert_display_snapshot!(res, @"1");
    let res = run_checks("return 1 || 2");
    insta::assert_display_snapshot!(res, @"1");
    let res = run_checks("return 1 && 0");
    insta::assert_display_snapshot!(res, @"0");
    let res = run_checks("return 1 && 2");
    insta::assert_display_snapshot!(res, @"2");

    let res = run_checks(
        "let x = 3;
        return 0 && (x = 1, 2) || x",
    );
    insta::assert_display_snapshot!(res, @"3");
    let res = run_checks(
        "let x = 3;
        return 0 || (x = 1, 2) || x",
    );
    insta::assert_display_snapshot!(res, @"2");
}

#[test]
fn functional_in_ops() {
    let res = run_checks("return 'toString' in { toString: 1 }");
    insta::assert_display_snapshot!(res, @"true");

    let res = run_checks("return 'toString' in {}");
    insta::assert_display_snapshot!(res, @"true");

    let res = run_checks("return 'xxxx' in {}");
    insta::assert_display_snapshot!(res, @"false");
}

#[test]
fn functional_instanceof_ops() {
    let res = run_checks("return {} instanceof Object");
    insta::assert_display_snapshot!(res, @"true");

    let res = run_checks("return {} instanceof Array");
    insta::assert_display_snapshot!(res, @"false");
}
