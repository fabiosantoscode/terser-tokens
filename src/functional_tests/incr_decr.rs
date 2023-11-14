use super::run_checks;

#[test]
fn functional_global_incrdecr() {
    let res = run_checks(
        "globalThis.x = 1;
        x++;
        return ++x;",
    );
    insta::assert_display_snapshot!(res, @"3");

    let res = run_checks(
        "let x = 1;
        x++;
        return ++x;",
    );
    insta::assert_display_snapshot!(res, @"3");

    let res = run_checks(
        "let o = { x: 1 };
        o.x++;
        return ++o.x;",
    );
    insta::assert_display_snapshot!(res, @"3");
}
