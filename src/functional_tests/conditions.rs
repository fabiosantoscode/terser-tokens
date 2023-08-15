use super::run_checks;

#[test]
fn if_stat() {
    let res = run_checks(
        r###"
            let x = 1;
            if (x == 1) x = 2;
            else x = 3;
            return x;
        "###,
    );
    insta::assert_display_snapshot!(res, @"2");

    let res = run_checks(
        r###"
            let x = 1;
            if (x == 1) return 2;
            else return 3;
        "###,
    );
    insta::assert_display_snapshot!(res, @"2");

    let res = run_checks(
        r###"
            let x = 1;
            if (1) x = 2;
            else 3;
            return x;
        "###,
    );
    insta::assert_display_snapshot!(res, @"2");
}

#[test]
fn ternary() {
    let res = run_checks(
        r###"
            let x = 1 ? 2 : 3;
            return x;
        "###,
    );
    insta::assert_display_snapshot!(res, @"2");

    let res = run_checks(
        r###"
            let x = 999;
            1 ? (x = 2) : 3;
            return x;
        "###,
    );
    insta::assert_display_snapshot!(res, @"2");
}
