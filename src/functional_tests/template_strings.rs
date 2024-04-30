use super::run_checks;

#[test]
fn functional_template_strings() {
    let res = run_checks("return ``");
    insta::assert_display_snapshot!(res, @"");

    let res = run_checks("return `a`");
    insta::assert_display_snapshot!(res, @"a");

    let res = run_checks("return `a${1 + 1}`");
    insta::assert_display_snapshot!(res, @"a2");

    let res = run_checks(
        "
    return (args => args[0] + args[1])`a${1}b`",
    );
    insta::assert_display_snapshot!(res, @"ab");

    let res = run_checks(
        "
    return ((args, a, b) => args[0] + (a + b))`a${1}b${2}`",
    );
    insta::assert_display_snapshot!(res, @"a3");
}

#[test]
fn functional_template_strings_escapes() {
    let res = run_checks("return [`literal newline: \n`]");
    insta::assert_display_snapshot!(res, @"[\"literal newline: \\n\"]");

    let res = run_checks("return [`newline: \\n`]");
    insta::assert_display_snapshot!(res, @"[\"newline: \\n\"]");

    let res = run_checks("return [`escaped newline: \\\\n`]");
    insta::assert_display_snapshot!(res, @"[\"escaped newline: \\\\n\"]");

    let res = run_checks(
        "
    return (args => args.raw[0])`a\\n`",
    );
    insta::assert_display_snapshot!(res, @"a\\n");
}
