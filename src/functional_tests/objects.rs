use super::run_checks;

#[test]
fn assigning_objects() {
    let res = run_checks(
        "let o = {};
        o.prop = 1;
        return [o.prop, o.noprop];",
    );
    insta::assert_display_snapshot!(res, @"[1, undefined]");

    let res = run_checks(
        "let o = {prop:{}};
        o.prop.propprop = 1;
        return [o.prop, o.prop.propprop];",
    );
    insta::assert_display_snapshot!(res, @"[{\"propprop\":1}, 1]");
}

#[test]
fn assigning_objects_computed() {
    let res = run_checks(
        "let o = {};
        o['pro' + 'p'] = 1;
        return [o.prop, o.noprop];",
    );
    insta::assert_display_snapshot!(res, @"[1, undefined]");

    let res = run_checks(
        "let o = {};
        o['prop'] = 1;
        return [o.prop, o.noprop];",
    );
    insta::assert_display_snapshot!(res, @"[1, undefined]");

    let res = run_checks(
        "let o = {['prop']:{}};
        o.prop['propprop'] = 1;
        return [o.prop, o['prop'].propprop];",
    );
    insta::assert_display_snapshot!(res, @"[{\"propprop\":1}, 1]");
}

#[test]
fn object_patterns() {
    let res = run_checks(
        "let o = {a: 1, b: 2, c: 3};
        let {a = 'default unused', nonExisting: non = 4, ...rest} = o;
        return [a, non, rest];",
    );
    insta::assert_display_snapshot!(res, @"[1, 4, {\"b\":2,\"c\":3}]");
}

// TODO this should log 1 2 3 4
// var {a: {[log(3)]: a} = (log(2), {}), [log(4)]: b, x } = {x: log(1)};
