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

#[test]
fn nested_labelled_break_loop() {
    let res = run_checks(
        r###"
        var x = 1;
        outer: while (x < 10) {
            x = x + 1;
            while (x >= 5) {
                x = x + 5;
                break outer;
            }
        }
        return x;
        "###,
    );
    insta::assert_display_snapshot!(res, @"10");
}

#[test]
fn functional_for_in_loop() {
    let res = run_checks(
        r###"
        var obj = {a: 1, b: 1};
        obj.__proto__.c = 1;
        for (var x in obj) {
            obj[x] = obj[x] + 1;
        }
        return obj;
        "###,
    );
    insta::assert_display_snapshot!(res, @r###"
    {"a":2,"b":2,"c":2}
    "###);

    let res = run_checks(
        r###"
        var obj = {a: 1, b: 1};
        for (var x in obj) {
            obj[x] = obj[x] + 1;
        }
        return obj;
        "###,
    );
    insta::assert_display_snapshot!(res, @r###"
    {"a":2,"b":2}
    "###);
}

#[test]
fn functional_loops_patterns() {
    let res = run_checks(
        r###"
        var array = []
        var obj = [{ c: 1 }, { c: 2 }, {}];
        for (var { c: a = 404 } of obj) {
            array.push(a);
        }
        for (var { c = 404 } of obj) {
            array.push(c);
        }
        return array;
        "###,
    );
    insta::assert_display_snapshot!(res, @"[1, 2, 404, 1, 2, 404]");
}

#[test]
fn functional_loops_async() {
    let res = run_checks(
        r###"
        var out = [];
        var order = 0;
        async function* asyncIter() {
            order++;
            out.push(['in_fn', order])
            yield await 1;
            order++;
            out.push(['in_fn', order])
            yield await 2;
            order++;
            out.push(['in_fn', order])
        }
        for await (var x of asyncIter()) {
            order++;
            out.push(['in_loop', x, order])
        }
        return out;
        "###,
    );
    insta::assert_display_snapshot!(res, @r###"
    [["in_fn", 1], ["in_loop", 1, 2], ["in_fn", 3], ["in_loop", 2, 4], ["in_fn", 5]]
    "###);
}

#[test]
fn functional_loops_cursed() {
    let res = run_checks(
        r###"
        var obj = {a: 1, b: 1};
        for (obj.it in obj) {
            obj[obj.it] = obj[obj.it] + 1;
        }
        return obj;
        "###,
    );
    insta::assert_display_snapshot!(res, @r###"
    {"a":2,"b":2,"it":"b"}
    "###);
}
