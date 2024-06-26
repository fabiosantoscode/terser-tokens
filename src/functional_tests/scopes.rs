use super::run_checks;

#[test]
fn functional_block_scopes() {
    let res = run_checks(
        "console.log(myVar);
        {
            const myVar = 6;
            console.log(myVar);
        }
        var myVar = 5;
        console.log(myVar);",
    );
    insta::assert_display_snapshot!(res, @"
    undefined
    6
    5
    ");
}

#[test]
fn functional_block_scopes_2() {
    let res = run_checks(
        "console.log(myVar);
        var myVar = 5;
        console.log(myVar);
        {
            const myVar = 6;
            console.log(myVar);
        }
        console.log(myVar);",
    );
    insta::assert_display_snapshot!(res, @"
    undefined
    5
    6
    5
    ");
}

/*
TODO error is slightly wrong because `const myVar` is removed
#[test]
fn functional_temporal_dead_zone() {
    let res = run_checks("
        try {
            myVar;
        } catch (e) {
            console.log(e.message);
        }
        const myVar = 5;
    ");
    insta::assert_display_snapshot!(res, @"
    Cannot access 'myVar' before initialization
    ");
}
*/
