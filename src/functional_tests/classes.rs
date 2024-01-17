use super::run_checks;

#[test]
fn functional_empty_class() {
    let res = run_checks("class X {}; return new X().constructor.name === X.name");
    insta::assert_display_snapshot!(res, @"true");

    let res = run_checks("var X = class X {}; return new X().constructor.name === X.name");
    insta::assert_display_snapshot!(res, @"true");
}

#[test]
fn functional_members_class() {
    let res = run_checks(
        "class X { static prop = true; prop = false; };
        return X.prop",
    );
    insta::assert_display_snapshot!(res, @"true");

    let res = run_checks(
        "class X { static prop = 'static'; prop = 'instance'; };
        let x = new X();
        return [X.prop, x.prop]",
    );
    insta::assert_display_snapshot!(res, @"[\"static\", \"instance\"]");

    let res = run_checks(
        "class X {
            static prop = 1;
            static {
                this.prop++;
                X.prop++;
            }
        };
        return X.prop",
    );
    insta::assert_display_snapshot!(res, @"3");
}

#[test]
fn class_private_props() {
    let res = run_checks(
        "class X {
            #prop = 1;
            #getProp() { return this.#prop; }
            getProp() { return this.#getProp(); }
            setProp(v) { this.#prop = v; }
        };
        let x = new X();
        x.setProp(2);
        return x.getProp()",
    );
    insta::assert_display_snapshot!(res, @"2");
}

#[test]
fn class_computed_members() {
    let res = run_checks(
        "let x = 1;
        class X { [x] = x + 1; };
        return new X()[1]",
    );
    insta::assert_display_snapshot!(res, @"2");

    let res = run_checks(
        "let x = 1;
        class X { [x]() { return x + 1; } };
        return new X()[1]()",
    );
    insta::assert_display_snapshot!(res, @"2");

    let res = run_checks(
        "let x = 1;
        class X { static [x]() { return x + 1; } };
        return X[1]()",
    );
    insta::assert_display_snapshot!(res, @"2");
}

#[test]
fn class_inheritance() {
    let res = run_checks(
        "class Base {
            prop = 1;
            getProp() { return this.prop; }
        }
        class X extends Base {
            prop = 2;
        }
        return [new X().getProp(), new Base().getProp()]",
    );
    insta::assert_display_snapshot!(res, @"[2, 1]");
}

#[test]
fn class_constructor() {
    let res = run_checks(
        "class X {
            constructor() { this.prop = 2; }
        }
        return new X().prop",
    );
    insta::assert_display_snapshot!(res, @"2");
}

#[test]
fn class_super() {
    let res = run_checks(
        "class Base {
            get1() { return 1; }
        }

        class X extends Base {
            get2() { return super.get1() + 1; }
        }
        return new X().get2()",
    );
    insta::assert_display_snapshot!(res, @"2");
}
