#[cfg(test)]
use crate::tokenizer::tokenizer::Tokenizer;

#[test]
fn general_test() {
    insta::assert_snapshot!(test_tok("'he\\'\"llo'?.world"), @"'he\\'\"llo' ?. world");
    insta::assert_snapshot!(test_tok("\"hello\"?.world"), @"\"hello\" ?. world");
    insta::assert_snapshot!(test_tok("1 -2"), @"1 - 2");
    insta::assert_snapshot!(test_tok("1.2...valueOf"), @"1.2 ... valueOf");
    insta::assert_snapshot!(test_tok("1.2.valueOf"), @"1.2 . valueOf");
    insta::assert_snapshot!(test_tok("0\n.valueOf"), @"0 . valueOf");
    insta::assert_snapshot!(test_tok("a =>"), @"a =>");
    insta::assert_snapshot!(test_tok("\"\\u{FF}\""), @"\"\\u{FF}\"");
    insta::assert_snapshot!(test_tok("1 / 2"), @"1 / 2");
    insta::assert_snapshot!(test_tok("1 | 0"), @"1 | 0");
    insta::assert_snapshot!(test_tok("1 /* 2 */ 3"), @"1 3");
    insta::assert_snapshot!(test_tok("1 /* 3"), @"1 <Failure>");
    insta::assert_snapshot!(test_tok("#private in this"), @"#private in this");
    insta::assert_snapshot!(test_tok("var \\u{ff} = 1"), @"var \\u{ff} = 1");
    insta::assert_snapshot!(test_tok("var f\\u{ff} = 1"), @"var f\\u{ff} = 1");
    insta::assert_snapshot!(test_tok("this.#private"), @"this . #private");
    insta::assert_snapshot!(test_tok("tpl`foo${1}`"), @"tpl `foo${ 1 }`");
    insta::assert_snapshot!(test_tok("`foo${{ x }}`"), @"`foo${ { x } }`");
    insta::assert_snapshot!(test_tok("`foo${`${{ x }}`}`"), @"`foo${ `${ { x } }` }`");
    insta::assert_snapshot!(test_tok("`foo${{ x: `hi${{ x }}` }}`"), @"`foo${ { x : `hi${ { x } }` } }`");
    insta::assert_snapshot!(test_tok("<<=>>>!==>==>"), @"<<= >>> !== >= =>");
    insta::assert_snapshot!(test_tok("function x(){return () => {}}"), @"function x ( ) { return ( ) => { } }");
    insta::assert_snapshot!(test_tok("1 'x"), @"1 <Failure>");
    insta::assert_snapshot!(test_tok("`\\r`"), @r###"`\r`"###);
    insta::assert_debug_snapshot!(test_tok("`\r`"), @r###""`\r`""###);
    insta::assert_debug_snapshot!(test_tok("`\\r\n`"), @r###""`\\r\n`""###);
    insta::assert_debug_snapshot!(test_tok("`\r\n`"), @r###""`\r\n`""###);

    insta::assert_debug_snapshot!(test_tok("(function (global, factory) {\n  typeof exports === 'object' && typeof module !== 'undefined' ? factory(exports, require('@jridgewell/source-map')) : \n typeof define === 'function' && define.amd ? define(['exports', '@jridgewell/source-map'], factory) : \n(global = typeof globalThis !== 'undefined' ? globalThis : global || self, factory(global.Terser = {}, global.sourceMap));\n}(this, (function (exports, sourceMap) { 'use strict';"), @r###""( function ( global , factory ) { typeof exports === 'object' && typeof module !== 'undefined' ? factory ( exports , require ( '@jridgewell/source-map' ) ) : typeof define === 'function' && define . amd ? define ( [ 'exports' , '@jridgewell/source-map' ] , factory ) : ( global = typeof globalThis !== 'undefined' ? globalThis : global | | self , factory ( global . Terser = { } , global . sourceMap ) ) ; } ( this , ( function ( exports , sourceMap ) { 'use strict' ;""###);
}

#[cfg(test)]
fn test_tok(input: &str) -> String {
    use crate::tokenizer::tokens::JsToken;

    let mut tx = Tokenizer::new(String::from(input), "test.js".to_string(), false, false);
    let mut tokens = String::new();
    loop {
        match tx.next_token() {
            Ok(JsToken::EOF) => {
                return tokens;
            }
            Ok(_) => {
                if tokens.len() > 0 {
                    tokens.push_str(" ");
                }
                tokens.push_str(&tx.text[tx.tokpos..tx.pos]);
            }
            Err(e) => {
                if tokens.len() > 0 {
                    tokens.push_str(" ");
                }
                match e {
                    nom::Err::Error(_) => tokens.push_str("<Error>"),
                    nom::Err::Failure(_) => tokens.push_str("<Failure>"),
                    nom::Err::Incomplete(_) => panic!("Incomplete"),
                };

                return tokens;
            }
        }
    }
}
