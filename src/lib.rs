#![feature(lazy_cell)]

extern crate lazy_static;

use std::cell::RefCell;
use std::sync::LazyLock;

use tokenizer::tokenizer::Tokenizer;
use wasm_bindgen::prelude::*;

use serde::Deserialize;
use serde::Serialize;
use tokenizer::tokens::{Comment, JsToken};

mod basic_blocks;
mod parser;
mod scope;
mod tokenizer;

#[derive(Serialize, Deserialize)]
struct TokenAndSourceInfo {
    token: JsToken,
    source_info: (
        String,
        String,
        usize,
        usize,
        usize,
        bool,
        Vec<Comment>,
        String,
    ),
}

static mut EXTERN_GLOBAL_TOKENIZER: LazyLock<RefCell<Tokenizer>> =
    LazyLock::new(|| Default::default());

#[wasm_bindgen]
pub fn init(input: &str, filename: &str) {
    unsafe {
        let new = RefCell::new(Tokenizer::new(input.into(), filename.into(), false, false));
        EXTERN_GLOBAL_TOKENIZER.swap(&new);
    }
}

#[wasm_bindgen]
pub fn get_token() -> Result<JsValue, JsValue> {
    let t: &mut Tokenizer = unsafe { &mut EXTERN_GLOBAL_TOKENIZER.borrow_mut() };

    match t.next_token() {
        Ok(tok) => {
            let (_, _, _line, _col, _pos, _nlb, _, _) = t.token_sourceinfo();
            Ok(tok.to_wasm_bindgen_jsvalue())
        }
        Err(e) => Err(JsValue::from_str(&format!("{:?}", e))),
    }
}
