#![feature(once_cell)]

use std::cell::RefCell;
use std::sync::LazyLock;

use tokenizer::tokenizer::Tokenizer;
use wasm_bindgen::prelude::*;

use serde::Serialize;
use serde::Deserialize;
use tokenizer::{tokens::{JsToken, Comment}};

mod tokenizer;
mod parser;


#[derive(Serialize, Deserialize)]
struct TokenAndSourceInfo {
    token: JsToken,
    source_info: (String, String, usize, usize, usize, bool, Vec<Comment>, String),
}

static mut EXTERN_GLOBAL_TOKENIZER: LazyLock<RefCell<Tokenizer>> = LazyLock::new(|| {
    Default::default()
});

#[wasm_bindgen]
pub fn init(input: &str, filename: &str) {unsafe{
    let new = RefCell::new(Tokenizer::new(input.into(), filename.into(), false, false));
    EXTERN_GLOBAL_TOKENIZER.swap(&new);


}}

#[wasm_bindgen]
pub fn get_token() -> Result<JsValue, JsValue> {
    let mut t: &mut Tokenizer = unsafe {
       &mut EXTERN_GLOBAL_TOKENIZER.borrow_mut()
    };

    match t.next_token() {
        Ok(tok) => {
            let (_, _, _line, _col, _pos, _nlb, _, _) = t.token_sourceinfo();
            Ok(tok.to_wasm_bindgen_jsvalue())
        }
        Err(e) => {
            Err(JsValue::from_str(&format!("{:?}", e)))
        }
    }
}
