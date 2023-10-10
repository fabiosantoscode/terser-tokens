use std::io;
use std::io::Read;

mod analyze;
mod basic_blocks;
mod block_ops;
mod compress;
mod from_ast;
mod functional_tests;
mod interpret;
mod scope;
mod swc_parse;
mod swc_stringify;
mod to_ast;

#[cfg(test)]
mod testutils;

pub fn main() {
    let mut input = String::new();
    io::stdin().lock().read_to_string(&mut input).unwrap();

    let module = compress::compress(&input);

    println!("{}", module)
}
