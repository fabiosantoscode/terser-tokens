use std::io;
use std::io::Read;

mod basic_blocks;
mod compress;
mod scope;
mod swc_parse;
mod swc_stringify;

pub fn main() {
    let mut input = String::new();
    io::stdin().lock().read_to_string(&mut input).unwrap();

    let module = compress::compress(&input);

    println!("{}", module)
}