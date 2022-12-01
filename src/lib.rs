pub mod scanner;
pub mod token;

use crate::scanner::*;

pub fn run(source: &str) {
    let scanner = Scanner::new(source);
    println!("{:?}", scanner.scan_tokens());
}