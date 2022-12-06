pub mod scanner;
pub mod token;
pub mod expression;

use crate::scanner::*;

pub fn run(source: &str) {
    let mut scanner = Scanner::new(source);
    println!("{:?}", scanner.scan_tokens());
}