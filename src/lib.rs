pub mod scanner;
pub mod token;
pub mod expression;
pub mod parser;
mod ast_printer;
mod error;
mod context;

use crate::scanner::*;
use crate::context::*;

pub fn run(source: &str) {
    let mut context = Context::new();
    let mut scanner = Scanner::new(source, context);
    println!("{:?}", scanner.scan_tokens());
}