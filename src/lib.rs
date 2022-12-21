pub mod scanner;
pub mod token;
pub mod expression;
pub mod parser;
mod ast_printer;
mod error;
mod context;

use crate::scanner::*;
use crate::context::*;
use crate::parser::*;

pub fn run(source: &str) {
    let context = Context::new(source);

    let mut scanner = Scanner::new(context);
    let tokens = scanner.scan_tokens().expect("Error while scanning");

    let mut parser = Parser::new(tokens, scanner.context());
    let expression = parser.expression().expect("Error while parsing");
}

pub trait Materializable<'a, T> {
    fn materialize(&self, context: &'a Context) -> T;
}