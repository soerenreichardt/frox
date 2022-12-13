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
    let context = Context::new();

    let mut scanner = Scanner::new(source, context);
    let tokens = match scanner.scan_tokens() {
        Ok(tokens) => tokens,
        Err(error) => panic!("Errors occured while scanning:\n {}", error.to_string().as_str())
    };

    let mut parser = Parser::new(tokens, scanner.context());
    let expression = match parser.expression() {
        Ok(expression) => expression,
        Err(error) => panic!("Errors occured while parsing:\n {}", error.to_string())
    };
}