pub mod scanner;
pub mod token;
pub mod expression;
pub mod parser;
pub mod interpreter;

mod ast_printer;
mod error;
mod context;

use crate::scanner::*;
use crate::context::*;
use crate::parser::*;
use crate::interpreter::*;
use crate::error::Result;

pub fn run(source: &str) -> Result<FroxValue> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;

    let mut parser = Parser::new(tokens, source);
    let expression = parser.expression()?;

    let interpreter = Interpreter::new(source);
    interpreter.evaluate(&expression)
}

pub trait Materializable<'a, T> {
    fn materialize(&self, context: &'a Context) -> T;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_run_simple_calculation() {
        let result = run("(2 * 4) / (1 + 1)");
        assert_eq!(FroxValue::Number(4.0), result.unwrap())
    }
}