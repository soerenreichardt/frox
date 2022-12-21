pub mod scanner;
pub mod token;
pub mod expression;
pub mod parser;
pub mod interpreter;

mod ast_printer;
mod error;
mod context;

use expression::LiteralValue;

use crate::scanner::*;
use crate::context::*;
use crate::parser::*;
use crate::interpreter::*;

pub fn run(source: &str) -> LiteralValue {
    let context = Context::new(source);

    let mut scanner = Scanner::new(context);
    let tokens = scanner.scan_tokens().expect("Error while scanning");

    let mut parser = Parser::new(tokens, scanner.context());
    let expression = parser.expression().expect("Error while parsing");

    let interpreter = Interpreter::new(parser.context());
    interpreter.evaluate(&expression.expression).expect("Runtime error")
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
        assert_eq!(LiteralValue::Number(4.0), result)
    }
}