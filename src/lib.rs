pub mod scanner;
pub mod token;
pub mod expression;
pub mod statement;
pub mod parser;
pub mod interpreter;

mod ast_printer;
mod error;
mod context;

use error::Result;

use crate::scanner::*;
use crate::context::*;
use crate::parser::*;
use crate::interpreter::*;

pub fn run(source: &str) -> Result<()> {
    run_with_print_stream(source, |string| println!("{}", string))
}

fn run_with_print_stream<F: FnMut(String) -> ()>(source: &str, mut print_stream: F) -> Result<()> {
    let mut scanner = Scanner::new(source);
    let tokens = scanner.scan_tokens()?;

    let mut parser = Parser::new(tokens, source);
    let statements = parser.parse()?;

    let mut interpreter = Interpreter::new(source);
    interpreter.interpret(&statements, &mut print_stream)
}

pub trait Materializable<'a, T> {
    fn materialize(&self, context: &'a Context) -> T;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_run_simple_calculation() {
        let mut buffer = "".to_string();
        run_with_print_stream("print (2 * 4) / (1 + 1);",  |string| buffer = string);
        assert_eq!(FroxValue::Number(4.0).to_string(), buffer)
    }
}