pub mod scanner;
pub mod token;
pub mod expression;
pub mod statement;
pub mod parser;
pub mod interpreter;

mod ast_printer;
mod error;
mod context;
mod environment;

use environment::Environment;
use error::Result;

use crate::scanner::*;
use crate::context::*;
use crate::parser::*;
use crate::interpreter::*;

pub struct FroxRunner {
    environment: Environment
}

impl FroxRunner {
    pub fn new() -> Self {
        FroxRunner { environment: Environment::new() }
    }

    pub fn run(&mut self, source: &str) -> Result<()> {
        self.run_with_print_stream(source, |string| println!("{}", string))
    }
    
    fn run_with_print_stream<F: FnMut(String) -> ()>(&mut self, source: &str, mut print_stream: F) -> Result<()> {
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;
    
        let mut parser = Parser::new(source);
        let statements = parser.parse(tokens)?;
    
        let mut interpreter = Interpreter::new(source, self.environment.clone());
        interpreter.interpret(&statements, &mut print_stream)
    }
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
        FroxRunner::new().run_with_print_stream("print (2 * 4) / (1 + 1);",  |string| buffer = string);
        assert_eq!(FroxValue::Number(4.0).to_string(), buffer)
    }
}