pub mod scanner;
pub mod token;
pub mod expression;
pub mod statement;
pub mod parser;
pub mod interpreter;

mod error;
mod context;
mod environment;
mod callable;

use std::cell::RefCell;
use std::rc::Rc;

use environment::Environment;
use error::Result;

use crate::scanner::*;
use crate::context::*;
use crate::parser::*;
use crate::interpreter::*;

pub struct FroxRunner<'a> {
    environment: Rc<RefCell<Environment<'a>>>
}

impl<'a> FroxRunner<'a> {
    pub fn new() -> Self {
        FroxRunner { environment: Environment::new().into()}
    }

    pub fn run(&mut self, source: Rc<str>) -> Result<()> {
        self.run_with_print_stream(source, |string| println!("{}", string))
    }
    
    fn run_with_print_stream<F: FnMut(String) -> ()>(&mut self, source: Rc<str>, mut print_stream: F) -> Result<()> {
        let mut scanner = Scanner::new(source.clone());
        let tokens = scanner.scan_tokens()?;
    
        let mut parser = Parser::new(source.clone());
        let statements = parser.parse(tokens)?;
    
        let mut interpreter = Interpreter::new(source.clone(), self.environment.clone());
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
        FroxRunner::new().run_with_print_stream("print (2 * 4) / (1 + 1);".into(),  |string| buffer = string).unwrap();
        assert_eq!(FroxValue::Number(4.0).to_string(), buffer)
    }

    #[test]
    fn should_execute_if_statement() {
        let mut buffer = "".to_string();
        FroxRunner::new().run_with_print_stream("if (1>2) print 1; else print 2;".into(), |string| buffer = string).unwrap();
        assert_eq!("2", buffer)
    }

    #[test]
    fn should_execute_for_loops() {
        let mut buffer = "".to_string();
        FroxRunner::new().run_with_print_stream("for (var i=0; i<3; i = i + 1) { print i; }".into(), |string| buffer.push_str(string.as_str())).unwrap();
        assert_eq!("012", buffer);
    }
}