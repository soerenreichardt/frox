pub mod expression;
pub mod interpreter;
pub mod parser;
pub mod scanner;
pub mod statement;
pub mod token;

mod callable;
mod context;
mod environment;
mod error;

use std::cell::RefCell;
use std::rc::Rc;

use environment::Environment;
use error::Result;

use crate::context::*;
use crate::interpreter::*;
use crate::parser::*;
use crate::scanner::*;

pub struct FroxRunner<'a> {
    environment: Rc<RefCell<Environment<'a>>>,
}

impl<'a> FroxRunner<'a> {
    pub fn new() -> Self {
        FroxRunner {
            environment: Environment::new().into(),
        }
    }

    pub fn run(&mut self, source: Rc<str>) -> Result<()> {
        self.run_with_print_stream(source, |string| println!("{}", string))
    }

    fn run_with_print_stream<F: FnMut(String) -> ()>(
        &mut self,
        source: Rc<str>,
        mut print_stream: F,
    ) -> Result<()> {
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
        FroxRunner::new()
            .run_with_print_stream("print (2 * 4) / (1 + 1);".into(), |string| buffer = string)
            .unwrap();
        assert_eq!(FroxValue::Number(4.0).to_string(), buffer)
    }

    #[test]
    fn should_execute_if_statement() {
        let mut buffer = "".to_string();
        FroxRunner::new()
            .run_with_print_stream("if (1>2) print 1; else print 2;".into(), |string| {
                buffer = string
            })
            .unwrap();
        assert_eq!("2", buffer)
    }

    #[test]
    fn should_execute_for_loops() {
        let mut buffer = "".to_string();
        FroxRunner::new()
            .run_with_print_stream(
                "for (var i=0; i<3; i = i + 1) { print i; }".into(),
                |string| buffer.push_str(string.as_str()),
            )
            .unwrap();
        assert_eq!("012", buffer);
    }

    #[test]
    fn should_handle_blocks_and_scopes() {
        let mut buffer = String::new();
        let source = r#"
        var a = "global a"; 
        var b = "global b"; 
        var c = "global c"; 
        {
            var a = "outer a"; 
            var b = "outer b";
            {
                var a = "inner a"; 
                print a;
                print b;
                print c;
            }
            print a; 
            print b; 
            print c;
        }
        print a; 
        print b; 
        print c;
        "#;
        FroxRunner::new().run_with_print_stream(source.into(), |string| buffer.push_str(string.as_str())).unwrap();

        assert_eq!(
            "\"inner a\"\"outer b\"\"global c\"\"outer a\"\"outer b\"\"global c\"\"global a\"\"global b\"\"global c\""
            , buffer);
    }

    #[test]
    fn should_execute_declared_functions() {
        let mut buffer = String::new();
        let source = r#"
        fun count(n) {
            if (n > 1) count(n - 1);
            print n;
        }
        
        count(3);
        "#;
        FroxRunner::new().run_with_print_stream(source.into(), |string| buffer.push_str(string.as_str())).unwrap();
        assert_eq!("nil123", buffer);
    }
}
