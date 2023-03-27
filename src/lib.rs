pub mod expression;
pub mod interpreter;
pub mod parser;
pub mod scanner;
pub mod statement;
pub mod token;
pub mod value;

mod callable;
mod context;
mod environment;
mod error;
mod resolver;
mod class;

use std::cell::RefCell;
use std::rc::Rc;

use environment::Environment;
use error::Result;
use resolver::Resolver;

use crate::context::*;
use crate::interpreter::*;
use crate::parser::*;
use crate::scanner::*;

pub struct FroxRunner {
    environment: Rc<RefCell<Environment>>,
}

impl FroxRunner {
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

        let mut resolver = Resolver::new(source.clone());
        let resolved_variables = resolver.resolve(&statements)?;

        let mut interpreter = Interpreter::new(source.clone(), self.environment.clone(), resolved_variables);
        interpreter.interpret(&statements, &mut print_stream)
    }
}

pub trait Materializable<'a, T> {
    fn materialize(&self, context: &'a Context) -> T;
}

#[cfg(test)]
mod tests {
    use crate::value::FroxValue;

    use super::*;

    #[test]
    fn should_run_simple_calculation() {
        assert_execution_equals("print (2 * 4) / (1 + 1);", FroxValue::Number(4.0).to_string().as_str())
    }

    #[test]
    fn should_execute_if_statement() {
        assert_execution_equals("if (1>2) print 1; else print 2;", "2");
    }

    #[test]
    fn should_execute_for_loops() {
        assert_execution_equals(
            "for (var i=0; i<3; i = i + 1) { print i; }", 
            "012"
        );
    }

    #[test]
    fn should_handle_blocks_and_scopes() {
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

        assert_execution_equals(
            source, 
            "\"inner a\"\"outer b\"\"global c\"\"outer a\"\"outer b\"\"global c\"\"global a\"\"global b\"\"global c\""
        );
    }

    #[test]
    fn should_execute_declared_functions() {
        let source = r#"
        fun count(n) {
            if (n > 1) count(n - 1);
            print n;
        }
        
        count(3);
        "#;
        assert_execution_equals(source, "123");
    }

    #[test]
    fn should_execute_functions_with_multiple_params() {
        let source = r#"
        fun sayHi(first, last) {
            print "Hi, " + first + " " + last + "!"; }
            sayHi("Dear", "Reader");
        "#;
        assert_execution_equals(source, "\"Hi, Dear Reader!\"");
    }

    #[test]
    fn should_run_closures() {
        let source = r#"
        fun makeCounter() { var i = 0;
            fun count() {
                i = i + 1;
                print i; 
            }
            return count; 
        }
        var counter = makeCounter(); 
        counter(); // "1". 
        counter(); // "2".
        "#;
        assert_execution_equals(source, "12")
    }

    #[test]
    fn should_run_lambdas() {
        let source = r#"
        fun thrice(fn) {
            for (var i = 1; i <= 3; i = i + 1) {
                fn(i); 
            }
        }
        thrice(fun (a) { 
            print a;
        });
        "#;
        assert_execution_equals(source, "123")
    }

    #[test]
    fn should_resolve_with_correct_environment() {
        let source = r#"
        var a = "global";
        {
            fun showA() {
                print a; 
            }
            showA();
            var a = "block"; 
            showA();
        }
        "#;
        assert_execution_equals(source, "\"global\"\"global\"");
    }

    #[test]
    fn should_print_class() {
        let source = r#"
        class Foo {
            bar() {
                print "baz";
            }
        }
        print Foo;
        "#;
        assert_execution_equals(source, "Foo");
    }

    #[test]
    fn should_print_instance() {
        let source = r#"
        class Bagel {}
        var bagel = Bagel();
        print bagel;
        "#;
        assert_execution_equals(source, "Bagel instance");
    }

    #[test]
    fn should_get_and_set_on_instances() {
        let source = r#"
        class Bagel {}
        var bagel = Bagel();
        bagel.content = "Lettuce";
        print bagel.content;
        "#;
        assert_execution_equals(source, "\"Lettuce\"");
    }

    #[test]
    fn should_execute_method() {
        let source = r#"
        class Bacon { eat() {
            print "Crunch crunch crunch!"; }
            }
            Bacon().eat();
        "#;
        assert_execution_equals(source, "\"Crunch crunch crunch!\"")
    }

    #[test]
    fn should_evaluate_this() {
        let source = r#"
        class Egotist { 
            speak() {
                print this; 
            }
        }
        var method = Egotist().speak; 
        method();
        "#;
        assert_execution_equals(source, "Egotist instance")
    }

    fn assert_execution_equals(source: &str, expected: &str) {
        let mut buffer = String::new();
        match FroxRunner::new().run_with_print_stream(source.into(), |string| buffer.push_str(string.as_str())) {
            Err(error) => println!("{}", error.format_error(source)),
            _ => ()
        };
        assert_eq!(expected, buffer);
    }
}
