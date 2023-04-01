use std::cell::RefCell;
use std::{time::UNIX_EPOCH, rc::Rc, fmt::Display};

use crate::class::Instance;
use crate::interpreter::Interpreter;
use crate::value::FroxValue;
use crate::{statement::Statement, environment::Environment};
use crate::error::{Error, Result};

pub(crate) trait Callable {
    fn name(&self) -> Rc<str>;

    fn arity(&self) -> u8;

    fn call<F: FnMut(String) -> ()>(&self, arguments: Vec<FroxValue>, interpreter: &mut Interpreter, print_stream: &mut F) -> Result<FroxValue>;
}

#[derive(PartialEq, Clone)]
pub struct DeclaredFunction {
    pub(crate) name: Rc<str>, 
    pub(crate) parameters: Rc<Vec<Rc<str>>>,
    pub(crate) body: Rc<Vec<Statement>>,
    pub(crate) closure: Rc<RefCell<Environment>>,
    pub(crate) is_initializer: bool
}

impl Callable for DeclaredFunction {
    fn name(&self) -> Rc<str> {
        self.name.clone()
    }

    fn arity(&self) -> u8 {
        self.parameters.len() as u8
    }

    fn call<F: FnMut(String) -> ()>(&self, arguments: Vec<FroxValue>, interpreter: &mut Interpreter, print_stream: &mut F) -> Result<FroxValue> {
        let mut environment = Environment::new_inner(self.closure.clone());
        for (parameter, argument) in self.parameters.iter().zip(arguments.iter()) {
            environment.define(parameter.to_string(), argument.clone());
        }

        match (self.is_initializer, interpreter.execute_block(&self.body, environment.into(), print_stream)) {
            (false, Ok(_)) => Ok(FroxValue::Nil),
            (true, Ok(_)) => Environment::get_at(self.closure.clone(), 0, "this".to_string()),
            (is_initializer, Err(Error::ReturnCall(return_value))) => {
                if is_initializer {
                    return Environment::get_at(self.closure.clone(), 0, "this".to_string());
                }
                Ok(return_value)
            },
            (_, Err(error)) => Err(error)
        }
    }
}

impl DeclaredFunction {
    pub(crate) fn bind(&self, instance: Rc<RefCell<Instance>>) -> DeclaredFunction {
        let mut env = Environment::new_inner(self.closure.clone());
        env.define("this".to_string(), FroxValue::Instance(instance));
        DeclaredFunction { 
            name: self.name.clone(), 
            parameters: self.parameters.clone(), 
            body: self.body.clone(), 
            closure: env.into(),
            is_initializer: self.is_initializer
        }
    }
}

impl Display for DeclaredFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(format!("<fn {}>", self.name).as_str())
    }
}

#[derive(PartialEq, PartialOrd, Clone)]
pub struct Clock;

impl Callable for Clock {
    fn name(&self) -> Rc<str> {
        "clock".into()
    }

    fn arity(&self) -> u8 {
        0
    }

    fn call<F: FnMut(String) -> ()>(&self, _arguments: Vec<FroxValue>, _interpreter: &mut Interpreter, _print_stream: &mut F) -> Result<FroxValue> {
        let now = std::time::SystemTime::now();
        let epoch_millis = now.duration_since(UNIX_EPOCH).expect("").as_millis() as f64;
        Ok(FroxValue::Number(epoch_millis))
    }
}

impl Display for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<native fn>")
    }
}