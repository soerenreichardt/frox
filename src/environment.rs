use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::error::Error::InterpreterError;
use crate::error::Result;

use crate::interpreter::FroxValue;
use crate::token::Lexeme;


#[derive(Clone)]
pub struct Environment<'a> {
    values: HashMap<String, FroxValue>,
    parent: Option<Rc<RefCell<Environment<'a>>>>
}

impl<'a> Environment<'a> {

    pub fn new() -> Self {
        Environment { values: HashMap::new(), parent: None }
    }

    pub fn new_inner(environment: Rc<RefCell<Environment<'a>>>) -> Self {
        Environment { values: HashMap::new(), parent: Some(environment) }
    }

    pub fn define(&mut self, name: String, value: FroxValue) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: String, lexeme: &Lexeme) -> Result<FroxValue> {
        match self.values.get(&name) {
            Some(value) => Ok(value.clone()),
            None => match &self.parent {
                    Some(parent) => parent.borrow().get(name, lexeme),
                    None => Err(InterpreterError(format!("Undefined variable '{}'.", name), Some(*lexeme)))
            }
        }
    }

    pub fn assign(&mut self, name: String, value: FroxValue, lexeme: &Lexeme) -> Result<FroxValue> {
        match self.values.get(&name) {
            Some(_) => {
                self.values.insert(name, value.clone());
                Ok(value.clone())
            },
            None => match &self.parent {
                Some(parent) => parent.borrow_mut().assign(name, value, lexeme),
                None => Err(InterpreterError(format!("Undefined variable '{}'.", name), Some(*lexeme)))
            }
        }
    }
}

impl<'a> From<Environment<'a>> for Rc<RefCell<Environment<'a>>> {
    fn from(env: Environment<'a>) -> Self {
        Rc::new(RefCell::new(env))
    }
}