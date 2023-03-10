use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::error::Error::InterpreterError;
use crate::error::Result;

use crate::token::Lexeme;
use crate::value::FroxValue;


#[derive(Clone)]
pub(crate) struct Environment<'a> {
    values: HashMap<String, FroxValue>,
    parent: Option<Rc<RefCell<Environment<'a>>>>
}

impl<'a> Environment<'a> {

    pub(crate) fn new() -> Self {
        Environment { values: HashMap::new(), parent: None }
    }

    pub(crate) fn new_inner(environment: Rc<RefCell<Environment<'a>>>) -> Self {
        Environment { values: HashMap::new(), parent: Some(environment) }
    }

    pub(crate) fn define(&mut self, name: String, value: FroxValue) {
        self.values.insert(name, value);
    }

    pub(crate) fn get(&self, name: String, lexeme: &Lexeme) -> Result<FroxValue> {
        match self.values.get(&name) {
            Some(value) => Ok(value.clone()),
            None => match &self.parent {
                    Some(parent) => parent.borrow().get(name, lexeme),
                    None => Err(InterpreterError(format!("Undefined variable '{}'.", name)))
            }
        }
    }

    pub(crate) fn assign(&mut self, name: String, value: FroxValue, lexeme: &Lexeme) -> Result<FroxValue> {
        match self.values.get(&name) {
            Some(_) => {
                self.values.insert(name, value.clone());
                Ok(value.clone())
            },
            None => match &self.parent {
                Some(parent) => parent.borrow_mut().assign(name, value, lexeme),
                None => Err(InterpreterError(format!("Undefined variable '{}'.", name)))
            }
        }
    }
}

impl<'a> From<Environment<'a>> for Rc<RefCell<Environment<'a>>> {
    fn from(env: Environment<'a>) -> Self {
        Rc::new(RefCell::new(env))
    }
}