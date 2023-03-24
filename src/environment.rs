use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::error::Error::InterpreterError;
use crate::error::Result;

use crate::token::Lexeme;
use crate::value::FroxValue;


#[derive(Clone, PartialEq)]
pub(crate) struct Environment {
    values: HashMap<String, FroxValue>,
    parent: Option<Rc<RefCell<Environment>>>
}

impl Environment {

    pub(crate) fn new() -> Self {
        Environment { values: HashMap::new(), parent: None }
    }

    pub(crate) fn new_inner(environment: Rc<RefCell<Environment>>) -> Self {
        Environment { values: HashMap::new(), parent: Some(environment) }
    }

    pub(crate) fn define(&mut self, name: String, value: FroxValue) {
        self.values.insert(name, value);
    }

    pub(crate) fn get(&self, name: String) -> Result<FroxValue> {
        match self.values.get(&name) {
            Some(value) => Ok(value.clone()),
            None => match &self.parent {
                    Some(parent) => parent.borrow().get(name),
                    None => Err(InterpreterError(format!("Undefined variable '{}'.", name)))
            }
        }
    }

    pub(crate) fn get_at(environment: Rc<RefCell<Environment>>, distance: usize, name: String) -> Result<FroxValue> {
        Environment::ancestor(environment, distance).borrow().get(name)
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

    pub(crate) fn assign_at(environment: Rc<RefCell<Environment>>, distance: usize, name: String, value: FroxValue) -> Result<FroxValue> {
        Environment::ancestor(environment, distance).borrow_mut().values.insert(name, value.clone());
        Ok(value.clone())
    }

    fn ancestor(env: Rc<RefCell<Environment>>, distance: usize) -> Rc<RefCell<Environment>> {
        if distance == 0 {
            return env;
        }

        match env.borrow().parent.clone() {
            Some(parent) => Environment::ancestor(parent, distance - 1),
            None => panic!()
        }
    }
}

impl From<Environment> for Rc<RefCell<Environment>> {
    fn from(env: Environment) -> Self {
        Rc::new(RefCell::new(env))
    }
}