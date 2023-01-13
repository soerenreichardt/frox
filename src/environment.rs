use std::collections::HashMap;
use crate::error::Error::InterpreterError;
use crate::error::Result;

use crate::interpreter::FroxValue;
use crate::token::Lexeme;


pub struct Environment {
    values: HashMap<String, FroxValue>,
    parent: Option<Box<Environment>>
}

impl Environment {

    pub fn new() -> Self {
        Environment { values: HashMap::new(), parent: None }
    }

    pub fn new_inner(environment: Environment) -> Self {
        Environment { values: HashMap::new(), parent: Some(Box::new(environment)) }
    }

    pub fn define(&mut self, name: String, value: FroxValue) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: String, lexeme: &Lexeme) -> Result<FroxValue> {
        match self.values.get(&name) {
            Some(value) => Ok(value.clone()),
            None => match &self.parent {
                    Some(parent) => parent.get(name, lexeme),
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
            None => match &mut self.parent {
                Some(parent) => parent.assign(name, value, lexeme),
                None => Err(InterpreterError(format!("Undefined variable '{}'.", name), Some(*lexeme)))
            }
        }
    }
}