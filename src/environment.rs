use std::collections::HashMap;
use crate::error::Error::InterpreterError;
use crate::error::Result;

use crate::interpreter::FroxValue;


pub struct Environment {
    values: HashMap<String, FroxValue>
}

impl Environment {

    pub fn new() -> Self {
        Environment { values: HashMap::new() }
    }

    pub fn define(&mut self, name: String, value: FroxValue) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: String) -> Result<FroxValue> {
        match self.values.get(&name) {
            Some(value) => Ok(value.clone()),
            None => Err(InterpreterError(format!("Undefined variable '{}'.", name), None))
        }
    }
}