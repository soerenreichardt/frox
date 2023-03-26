use std::{rc::Rc, collections::HashMap, cell::RefCell};

use crate::{error::{Result, Error}, value::FroxValue};

#[derive(PartialEq, Clone)]
pub struct Class {
    pub name: Rc<str>,
    fields: HashMap<Rc<str>, FroxValue>
}

impl Class {
    pub(crate) fn new(name: Rc<str>) -> Self {
        Class { name, fields: HashMap::new() }
    }

    pub(crate) fn get(&self, field_name: Rc<str>) -> Result<FroxValue> {
        match self.fields.get(&field_name) {
            Some(value) => Ok(value.clone()),
            None => Err(Error::InterpreterError(format!("Undefined property '{}'", field_name)))
        }
    }

    pub(crate) fn set(&mut self, field_name: Rc<str>, value: FroxValue) {
        self.fields.insert(field_name, value);
    }
}

impl Into<Rc<RefCell<Class>>> for Class {
    fn into(self) -> Rc<RefCell<Class>> {
        Rc::new(RefCell::new(self))
    }
}