use std::{rc::Rc, collections::HashMap, cell::RefCell};

use crate::{error::{Result, Error}, value::FroxValue, callable::DeclaredFunction};

#[derive(PartialEq)]
pub struct Class {
    pub name: Rc<str>,
    methods: HashMap<Rc<str>, Rc<DeclaredFunction>> 
}

#[derive(PartialEq)]
pub struct Instance {
    fields: HashMap<Rc<str>, FroxValue>,
    class: Rc<Class>
}

impl Class {
    pub(crate) fn new(name: Rc<str>, methods: HashMap<Rc<str>, Rc<DeclaredFunction>>) -> Self {
        Class { name, methods }
    }

    pub(crate) fn instantiate(class: Rc<Class>) -> Instance {
        Instance::new(class)
    }

    fn find_method(&self, name: Rc<str>) -> Option<FroxValue> {
        self.methods
            .get(&name)
            .map(|function| FroxValue::Function(function.clone()))
    }
}

impl Instance {
    pub(crate) fn new(class: Rc<Class>) -> Self {
        Instance {
            fields: HashMap::new(),
            class
        }
    }

    pub(crate) fn name(&self) -> Rc<str> {
        self.class.name.clone()
    }

    pub(crate) fn get(&self, field_name: Rc<str>) -> Result<FroxValue> {
        let method = self.class.find_method(field_name.clone());
        let value = self.fields.get(&field_name).or(method.as_ref());
        match value {
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

impl Into<Rc<RefCell<Instance>>> for Instance {
    fn into(self) -> Rc<RefCell<Instance>> {
        Rc::new(RefCell::new(self))
    }
}