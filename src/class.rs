use std::{rc::Rc, collections::HashMap, cell::RefCell};

use crate::{error::{Result, Error}, value::FroxValue, callable::{DeclaredFunction, Callable}};

#[derive(PartialEq)]
pub struct Class {
    pub name: Rc<str>,
    superclass: Option<Rc<Class>>,
    methods: HashMap<Rc<str>, Rc<DeclaredFunction>> 
}

pub(crate) struct Initializer {
    pub(crate) class: Rc<Class>
}

#[derive(PartialEq)]
pub struct Instance {
    fields: HashMap<Rc<str>, FroxValue>,
    class: Rc<Class>
}

impl Class {
    pub(crate) fn new(name: Rc<str>, superclass: Option<Rc<Class>>, methods: HashMap<Rc<str>, Rc<DeclaredFunction>>) -> Self {
        Class { name, superclass, methods }
    }

    pub(crate) fn find_method(&self, name: &str) -> Option<&Rc<DeclaredFunction>> {
        match self.methods.get(name) {
            Some(method) => Some(method),
            None => match &self.superclass {
                Some(superclass) => superclass.find_method(name),
                None => None
            }
        }
    }
}

impl Callable for Initializer {
    fn name(&self) -> Rc<str> {
        self.class.name.clone()
    }

    fn arity(&self) -> u8 {
        match self.class.find_method("init") {
            Some(constructor) => constructor.arity(),
            None => 0
        }
    }

    fn call<F: FnMut(String) -> ()>(&self, arguments: Vec<FroxValue>, interpreter: &mut crate::interpreter::Interpreter, print_stream: &mut F) -> Result<FroxValue> {
        let instance = Rc::new(RefCell::new(Instance::new(self.class.clone())));
        if let Some(constructor) = self.class.find_method("init") {
            constructor.bind(instance.clone()).call(arguments, interpreter, print_stream)?;
        }

        Ok(FroxValue::Instance(instance.clone()))
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
        match self.fields.get(&field_name) {
            Some(value) => Ok(value.clone()),
            None => match self.class.find_method(&field_name) {
                Some(method) => Ok(FroxValue::Function(method.clone())),
                None => Err(Error::InterpreterError(format!("Undefined property '{}'", field_name)))
            }
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