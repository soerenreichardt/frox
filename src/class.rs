use std::rc::Rc;

#[derive(PartialEq, Clone)]
pub struct Class {
    pub name: Rc<str>
}