use std::rc::Rc;

use crate::error::{ErrorCollector, Error};

pub struct Context {
    pub source: Rc<str>,
    pub error_collector: ErrorCollector
}

impl Context {
    pub fn new(source: Rc<str>) -> Self {
        Context {
            source,
            error_collector: ErrorCollector::new()
        }
    }

    pub(crate) fn collect_error(&mut self, error: Error) {
        self.error_collector.collect(error)
    }

    pub(crate) fn flush_errors<Res>(&mut self, result: Res) -> crate::error::Result<Res> {
        match self.error_collector.flush_errors(&self.source) {
            Some(message) => Err(Error::FroxError(message)),
            None => Ok(result)
        }
    }
}