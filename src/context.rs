use crate::error::ErrorCollector;

pub struct Context {
    pub error_collector: ErrorCollector
}

impl Context {
    pub fn new() -> Self {
        Context {
            error_collector: ErrorCollector::new()
        }
    }
}