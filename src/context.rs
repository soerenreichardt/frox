use crate::error::ErrorCollector;

pub struct Context<'a> {
    pub source: &'a str,
    pub error_collector: ErrorCollector
}

impl<'a> Context<'a> {
    pub fn new(source: &'a str) -> Self {
        Context {
            source,
            error_collector: ErrorCollector::new()
        }
    }
}

impl<'a> Default for Context<'a> {
    fn default() -> Self {
        Self { source: Default::default(), error_collector: Default::default() }
    }
}