use std::fmt::Display;

use crate::token::Lexeme;

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    ScannerError(String),
    ParserError(String, Option<Lexeme>)
}

pub struct ErrorCollector {
    errors: Vec<String>
}

impl Error {
    fn format_error(&self, source: &str, line: usize, position: usize) -> String {
        let mut position = position;
        let lines = source.lines().collect::<Vec<_>>();
        for line in &lines[0..line-1] {
            position -= line.len();
        }

        let error_line = lines[line - 1];
        let spacing = " ".repeat(position - 1);
        let marker = format!("{}^", spacing);
        let message = format!("{}{}", spacing, self.to_string());
        format!(
            "Error occured on line {}:\n{}\n{}\n{}", 
            line, 
            error_line, 
            marker, 
            message
        ) 
    }
}

impl ErrorCollector {
    pub fn new() -> Self {
        ErrorCollector { errors: Vec::new() }
    }

    pub fn collect_and_format(&mut self, error: Error, source: &str, line: usize, position: usize) {
        let message = error.format_error(source, line, position);
        self.errors.push(message)
    }

    pub fn collect(&mut self, error: Error) {
        self.errors.push(error.to_string())
    }

    pub fn flush_errors(&mut self) -> Option<String> {
        if self.contains_errors() {
            let message = self.to_string();
            self.errors.clear();
            return Some(message)
        }
        None
    }

    fn contains_errors(&self) -> bool {
        self.errors.len() > 0
    }
}

impl Default for ErrorCollector {
    fn default() -> Self {
        Self { errors: Default::default() }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ScannerError(message) => f.write_str(message),
            Self::ParserError(message, lexeme) => f.write_str(message)
        }
    }
}

impl Display for ErrorCollector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.errors
            .iter()
            .map(|error| error.to_string())
            .collect::<Vec<_>>().join("
        ").as_str())
    }
}
