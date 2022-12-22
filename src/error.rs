use std::fmt::Display;

use crate::{token::Lexeme};

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    FroxError(String),
    ScannerError(String, usize, usize),
    ParserError(String, Option<Lexeme>),
    InterpreterError(String, Option<Lexeme>)
}

pub struct ErrorCollector {
    errors: Vec<Error>
}

impl Error {
    pub fn format_error(&self, source: &str) -> String {
        match self {
            Self::ScannerError(message, line, position) => Self::format_scanner_error(source, message.as_str(), *line, *position),
            Self::ParserError(message, Some(lexeme)) => Self::pretty_print_error(source, message, lexeme),
            Self::ParserError(_, None) => self.to_string(),
            Self::InterpreterError(message, Some(lexeme)) => Self::pretty_print_error(source, message, lexeme),
            Self::InterpreterError(_, None) => self.to_string(),
            Self::FroxError(_) => panic!("Cannot format error of type FroxError")
        }
    }

    fn format_scanner_error(source: &str, error_message: &str, line: usize, position: usize) -> String {
        let mut position = position;
        let lines = source.lines().collect::<Vec<_>>();
        for line in &lines[0..line-1] {
            position -= line.len();
        }

        let error_line = lines[line - 1];
        let spacing = " ".repeat(position - 1);
        let marker = format!("{}^", spacing);
        let message = format!("{}{}", spacing, error_message);
        format!(
            "Error occured on line {}:\n{}\n{}\n{}", 
            line, 
            error_line, 
            marker, 
            message
        ) 
    }

    fn pretty_print_error(source: &str, error_message: &str, Lexeme {start, end: _ }: &Lexeme) -> String {
        println!("{}", start);
        let mut position: usize = 0;
        let lines = source.lines().collect::<Vec<_>>();

        let line = lines
            .iter()
            .enumerate()
            .find(|(_, line)| {
                if position + line.len() >= *start {
                    return true
                }
                position += line.len();
                false
            })
            .map(|(line_num, _)| line_num)
            .expect("Did not find error position in source");

        let position_in_line = start - position;
        let error_line = lines[line];
        let spacing = " ".repeat(position_in_line);
        let marker = format!("{}^", spacing);
        let message = format!("{}{}", spacing, error_message);
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

    pub fn collect(&mut self, error: Error) {
        self.errors.push(error)
    }

    pub fn flush_errors(&mut self, source: &str) -> Option<String> {
        if self.contains_errors() {
            let message = self.join_errors(source, "
            ");
            self.errors.clear();
            return Some(message)
        }
        None
    }

    fn contains_errors(&self) -> bool {
        self.errors.len() > 0
    }

    fn join_errors(&self, source: &str, separator: &str) -> String {
        self.errors
            .iter()
            .map(|error| error.format_error(source))
            .collect::<Vec<_>>().join(separator)
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
            Self::FroxError(message) => f.write_str(message),            
            Self::ScannerError(message, ..) => f.write_str(message),            
            Self::ParserError(message, ..) => f.write_str(message)   ,
            Self::InterpreterError(message, ..) => f.write_str(message)         
        }
    }
}
