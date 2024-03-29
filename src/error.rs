use std::fmt::Display;

use crate::{token::Lexeme, value::FroxValue};

pub type Result<T> = core::result::Result<T, Error>;

#[derive(Debug, PartialEq)]
pub enum Error {
    FroxError(String),
    ScannerError(String, usize, usize),
    ParserError(String, Option<Lexeme>),
    InterpreterError(String),
    ResolverError(String, Option<Lexeme>),
    ReturnCall(FroxValue)
}

pub struct ErrorCollector {
    errors: Vec<Error>
}

impl Error {

    pub fn format_interpreter_error(error: &Error, lexeme: &Lexeme, source: &str) -> String {
        match error {
            Error::InterpreterError(msg) => Self::pretty_print_error(source, msg, &lexeme),
            Error::FroxError(msg) => Self::pretty_print_error(source, msg, &lexeme),
            _ => panic!("Should only be called on interpreter errors")
        }
    }

    pub fn format_error(&self, source: &str) -> String {
        match self {
            Self::ScannerError(message, line, position) => Self::format_scanner_error(source, message.as_str(), *line, *position),
            Self::ParserError(message, Some(lexeme)) => Self::pretty_print_error(source, message, lexeme),
            Self::ParserError(_, None) => self.to_string(),
            Self::InterpreterError(_) => self.to_string(),
            Self::FroxError(message) => message.to_owned(),
            Self::ResolverError(message, Some(lexeme)) => Self::pretty_print_error(source, message, lexeme),
            Self::ResolverError(_, None) => self.to_string(),
            Self::ReturnCall(_) => self.to_string()
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

    fn pretty_print_error(source: &str, error_message: &str, Lexeme {start, end }: &Lexeme) -> String {
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

        let expression_length = end - start - 1;
        let marker = if expression_length == 0 { 
            format!("{}^", spacing) 
        } else {
            format!("{}^{}^", spacing, "-".repeat(expression_length - 1))
        };
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

    pub(crate) fn collect(&mut self, error: Error) {
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
            Self::ParserError(message, ..) => f.write_str(message),
            Self::InterpreterError(message, ..) => f.write_str(message),
            Self::ResolverError(message, _) => f.write_str(message),
            Self::ReturnCall(value) => f.write_str(format!("return {:?}", value).as_str())
        }
    }
}
