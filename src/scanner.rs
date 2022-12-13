use std::{str::FromStr};

use crate::context::Context;
use crate::token::*;
use crate::error::*;

pub struct Scanner<'a> {
    source: &'a str,
    context: Context,
    start: usize,
    current: usize,
    line: usize
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str, context: Context) -> Self {
        Scanner {
            source,
            context,
            start: 0,
            current: 0,
            line: 1
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token<'a>>> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.start = self.current;
            let substring = self.advance();
            let token_type = self.scan_token(substring);

            match token_type {
                Some(token_type) => match token_type {
                    Ok(token_type) =>  self.add_token(token_type, &mut tokens),
                    Err(error) => self.context.error_collector.collect_and_format(error, self.source, self.line, self.current)
                },
                None => continue
            }
        }

        match self.context.error_collector.flush_errors() {
            Some(message) => Err(Error::ScannerError(message)),
            None => Ok(tokens)
        }
    }

    pub fn context(&mut self) -> Context {
        std::mem::replace(&mut self.context, Context::new())
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self, substring: &'a str) -> Option<Result<TokenType>> {
        match substring {
            " " | "\r" | "\t" => {
                None
            },
            "\n" => {
                self.line += 1;
                None
            },
            "\"" => self.parse_string_literal(),
            _ => if Self::is_digit(substring) {
                self.parse_number_literal()
            } else if Self::is_letter(substring) {
                self.parse_identifier()
            } else {
                self.parse_operator(substring)
            }
        }
    }

    fn advance(&mut self) -> &'a str {
        self.current += 1;
        self.source.get(self.current-1..self.current).unwrap()
    }

    fn peek(&self) -> &'a str {
        if self.is_at_end() {
            return "\0";
        }
        self.source.get(self.current..self.current+1).unwrap()
    }

    fn peek_next(&self) -> &'a str {
        if self.current + 1 >= self.source.len() {
            return "\0";
        }
        self.source.get(self.current+1..self.current+2).unwrap()
    }

    fn add_token(&self, token_type: TokenType, tokens: &mut Vec<Token<'a>>) {
        let substring = &self.source[self.start..self.current];
        tokens.push(Token::new(token_type, substring, self.line))
    }

    fn match_token(&mut self, expected: &'a str) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() != expected {
            return false;
        }

        self.current += 1;
        true
    }


    fn parse_string_literal(&mut self) -> Option<Result<TokenType>> {
        while self.peek() != "\"" && !self.is_at_end() {
            if self.peek() == "\n" {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Some(Err(Error::ScannerError("Unterminated string.".to_string())));
        }

        self.advance();

        Some(Ok(TokenType::String))
    }

    fn parse_number_literal(&mut self) -> Option<Result<TokenType>> {
        while Self::is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == "." && Self::is_digit(self.peek_next()) {
            self.advance();

            while Self::is_digit(self.peek()) {
                self.advance();
            }
        }

        let number_string = &self.source[self.start..self.current];
        Some(match number_string.parse::<f64>() {
            Ok(_) => Ok(TokenType::Number),
            Err(parsing_error) => Err(Error::ScannerError(parsing_error.to_string()))
        })
    }

    fn parse_identifier(&mut self) -> Option<Result<TokenType>> {
        while Self::is_alpha_numberic(self.peek()) {
            self.advance();
        }

        let substring = &self.source[self.start..self.current];
        Some(match TokenType::from_str(substring) {
            a@Ok(_) => a,
            Err(_) => Ok(TokenType::Identifier)
        })
    }

    fn parse_operator(&mut self, substring: &'a str) -> Option<Result<TokenType>> {
        match TokenType::from_str(substring) {
            b@Ok(TokenType::Bang) => if self.match_token("=") { Some(Ok(TokenType::BangEqual)) } else { Some(b) },
            e@Ok(TokenType::Equal) => if self.match_token("=") { Some(Ok(TokenType::EqualEqual)) } else { Some(e) },
            l@Ok(TokenType::Less) => if self.match_token("=") { Some(Ok(TokenType::LessEqual)) } else { Some(l) },
            g@Ok(TokenType::Greater) => if self.match_token("=") { Some(Ok(TokenType::GreaterEqual)) } else { Some(g) }
            s@Ok(TokenType::Slash) => if self.match_token("/") {
                while self.peek() != "\n" && !self.is_at_end() {
                    self.advance();
                }
                if self.is_at_end() {
                    return None;
                } else {
                    let substring = self.advance();
                    self.scan_token(substring)
                }
            } else {
                Some(s)
            },
            other => Some(other)
        }
    }

    fn format_error(&self, message: String) -> String {
        let source = self.source;
        let mut current = self.current;

        let lines = source.lines().collect::<Vec<_>>();
        for line in &lines[0..self.line-1] {
            current -= line.len();
        }

        let error_line = lines[self.line - 1];
        let spacing = " ".repeat(current - 1);
        let marker = format!("{}^", spacing);
        let message = format!("{}{}", spacing, message);
        format!(
            "Error occured on line {}:\n{}\n{}\n{}", 
            self.line, 
            error_line, 
            marker, 
            message
        ) 
    }

    fn is_digit(substring: &'a str) -> bool {
        substring.parse::<f64>().is_ok()
    }

    fn is_letter(substring: &'a str) -> bool {
        let c = substring.chars().next().unwrap();
        c.is_alphabetic() || c == '_'
    }

    fn is_alpha_numberic(substring: &'a str) -> bool {
        Scanner::is_digit(substring) || Scanner::is_letter(substring)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_parse_token_types() {
        let mut scanner = Scanner::new("{}(),.-+;*/!!===<<=>>=", Context::new());
        let tokens = scanner.scan_tokens().unwrap();
        let token_types = tokens.iter()
            .map(|token| &token.token_type)
            .collect::<Vec<_>>();

        let expected = vec![
            &TokenType::LeftBrace, 
            &TokenType::RightBrace, 
            &TokenType::LeftParen, 
            &TokenType::RightParen,
            &TokenType::Comma,
            &TokenType::Dot,
            &TokenType::Minus,
            &TokenType::Plus,
            &TokenType::Semicolon,
            &TokenType::Star,
            &TokenType::Slash,
            &TokenType::Bang,
            &TokenType::BangEqual,
            &TokenType::EqualEqual,
            &TokenType::Less,
            &TokenType::LessEqual,
            &TokenType::Greater,
            &TokenType::GreaterEqual
        ];
        assert_eq!(expected, token_types);
    }

    #[test]
    fn should_parse_comment() {
        let mut scanner = Scanner::new("//some comment", Context::new());
        let tokens = scanner.scan_tokens().unwrap();
        let token_types = tokens.iter()
            .map(|token| &token.token_type)
            .collect::<Vec<_>>();

        println!("Tokens: {:?}", token_types);
        assert!(token_types.is_empty());
    }

    #[test]
    fn should_ignore_whitespace_with_comment() {
        let mut scanner = Scanner::new("(( )){} // grouping stuff", Context::new());
        let tokens = scanner.scan_tokens().unwrap();
        let token_types = tokens.iter()
            .map(|token| &token.token_type)
            .collect::<Vec<_>>();

        let expected = vec![
            &TokenType::LeftParen,
            &TokenType::LeftParen,
            &TokenType::RightParen,
            &TokenType::RightParen,
            &TokenType::LeftBrace,
            &TokenType::RightBrace
        ];

        assert_eq!(expected, token_types);
    }

    #[test]
    fn should_increment_counter_on_line_break() {
        let mut scanner = Scanner::new("(
            // comment, but still count the newline
        )", Context::new());

        assert_eq!(1, scanner.line);

        let tokens = scanner.scan_tokens().unwrap();
        let token_types = tokens.iter()
            .map(|token| &token.token_type)
            .collect::<Vec<_>>();

        assert_eq!(vec![&TokenType::LeftParen, &TokenType::RightParen], token_types);
        assert_eq!(3, scanner.line);
    }

    #[test]
    fn should_parse_string_literals() {
        let mut scanner = Scanner::new("\"literally a string\"", Context::new());
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(vec![Token::new(TokenType::String, "\"literally a string\"", 1)], tokens);
    }

    #[test]
    fn should_parse_number_literals() {
        let mut scanner = Scanner::new("13.37\n1337", Context::new());
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(vec![Token::new(TokenType::Number, "13.37", 1), Token::new(TokenType::Number, "1337", 2)], tokens);
    }

    #[test]
    fn should_parse_identifiers_and_keywords() {
        let mut scanner = Scanner::new("and class while foo", Context::new());
        let tokens = scanner.scan_tokens().unwrap();
        let token_types = tokens.iter()
            .map(|token| &token.token_type)
            .collect::<Vec<_>>();

        let expected = vec![
            &TokenType::And,
            &TokenType::Class,
            &TokenType::While,
            &TokenType::Identifier
        ];

        assert_eq!(expected, token_types);
        assert_eq!("foo", tokens.get(3).unwrap().lexeme)
    }

    #[test]
    fn should_store_correct_lexemes_and_lines() {
        let mut scanner = Scanner::new("{<=\"foo\nbar\"", Context::new());
        let tokens = scanner.scan_tokens().unwrap();

        let expected = vec![
            Token::new(TokenType::LeftBrace, "{", 1),
            Token::new(TokenType::LessEqual, "<=", 1),
            Token::new(TokenType::String, "\"foo\nbar\"", 2)
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn should_return_err_on_unknown_character() {
        let mut scanner = Scanner::new("{}[", Context::new());
        let tokens = scanner.scan_tokens();
        let expected = Error::ScannerError(format!("Error occured on line 1:\n{{}}[\n  ^\n  Unexpected character: `[`").to_string());

        assert_eq!(tokens.is_err(), true);
        assert_eq!(expected, tokens.err().unwrap());
    }
}