use std::str::FromStr;

use crate::token::*;

pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    line: usize
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            source,
            start: 0,
            current: 0,
            line: 1
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token<'a>>, String> {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while !self.is_at_end() {
            self.start = self.current;
            let substring = self.advance();
            let token_type = self.scan_token(substring);
        
            match token_type {
                Ok(token_type) => self.add_token(token_type, &mut tokens),
                Err(message) => errors.push(message)
            }
        }

        if errors.len() > 0 {
            return Err(errors.join(" "));
        }
        Ok(tokens)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self, substring: &'a str) -> Result<TokenType<'a>, String> {
        match substring {
            " " | "\r" | "\t" => {
                let substring = self.advance();
                self.start += 1;
                self.scan_token(substring)
            },
            "\n" => {
                self.line += 1;
                let substring = self.advance();
                self.start += 1;
                self.scan_token(substring)
            },
            "\"" => self.parse_string_literal(),
            _ => if self.is_digit(substring) {
                self.parse_number_literal()
            } else {
                match TokenType::from_str(substring) {
                    b@Ok(TokenType::Bang) => if self.match_token("=") { Ok(TokenType::BangEqual) } else { b },
                    e@Ok(TokenType::Equal) => if self.match_token("=") { Ok(TokenType::EqualEqual) } else { e },
                    l@Ok(TokenType::Less) => if self.match_token("=") { Ok(TokenType::LessEqual) } else { l },
                    g@Ok(TokenType::Greater) => if self.match_token("=") { Ok(TokenType::GreaterEqual) } else { g }
                    s@Ok(TokenType::Slash) => if self.match_token("/") {
                        while self.peek() != "\n" && !self.is_at_end() {
                            self.advance();
                        }
                        if self.is_at_end() {
                            return Ok(TokenType::Eof);
                        } else {
                            let substring = self.advance();
                            self.scan_token(substring)
                        }
                    } else {
                        s
                    },
                    other => other
                }
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

    fn add_token(&self, token_type: TokenType<'a>, tokens: &mut Vec<Token<'a>>) {
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

    fn parse_string_literal(&mut self) -> Result<TokenType<'a>, String> {
        while self.peek() != "\"" && !self.is_at_end() {
            if self.peek() == "\n" {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err("Unterminated string.".to_string());
        }

        self.advance();

        let value = &self.source[self.start + 1..self.current - 1];
        Ok(TokenType::String(value))
    }

    fn parse_number_literal(&mut self) -> Result<TokenType<'a>, String> {
        while self.is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == "." && self.is_digit(self.peek_next()) {
            self.advance();

            while self.is_digit(self.peek()) {
                self.advance();
            }
        }

        let number_string = &self.source[self.start..self.current];
        println!("{:?}", number_string);
        match number_string.parse::<f64>() {
            Ok(number) => Ok(TokenType::Number(number)),
            Err(parsing_error) => Err(parsing_error.to_string())
        }
    }

    fn is_digit(&self, substring: &'a str) -> bool {
        substring.parse::<f64>().is_ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_parse_token_types() {
        let mut scanner = Scanner::new("{}(),.-+;*/!!===<<=>>=");
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
            &TokenType::GreaterEqual,
        ];
        assert_eq!(expected, token_types);
    }

    #[test]
    fn should_parse_comment() {
        let mut scanner = Scanner::new("//some comment");
        let tokens = scanner.scan_tokens().unwrap();
        let token_types = tokens.iter()
            .map(|token| &token.token_type)
            .collect::<Vec<_>>();

        println!("Tokens: {:?}", token_types);
        assert_eq!(vec![&TokenType::Eof], token_types);
    }

    #[test]
    fn should_ignore_whitespace_with_comment() {
        let mut scanner = Scanner::new("(( )){} // grouping stuff");
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
            &TokenType::RightBrace,
            &TokenType::Eof
        ];

        assert_eq!(expected, token_types);
    }

    #[test]
    fn should_increment_counter_on_line_break() {
        let mut scanner = Scanner::new("(
            // comment, but still count the newline
        )");

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
        let mut scanner = Scanner::new("\"literally a string\"");
        let tokens = scanner.scan_tokens().unwrap();
        let token_types = tokens.iter()
            .map(|token| &token.token_type)
            .collect::<Vec<_>>();

        assert_eq!(vec![&TokenType::String("literally a string")], token_types);
    }

    #[test]
    fn should_parse_number_literals() {
        let mut scanner = Scanner::new("13.37\n1337");
        let tokens = scanner.scan_tokens().unwrap();
        let token_types = tokens.iter()
            .map(|token| &token.token_type)
            .collect::<Vec<_>>();

        assert_eq!(vec![&TokenType::Number(13.37), &TokenType::Number(1337.0)], token_types);
    }

    #[test]
    fn should_store_correct_lexemes_and_lines() {
        let mut scanner = Scanner::new("{<=\"foo\nbar\"");
        let tokens = scanner.scan_tokens().unwrap();

        let expected = vec![
            Token::new(TokenType::LeftBrace, "{", 1),
            Token::new(TokenType::LessEqual, "<=", 1),
            Token::new(TokenType::String("foo\nbar"), "\"foo\nbar\"", 2)
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn should_return_err_on_unknown_character() {
        let mut scanner = Scanner::new("{}[");
        let tokens = scanner.scan_tokens();
        let expected = "Unexpected character: `[`".to_string();

        assert_eq!(tokens.is_err(), true);
        assert_eq!(expected, tokens.err().unwrap());
    }
}