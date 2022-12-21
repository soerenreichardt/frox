use crate::context::Context;
use crate::token::*;
use crate::error::*;

pub struct Scanner<'a> {
    context: Context<'a>,
    start: usize,
    current: usize,
    line: usize
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Scanner {
            context: Context::new(source),
            start: 0,
            current: 0,
            line: 1
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.start = self.current;
            let substring = self.advance();
            let token_type = self.scan_token(substring);

            match token_type {
                Some(token_type) => match token_type {
                    Ok(token_type) =>  self.add_token(token_type, &mut tokens),
                    Err(error) => self.context.collect_error(error)
                },
                None => continue
            }
        }

        self.context.flush_errors(tokens)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.context.source.len()
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
        self.context.source.get(self.current-1..self.current).unwrap()
    }

    fn peek(&self) -> &'a str {
        if self.is_at_end() {
            return "\0";
        }
        self.context.source.get(self.current..self.current+1).unwrap()
    }

    fn peek_next(&self) -> &'a str {
        if self.current + 1 >= self.context.source.len() {
            return "\0";
        }
        self.context.source.get(self.current+1..self.current+2).unwrap()
    }

    fn add_token(&self, token_type: TokenType, tokens: &mut Vec<Token>) {
        tokens.push(Token::new(token_type, (self.start, self.current), self.line))
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
            return Some(Err(Error::ScannerError("Unterminated string.".to_string(), self.line, self.current)));
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

        let number_string = &self.context.source[self.start..self.current];
        Some(match number_string.parse::<f64>() {
            Ok(_) => Ok(TokenType::Number),
            Err(parsing_error) => Err(Error::ScannerError(parsing_error.to_string(), self.line, self.current))
        })
    }

    fn parse_identifier(&mut self) -> Option<Result<TokenType>> {
        while Self::is_alpha_numberic(self.peek()) {
            self.advance();
        }

        let substring = &self.context.source[self.start..self.current];
        Some(match TokenType::from_str(substring, self.line, self.current) {
            a@Ok(_) => a,
            Err(_) => Ok(TokenType::Identifier)
        })
    }

    fn parse_operator(&mut self, substring: &'a str) -> Option<Result<TokenType>> {
        match TokenType::from_str(substring, self.line, self.current) {
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
    use crate::Materializable;

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
            &TokenType::GreaterEqual
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
        assert!(token_types.is_empty());
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
            &TokenType::RightBrace
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
        let context = Context::new("\"literally a string\"");
        let mut scanner = Scanner::new("\"literally a string\"");
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(vec![Token::new(TokenType::String, (0, 20), 1)], tokens);
    }

    #[test]
    fn should_parse_number_literals() {
        let context = Context::new("13.37\n1337");
        let mut scanner = Scanner::new("13.37\n1337");
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(vec![Token::new(TokenType::Number, (0, 5), 1), Token::new(TokenType::Number, (6, 10), 2)], tokens);
    }

    #[test]
    fn should_parse_identifiers_and_keywords() {
        let source = "and class while foo";
        let mut scanner = Scanner::new(source);
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
        assert_eq!("foo", tokens.get(3).unwrap().lexeme.materialize(&Context::new(source)))
    }

    #[test]
    fn should_store_correct_lexemes_and_lines() {
        let mut scanner = Scanner::new("{<=\"foo\nbar\"");
        let tokens = scanner.scan_tokens().unwrap();

        let expected = vec![
            Token::new(TokenType::LeftBrace, (0, 1), 1),
            Token::new(TokenType::LessEqual, (1, 3), 1),
            Token::new(TokenType::String, (3, 12), 2)
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn should_return_err_on_unknown_character() {
        let mut scanner = Scanner::new("{}[");
        let tokens = scanner.scan_tokens();
        let expected = Error::FroxError(
            format!("Error occured on line 1:\n{{}}[\n  ^\n  Unexpected character: `[`").to_string(),
        );

        assert_eq!(tokens.is_err(), true);
        assert_eq!(expected, tokens.err().unwrap());
    }
}