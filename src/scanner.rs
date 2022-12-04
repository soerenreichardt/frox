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
                Ok(token_type) => self.add_token(substring, token_type, &mut tokens),
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

    fn scan_token(&mut self, substring: &'a str) -> Result<TokenType, String> {
        match substring {
            " " | "\r" | "\t" => {
                let substring = self.advance();
                self.scan_token(substring)
            }
            "\n" => {
                self.line += 1;
                let substring = self.advance();
                self.scan_token(substring)
            }
            _ => match TokenType::from_str(substring) {
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

    fn add_token(&self, substring: &'a str, token_type: TokenType, tokens: &mut Vec<Token<'a>>) {
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
    fn should_return_err_on_unknown_character() {
        let mut scanner = Scanner::new("{}[");
        let tokens = scanner.scan_tokens();
        let expected = "Unexpected character: `[`".to_string();

        assert_eq!(tokens.is_err(), true);
        assert_eq!(expected, tokens.err().unwrap());
    }
}