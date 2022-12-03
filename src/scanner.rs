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

    pub fn scan_tokens(mut self) -> Result<Vec<Token<'a>>, String> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.start = self.current;
            let substring = self.advance();
            let token_type = Self::scan_token(substring);
        
            match token_type {
                Ok(token_type) => self.add_token(substring, token_type, &mut tokens),
                Err(message) => return Err(message)
            }
        }

        Ok(tokens)
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(substring: &'a str) -> Result<TokenType, String> {
        TokenType::from_str(substring)
    }

    fn advance(&mut self) -> &'a str {
        self.current += 1;
        let next_character = self.source.get(self.start..self.current).unwrap();
        next_character
    }

    fn add_token(&self, substring: &'a str, token_type: TokenType, tokens: &mut Vec<Token<'a>>) {
        tokens.push(Token::new(token_type, substring, self.line))
    }
}