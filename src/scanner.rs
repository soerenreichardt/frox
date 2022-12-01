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

    pub fn scan_tokens(mut self) -> Vec<Token<'a>> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.start = self.current;
            let substring = self.advance();
            let token_type = self.scan_token(substring);
            self.add_token(substring, token_type, &mut tokens)
        }

        tokens
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_token(&mut self, substring: &'a str) -> TokenType {
        TokenType::from_str(substring).unwrap()
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