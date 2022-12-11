use std::iter::Peekable;
use std::vec::IntoIter;

use crate::expression::{BinaryOperator, UnaryOperator, LiteralValue};
use crate::{expression::Expression};
use crate::token::*;

struct Parser<'a> {
    token_iterator: Peekable<TokenIterator<'a>>
}

struct TokenIterator<'a> {
    tokens: IntoIter<Token<'a>>,
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.next()
    }
}

impl<'a> Parser<'a> {
    fn new(tokens: Vec<Token<'a>>) -> Self {
        let token_holder = TokenIterator {
            tokens: tokens.into_iter()
        };

        Parser {
            token_iterator: token_holder.peekable()
        }
    }

    fn expression(&mut self) -> Expression {
        self.equality()
    }

    fn equality(&mut self) -> Expression {
        let mut expression = self.comparison();
        loop {
            let token_type = self.token_iterator.peek().map(|token| token.token_type);
            let operator = match token_type {
                Some(TokenType::BangEqual) => BinaryOperator::CompareNot,
                Some(TokenType::EqualEqual) => BinaryOperator::Compare,
                _ => break
            };
            self.token_iterator.next();
            let right = self.comparison();
            expression = Expression::Binary(Box::new(expression), Box::new(right), operator);
        }
        expression
    }

    fn comparison(&mut self) -> Expression {
        let mut expression = self.term();
        loop {
            let token_type = self.token_iterator.peek().map(|token| token.token_type);
            let operator = match token_type {
                Some(TokenType::Greater) => BinaryOperator::GreaterThan,
                Some(TokenType::GreaterEqual) => BinaryOperator::GreaterThenOrEqual,
                Some(TokenType::Less) => BinaryOperator::LessThan,
                Some(TokenType::LessEqual) => BinaryOperator::LessThanOrEqual,
                _ => break
            };

            self.token_iterator.next();
            let right = self.term();
            expression = Expression::Binary(Box::new(expression), Box::new(right), operator);
        }
        expression
    }

    fn term(&mut self) -> Expression {
        let mut expression = self.factor();

        loop {
            let token_type = self.token_iterator.peek().map(|token| token.token_type);
            let operator = match token_type {
                Some(TokenType::Plus) => BinaryOperator::Add,
                Some(TokenType::Minus) => BinaryOperator::Subtract,
                _ => break
            };

            self.token_iterator.next();
            let right = self.factor();
            expression = Expression::Binary(Box::new(expression), Box::new(right), operator);
        }
        expression
    }

    fn factor(&mut self) -> Expression {
        let mut expression = self.unary();

        loop {
            let token_type = self.token_iterator.peek().map(|token| token.token_type);
            let operator = match token_type {
                Some(TokenType::Star) => BinaryOperator::Multiply,
                Some(TokenType::Slash) => BinaryOperator::Divide,
                _ => break
            };

            self.token_iterator.next();
            let right = self.unary();
            expression = Expression::Binary(Box::new(expression), Box::new(right), operator);
        }
        expression
    }

    fn unary(&mut self) -> Expression {
        let token_type = self.token_iterator.peek().map(|token| token.token_type);
        let operator = match token_type {
            Some(TokenType::Bang) => UnaryOperator::Not,
            Some(TokenType::Minus) => UnaryOperator::Minus,
            _ => return self.primary()
        };

        self.token_iterator.next();
        let right = self.unary();
        return Expression::Unary(operator, Box::new(right));
    }

    fn primary(&mut self) -> Expression {
        let expression = self.token_iterator.peek().map(|token| match token {
            Token { token_type: TokenType::False, .. } => Expression::Literal(LiteralValue::Boolean(false)),
            Token { token_type: TokenType::True, .. } => Expression::Literal(LiteralValue::Boolean(true)),
            Token { token_type: TokenType::Nil, .. } => Expression::Literal(LiteralValue::Nil),
            Token { token_type: TokenType::Number, lexeme, .. } => Expression::Literal(LiteralValue::Number(lexeme.parse::<f64>().unwrap())),
            Token { token_type: TokenType::String, lexeme, .. } => Expression::Literal(LiteralValue::String(lexeme.to_string())),
            _ => panic!()
        });

        match expression {
            Some(expression) => {
                self.token_iterator.next();
                return expression;
            },
            None => panic!()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::Scanner;

    use super::*;

    #[test]
    fn foo() {
        let mut scanner = Scanner::new("1 == 2");
        let tokens = scanner.scan_tokens().unwrap();
        let mut parser = Parser::new(tokens);
        let expression = parser.expression();
        assert_eq!(
            Expression::Binary(
                Box::new(Expression::Literal(LiteralValue::Number(1.0))), 
                Box::new(Expression::Literal(LiteralValue::Number(2.0))), 
                BinaryOperator::Compare
            ),
            expression
        );
    }
}