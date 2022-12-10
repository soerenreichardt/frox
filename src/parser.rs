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
            if let Some(token) = self.token_iterator.peek() {
                let operator = match token.token_type {
                    TokenType::BangEqual => BinaryOperator::CompareNot,
                    TokenType::EqualEqual => BinaryOperator::Compare,
                    _ => break
                };

                self.token_iterator.next();
                let right = self.comparison();
                expression = Expression::Binary(Box::new(expression), Box::new(right), operator);
            } else {
                break
            }
        }
        expression
    }

    fn comparison(&mut self) -> Expression {
        let mut expression = self.term();
        loop {
            if let Some(token) = self.token_iterator.peek() {
                let operator = match token.token_type {
                    TokenType::Greater => BinaryOperator::GreaterThan,
                    TokenType::GreaterEqual => BinaryOperator::GreaterThenOrEqual,
                    TokenType::Less => BinaryOperator::LessThan,
                    TokenType::LessEqual => BinaryOperator::LessThanOrEqual,
                    _ => break
                };

                self.token_iterator.next();
                let right = self.term();
                expression = Expression::Binary(Box::new(expression), Box::new(right), operator);
            } else {
                break
            }
        }
        expression
    }

    fn term(&mut self) -> Expression {
        let mut expression = self.factor();

        loop {
            if let Some(token) = self.token_iterator.peek() {
                let operator = match token.token_type {
                    TokenType::Plus => BinaryOperator::Add,
                    TokenType::Minus => BinaryOperator::Subtract,
                    _ => break
                };

                self.token_iterator.next();
                let right = self.factor();
                expression = Expression::Binary(Box::new(expression), Box::new(right), operator);
            } else {
                break
            }
        }
        expression
    }

    fn factor(&mut self) -> Expression {
        let mut expression = self.unary();

        loop {
            if let Some(token) = self.token_iterator.peek() {
                let operator = match token.token_type {
                    TokenType::Star => BinaryOperator::Multiply,
                    TokenType::Slash => BinaryOperator::Divide,
                    _ => break
                };

                self.token_iterator.next();
                let right = self.unary();
                expression = Expression::Binary(Box::new(expression), Box::new(right), operator);
            } else {
                break
            }
        }
        expression
    }

    fn unary(&mut self) -> Expression {
        if let Some(token) = self.token_iterator.peek() {
            let operator = match token.token_type {
                TokenType::Bang => UnaryOperator::Not,
                TokenType::Minus => UnaryOperator::Minus,
                _ => return self.primary()
            };

            self.token_iterator.next();
            let right = self.unary();
            return Expression::Unary(operator, Box::new(right));
        }
        self.primary()
    }

    fn primary(&mut self) -> Expression {
        if let Some(token) = self.token_iterator.peek() {
            let expression = match token.token_type {
                TokenType::False => Some(Expression::Literal(LiteralValue::Boolean(false))),
                TokenType::True => Some(Expression::Literal(LiteralValue::Boolean(true))),
                TokenType::Nil => Some(Expression::Literal(LiteralValue::Nil)),
                TokenType::Number => Some(Expression::Literal(LiteralValue::Number(token.lexeme.parse::<f64>().unwrap()))),
                TokenType::String => Some(Expression::Literal(LiteralValue::String(token.lexeme.to_string()))),
                _ => None
            };

            match expression {
                Some(expression) => {
                    self.token_iterator.next();
                    return expression;
                },
                None => panic!()
            }
        }
        panic!()
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