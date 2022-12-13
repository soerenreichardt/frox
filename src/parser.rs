use std::iter::Peekable;
use std::vec::IntoIter;

use crate::context::Context;
use crate::expression::{BinaryOperator, UnaryOperator, LiteralValue};
use crate::{expression::Expression};
use crate::token::*;
use crate::error::*;

pub struct Parser<'a> {
    token_iterator: Peekable<TokenIterator<'a>>,
    context: Context
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
    pub fn new(tokens: Vec<Token<'a>>, context: Context) -> Self {
        let token_holder = TokenIterator {
            tokens: tokens.into_iter()
        };

        Parser {
            token_iterator: token_holder.peekable(),
            context
        }
    }

    pub fn expression(&mut self) -> Result<Expression> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expression> {
        let mut expression = match self.comparison() {
            Ok(comparison) => comparison,
            e@Err(_) => return e
        };

        loop {
            let token_type = self.token_iterator.peek().map(|token| token.token_type);
            let operator = match token_type {
                Some(TokenType::BangEqual) => BinaryOperator::CompareNot,
                Some(TokenType::EqualEqual) => BinaryOperator::Compare,
                _ => break
            };
            self.token_iterator.next();
            match self.comparison() {
                Ok(right) => expression = Expression::Binary(Box::new(expression), Box::new(right), operator),
                e@Err(_) => return e
            }
        }
        Ok(expression)
    }

    fn comparison(&mut self) -> Result<Expression> {
        let mut expression = match self.term() {
            Ok(term) => term,
            e@Err(_) => return e
        };

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
            match self.term() {
                Ok(right) => expression = Expression::Binary(Box::new(expression), Box::new(right), operator),
                e@Err(_) => return e
            }
        }
        Ok(expression)
    }

    fn term(&mut self) -> Result<Expression> {
        let mut expression = match self.factor() {
            Ok(factor) => factor,
            Err(error) => return Err(error)
        };

        loop {
            let token_type = self.token_iterator.peek().map(|token| token.token_type);
            let operator = match token_type {
                Some(TokenType::Plus) => BinaryOperator::Add,
                Some(TokenType::Minus) => BinaryOperator::Subtract,
                _ => break
            };

            self.token_iterator.next();
            match self.factor() {
                Ok(right) => expression = Expression::Binary(Box::new(expression), Box::new(right), operator),
                e@Err(_) => return e
            }
        }
        Ok(expression)
    }

    fn factor(&mut self) -> Result<Expression> {
        let mut expression = match self.unary() {
            Ok(unary) => unary,
            Err(error) => return Err(error)
        };

        loop {
            let token_type = self.token_iterator.peek().map(|token| token.token_type);
            let operator = match token_type {
                Some(TokenType::Star) => BinaryOperator::Multiply,
                Some(TokenType::Slash) => BinaryOperator::Divide,
                _ => break
            };

            self.token_iterator.next();
            match self.unary() {
                Ok(right) => expression = Expression::Binary(Box::new(expression), Box::new(right), operator),
                e@Err(_) => return e
            }
        }
        Ok(expression)
    }

    fn unary(&mut self) -> Result<Expression> {
        let token_type = self.token_iterator.peek().map(|token| token.token_type);
        let operator = match token_type {
            Some(TokenType::Bang) => UnaryOperator::Not,
            Some(TokenType::Minus) => UnaryOperator::Minus,
            _ => return self.primary()
        };

        self.token_iterator.next();
        self.unary().map(|right| Expression::Unary(operator, Box::new(right)))
    }

    fn primary(&mut self) -> Result<Expression> {
        let expression = match self.token_iterator.peek() {
            Some(token) => match token {
                Token { token_type: TokenType::False, .. } => Some(Expression::Literal(LiteralValue::Boolean(false))),
                Token { token_type: TokenType::True, .. } => Some(Expression::Literal(LiteralValue::Boolean(true))),
                Token { token_type: TokenType::Nil, .. } => Some(Expression::Literal(LiteralValue::Nil)),
                Token { token_type: TokenType::Number, lexeme, .. } => Some(Expression::Literal(LiteralValue::Number(lexeme.parse::<f64>().unwrap()))),
                Token { token_type: TokenType::String, lexeme, .. } => Some(Expression::Literal(LiteralValue::String(lexeme.to_string()))),
                _ => None
            },
            None => None
        };
        
        match expression {
            Some(expression) => {
                self.token_iterator.next();
                return Ok(expression);
            },
            None => Err(Error::ParserError("Could not match expression".to_string()))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{scanner::Scanner, context::Context};

    use super::*;

    #[test]
    fn should_parse_expression() {
        let mut scanner = Scanner::new("1 == 2", Context::new());
        let tokens = scanner.scan_tokens().unwrap();
        let mut parser = Parser::new(tokens, Context::new());
        let expression = parser.expression().unwrap();
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