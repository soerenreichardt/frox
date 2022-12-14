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
        let mut expression = self.comparison()?;

        loop {
            let token_type = self.token_iterator.peek().map(|token| token.token_type);
            let operator = match token_type {
                Some(TokenType::BangEqual) => BinaryOperator::CompareNot,
                Some(TokenType::EqualEqual) => BinaryOperator::Compare,
                _ => break
            };
            self.token_iterator.next();
            let right = self.comparison()?;
            expression = Expression::Binary(Box::new(expression), Box::new(right), operator);
        }
        Ok(expression)
    }

    fn comparison(&mut self) -> Result<Expression> {
        let mut expression = self.term()?;

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
            let right = self.term()?;
            expression = Expression::Binary(Box::new(expression), Box::new(right), operator);
        }
        Ok(expression)
    }

    fn term(&mut self) -> Result<Expression> {
        let mut expression = self.factor()?;

        loop {
            let token_type = self.token_iterator.peek().map(|token| token.token_type);
            let operator = match token_type {
                Some(TokenType::Plus) => BinaryOperator::Add,
                Some(TokenType::Minus) => BinaryOperator::Subtract,
                _ => break
            };

            self.token_iterator.next();
            let right = self.factor()?;
            expression = Expression::Binary(Box::new(expression), Box::new(right), operator);
        }
        Ok(expression)
    }

    fn factor(&mut self) -> Result<Expression> {
        let mut expression = self.unary()?;

        loop {
            let token_type = self.token_iterator.peek().map(|token| token.token_type);
            let operator = match token_type {
                Some(TokenType::Star) => BinaryOperator::Multiply,
                Some(TokenType::Slash) => BinaryOperator::Divide,
                _ => break
            };

            self.token_iterator.next();
            let right = self.unary()?;
            expression = Expression::Binary(Box::new(expression), Box::new(right), operator);
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
                Token { token_type: TokenType::False, .. } => Ok(Expression::Literal(LiteralValue::Boolean(false))),
                Token { token_type: TokenType::True, .. } => Ok(Expression::Literal(LiteralValue::Boolean(true))),
                Token { token_type: TokenType::Nil, .. } => Ok(Expression::Literal(LiteralValue::Nil)),
                Token { token_type: TokenType::Number, lexeme, .. } => Ok(Expression::Literal(LiteralValue::Number(lexeme.parse::<f64>().unwrap()))),
                Token { token_type: TokenType::String, lexeme, .. } => Ok(Expression::Literal(LiteralValue::String(Self::remove_first_and_last(lexeme).to_string()))),
                Token { token_type: TokenType::LeftParen, .. } => {
                    self.token_iterator.next();
                    let expression = self.expression()?;
                    self.consume(&TokenType::RightParen)?;
                    Ok(Expression::Grouping(Box::new(expression)))
                },
                _ => Err(Error::ParserError("Could not match expression".to_string()))
            },
            None => Err(Error::ParserError("Reached end of file while parsing".to_string()))
        }?;
        
        self.token_iterator.next();
        Ok(expression)
    }

    fn consume(&mut self, expected_token_type: &TokenType) -> Result<()> {
        match self.token_iterator.peek().map(|token| token.token_type) {
            Some(token_type) if &token_type == expected_token_type => {
                self.token_iterator.next();
                Ok(())
            },
            Some(_) | None => Err(Error::ParserError(format!("Expected token to be of type {:?}", expected_token_type)))
        }
    }

    fn remove_first_and_last(value: &str) -> &str {
        let mut chars = value.chars();
        chars.next();
        chars.next_back();
        chars.as_str()
    }
}

#[cfg(test)]
mod tests {
    use crate::context::Context;

    use super::*;

    #[test]
    fn should_parse_comparison_expression() {
        let tokens = vec![
            Token::new(TokenType::Number, "1", 1),
            Token::new(TokenType::EqualEqual, "==", 1),
            Token::new(TokenType::Number, "2", 1)
        ];
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

    #[test]
    fn should_parse_grouping_expression() {
        let tokens = vec![
            Token::new(TokenType::LeftParen, "(", 1),
            Token::new(TokenType::String, "\"foo\"", 1),
            Token::new(TokenType::RightParen, ")", 1),
        ];
        let mut parser = Parser::new(tokens, Context::new());
        let expression = parser.expression().unwrap();
        assert_eq!(
            Expression::Grouping(
                Box::new(Expression::Literal(LiteralValue::String("foo".to_string())))
            ),
            expression
        )
    }
}