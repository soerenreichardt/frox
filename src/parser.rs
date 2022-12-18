use std::iter::Peekable;
use std::vec::IntoIter;

use crate::context::Context;
use crate::expression::{BinaryOperator, UnaryOperator, LiteralValue, MaterializableExpression};
use crate::{expression::Expression};
use crate::{token::*, Materializable};
use crate::error::*;

pub struct Parser<'a> {
    token_iterator: Peekable<TokenIterator>,
    context: Context<'a>
}

struct TokenIterator {
    tokens: IntoIter<Token>,
}

impl Iterator for TokenIterator {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.next()
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, context: Context<'a>) -> Self {
        let token_holder = TokenIterator {
            tokens: tokens.into_iter()
        };

        Parser {
            token_iterator: token_holder.peekable(),
            context
        }
    }

    pub fn expression(&mut self) -> Result<MaterializableExpression> {
        self.equality()
    }

    fn equality(&mut self) -> Result<MaterializableExpression> {
        let mut materializable_expression = self.comparison()?;

        loop {
            let token_type = self.token_iterator.peek().map(|token| token.token_type);
            let operator = match token_type {
                Some(TokenType::BangEqual) => BinaryOperator::CompareNot,
                Some(TokenType::EqualEqual) => BinaryOperator::Compare,
                _ => break
            };
            self.token_iterator.next();
            let right = self.comparison()?;
            materializable_expression = MaterializableExpression::new(
                Expression::Binary(Box::new(materializable_expression.expression), Box::new(right.expression), operator),
                materializable_expression.lexeme.union(&right.lexeme)
            );
        }
        Ok(materializable_expression)
    }

    fn comparison(&mut self) -> Result<MaterializableExpression> {
        let mut materializable_expression = self.term()?;

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
            materializable_expression = MaterializableExpression::new(
                Expression::Binary(Box::new(materializable_expression.expression), Box::new(right.expression), operator),
                materializable_expression.lexeme.union(&right.lexeme)
            );
        }
        Ok(materializable_expression)
    }

    fn term(&mut self) -> Result<MaterializableExpression> {
        let mut materializable_expression = self.factor()?;

        loop {
            let token_type = self.token_iterator.peek().map(|token| token.token_type);
            let operator = match token_type {
                Some(TokenType::Plus) => BinaryOperator::Add,
                Some(TokenType::Minus) => BinaryOperator::Subtract,
                _ => break
            };

            self.token_iterator.next();
            let right = self.factor()?;
            materializable_expression = MaterializableExpression::new(
                Expression::Binary(Box::new(materializable_expression.expression), Box::new(right.expression), operator),
                materializable_expression.lexeme.union(&right.lexeme)
            );
        }
        Ok(materializable_expression)
    }

    fn factor(&mut self) -> Result<MaterializableExpression> {
        let mut materializable_expression = self.unary()?;

        loop {
            let token_type = self.token_iterator.peek().map(|token| token.token_type);
            let operator = match token_type {
                Some(TokenType::Star) => BinaryOperator::Multiply,
                Some(TokenType::Slash) => BinaryOperator::Divide,
                _ => break
            };

            self.token_iterator.next();
            let right = self.unary()?;
            materializable_expression = MaterializableExpression::new(
                Expression::Binary(Box::new(materializable_expression.expression), Box::new(right.expression), operator),
                materializable_expression.lexeme.union(&right.lexeme)
            );
        }
        Ok(materializable_expression)
    }

    fn unary(&mut self) -> Result<MaterializableExpression> {
        let token_type = self.token_iterator.peek().map(|token| token.token_type);
        let operator = match token_type {
            Some(TokenType::Bang) => UnaryOperator::Not,
            Some(TokenType::Minus) => UnaryOperator::Minus,
            _ => return self.primary()
        };

        self.token_iterator.next();
        let materializable_expression = self.unary()?;
        Ok(MaterializableExpression::new(
            Expression::Unary(operator, Box::new(materializable_expression.expression)),
            materializable_expression.lexeme
        ))
    }

    fn primary(&mut self) -> Result<MaterializableExpression> {
        let materializable_expression = match self.token_iterator.peek() {
            Some(token) => match token {
                Token { token_type: TokenType::False, .. } => 
                    Ok(MaterializableExpression::new(Expression::Literal(LiteralValue::Boolean(false)), token.lexeme)),
                Token { token_type: TokenType::True, .. } => 
                    Ok(MaterializableExpression::new(Expression::Literal(LiteralValue::Boolean(true)), token.lexeme)),
                Token { token_type: TokenType::Nil, .. } => 
                    Ok(MaterializableExpression::new(Expression::Literal(LiteralValue::Nil), token.lexeme)),
                Token { token_type: TokenType::Number, lexeme, .. } => 
                    Ok(MaterializableExpression::new(
                        Expression::Literal(LiteralValue::Number(lexeme.materialize(&self.context).parse::<f64>().unwrap())),
                        token.lexeme
                    )),
                Token { token_type: TokenType::String, lexeme, .. } => 
                    Ok(MaterializableExpression::new(
                        Expression::Literal(LiteralValue::String(Self::remove_first_and_last(lexeme.materialize(&self.context)).to_string())),
                        token.lexeme
                    )),
                Token { token_type: TokenType::LeftParen, .. } => {
                    Ok(MaterializableExpression::new(
                        // This expression is a dummy as the correct inner expression will be
                        // evaluated after this match block.
                        Expression::Grouping(Box::new(Expression::Literal(LiteralValue::Nil))),
                        token.lexeme
                    ))
                },
                _ => Err(Error::ParserError("Could not match expression".to_string()))
            },
            None => Err(Error::ParserError("Reached end of file while parsing".to_string()))
        }?;
        
        let materializable_expression = match materializable_expression {
            MaterializableExpression { expression: Expression::Grouping(_), lexeme } => {
                self.token_iterator.next();
                let materializable_expression = self.expression()?;
                let found_parenthesis = self.consume(&TokenType::RightParen)?;
                MaterializableExpression::new(
                    Expression::Grouping(Box::new(materializable_expression.expression)),
                    lexeme.union(&found_parenthesis.lexeme)
                )
            }
            expr => expr
        };

        self.token_iterator.next();
        Ok(materializable_expression)
    }

    fn consume(&mut self, expected_token_type: &TokenType) -> Result<Token> {
        let found_token = self.token_iterator.next_if(|token| 
            if &token.token_type == expected_token_type { return true } else { return false }
        );

        match found_token {
            Some(token) => Ok(token),
            None =>  Err(Error::ParserError(format!("Expected token to be of type {:?}", expected_token_type)))
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
            Token::new(TokenType::Number, (0, 1), 1),
            Token::new(TokenType::EqualEqual, (1, 3), 1),
            Token::new(TokenType::Number, (3, 4), 1)
        ];
        let mut parser = Parser::new(tokens, Context::new("1==2"));
        let expression = parser.expression().unwrap();
        assert_eq!(
            Expression::Binary(
                Box::new(Expression::Literal(LiteralValue::Number(1.0))), 
                Box::new(Expression::Literal(LiteralValue::Number(2.0))), 
                BinaryOperator::Compare
            ),
            expression.expression
        );
    }

    #[test]
    fn should_parse_grouping_expression() {
        let tokens = vec![
            Token::new(TokenType::LeftParen, (0, 1), 1),
            Token::new(TokenType::String, (1, 6), 1),
            Token::new(TokenType::RightParen, (6, 7), 1),
        ];
        let mut parser = Parser::new(tokens, Context::new("(\"foo\""));
        let expression = parser.expression().unwrap();
        assert_eq!(
            Expression::Grouping(
                Box::new(Expression::Literal(LiteralValue::String("foo".to_string())))
            ),
            expression.expression
        )
    }
}