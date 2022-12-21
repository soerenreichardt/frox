use std::iter::Peekable;
use std::vec::IntoIter;

use crate::context::{Context};
use crate::expression::{BinaryOperator, UnaryOperator, LiteralValue, MaterializableExpression};
use crate::expression::Expression;
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
    pub fn new(tokens: Vec<Token>, source: &'a str) -> Self {
        let token_holder = TokenIterator {
            tokens: tokens.into_iter()
        };

        Parser {
            token_iterator: token_holder.peekable(),
            context: Context::new(source),
        }
    }

    pub fn expression(&mut self) -> Result<MaterializableExpression<'a>> {
        self.equality().map_err(|error| Error::FroxError(error.format_error(self.context.source)))
    }

    fn equality(&mut self) -> Result<MaterializableExpression<'a>> {
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
            let union_lexeme = materializable_expression.lexeme.union(&right.lexeme);
            materializable_expression = MaterializableExpression::new(
                Expression::Binary(Box::new(materializable_expression), Box::new(right), operator),
                union_lexeme
            );
        }
        Ok(materializable_expression)
    }

    fn comparison(&mut self) -> Result<MaterializableExpression<'a>> {
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
            let union_lexeme = materializable_expression.lexeme.union(&right.lexeme);
            materializable_expression = MaterializableExpression::new(
                Expression::Binary(Box::new(materializable_expression), Box::new(right), operator),
                union_lexeme
            );
        }
        Ok(materializable_expression)
    }

    fn term(&mut self) -> Result<MaterializableExpression<'a>> {
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
            let union_lexeme = materializable_expression.lexeme.union(&right.lexeme);
            materializable_expression = MaterializableExpression::new(
                Expression::Binary(Box::new(materializable_expression), Box::new(right), operator),
                union_lexeme
            );
        }
        Ok(materializable_expression)
    }

    fn factor(&mut self) -> Result<MaterializableExpression<'a>> {
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
            let union_lexeme = materializable_expression.lexeme.union(&right.lexeme);
            materializable_expression = MaterializableExpression::new(
                Expression::Binary(Box::new(materializable_expression), Box::new(right), operator),
                union_lexeme
            );
        }
        Ok(materializable_expression)
    }

    fn unary(&mut self) -> Result<MaterializableExpression<'a>> {
        let token_type = self.token_iterator.peek().map(|token| token.token_type);
        let operator = match token_type {
            Some(TokenType::Bang) => UnaryOperator::Not,
            Some(TokenType::Minus) => UnaryOperator::Minus,
            _ => return self.primary()
        };

        self.token_iterator.next();
        let materializable_expression = self.unary()?;
        let lexeme = materializable_expression.lexeme;
        Ok(MaterializableExpression::new(
            Expression::Unary(operator, Box::new(materializable_expression)),
            lexeme
        ))
    }

    fn primary(&mut self) -> Result<MaterializableExpression<'a>> {
        let materializable_expression = match self.token_iterator.peek() {
            Some(token@Token { token_type: TokenType::False, .. }) => 
                Ok(MaterializableExpression::new(Expression::Literal(LiteralValue::Boolean(false)), token.lexeme)),
            Some(token@Token { token_type: TokenType::True, .. }) =>
                Ok(MaterializableExpression::new(Expression::Literal(LiteralValue::Boolean(true)), token.lexeme)),
            Some(token@Token { token_type: TokenType::Nil, .. }) => 
                Ok(MaterializableExpression::new(Expression::Literal(LiteralValue::Nil), token.lexeme)),
            Some(token@Token { token_type: TokenType::Number, lexeme, .. }) => 
                Ok(MaterializableExpression::new(
                    Expression::Literal(LiteralValue::Number(lexeme.materialize(&self.context).parse::<f64>().unwrap())),
                    token.lexeme
                )),
            Some(token@Token { token_type: TokenType::String, lexeme, .. }) => 
                Ok(MaterializableExpression::new(
                    Expression::Literal(LiteralValue::String(&self.context.source[lexeme.start+1..lexeme.end-1])),
                    token.lexeme
                )),
            Some(token@Token { token_type: TokenType::LeftParen, .. }) => {
                Ok(MaterializableExpression::new(
                    // This expression is a dummy as the correct inner expression will be
                    // evaluated after this match block.
                    Expression::Grouping(Box::new(MaterializableExpression::new(Expression::Literal(LiteralValue::Nil), token.lexeme))),
                    token.lexeme
                ))
            },
            Some(token) => Err(Error::ParserError("Could not match expression".to_string(), Some(token.lexeme))),
            None => Err(Error::ParserError("Reached end of file while parsing".to_string(), None))
        }?;
        
        let materializable_expression = match materializable_expression {
            MaterializableExpression { expression: Expression::Grouping(_), lexeme } => {
                self.token_iterator.next();
                let materializable_expression = self.expression()?;
                let found_parenthesis = self.consume(&TokenType::RightParen)?;
                MaterializableExpression::new(
                    Expression::Grouping(Box::new(materializable_expression)),
                    lexeme.union(&found_parenthesis.lexeme)
                )
            }
            expr => expr
        };

        self.token_iterator.next();
        Ok(materializable_expression)
    }

    fn consume(&mut self, expected_token_type: &TokenType) -> Result<&Token> {
        match self.token_iterator.peek() {
            Some(token) if &token.token_type == expected_token_type => Ok(token),
            Some(token) => Err(Error::ParserError(format!("Expected token to be of type {:?}", expected_token_type), Some(token.lexeme))),
            None => Err(Error::ParserError("Reached end of file while parsing".to_string(), None))
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
    use super::*;

    #[test]
    fn should_parse_comparison_expression() {
        let tokens = vec![
            Token::new(TokenType::Number, (0, 1), 1),
            Token::new(TokenType::EqualEqual, (1, 3), 1),
            Token::new(TokenType::Number, (3, 4), 1)
        ];
        let mut parser = Parser::new(tokens, "1==2");
        let expression = parser.expression().unwrap();
        assert_eq!(
            Expression::Binary(
                Box::new(Expression::Literal(LiteralValue::Number(1.0)).wrap(Lexeme::new(0, 1))), 
                Box::new(Expression::Literal(LiteralValue::Number(2.0)).wrap(Lexeme::new(3, 4))), 
                BinaryOperator::Compare
            ).wrap(Lexeme::new(0, 4)),
            expression
        );
    }

    #[test]
    fn should_parse_grouping_expression() {
        let tokens = vec![
            Token::new(TokenType::LeftParen, (0, 1), 1),
            Token::new(TokenType::String, (1, 6), 1),
            Token::new(TokenType::RightParen, (6, 7), 1),
        ];
        let mut parser = Parser::new(tokens, "(\"foo\"");
        let expression = parser.expression().unwrap();
        assert_eq!(
            Expression::Grouping(
                Box::new(Expression::Literal(LiteralValue::String("foo")).wrap(Lexeme::new(1, 6)))
            ).wrap(Lexeme::new(0, 7)),
            expression
        )
    }

    #[test]
    fn should_print_error_when_failing_to_match_grouping() {
        let tokens = vec![
            Token::new(TokenType::LeftParen, (0, 1), 1),
            Token::new(TokenType::Number, (1, 2), 1),
            Token::new(TokenType::And, (2, 3), 1)
        ];
        let mut parser = Parser::new(tokens, "(1=");
        let expression = parser.expression();
        assert_eq!(
            "Error occured on line 0:\n(1=\n  ^\n  Expected token to be of type RightParen",
            expression.err().unwrap().to_string()
        )
    }
}