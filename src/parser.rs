use std::iter::Peekable;
use std::vec::IntoIter;

use crate::context::Context;
use crate::expression::{BinaryOperator, UnaryOperator, LiteralValue, MaterializableExpression};
use crate::expression::Expression;
use crate::statement::Statement;
use crate::{token::*, Materializable};
use crate::error::*;

pub struct Parser<'a> {
    token_iterator: Peekable<TokenIterator>,
    context: Context<'a>
}

struct TokenIterator {
    tokens: IntoIter<Token>,
}

impl Default for TokenIterator {
    fn default() -> Self {
        Self { tokens: Vec::new().into_iter() }
    }
}

impl Iterator for TokenIterator {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.tokens.next()
    }
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Parser {
            token_iterator: TokenIterator::default().peekable(),
            context: Context::new(source),
        }
    }

    fn init(&mut self, tokens: Vec<Token>) {
        let token_holder = TokenIterator {
            tokens: tokens.into_iter()
        };
        self.token_iterator = token_holder.peekable();
    }

    pub fn parse(&mut self, tokens: Vec<Token>) -> Result<Vec<Statement<'a>>> {
        self.init(tokens);
        let mut statements = Vec::new();
        while self.token_iterator.peek().is_some() {
            match self.declaration() {
                Ok(statement) => statements.push(statement),
                Err(error) => self.context.collect_error(error)
            }
        }

        self.context.flush_errors(statements)
    }

    fn declaration(&mut self) -> Result<Statement<'a>> {
        match self.token_iterator.peek() {
            Some(Token { token_type: TokenType::Var, .. }) => self.variable_declaration(),
            _ => self.statement()
        }

        // todo: synchronize
    }

    fn variable_declaration(&mut self) -> Result<Statement<'a>> {
        self.token_iterator.next();
        let name = self.consume(&TokenType::Identifier)?.lexeme;

        let initializer = match self.token_iterator.peek().map(|token| token.token_type) {
            Some(TokenType::Equal) => {
                self.token_iterator.next();
                Some(self.expression()?)
            },
            _ => None
        };

        self.consume(&TokenType::Semicolon)?;

        Ok(Statement::Var(name, initializer))
    }

    fn statement(&mut self) -> Result<Statement<'a>> {
        match self.token_iterator.peek().map(|token| token.token_type) {
            Some(TokenType::Print) => self.print_statement(),
            Some(TokenType::LeftBrace) => self.block_statement(),
            _ => self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Statement<'a>> {
        self.token_iterator.next();
        let value = self.expression()?;
        self.consume(&TokenType::Semicolon)?;
        Ok(Statement::Print(value))
    }

    fn block_statement(&mut self) -> Result<Statement<'a>> {
        self.token_iterator.next();
        let statements = self.block()?;
        Ok(Statement::Block(statements))
    }

    fn block(&mut self) -> Result<Vec<Statement<'a>>> {
        let mut statements = Vec::new();

        loop {
            match self.token_iterator.peek().map(|token| token.token_type) {
                Some(TokenType::RightBrace) | None => break,
                Some(_) => statements.push(self.declaration()?)
            };
        }
        self.consume(&TokenType::RightBrace)?;
        Ok(statements)
    }

    fn expression_statement(&mut self) -> Result<Statement<'a>> {
        let expression = self.expression()?;
        self.consume(&TokenType::Semicolon)?;
        self.token_iterator.next();
        Ok(Statement::Expression(expression))
    }

    fn expression(&mut self) -> Result<MaterializableExpression<'a>> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<MaterializableExpression<'a>> {
        let materializable_expression = self.equality()?;

        let equals_token = self.token_iterator.next_if(|token| match token {
            Token { token_type: TokenType::Equal, .. } => true,
            _ => false
        });

        match equals_token {
            Some(token) => {
                let value = self.assignment()?;
                let value_lexeme = value.lexeme;

                match materializable_expression.expression {
                    Expression::Variable(name) => Ok(Expression::Assigment(name, Box::new(value)).wrap(materializable_expression.lexeme.union(&value_lexeme))),
                    _ => Err(Error::ParserError("Invalid assignment target".to_string(), Some(token.lexeme)))
                }
            }
            None => Ok(materializable_expression)
        }
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
        match self.token_iterator.next() {
            Some (token) => {
                let expression = match token.token_type {
                    TokenType::False => Expression::Literal(LiteralValue::Boolean(false)).wrap(token.lexeme),
                    TokenType::True => Expression::Literal(LiteralValue::Boolean(true)).wrap(token.lexeme),
                    TokenType::Nil => Expression::Literal(LiteralValue::Nil).wrap(token.lexeme),
                    TokenType::Number => Expression::Literal(LiteralValue::Number(token.lexeme.materialize(&self.context).parse::<f64>().unwrap())).wrap(token.lexeme),
                    TokenType::String => Expression::Literal(LiteralValue::String(&self.context.source[token.lexeme.start+1..token.lexeme.end-1])).wrap(token.lexeme),
                    TokenType::LeftParen => self.grouping_expression(&token.lexeme)?,
                    TokenType::Identifier => Expression::Variable(token.lexeme).wrap(token.lexeme),
                    _ => return Err(Error::ParserError("Could not match expression".to_string(), Some(token.lexeme)))
                };
                Ok(expression)
            },
            None => Err(Error::ParserError("Reached end of file while parsing".to_string(), None))
        }
    }

    fn grouping_expression(&mut self, lexeme: &Lexeme) -> Result<MaterializableExpression<'a>> {
        let expression = self.expression()?;
        let matchig_parenthesis = self.consume(&TokenType::RightParen)?;
        Ok(Expression::Grouping(Box::new(expression)).wrap(lexeme.union(&matchig_parenthesis.lexeme)))
    }

    fn consume(&mut self, expected_token_type: &TokenType) -> Result<Token> {
        match self.token_iterator.next() {
            Some(token) if &token.token_type == expected_token_type => Ok(token),
            Some(token) => Err(Error::ParserError(format!("Expected token to be of type {:?}", expected_token_type), Some(token.lexeme))),
            None => Err(Error::ParserError("Reached end of file while parsing. Maybe you are missing a ';'?".to_string(), None))
        }
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
            Token::new(TokenType::Number, (3, 4), 1),
            Token::new(TokenType::Semicolon, (4, 5), 1)
        ];
        let mut parser = Parser::new("1==2;");
        let expression = parser.parse(tokens).unwrap();
        assert_eq!(
            Statement::Expression(Expression::Binary(
                Box::new(Expression::Literal(LiteralValue::Number(1.0)).wrap(Lexeme::new(0, 1))), 
                Box::new(Expression::Literal(LiteralValue::Number(2.0)).wrap(Lexeme::new(3, 4))), 
                BinaryOperator::Compare
            ).wrap(Lexeme::new(0, 4))),
            *expression.get(0).unwrap()
        );
    }

    #[test]
    fn should_parse_grouping_expression() {
        let tokens = vec![
            Token::new(TokenType::LeftParen, (0, 1), 1),
            Token::new(TokenType::String, (1, 6), 1),
            Token::new(TokenType::RightParen, (6, 7), 1),
            Token::new(TokenType::Semicolon, (7, 8), 1)
        ];
        let mut parser = Parser::new("(\"foo\";");
        let expression = parser.parse(tokens).unwrap();
        assert_eq!(
            Statement::Expression(Expression::Grouping(
                Box::new(Expression::Literal(LiteralValue::String("foo")).wrap(Lexeme::new(1, 6)))
            ).wrap(Lexeme::new(0, 7))),
            *expression.get(0).unwrap()
        )
    }

    #[test]
    fn should_print_error_when_failing_to_match_grouping() {
        let tokens = vec![
            Token::new(TokenType::LeftParen, (0, 1), 1),
            Token::new(TokenType::Number, (1, 2), 1),
            Token::new(TokenType::And, (2, 3), 1)
        ];
        let mut parser = Parser::new("(1=");
        let expression = parser.parse(tokens);
        assert!(
            expression.err().unwrap().to_string().contains("Error occured on line 0:\n(1=\n  ^\n  Expected token to be of type RightParen")
        )
    }

    #[test]
    fn should_parse_print_statement() {
        let tokens = vec![
            Token::new(TokenType::Print, (0, 5), 1),
            Token::new(TokenType::String, (6, 11), 1),
            Token::new(TokenType::Semicolon, (11, 14), 1)
        ];
        let mut parser = Parser::new("print \"foo\";");
        let statement = parser.parse(tokens).unwrap();
        assert_eq!(
            Statement::Print(
                Expression::Literal(LiteralValue::String("foo")).wrap(Lexeme::new(6, 11))
            ),
            *statement.get(0).unwrap()
        )
    }

    #[test]
    fn should_parse_variable_declaration() {
        let tokens = vec![
            Token::new(TokenType::Var, (0, 3), 1),
            Token::new(TokenType::Identifier, (4, 5), 1),
            Token::new(TokenType::Equal, (6, 7), 1),
            Token::new(TokenType::Number, (8, 9), 1),
            Token::new(TokenType::Semicolon, (9, 10), 1)
        ];
        let mut parser = Parser::new("var a = 1;");
        let statements = parser.parse(tokens).unwrap();
        assert_eq!(
            Statement::Var(
                Lexeme { start: 4, end: 5 }, 
                Some(Expression::Literal(LiteralValue::Number(1.0)).wrap(Lexeme::new(8, 9)))
            ),
            *statements.get(0).unwrap()
        )
    }

    #[test]
    fn should_parse_blocks() {
        let tokens = vec![
            Token::new(TokenType::LeftBrace, (0, 1), 1),
            Token::new(TokenType::Var, (2, 5), 1),
            Token::new(TokenType::Identifier, (6, 7), 1),
            Token::new(TokenType::Equal, (8, 9), 1),
            Token::new(TokenType::Number, (10, 11), 1),
            Token::new(TokenType::Semicolon, (11, 12), 1),
            Token::new(TokenType::RightBrace, (12, 13), 1)
        ];
        let mut parser = Parser::new("{ var a = 1; }");
        let statements = parser.parse(tokens).unwrap();
        assert_eq!(
            Statement::Block(vec![
                Statement::Var(
                    Lexeme { start: 6, end: 7 },
                    Some(Expression::Literal(LiteralValue::Number(1.0)).wrap(Lexeme::new(10, 11)))
                )
            ]),
            *statements.get(0).unwrap()
        )
    }
}