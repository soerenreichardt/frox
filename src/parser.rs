use std::iter::Peekable;
use std::rc::Rc;
use std::vec::IntoIter;

use crate::context::Context;
use crate::expression::{BinaryOperator, UnaryOperator, LiteralValue, MaterializableExpression, LogicalOperator};
use crate::expression::Expression;
use crate::statement::Statement;
use crate::{token::*, Materializable};
use crate::error::*;

pub struct Parser {
    token_iterator: Peekable<TokenIterator>,
    context: Context
}

struct TokenIterator {
    tokens: IntoIter<Token>,
}

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum FunctionKind {
    Function,
    Method,
    Lambda,
    StaticMethod
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

impl Parser {
    pub fn new(source: Rc<str>) -> Self {
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

    pub(crate) fn parse(&mut self, tokens: Vec<Token>) -> Result<Vec<Statement>> {
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

    fn declaration(&mut self) -> Result<Statement> {
        match self.token_iterator.peek() {
            Some(Token { token_type: TokenType::Class, ..}) => self.class_declaration(),
            Some(Token { token_type: TokenType::Fun, ..}) => {
                self.token_iterator.next();
                let name = self.consume(&TokenType::Identifier)?.lexeme;
                self.function_declaration(Some(name), FunctionKind::Function)
            },
            Some(Token { token_type: TokenType::Var, .. }) => self.variable_declaration(),
            _ => self.statement()
        }

        // todo: synchronize
    }

    fn class_declaration(&mut self) -> Result<Statement> {
        self.token_iterator.next();
        let name = self.consume(&TokenType::Identifier)?;
        self.consume(&TokenType::LeftBrace)?;

        let mut methods = Vec::new();
        loop {
            match self.token_iterator.peek().map(|token| token.token_type) {
                Some(TokenType::RightBrace) | None => break,
                _ => {
                    let method =  match self.token_iterator.peek().map(|token| token.token_type) {
                        Some(TokenType::Class) => self.static_method()?,
                        _ => {
                            let name = Some(self.consume(&TokenType::Identifier)?.lexeme);
                            self.function_declaration(name, FunctionKind::Method)?
                        }
                    };
                    methods.push(method);
                }
            }
        }

        self.consume(&TokenType::RightBrace)?;
        
        Ok(Statement::Class(name.lexeme, methods))
    }

    fn static_method(&mut self) -> Result<Statement> {
        println!("static method");
        self.token_iterator.next();
        let name = self.consume(&TokenType::Identifier)?;
        self.function_declaration(Some(name.lexeme), FunctionKind::StaticMethod)
    }

    fn function_declaration(&mut self, name: Option<Lexeme>, function_kind: FunctionKind) -> Result<Statement> {
        self.consume(&TokenType::LeftParen)?;
        let mut parameters = Vec::new();
        if let Some(TokenType::Identifier) = self.token_iterator.peek().map(|token| token.token_type) {
            loop {
                if parameters.len() > 256 {
                    return Err(Error::ParserError("Can't have more than 255 parameters".to_string(), None))
                }

                parameters.push(self.consume(&TokenType::Identifier)?.lexeme);
                match self.token_iterator.peek() {
                    Some(Token { token_type: TokenType::Comma, .. }) => {
                        self.token_iterator.next();
                        continue
                    },
                    Some(_) => break,
                    _ => return Err(Error::ParserError("Reached end of file while parsing function parameters".to_string(), None))
                }
            }
        }

        self.consume(&TokenType::RightParen)?;
        self.consume(&TokenType::LeftBrace)?;
        let body = self.block()?;
        
        Ok(Statement::Function(name, parameters, body, function_kind))
    }

    fn variable_declaration(&mut self) -> Result<Statement> {
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

    fn statement(&mut self) -> Result<Statement> {
        match self.token_iterator.peek().map(|token| token.token_type) {
            Some(TokenType::For) => self.for_statement(),
            Some(TokenType::If) => self.if_statement(),
            Some(TokenType::Print) => self.print_statement(),
            Some(TokenType::Return) => self.return_statement(),
            Some(TokenType::While) => self.while_statement(),
            Some(TokenType::LeftBrace) => self.block_statement(),
            _ => self.expression_statement()
        }
    }

    fn for_statement(&mut self) -> Result<Statement> {
        self.token_iterator.next();
        self.consume(&TokenType::LeftParen)?;

        let initializer = match self.token_iterator.peek().map(|token| token.token_type) {
            Some(TokenType::Semicolon) => None,
            Some(TokenType::Var) => Some(self.variable_declaration()?),
            _ => Some(self.expression_statement()?)
        };

        if initializer.is_none() { self.consume(&TokenType::Semicolon)?; }

        let condition = match self.token_iterator.peek().map(|token| token.token_type) {
            Some(TokenType::Semicolon) => None,
            _ => Some(self.expression()?)
        };

        self.consume(&TokenType::Semicolon)?;

        let increment = match self.token_iterator.peek().map(|token| token.token_type) {
            Some(TokenType::RightParen) => None,
            _ => Some(self.expression()?)
        };

        self.consume(&TokenType::RightParen)?;

        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Statement::Block( vec![ body, Statement::Expression(increment) ] )
        };

        let condition = match condition {
            Some(condition) => condition,
            None => Expression::Literal(LiteralValue::Boolean(true)).wrap(Lexeme::new(0, 0))
        };
        body = Statement::While(condition, Box::new(body));
    
        if let Some(initializer) = initializer {
            body = Statement::Block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn if_statement(&mut self) -> Result<Statement> {
        self.token_iterator.next();
        self.consume(&TokenType::LeftParen)?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen)?;

        let then_branch = self.statement()?;
        let else_branch = match self.token_iterator.peek().map(|token| token.token_type) {
            Some(TokenType::Else) => {
                self.token_iterator.next();
                Some(self.statement()?)
            },
            _ => None
        };
        
        Ok(Statement::If(
            condition,
            Box::new(then_branch),
            else_branch.map(Box::new)
        ))
    }

    fn print_statement(&mut self) -> Result<Statement> {
        self.token_iterator.next();
        let value = self.expression()?;
        self.consume(&TokenType::Semicolon)?;
        Ok(Statement::Print(value))
    }

    fn return_statement(&mut self) -> Result<Statement> {
        self.token_iterator.next();
        let value = match self.token_iterator.peek().map(|token| token.token_type) {
            Some(TokenType::Semicolon) | None => None,
            Some(_) => Some(self.expression()?),
        };

        self.consume(&TokenType::Semicolon)?;
        Ok(Statement::Return(value))
    }

    fn while_statement(&mut self) -> Result<Statement> {
        self.token_iterator.next();
        self.consume(&TokenType::LeftParen)?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen)?;
        let body = self.statement()?;

        Ok(Statement::While(condition, Box::new(body)))
    }

    fn block_statement(&mut self) -> Result<Statement> {
        self.token_iterator.next();
        let statements = self.block()?;
        Ok(Statement::Block(statements))
    }

    fn block(&mut self) -> Result<Vec<Statement>> {
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

    fn expression_statement(&mut self) -> Result<Statement> {
        let expression = self.expression()?;
        self.consume(&TokenType::Semicolon)?;
        Ok(Statement::Expression(expression))
    }

    fn expression(&mut self) -> Result<MaterializableExpression> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<MaterializableExpression> {
        let materializable_expression = self.logical()?;

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
                    Expression::Get(instance, lexeme) => Ok(Expression::Set(instance, lexeme, Box::new(value)).wrap(materializable_expression.lexeme.union(&value_lexeme))),
                    _ => Err(Error::ParserError("Invalid assignment target".to_string(), Some(token.lexeme)))
                }
            }
            None => Ok(materializable_expression)
        }
    }

    fn logical(&mut self) -> Result<MaterializableExpression> {
        let expression = self.equality()?;

        let logical_operator = match self.token_iterator.peek().map(|token| token.token_type) {
            Some(TokenType::Or) => Some(LogicalOperator::Or),
            Some(TokenType::And) => Some(LogicalOperator::And),
            _ => None
        };

        if let Some(logical_operator) = logical_operator {
            self.token_iterator.next();
            let rhs = self.logical()?;
            let union_lexeme = expression.lexeme.union(&rhs.lexeme);
            return Ok(MaterializableExpression::new(
                Expression::Logical(Box::new(expression), Box::new(rhs), logical_operator), 
                union_lexeme
            ))
        }
        Ok(expression)
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
            let union_lexeme = materializable_expression.lexeme.union(&right.lexeme);
            materializable_expression = MaterializableExpression::new(
                Expression::Binary(Box::new(materializable_expression), Box::new(right), operator),
                union_lexeme
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
            let union_lexeme = materializable_expression.lexeme.union(&right.lexeme);
            materializable_expression = MaterializableExpression::new(
                Expression::Binary(Box::new(materializable_expression), Box::new(right), operator),
                union_lexeme
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
            let union_lexeme = materializable_expression.lexeme.union(&right.lexeme);
            materializable_expression = MaterializableExpression::new(
                Expression::Binary(Box::new(materializable_expression), Box::new(right), operator),
                union_lexeme
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
            let union_lexeme = materializable_expression.lexeme.union(&right.lexeme);
            materializable_expression = MaterializableExpression::new(
                Expression::Binary(Box::new(materializable_expression), Box::new(right), operator),
                union_lexeme
            );
        }
        Ok(materializable_expression)
    }

    fn unary(&mut self) -> Result<MaterializableExpression> {
        let token_type = self.token_iterator.peek().map(|token| token.token_type);
        let operator = match token_type {
            Some(TokenType::Bang) => UnaryOperator::Not,
            Some(TokenType::Minus) => UnaryOperator::Minus,
            _ => return self.call()
        };

        self.token_iterator.next();
        let materializable_expression = self.unary()?;
        let lexeme = materializable_expression.lexeme;
        Ok(MaterializableExpression::new(
            Expression::Unary(operator, Box::new(materializable_expression)),
            lexeme
        ))
    }

    fn call(&mut self) -> Result<MaterializableExpression> {
        let mut expression = self.primary()?;

        loop {
            match self.token_iterator.peek().map(|token| token.token_type) {
                Some(TokenType::LeftParen) => expression = self.finish_call(expression)?,
                Some(TokenType::Dot) => {
                    self.token_iterator.next();
                    let token = self.consume(&TokenType::Identifier)?;
                    let lexeme = expression.lexeme;
                    expression = Expression::Get(Box::new(expression), token.lexeme).wrap(lexeme.union(&token.lexeme));
                }
                _ => break
            }
        }

        Ok(expression)
    }

    fn finish_call(&mut self, callee: MaterializableExpression) -> Result<MaterializableExpression> {
        self.token_iterator.next();
        let mut arguments = Vec::new();

        loop {
            match self.token_iterator.peek().map(|token| token.token_type) {
                Some(TokenType::RightParen) => {
                    break
                },
                Some(TokenType::Comma) => {
                    self.token_iterator.next();
                    continue
                },
                Some(_) => arguments.push(Box::new(self.expression()?)),
                None => break
            }
            if arguments.len() > 256 {
                return Err(Error::ParserError("Can't have more than 255 arguments".to_string(), None))
            }
        }

        let closing_parenthesis = self.consume(&TokenType::RightParen)?;
        let union_lexeme = callee.lexeme.union(&closing_parenthesis.lexeme);
        Ok(MaterializableExpression::new(
            Expression::Call(Box::new(callee), closing_parenthesis.lexeme, arguments),
            union_lexeme
        ))
    }

    fn primary(&mut self) -> Result<MaterializableExpression> {
        match self.token_iterator.next() {
            Some (token) => {
                let expression = match token.token_type {
                    TokenType::False => Expression::Literal(LiteralValue::Boolean(false)).wrap(token.lexeme),
                    TokenType::True => Expression::Literal(LiteralValue::Boolean(true)).wrap(token.lexeme),
                    TokenType::Nil => Expression::Literal(LiteralValue::Nil).wrap(token.lexeme),
                    TokenType::Number => Expression::Literal(LiteralValue::Number(token.lexeme.materialize(&self.context).parse::<f64>().unwrap())).wrap(token.lexeme),
                    TokenType::String => Expression::Literal(LiteralValue::String(self.context.source[token.lexeme.start+1..token.lexeme.end-1].into())).wrap(token.lexeme),
                    TokenType::LeftParen => self.grouping_expression(&token.lexeme)?,
                    TokenType::This => Expression::This(token.lexeme).wrap(token.lexeme),
                    TokenType::Identifier => Expression::Variable(token.lexeme).wrap(token.lexeme),
                    TokenType::Fun => self.lambda(token.lexeme)?,
                    _ => return Err(Error::ParserError("Could not match expression".to_string(), Some(token.lexeme)))
                };
                Ok(expression)
            },
            None => Err(Error::ParserError("Reached end of file while parsing".to_string(), None))
        }
    }

    fn lambda(&mut self, lexeme: Lexeme) -> Result<MaterializableExpression> {
        Ok(Expression::Lambda(Box::new(self.function_declaration(None, FunctionKind::Lambda)?)).wrap(lexeme))
    }

    fn grouping_expression(&mut self, lexeme: &Lexeme) -> Result<MaterializableExpression> {
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
        let mut parser = Parser::new("1==2;".into());
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
        let mut parser = Parser::new("(\"foo\";".into());
        let expression = parser.parse(tokens).unwrap();
        assert_eq!(
            Statement::Expression(Expression::Grouping(
                Box::new(Expression::Literal(LiteralValue::String("foo".into())).wrap(Lexeme::new(1, 6)))
            ).wrap(Lexeme::new(0, 7))),
            *expression.get(0).unwrap()
        )
    }

    #[test]
    fn should_parse_print_statement() {
        let tokens = vec![
            Token::new(TokenType::Print, (0, 5), 1),
            Token::new(TokenType::String, (6, 11), 1),
            Token::new(TokenType::Semicolon, (11, 14), 1)
        ];
        let mut parser = Parser::new("print \"foo\";".into());
        let statement = parser.parse(tokens).unwrap();
        assert_eq!(
            Statement::Print(
                Expression::Literal(LiteralValue::String("foo".into())).wrap(Lexeme::new(6, 11))
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
        let mut parser = Parser::new("var a = 1;".into());
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
        let mut parser = Parser::new("{ var a = 1; }".into());
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
    #[test]
    fn should_parse_logical_expression() {
        let tokens = vec![
            Token::new(TokenType::True, (0, 4), 1),
            Token::new(TokenType::Or, (5, 7), 1),
            Token::new(TokenType::True, (8, 11), 1),
            Token::new(TokenType::Semicolon, (11, 11), 1)
        ];
        let mut parser = Parser::new("true or true".into());
        let statements = parser.parse(tokens).unwrap();
        assert_eq!(
            Statement::Expression(
                Expression::Logical(
                    Box::new(Expression::Literal(LiteralValue::Boolean(true)).wrap(Lexeme::new(0, 4))), 
                    Box::new(Expression::Literal(LiteralValue::Boolean(true)).wrap(Lexeme::new(8, 11))), 
                    LogicalOperator::Or
                ).wrap(Lexeme::new(0, 11))
            ),
            *statements.get(0).unwrap()
        )
    }

    #[test]
    fn should_parse_function_call() {
        let tokens = vec![
            Token::new(TokenType::Identifier, (0, 3), 1),
            Token::new(TokenType::LeftParen, (3, 4), 1),
            Token::new(TokenType::RightParen, (4, 5), 1),
            Token::new(TokenType::Semicolon, (5, 6), 1)
        ];
        let mut parser = Parser::new("foo();".into());
        let statements = parser.parse(tokens).unwrap();
        assert_eq!(
            Statement::Expression(
                Expression::Call(
                    Box::new(Expression::Variable(Lexeme::new(0, 3)).wrap(Lexeme::new(0, 3))), 
                    Lexeme::new(4, 5), 
                    Vec::new()
                ).wrap(Lexeme::new(0, 5))
            ),
            *statements.get(0).unwrap()
        )
    }

    #[test]
    fn should_parse_function_call_with_params() {
        let tokens = vec![
            Token::new(TokenType::Identifier, (0, 3), 1),
            Token::new(TokenType::LeftParen, (3, 4), 1),
            Token::new(TokenType::Number, (4, 5), 1),
            Token::new(TokenType::Comma, (5, 6), 1),
            Token::new(TokenType::Number, (6, 7), 1),
            Token::new(TokenType::RightParen, (7, 8), 1),
            Token::new(TokenType::Semicolon, (8, 9), 1)
        ];
        let mut parser = Parser::new("foo(1,2);".into());
        let statements = parser.parse(tokens).unwrap();
        assert_eq!(
            Statement::Expression(
                Expression::Call(
                    Box::new(Expression::Variable(Lexeme::new(0, 3)).wrap(Lexeme::new(0, 3))), 
                    Lexeme::new(7, 8), 
                    vec![
                        Box::new(Expression::Literal(LiteralValue::Number(1.0)).wrap(Lexeme::new(4, 5))),
                        Box::new(Expression::Literal(LiteralValue::Number(2.0)).wrap(Lexeme::new(6, 7)))
                    ]
                ).wrap(Lexeme::new(0, 8))
            ),
            *statements.get(0).unwrap()
        )
    }

    #[test]
    fn should_parse_class() {
        let tokens = vec![
            Token::new(TokenType::Class, (0, 5), 1),
            Token::new(TokenType::Identifier, (6, 9), 1),
            Token::new(TokenType::LeftBrace, (10, 11), 1),
            Token::new(TokenType::Identifier, (12, 13), 1),
            Token::new(TokenType::LeftParen, (13, 14), 1),
            Token::new(TokenType::RightParen, (14, 15), 1),
            Token::new(TokenType::LeftBrace, (16, 17), 1),
            Token::new(TokenType::Print, (18, 23), 1),
            Token::new(TokenType::Number, (24, 25), 1),
            Token::new(TokenType::Semicolon, (25, 26), 1),
            Token::new(TokenType::RightBrace, (27, 28), 1),
            Token::new(TokenType::RightBrace, (29, 30), 1),
        ];
        let mut parser = Parser::new("class foo { a() { print 0; } }".into());
        let statements = parser.parse(tokens).unwrap();
        assert_eq!(
            Statement::Class(
                Lexeme::new(6, 9), 
                vec![
                    Statement::Function(
                        Some(Lexeme::new(12, 13)), 
                        Vec::new(), 
                        vec![Statement::Print( Expression::Literal(LiteralValue::Number(0.0)).wrap(Lexeme::new(24, 25)) ) ],
                        FunctionKind::Method
                    )
                ]
            ),
            *statements.get(0).unwrap()
        )
    }
}