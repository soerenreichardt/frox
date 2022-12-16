use std::fmt::Display;

use crate::token::Lexeme;

#[derive(Debug, PartialEq)]
pub enum Expression {
    Binary(Box<Expression>, Box<Expression>, BinaryOperator),
    Grouping(Box<Expression>),
    Literal(LiteralValue),
    Unary(UnaryOperator, Box<Expression>),
}

pub struct MaterializableExpression {
    expression: Expression,
    lexeme: Lexeme
}

#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Not
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    Multiply,
    Divide,
    Compare,
    CompareNot,
    GreaterThan,
    GreaterThenOrEqual,
    LessThan,
    LessThanOrEqual,
    Add,
    Subtract
}

impl Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::String(string) => f.write_str(string),
            LiteralValue::Number(number) => f.write_str(number.to_string().as_str()),
            LiteralValue::Boolean(boolean) => f.write_str(boolean.to_string().as_str()),
            LiteralValue::Nil => f.write_str("nil")
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Multiply => f.write_str("*"),
            Self::Divide => f.write_str("/"),
            Self::Compare => f.write_str("=="),
            Self::CompareNot => f.write_str("!="),
            Self::GreaterThan => f.write_str(">"),
            Self::GreaterThenOrEqual => f.write_str(">="),
            Self::LessThan => f.write_str("<"),
            Self::LessThanOrEqual => f.write_str("<="),
            Self::Add => f.write_str("+"),
            Self::Subtract => f.write_str("-"),
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Minus => f.write_str("-"),
            Self::Not => f.write_str("!")
        }
    }
}

impl MaterializableExpression {
    fn new(expression: Expression, lexeme: Lexeme) -> Self {
        MaterializableExpression { expression, lexeme }
    }
}
