use std::fmt::Display;

use crate::{token::Lexeme};

#[derive(Debug, PartialEq)]
pub enum Expression<'a> {
    Binary(Box<MaterializableExpression<'a>>, Box<MaterializableExpression<'a>>, BinaryOperator),
    Grouping(Box<MaterializableExpression<'a>>),
    Literal(LiteralValue<'a>),
    Unary(UnaryOperator, Box<MaterializableExpression<'a>>),
}

#[derive(Debug, PartialEq)]
pub struct MaterializableExpression<'a> {
    pub expression: Expression<'a>,
    pub lexeme: Lexeme
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue<'a> {
    String(&'a str),
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

impl<'a> Display for LiteralValue<'a> {
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

impl<'a> Display for Expression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binary(lhs, rhs, op) => f.write_str(format!("{} {} {}", lhs.to_string(), rhs.to_string(), op.to_string()).as_str()),
            Self::Grouping(expression) => f.write_str(format!("({})", expression.to_string()).as_str()),
            Self::Unary(op, expression) => f.write_str(format!("{}{}", op.to_string(), expression.to_string()).as_str()),
            Self::Literal(literal_value) => f.write_str(literal_value.to_string().as_str())
        }
    }
}

impl<'a> Display for MaterializableExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.expression.to_string().as_str())
    }
}

impl<'a> Expression<'a> {
    pub fn wrap_default(self) -> MaterializableExpression<'a> {
        MaterializableExpression::new(self, Lexeme::default())
    }

    pub fn wrap(self, lexeme: Lexeme) -> MaterializableExpression<'a> {
        MaterializableExpression::new(self, lexeme)
    }
}

impl<'a> MaterializableExpression<'a> {
    pub fn new(expression: Expression<'a>, lexeme: Lexeme) -> Self {
        MaterializableExpression { expression, lexeme }
    }
}
