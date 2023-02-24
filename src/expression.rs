use std::{fmt::Display, rc::Rc};

use crate::token::Lexeme;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Assigment(Lexeme, Box<MaterializableExpression>),
    Binary(Box<MaterializableExpression>, Box<MaterializableExpression>, BinaryOperator),
    Call(Box<MaterializableExpression>, Lexeme, Vec<Box<MaterializableExpression>>),
    Grouping(Box<MaterializableExpression>),
    Literal(LiteralValue),
    Logical(Box<MaterializableExpression>, Box<MaterializableExpression>, LogicalOperator),
    Unary(UnaryOperator, Box<MaterializableExpression>),
    Variable(Lexeme)
}

#[derive(Debug, PartialEq, Clone)]
pub struct MaterializableExpression {
    pub expression: Expression,
    pub lexeme: Lexeme
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    String(Rc<str>),
    Number(f64),
    Boolean(bool),
    Nil
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    Minus,
    Not
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub enum LogicalOperator {
    And,
    Or
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

impl Display for LogicalOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::And => f.write_str("and"),
            Self::Or => f.write_str("or")
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assigment(name, expression) => f.write_str(format!("{:?} = {}", name, expression.to_string()).as_str()),
            Self::Binary(lhs, rhs, op) => f.write_str(format!("{} {} {}", lhs.to_string(), rhs.to_string(), op.to_string()).as_str()),
            Self::Call(callee, _, arguments) => f.write_str(format!("{}({:?})", callee.to_string(), arguments).as_str()),
            Self::Grouping(expression) => f.write_str(format!("({})", expression.to_string()).as_str()),
            Self::Unary(op, expression) => f.write_str(format!("{}{}", op.to_string(), expression.to_string()).as_str()),
            Self::Literal(literal_value) => f.write_str(literal_value.to_string().as_str()),
            Self::Logical(lhs, rhs, op) => f.write_str(format!("{} {} {}", lhs.to_string(), rhs.to_string(), op.to_string()).as_str()),
            Self::Variable(name) => f.write_str(format!("var {:?}", name).as_str())
        }
    }
}

impl Display for MaterializableExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.expression.to_string().as_str())
    }
}

impl Expression {
    pub fn wrap_default(self) -> MaterializableExpression {
        MaterializableExpression::new(self, Lexeme::default())
    }

    pub fn wrap(self, lexeme: Lexeme) -> MaterializableExpression {
        MaterializableExpression::new(self, lexeme)
    }
}

impl MaterializableExpression {
    pub fn new(expression: Expression, lexeme: Lexeme) -> Self {
        MaterializableExpression { expression, lexeme }
    }
}
