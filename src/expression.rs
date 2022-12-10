use std::fmt::Display;

use crate::token::*;

pub enum Expression {
    Binary(Box<Expression>, Box<Expression>, BinaryOperator),
    Grouping(Box<Expression>),
    Literal(LiteralValue),
    Unary(UnaryOperator, Box<Expression>)
}

pub enum UnaryOperator {
    Minus
}

pub enum BinaryOperator {
    Star
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Star => f.write_str("*")
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Minus => f.write_str("-")
        }
    }
}
