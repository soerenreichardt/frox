use crate::token::*;

pub enum Expression<'a> {
    Binary(Box<Expression<'a>>, Box<Expression<'a>>, Token<'a>),
    Grouping(Box<Expression<'a>>),
    Literal(Token<'a>),
    Unary(Token<'a>, Box<Expression<'a>>)
}
