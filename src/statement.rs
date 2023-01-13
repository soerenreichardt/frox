use crate::{expression::MaterializableExpression, token::Lexeme};

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Expression(MaterializableExpression<'a>),
    Print(MaterializableExpression<'a>),
    Var(Lexeme, Option<MaterializableExpression<'a>>),
    Block(Vec<Statement<'a>>)
}