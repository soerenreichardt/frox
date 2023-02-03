use crate::{expression::MaterializableExpression, token::Lexeme};

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Expression(MaterializableExpression<'a>),
    Print(MaterializableExpression<'a>),
    Var(Lexeme, Option<MaterializableExpression<'a>>),
    Block(Vec<Statement<'a>>),
    If(MaterializableExpression<'a>, Box<Statement<'a>>, Option<Box<Statement<'a>>>),
    While(MaterializableExpression<'a>, Box<Statement<'a>>),
    Function(Lexeme, Vec<Lexeme>, Vec<Statement<'a>>)
}