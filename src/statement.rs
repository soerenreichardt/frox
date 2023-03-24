use crate::{expression::MaterializableExpression, token::Lexeme};

#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum Statement {
    Expression(MaterializableExpression),
    Print(MaterializableExpression),
    Var(Lexeme, Option<MaterializableExpression>),
    Block(Vec<Statement>),
    If(MaterializableExpression, Box<Statement>, Option<Box<Statement>>),
    While(MaterializableExpression, Box<Statement>),
    Function(Option<Lexeme>, Vec<Lexeme>, Vec<Statement>),
    Return(Option<MaterializableExpression>),
    Class(Lexeme, Vec<Statement>)
}