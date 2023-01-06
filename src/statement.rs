use crate::expression::MaterializableExpression;

#[derive(Debug, PartialEq)]
pub enum Statement<'a> {
    Expression(MaterializableExpression<'a>),
    Print(MaterializableExpression<'a>)
}