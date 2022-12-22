use std::{str::FromStr, fmt::Formatter};

use crate::{context::{Context}, expression::{Expression, LiteralValue, UnaryOperator, BinaryOperator, MaterializableExpression}, error::Error, token::Lexeme};
use crate::error::Result;

pub struct Interpreter<'a> {
    context: Context<'a>
}

#[derive(PartialEq)]
pub enum FroxValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Nil
}

impl<'a> Interpreter<'a> {
    pub fn new(source: &'a str) -> Self {
        Interpreter { context: Context::new(source) }
    }

    pub fn evaluate(&self, MaterializableExpression { expression, lexeme }: &MaterializableExpression) -> Result<FroxValue> {
        match expression {
            Expression::Grouping(inner_expression) => self.evaluate(inner_expression.as_ref()),
            Expression::Unary(operator, inner_expression) => {
                let right = self.evaluate(&inner_expression)?;
                match operator {
                    UnaryOperator::Minus => self.minus(right, *lexeme),
                    UnaryOperator::Not => self.not(right, *lexeme)
                }
            },
            Expression::Binary(left, right, operator) => {
                let lhs = self.evaluate(&left)?;
                let rhs = self.evaluate(&right)?;
                match operator {
                    BinaryOperator::Subtract => self.subtract(lhs, rhs, *lexeme),
                    BinaryOperator::Divide => self.divide(lhs, rhs, *lexeme),
                    BinaryOperator::Multiply => self.multiply(lhs, rhs, *lexeme),
                    BinaryOperator::Add => self.add(lhs, rhs, *lexeme),

                    BinaryOperator::GreaterThan => self.greater_than(lhs, rhs, *lexeme),
                    BinaryOperator::GreaterThenOrEqual => self.greater_than_or_equal(lhs, rhs, *lexeme),
                    BinaryOperator::LessThan => self.less_than(lhs, rhs, *lexeme),
                    BinaryOperator::LessThanOrEqual => self.less_than_or_equal(lhs, rhs, *lexeme),
                    BinaryOperator::Compare => Ok(FroxValue::Boolean(self.equals(lhs, rhs))),
                    BinaryOperator::CompareNot => Ok(FroxValue::Boolean(!self.equals(lhs, rhs)))
                }
            },
            Expression::Literal(literal_value) => Ok(FroxValue::from_literal_value(literal_value))
        }.map_err(|error| Error::FroxError(error.format_error(self.context.source)))
    }

    fn minus(&self, literal: FroxValue, lexeme: Lexeme) -> Result<FroxValue> {
        match literal {
            FroxValue::Number(number) => Ok(FroxValue::Number(-number)),
            _ => Err(Error::InterpreterError(format!("Unary '-' operator expected number literal, but got {:?}", literal), Some(lexeme)))
        }
    }

    fn not(&self, literal: FroxValue, lexeme: Lexeme) -> Result<FroxValue> {
        self.to_boolean(literal, lexeme).map(|bool| FroxValue::Boolean(bool))
    }

    fn subtract(&self, lhs: FroxValue, rhs: FroxValue, lexeme: Lexeme) -> Result<FroxValue> {
        self.apply_arithmetic_operation(lhs, rhs, lexeme, |lhs, rhs| lhs - rhs)
    }

    fn divide(&self, lhs: FroxValue, rhs: FroxValue, lexeme: Lexeme) -> Result<FroxValue> {
        self.apply_arithmetic_operation(lhs, rhs, lexeme, |lhs, rhs| lhs / rhs)
    }

    fn multiply(&self, lhs: FroxValue, rhs: FroxValue, lexeme: Lexeme) -> Result<FroxValue> {
        self.apply_arithmetic_operation(lhs, rhs, lexeme, |lhs, rhs| lhs * rhs)
    }

    fn add(&self, lhs: FroxValue, rhs: FroxValue, lexeme: Lexeme) -> Result<FroxValue> {
        match (lhs, rhs) {
            (FroxValue::Number(left_value), FroxValue::Number(right_value)) => Ok(FroxValue::Number(left_value + right_value)),
            (FroxValue::String(left_value), FroxValue::String(right_value)) => {
                Ok(FroxValue::String([left_value, right_value].concat()))
            },
            _ => Err(
                Error::InterpreterError(
                    format!("Binary '+' operator expected both operands to be number or string literals"), 
                    Some(lexeme)
                )
            )
        }
    }

    fn less_than(&self, lhs: FroxValue, rhs: FroxValue, lexeme: Lexeme) -> Result<FroxValue> {
        self.apply_comparison(lhs, rhs, lexeme, |lhs, rhs| lhs < rhs)
    }

    fn less_than_or_equal(&self, lhs: FroxValue, rhs: FroxValue, lexeme: Lexeme) -> Result<FroxValue> {
        self.apply_comparison(lhs, rhs, lexeme, |lhs, rhs| lhs <= rhs)
    }

    fn greater_than(&self, lhs: FroxValue, rhs: FroxValue, lexeme: Lexeme) -> Result<FroxValue> {
        self.apply_comparison(lhs, rhs, lexeme, |lhs, rhs| lhs > rhs)
    }

    fn greater_than_or_equal(&self, lhs: FroxValue, rhs: FroxValue, lexeme: Lexeme) -> Result<FroxValue> {
        self.apply_comparison(lhs, rhs, lexeme, |lhs, rhs| lhs >= rhs)
    }

    fn equals(&self, lhs: FroxValue, rhs: FroxValue) -> bool {
        match (lhs, rhs) {
            (FroxValue::Number(left_value), FroxValue::Number(right_value)) => left_value == right_value,
            (FroxValue::String(left_value), FroxValue::String(right_value)) => left_value == right_value,
            (FroxValue::Boolean(left_value), FroxValue::Boolean(right_value)) => left_value == right_value,
            (FroxValue::Nil, FroxValue::Nil) => true,
            _ => false
        }
    }

    fn to_boolean(&self, literal: FroxValue, lexeme: Lexeme) -> Result<bool> {
        match literal {
            FroxValue::Boolean(bool) => Ok(bool),
            FroxValue::Nil => Ok(false),
            _ => Err(Error::InterpreterError(format!("{:?} cannot be coerced into a boolean literal", literal), Some(lexeme)))
        }
    }

    fn apply_arithmetic_operation<F: Fn(f64, f64) -> f64>(
        &self, 
        lhs: FroxValue, 
        rhs: FroxValue,
        lexeme: Lexeme,
        number_operation: F) 
    -> Result<FroxValue> {
        let (left_value, right_value) = self.extract_numbers_from_literals(lhs, rhs, lexeme)?;
        Ok(FroxValue::Number(number_operation(left_value, right_value)))
    }

    fn apply_comparison<F: Fn(f64, f64) -> bool>(
        &self, 
        lhs: FroxValue, 
        rhs: FroxValue, 
        lexeme: Lexeme,
        comparison_operation: F) 
    -> Result<FroxValue> {
        let (left_value, right_value) = self.extract_numbers_from_literals(lhs, rhs, lexeme)?;
        Ok(FroxValue::Boolean(comparison_operation(left_value, right_value)))
    }

    fn extract_numbers_from_literals(&self, lhs: FroxValue, rhs: FroxValue, lexeme: Lexeme) -> Result<(f64, f64)> {
        match (lhs, rhs) {
            (FroxValue::Number(left_value), FroxValue::Number(right_value)) => Ok((left_value, right_value)),
            (left, right) => Err(
                Error::InterpreterError(
                    format!("Expected both operands to be of type number, but got ({:?}, {:?})", left, right),
                    Some(lexeme)
                )
            )
        }
    }
}

impl<'a> FroxValue {
    fn from_literal_value(literal_value: &LiteralValue) -> Self {
        match literal_value {
            LiteralValue::Boolean(bool) => FroxValue::Boolean(*bool),
            LiteralValue::String(string) => FroxValue::String(String::from_str(string).unwrap()),
            LiteralValue::Number(number) => FroxValue::Number(*number),
            LiteralValue::Nil => FroxValue::Nil
        }
    }
}

impl std::fmt::Debug for FroxValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FroxValue::Boolean(bool) => f.write_str(format!("{}", bool).as_str()),
            FroxValue::Number(number) => f.write_str(format!("{}", number).as_str()),
            FroxValue::String(string) => f.write_str(["\"", string.as_str(), "\""].concat().as_str()),
            FroxValue::Nil => f.write_str("nil")
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_evaluate_arithmetic_expressions() {
        let actual = evaluate_binary_arithmetic(1.0, 2.0, BinaryOperator::Add);
        assert_eq!(3.0, actual);

        let actual = evaluate_binary_arithmetic(1.0, 2.0, BinaryOperator::Subtract);
        assert_eq!(-1.0, actual);

        let actual = evaluate_binary_arithmetic(7.0, 2.0, BinaryOperator::Divide);
        assert_eq!(3.5, actual);

        let actual = evaluate_binary_arithmetic(3.0, 2.0, BinaryOperator::Multiply);
        assert_eq!(6.0, actual);
    }

    #[test]
    fn should_evaluate_comparisons() {
        let actual = evaluate_comparison(LiteralValue::Number(42.0), LiteralValue::Number(42.0), BinaryOperator::Compare);
        assert_eq!(true, actual, "42.0 == 42.0");

        let actual = evaluate_comparison(LiteralValue::Number(42.0), LiteralValue::Number(42.0), BinaryOperator::CompareNot);
        assert_eq!(false, actual, "42.0 != 42.0");

        let actual = evaluate_comparison(LiteralValue::Number(42.0), LiteralValue::Number(43.0), BinaryOperator::Compare);
        assert_eq!(false, actual, "42.0 == 43.0");

        let actual = evaluate_comparison(LiteralValue::Number(42.0), LiteralValue::Number(43.0), BinaryOperator::CompareNot);
        assert_eq!(true, actual, "42.0 != 43.0");

        let actual = evaluate_comparison(LiteralValue::Number(42.0), LiteralValue::Number(42.0), BinaryOperator::LessThan);
        assert_eq!(false, actual, "42.0 < 42.0");

        let actual = evaluate_comparison(LiteralValue::Number(42.0), LiteralValue::Number(42.0), BinaryOperator::LessThanOrEqual);
        assert_eq!(true, actual, "42.0 <= 42.0");

        let actual = evaluate_comparison(LiteralValue::Nil, LiteralValue::Nil, BinaryOperator::Compare);
        assert_eq!(true, actual, "Nil == Nil");

        let actual = evaluate_comparison(LiteralValue::Nil, LiteralValue::String("foo"), BinaryOperator::Compare);
        assert_eq!(false, actual, "Nil == \"foo\"");
    }

    #[test]
    fn should_concatenate_strings() {
        let expression = Expression::Binary(
            Box::new(Expression::Literal(LiteralValue::String("foo")).wrap_default()), 
            Box::new(Expression::Literal(LiteralValue::String("bar")).wrap_default()),
            BinaryOperator::Add
        ).wrap_default();
        let interpreter = Interpreter::new("");
        let value = match interpreter.evaluate(&expression) {
            Ok(FroxValue::String(value)) => value,
            _ => panic!("{:?}", expression)
        };

        assert_eq!("foobar", value, "foo + bar")
    }

    fn evaluate_binary_arithmetic(lhs: f64, rhs: f64, operator: BinaryOperator) -> f64 {
        let expression = Expression::Binary(
            Box::new(Expression::Literal(LiteralValue::Number(lhs)).wrap_default()), 
            Box::new(Expression::Literal(LiteralValue::Number(rhs)).wrap_default()), 
            operator
        ).wrap_default();
        let interpreter = Interpreter::new("");
        match interpreter.evaluate(&expression) {
            Ok(FroxValue::Number(value)) => value,
            _ => panic!("{:?}", expression)
        }
    }

    fn evaluate_comparison(lhs: LiteralValue, rhs: LiteralValue, operator: BinaryOperator) -> bool {
        let expression = Expression::Binary(
            Box::new(Expression::Literal(lhs).wrap_default()), 
            Box::new(Expression::Literal(rhs).wrap_default()), 
            operator
        ).wrap_default();
        let interpreter = Interpreter::new("");
        match interpreter.evaluate(&expression) {
            Ok(FroxValue::Boolean(value)) => value,
            _ => panic!("{:?}", expression)
        }
    }
}