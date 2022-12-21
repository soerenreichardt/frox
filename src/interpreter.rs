use crate::{context::Context, expression::{Expression, LiteralValue, UnaryOperator, BinaryOperator}, error::Error};
use crate::error::Result;

struct Interpreter<'a> {
    context: Context<'a>
}

impl<'a> Interpreter<'a> {
    pub fn new(context: Context<'a>) -> Self {
        Interpreter { context }
    }

    pub fn evaluate(&self, expression: &Expression) -> Result<LiteralValue> {
        match expression {
            Expression::Grouping(inner_expression) => self.evaluate(inner_expression.as_ref()),
            Expression::Unary(operator, inner_expression) => {
                let right = self.evaluate(inner_expression)?;
                match operator {
                    UnaryOperator::Minus => self.minus(right),
                    UnaryOperator::Not => self.not(right)
                }
            },
            Expression::Binary(left, right, operator) => {
                let lhs = self.evaluate(left)?;
                let rhs = self.evaluate(right)?;
                match operator {
                    BinaryOperator::Subtract => self.subtract(lhs, rhs),
                    BinaryOperator::Divide => self.divide(lhs, rhs),
                    BinaryOperator::Multiply => self.multiply(lhs, rhs),
                    BinaryOperator::Add => self.add(lhs, rhs),

                    BinaryOperator::GreaterThan => self.greater_than(lhs, rhs),
                    BinaryOperator::GreaterThenOrEqual => self.greater_than_or_equal(lhs, rhs),
                    BinaryOperator::LessThan => self.less_than(lhs, rhs),
                    BinaryOperator::LessThanOrEqual => self.less_than_or_equal(lhs, rhs),
                    BinaryOperator::Compare => Ok(LiteralValue::Boolean(self.equals(lhs, rhs))),
                    BinaryOperator::CompareNot => Ok(LiteralValue::Boolean(!self.equals(lhs, rhs)))
                }
            },
            Expression::Literal(literal_value) => Ok(literal_value.clone())
        }
    }

    fn minus(&self, literal: LiteralValue) -> Result<LiteralValue> {
        match literal {
            LiteralValue::Number(number) => Ok(LiteralValue::Number(-number)),
            _ => Err(Error::InterpreterError(format!("Unary '-' operator expected number literal, but got {:?}", literal)))
        }
    }

    fn not(&self, literal: LiteralValue) -> Result<LiteralValue> {
        self.to_boolean(literal).map(|bool| LiteralValue::Boolean(bool))
    }

    fn subtract(&self, lhs: LiteralValue, rhs: LiteralValue) -> Result<LiteralValue> {
        self.apply_arithmetic_operation(lhs, rhs, |lhs, rhs| lhs - rhs)
    }

    fn divide(&self, lhs: LiteralValue, rhs: LiteralValue) -> Result<LiteralValue> {
        self.apply_arithmetic_operation(lhs, rhs, |lhs, rhs| lhs / rhs)
    }

    fn multiply(&self, lhs: LiteralValue, rhs: LiteralValue) -> Result<LiteralValue> {
        self.apply_arithmetic_operation(lhs, rhs, |lhs, rhs| lhs * rhs)
    }

    fn add(&self, lhs: LiteralValue, rhs: LiteralValue) -> Result<LiteralValue> {
        match (lhs, rhs) {
            (LiteralValue::Number(left_value), LiteralValue::Number(right_value)) => Ok(LiteralValue::Number(left_value + right_value)),
            (LiteralValue::String(left_value), LiteralValue::String(right_value)) => {
                let mut concat_string = String::new();
                concat_string.push_str(left_value.as_str());
                concat_string.push_str(right_value.as_str());
                Ok(LiteralValue::String(concat_string))
            },
            _ => Err(Error::InterpreterError("".to_string()))
        }
    }

    fn less_than(&self, lhs: LiteralValue, rhs: LiteralValue) -> Result<LiteralValue> {
        self.apply_comparison(lhs, rhs, |lhs, rhs| lhs < rhs)
    }

    fn less_than_or_equal(&self, lhs: LiteralValue, rhs: LiteralValue) -> Result<LiteralValue> {
        self.apply_comparison(lhs, rhs, |lhs, rhs| lhs <= rhs)
    }

    fn greater_than(&self, lhs: LiteralValue, rhs: LiteralValue) -> Result<LiteralValue> {
        self.apply_comparison(lhs, rhs, |lhs, rhs| lhs > rhs)
    }

    fn greater_than_or_equal(&self, lhs: LiteralValue, rhs: LiteralValue) -> Result<LiteralValue> {
        self.apply_comparison(lhs, rhs, |lhs, rhs| lhs >= rhs)
    }

    fn equals(&self, lhs: LiteralValue, rhs: LiteralValue) -> bool {
        match (lhs, rhs) {
            (LiteralValue::Number(left_value), LiteralValue::Number(right_value)) => left_value == right_value,
            (LiteralValue::String(left_value), LiteralValue::String(right_value)) => left_value == right_value,
            (LiteralValue::Boolean(left_value), LiteralValue::Boolean(right_value)) => left_value == right_value,
            (LiteralValue::Nil, LiteralValue::Nil) => true,
            _ => false
        }
    }

    fn to_boolean(&self, literal: LiteralValue) -> Result<bool> {
        match literal {
            LiteralValue::Boolean(bool) => Ok(bool),
            LiteralValue::Nil => Ok(false),
            _ => Err(Error::InterpreterError(format!("{:?} cannot be coerced into a boolean literal", literal)))
        }
    }

    fn apply_arithmetic_operation<F: Fn(f64, f64) -> f64>(
        &self, 
        lhs: LiteralValue, 
        rhs: LiteralValue, 
        number_operation: F) 
    -> Result<LiteralValue> {
        let (left_value, right_value) = self.extract_numbers_from_literals(lhs, rhs)?;
        Ok(LiteralValue::Number(number_operation(left_value, right_value)))
    }

    fn apply_comparison<F: Fn(f64, f64) -> bool>(
        &self, 
        lhs: LiteralValue, 
        rhs: LiteralValue, 
        comparison_operation: F) 
    -> Result<LiteralValue> {
        let (left_value, right_value) = self.extract_numbers_from_literals(lhs, rhs)?;
        Ok(LiteralValue::Boolean(comparison_operation(left_value, right_value)))
    }

    fn extract_numbers_from_literals(&self, lhs: LiteralValue, rhs: LiteralValue) -> Result<(f64, f64)> {
        match (lhs, rhs) {
            (LiteralValue::Number(left_value), LiteralValue::Number(right_value)) => Ok((left_value, right_value)),
            (left, right) => Err(Error::InterpreterError(format!("Expected both operands to be of type number, but got ({:?}, {:?})", left, right)))
        }
    }

    fn extract_strings_from_literals(&self, lhs: LiteralValue, rhs: LiteralValue) -> Result<(String, String)> {
        match (lhs, rhs) {
            (LiteralValue::String(left_value), LiteralValue::String(right_value)) => Ok((left_value, right_value)),
            (left, right) => Err(Error::InterpreterError(format!("Expected both operands to be of type string, but got ({:?}, {:?})", left, right)))
        }
    }
}