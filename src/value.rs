use std::fmt::{Formatter, Display};
use std::str::FromStr;

use crate::callable::{DeclaredFunction, Clock, Callable};
use crate::class::Class;
use crate::expression::LiteralValue;
use crate::error::{Result, Error};

#[derive(PartialEq, Clone)]
pub enum FroxValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Function(DeclaredFunction),
    Clock(Clock),
    Class(Class),
    Instance(Class),
    Nil
}

impl FroxValue {
    pub fn from_literal_value(literal_value: &LiteralValue) -> Self {
        match literal_value {
            LiteralValue::Boolean(bool) => FroxValue::Boolean(*bool),
            LiteralValue::String(string) => FroxValue::String(String::from_str(string).unwrap()),
            LiteralValue::Number(number) => FroxValue::Number(*number),
            LiteralValue::Nil => FroxValue::Nil
        }
    }

    pub(crate) fn to_boolean(self) -> Result<bool> {
        match self {
            FroxValue::Boolean(bool) => Ok(bool),
            FroxValue::Nil => Ok(false),
            _ => Err(Error::InterpreterError(format!("{:?} cannot be coerced into a boolean literal", self)))
        }
    }

    fn extract_numbers_from_literals(self, rhs: FroxValue) -> Result<(f64, f64)> {
        match (self, rhs) {
            (FroxValue::Number(left_value), FroxValue::Number(right_value)) => Ok((left_value, right_value)),
            (left, right) => 
                Err(Error::InterpreterError(format!("Expected both operands to be of type number, but got ({:?}, {:?})", left, right)))
         }
    }

    fn apply_comparison<F: Fn(f64, f64) -> bool>(
        self, 
        rhs: FroxValue,
        comparison_operation: F) 
    -> Result<FroxValue> {
        let (left_value, right_value) = self.extract_numbers_from_literals(rhs)?;
        Ok(FroxValue::Boolean(comparison_operation(left_value, right_value)))
    }

    pub(crate) fn less_than(self, rhs: FroxValue) -> Result<FroxValue> {
        self.apply_comparison(rhs, |lhs, rhs| lhs < rhs)
    }

    pub(crate) fn less_than_or_equal(self, rhs: FroxValue) -> Result<FroxValue> {
        self.apply_comparison(rhs, |lhs, rhs| lhs <= rhs)
    }

    pub(crate) fn greater_than(self, rhs: FroxValue) -> Result<FroxValue> {
        self.apply_comparison(rhs, |lhs, rhs| lhs > rhs)
    }

    pub(crate) fn greater_than_or_equal(self, rhs: FroxValue) -> Result<FroxValue> {
        self.apply_comparison(rhs, |lhs, rhs| lhs >= rhs)
    }

    pub(crate) fn equals(self, rhs: FroxValue) -> Result<FroxValue> {
        let bool_value = match (self, rhs) {
            (FroxValue::Number(left_value), FroxValue::Number(right_value)) => left_value == right_value,
            (FroxValue::String(left_value), FroxValue::String(right_value)) => left_value == right_value,
            (FroxValue::Boolean(left_value), FroxValue::Boolean(right_value)) => left_value == right_value,
            (FroxValue::Nil, FroxValue::Nil) => true,
            _ => false
        };
        Ok(FroxValue::Boolean(bool_value))
    }

    pub(crate) fn not_equals(self, rhs:FroxValue) -> Result<FroxValue> {
        let equals = self.equals(rhs)?;
        !equals
    }
}

impl<'a> std::fmt::Debug for FroxValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FroxValue::Boolean(bool) => f.write_str(format!("{}", bool).as_str()),
            FroxValue::Number(number) => f.write_str(format!("{}", number).as_str()),
            FroxValue::String(string) => f.write_str(["\"", string.as_str(), "\""].concat().as_str()),
            FroxValue::Function(callable) => f.write_str(format!("fn({})", callable.arity()).as_str()),
            FroxValue::Clock(_) => f.write_str("clock()"),
            FroxValue::Class(class) => f.write_str(&class.name),
            FroxValue::Instance(class) => f.write_str(format!("{} instance", class.name).as_str()),
            FroxValue::Nil => f.write_str("nil")
        }
    }
}

impl Display for FroxValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(format!("{:?}", self).as_str())
    }
}

impl std::ops::Neg for FroxValue {
    type Output = Result<FroxValue>;

    fn neg(self) -> Self::Output {
        match self {
            FroxValue::Number(number) => Ok(FroxValue::Number(-number)),
            _ => Err(Error::InterpreterError(format!("Unary '-' operator expected number literal, but got {:?}", self)))
        }
    }
}

impl std::ops::Not for FroxValue {
    type Output = Result<FroxValue>;

    fn not(self) -> Self::Output {
        self.to_boolean().map(|bool| FroxValue::Boolean(!bool))
    }
}

impl std::ops::Sub for FroxValue {
    type Output = Result<FroxValue>;

    fn sub(self, rhs: Self) -> Self::Output {
        let (left, right) = self.extract_numbers_from_literals(rhs)?;
        Ok(FroxValue::Number(left - right))
    }
}

impl std::ops::Div for FroxValue {
    type Output = Result<FroxValue>;

    fn div(self, rhs: Self) -> Self::Output {
        let (left, right) = self.extract_numbers_from_literals(rhs)?;
        Ok(FroxValue::Number(left / right))
    }
}

impl std::ops::Mul for FroxValue {
    type Output = Result<FroxValue>;

    fn mul(self, rhs: Self) -> Self::Output {
        let (left, right) = self.extract_numbers_from_literals(rhs)?;
        Ok(FroxValue::Number(left * right))
    }
}

impl std::ops::Add for FroxValue {
    type Output = Result<FroxValue>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (FroxValue::Number(left_value), FroxValue::Number(right_value)) => Ok(FroxValue::Number(left_value + right_value)),
            (FroxValue::String(left_value), FroxValue::String(right_value)) => {
                Ok(FroxValue::String([left_value, right_value].concat()))
            },
            _ => Err(Error::InterpreterError(format!("Binary '+' operator expected both operands to be number or string literals")))
        }
    }
}
