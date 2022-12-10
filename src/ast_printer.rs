use crate::{expression::Expression};

#[allow(dead_code)]
struct AstPrinter {
}

impl AstPrinter {
    #[allow(dead_code)]
    fn evaluate(expression: &Expression) -> String {
        match expression {
            Expression::Binary(left, right, token_type) => 
                AstPrinter::parenthesize(token_type.to_string().as_str(), &[left, right]),
            Expression::Grouping(expression) => 
                AstPrinter::parenthesize("group", &[expression]),
            Expression::Literal(literal_value) => literal_value.to_string(),
            Expression::Unary(token_type, expression) =>
                AstPrinter::parenthesize(token_type.to_string().as_str(), &[expression])
        }
    }

    fn parenthesize(name: &str, expressions: &[&Expression]) -> String {
        let mut strings: Vec<String> = Vec::new();

        strings.push("(".to_string());
        strings.push(name.to_string());
        for &expression in expressions {
            strings.push(" ".to_string());
            strings.push(AstPrinter::evaluate(expression));
        }
        strings.push(")".to_string());
        strings.join("")
    }
}

#[cfg(test)]
mod tests {
    use crate::{expression::{BinaryOperator, UnaryOperator, LiteralValue}};

    use super::*;

    #[test]
    fn should_print_expression_tree() {
        let expression = Box::new(Expression::Binary(
            Box::new(Expression::Unary(
                UnaryOperator::Minus, 
                Box::new(Expression::Literal(LiteralValue::String("123".to_string())))
            )), 
            Box::new(Expression::Grouping(
                Box::new(Expression::Literal(LiteralValue::Number(45.67)))
            )), 
            BinaryOperator::Multiply
        ));

        assert_eq!("(* (- 123) (group 45.67))", AstPrinter::evaluate(&expression))
    }
}