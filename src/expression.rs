use crate::token::*;

enum Expression<'a> {
    Binary(Box<Expression<'a>>, Box<Expression<'a>>, Token<'a>),
    Grouping(Box<Expression<'a>>),
    Literal(Token<'a>),
    Unary(Token<'a>, Box<Expression<'a>>)
}

struct AstPrinter {
}

impl AstPrinter {
    fn evaluate<'a>(expression: &Expression<'a>) -> String {
        match expression {
            Expression::Binary(left, right, token) => 
                AstPrinter::parenthesize(token.lexeme, &[left, right]),
            Expression::Grouping(expression) => 
                AstPrinter::parenthesize("group", &[expression]),
            Expression::Literal(token) => 
                token.lexeme.to_string(),
            Expression::Unary(token, expression) =>
                AstPrinter::parenthesize(token.lexeme, &[expression])
        }
    }

    fn parenthesize<'a>(name: &str, expressions: &[&Expression<'a>]) -> String {
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
    use super::*;

    #[test]
    fn should_print_expression_tree() {
        let expression = Box::new(Expression::Binary(
            Box::new(Expression::Unary(
                Token::new(TokenType::Minus, "-", 1), 
                Box::new(Expression::Literal(Token::new(TokenType::Number(123.0), "123", 1)))
            )), 
            Box::new(Expression::Grouping(
                Box::new(Expression::Literal(Token::new(TokenType::Number(45.67), "45.67", 1)))
            )), 
            Token::new(TokenType::Star, "*", 1)
        ));

        assert_eq!("(* (- 123) (group 45.67))", AstPrinter::evaluate(&expression))
    }
}