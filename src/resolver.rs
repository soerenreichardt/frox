use std::{rc::Rc, collections::HashMap};

use crate::{statement::Statement, token::{Lexeme}, expression::{MaterializableExpression, Expression}, error::{Result, Error}, context::Context, Materializable};

pub(crate) struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
    context: Context,
    local_variables: LocalVariables
}

#[derive(Default, Debug)]
pub(crate) struct LocalVariables {
    locals: HashMap<Lexeme, usize>
}

impl Resolver {
    pub(crate) fn new(source: Rc<str>) -> Self {
        Resolver { scopes: Vec::new(), context: Context::new(source), local_variables: LocalVariables::default() }
    }

    pub(crate) fn resolve(&mut self, statements: &[Statement]) -> Result<&LocalVariables> {
        for statement in statements {
            match self.resolve_statement(statement) {
                Err(error) => self.context.collect_error(error),
                _ => ()
            }
        }
        self.context.flush_errors(&self.local_variables)
    }

    fn resolve_statement(&mut self, statement: &Statement) -> Result<()> {
        match statement {
            Statement::Block(inner_statements) => {
                self.begin_scope();
                self.resolve(inner_statements)?;
                self.end_scope();
            },
            Statement::Var(name, initializer) => {
                self.declare(name);
                if let Some(initializer) = initializer {
                    self.resolve_expression(initializer)?;
                }
                self.define(name);
            },
            Statement::Expression(expression) => self.resolve_expression(expression)?,
            Statement::If(condition, then_statement, else_statement) => {
                self.resolve_expression(condition)?;
                self.resolve_statement(then_statement)?;
                if let Some(else_statement) = else_statement {
                    self.resolve_statement(else_statement)?;
                }
            },
            Statement::Function(name, parameters, body) => {
                name.map(|name| {
                    self.declare(&name);
                    self.define(&name);
                });
                self.begin_scope();
                for parameter in parameters.iter() {
                    self.declare(parameter);
                    self.define(parameter);
                }
                self.resolve(body)?;
                self.end_scope();
            },
            Statement::Print(expression) => self.resolve_expression(expression)?,
            Statement::Return(expression) => {
                if let Some(expression) = expression {
                    self.resolve_expression(expression)?;
                }
            },
            Statement::While(condition, body) => {
                self.resolve_expression(condition)?;
                self.resolve_statement(body)?;
            }
        };
        Ok(())
    }

    fn resolve_expression(&mut self, MaterializableExpression { expression, lexeme: _ }: &MaterializableExpression) -> Result<()> {
        match expression {
            Expression::Variable(lexeme) => {
                let name = lexeme.materialize(&self.context);
                if !self.scopes.is_empty() && self.scopes.last().expect("No values present").get(name) == Some(&false) {
                    return Err(Error::ResolverError("Can't read local variable in its own initializer".to_string(), lexeme.clone()))
                }
                self.resolve_local(name.to_string(), lexeme)?;
            },
            Expression::Assigment(lexeme, expression) => {
                self.resolve_expression(expression)?;
                self.resolve_local(lexeme.materialize(&self.context).to_string(), lexeme)?;
            },
            Expression::Binary(lhs, rhs, _) => {
                self.resolve_expression(lhs)?;
                self.resolve_expression(rhs)?;
            },
            Expression::Logical(lhs, rhs, _) => {
                self.resolve_expression(lhs)?;
                self.resolve_expression(rhs)?;
            },
            Expression::Unary(_, expr) => self.resolve_expression(expr)?,
            Expression::Call(callee, _, arguments) => {
                self.resolve_expression(callee)?;
                for argument in arguments.iter() {
                    self.resolve_expression(argument)?;
                }
            },
            Expression::Grouping(inner) => self.resolve_expression(inner)?,
            Expression::Literal(_) => (),
            Expression::Lambda(body) => self.resolve_statement(body)?
        }
        Ok(())
    }

    fn resolve_local(&mut self, name: String, lexeme: &Lexeme) -> Result<()> {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name) {
                self.local_variables.insert(lexeme.clone(), self.scopes.len() - 1 - i);
                return Ok(())
            }
        }
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, lexeme: &Lexeme) {
        if self.scopes.is_empty() {
            return;
        }

        let name = lexeme.materialize(&self.context);
        self.scopes
            .last_mut().expect("No values present")
            .insert(name.to_string(), false);
    }

    fn define(&mut self, lexeme: &Lexeme) {
        if self.scopes.is_empty() {
            return;
        }

        let name = lexeme.materialize(&self.context);
        self.scopes
            .last_mut().expect("No values present")
            .insert(name.to_string(), true);
    }
}

impl LocalVariables {
    pub(crate) fn insert(&mut self, lexeme: Lexeme, distance: usize) {
        self.locals.insert(lexeme, distance);
    }

    pub(crate) fn get(&self, lexeme: Lexeme) -> Option<&usize> {
        self.locals.get(&lexeme)
    }
}