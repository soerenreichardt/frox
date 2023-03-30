use std::{rc::Rc, collections::HashMap};

use crate::{statement::Statement, token::{Lexeme}, expression::{MaterializableExpression, Expression}, error::{Result, Error}, context::Context, Materializable};

pub(crate) struct Resolver<'a> {
    scopes: Vec<HashMap<String, bool>>,
    context: Context,
    local_variables: LocalVariables<'a>,
    current_function: &'a FunctionType,
    current_class: &'a ClassType
}

#[derive(Default, Debug)]
pub(crate) struct LocalVariables<'a> {
    locals: HashMap<&'a Lexeme, usize>
}

enum FunctionType {
    None,
    Function,
    Initializer,
    Method
}

enum ClassType {
    None,
    Class
}

impl<'a> Resolver<'a> {
    pub(crate) fn new(source: Rc<str>) -> Self {
        Resolver { 
            scopes: Vec::new(), 
            context: Context::new(source), 
            local_variables: LocalVariables::default(), 
            current_function: &FunctionType::None ,
            current_class: &ClassType::None
        }
    }

    pub(crate) fn resolve(&mut self, statements: &'a [Statement]) -> Result<&LocalVariables> {
        for statement in statements {
            match self.resolve_statement(statement) {
                Err(error) => self.context.collect_error(error),
                _ => ()
            }
        }
        self.context.flush_errors(&self.local_variables)
    }

    fn resolve_statement(&mut self, statement: &'a Statement) -> Result<()> {
        match statement {
            Statement::Block(inner_statements) => {
                self.begin_scope();
                self.resolve(inner_statements)?;
                self.end_scope();
            },
            Statement::Var(name, initializer) => {
                self.declare(name)?;
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
            Statement::Function(name, parameters, body, functino_kind) => {
                if let Some(name) = name {
                    self.declare(&name)?;
                    self.define(&name);
                }

                self.resolve_function(parameters, body, FunctionType::Function)?
            },
            Statement::Print(expression) => self.resolve_expression(expression)?,
            Statement::Return(expression) => match self.current_function {
                FunctionType::None => return Err(Error::ResolverError("Can't return from top-level code".to_string(), None)),
                FunctionType::Initializer => return Err(Error::ResolverError("Cannot return a value from an initializer".to_string(), None)),
                _ => if let Some(expression) = expression {
                    self.resolve_expression(expression)?;
                }
            },
            Statement::While(condition, body) => {
                self.resolve_expression(condition)?;
                self.resolve_statement(body)?;
            },
            Statement::Class(lexeme, superclass, methods) => {
                let enclosing_class = self.current_class;
                self.current_class = &ClassType::Class;

                self.declare(lexeme)?;
                self.define(lexeme);

                match superclass {
                    Some(superclass) if superclass.lexeme.materialize(&self.context).eq(lexeme.materialize(&self.context)) =>
                        Err(Error::ResolverError("A class cannot inherit from itself".to_string(), Some(superclass.lexeme))),
                    Some(superclass) => {
                        self.resolve_expression(&superclass);
                        self.begin_scope();
                        self.scopes.last_mut().expect("No values present").insert("super".to_string(), true);
                        Ok(())
                    },
                    None => Ok(())
                }?;

                self.begin_scope();
                self.scopes.last_mut().expect("No values present").insert("this".to_string(), true);
                for method in methods {
                    let mut declaration = FunctionType::Method;
                    match method {
                        Statement::Function(lexeme, parameters, body, function_kind) => {
                            if let Some(lexeme) = lexeme {
                                if lexeme.materialize(&self.context).eq("init") {
                                    declaration = FunctionType::Initializer;
                                }
                            }
                            self.resolve_function(parameters, body, declaration)?;
                            Ok(())
                        },
                        _ => Err(Error::ResolverError(format!("Expected method, but got {:?}", method).to_string(), None))
                    }?
                }
                self.end_scope();

                superclass.as_ref().map(|_| self.end_scope());

                self.current_class = enclosing_class;
            }
        };
        Ok(())
    }

    fn resolve_expression(&mut self, MaterializableExpression { expression, lexeme: _ }: &'a MaterializableExpression) -> Result<()> {
        match expression {
            Expression::Variable(lexeme) => {
                let name = lexeme.materialize(&self.context);
                if !self.scopes.is_empty() && self.scopes.last().expect("No values present").get(name) == Some(&false) {
                    return Err(Error::ResolverError("Can't read local variable in its own initializer".to_string(), Some(*lexeme)))
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
            Expression::Lambda(body) => self.resolve_statement(body)?,
            Expression::Get(instance, _) => self.resolve_expression(instance)?,
            Expression::Set(instance, _, value) => {
                self.resolve_expression(value)?;
                self.resolve_expression(instance)?;
            },
            Expression::This(lexeme) => match self.current_class {
                ClassType::None => return Err(Error::ResolverError("Cannot use 'this' outside of a class".to_string(), Some(*lexeme))),
                _ => self.resolve_local(lexeme.materialize(&self.context).to_string(), lexeme)?
            },
            Expression::Super(lexeme, method) => self.resolve_local(lexeme.materialize(&self.context).to_string(), method)?
        }
        Ok(())
    }

    fn resolve_local(&mut self, name: String, lexeme: &'a Lexeme) -> Result<()> {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name) {
                self.local_variables.insert(lexeme, self.scopes.len() - 1 - i);
                return Ok(())
            }
        }
        Ok(())
    }

    fn resolve_function(&mut self, parameters: &[Lexeme], body: &'a [Statement], function_type: FunctionType) -> Result<()> {
        let enclosing_function = self.current_function;
        self.current_function = &FunctionType::Function;

        self.begin_scope();
        for parameter in parameters.iter() {
            self.declare(parameter)?;
            self.define(parameter);
        }
        self.resolve(body)?;
        self.end_scope();

        self.current_function = enclosing_function;
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, lexeme: &Lexeme) -> Result<()> {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let name = lexeme.materialize(&self.context).to_string();
        let scope = self.scopes.last_mut().expect("No values present");
        if scope.contains_key(&name) {
            return Err(Error::ResolverError(format!("A variable with name `{}` already exists in this scope", name), Some(*lexeme)));
        }
        scope.insert(name, false);
        Ok(())
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

impl<'a> LocalVariables<'a> {
    pub(crate) fn insert(&mut self, lexeme: &'a Lexeme, distance: usize) {
        self.locals.insert(lexeme, distance);
    }

    pub(crate) fn get(&self, lexeme: Lexeme) -> Option<&usize> {
        self.locals.get(&lexeme)
    }
}
