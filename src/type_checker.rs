use crate::ast::{BinaryOp, Expr, Function, LumoraType, Program, Stmt, TopLevelDeclaration};
use crate::errors::{LumoraError, Span};
use crate::lexer::{Spanned, Token};
use crate::parser::Parser;
use logos::Logos;
use std::collections::HashMap;

pub struct TypeChecker {
    variables: HashMap<String, LumoraType>,
    pub functions: HashMap<String, (Vec<LumoraType>, LumoraType)>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn check_program(&mut self, program: &Program) -> Result<(), LumoraError> {
        for module_name in &program.uses {
            let module_path = module_name.clone();
            let imported_source =
                std::fs::read_to_string(&module_path).map_err(|e| LumoraError::ParseError {
                    code: "L017".to_string(),
                    span: None,
                    message: format!("Could not read imported module {}: {}", module_path, e),
                    help: None,
                })?;
            let mut lexer = Token::lexer(&imported_source).spanned();
            let mut tokens = Vec::new();
            while let Some((token_result, span)) = lexer.next() {
                let token = token_result.map_err(|_| LumoraError::ParseError {
                    code: "L018".to_string(),
                    span: Some(Span::new(
                        span.clone(),
                        module_path.clone(),
                        &imported_source,
                    )),
                    message: "Lexing error in imported module: Unrecognized token".to_string(),
                    help: None,
                })?;
                tokens.push(Spanned { value: token, span });
            }

            let mut parser = Parser::new(tokens, module_path.clone(), imported_source.clone());
            let imported_program = parser.parse()?;
            let mut imported_type_checker = TypeChecker::new();
            imported_type_checker.check_program(&imported_program)?;

            for declaration in &imported_program.declarations {
                match declaration {
                    TopLevelDeclaration::Function(function) => {
                        if function.is_exported {
                            let param_types =
                                function.params.iter().map(|(_, ty)| ty.clone()).collect();
                            self.functions.insert(
                                function.name.clone(),
                                (param_types, function.return_type.clone()),
                            );
                        }
                    }
                    TopLevelDeclaration::ExternalFunction(ext_func) => {
                        let param_types = ext_func.params.clone();
                        self.functions.insert(
                            ext_func.name.clone(),
                            (param_types, ext_func.return_type.clone()),
                        );
                    }
                }
            }
        }

        for declaration in &program.declarations {
            match declaration {
                TopLevelDeclaration::Function(function) => {
                    let param_types = function.params.iter().map(|(_, ty)| ty.clone()).collect();
                    self.functions.insert(
                        function.name.clone(),
                        (param_types, function.return_type.clone()),
                    );
                }
                TopLevelDeclaration::ExternalFunction(ext_func) => {
                    let param_types = ext_func.params.clone();
                    self.functions.insert(
                        ext_func.name.clone(),
                        (param_types, ext_func.return_type.clone()),
                    );
                }
            }
        }

        for declaration in &program.declarations {
            if let TopLevelDeclaration::Function(function) = declaration {
                self.check_function(function)?;
            }
        }

        Ok(())
    }

    fn check_function(&mut self, function: &Function) -> Result<(), LumoraError> {
        self.variables.clear();

        for (name, ty) in &function.params {
            self.variables.insert(name.clone(), ty.clone());
        }

        for stmt in &function.body {
            self.check_statement(stmt)?;
        }

        Ok(())
    }

    fn check_statement(&mut self, stmt: &Stmt) -> Result<(), LumoraError> {
        match stmt {
            Stmt::Let {
                is_exported: _,
                name,
                ty,
                value,
            } => {
                let value_type = self.check_expression(value)?;
                if *ty != value_type {
                    return Err(LumoraError::TypeError {
                        code: "L019".to_string(),
                        span: None,
                        message: format!(
                            "Type mismatch: expected {:?}, found {:?}",
                            ty, value_type
                        ),
                        help: None,
                    });
                }
                self.variables.insert(name.clone(), ty.clone());
                Ok(())
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                let cond_type = self.check_expression(condition)?;
                if cond_type != LumoraType::Bool {
                    return Err(LumoraError::TypeError {
                        code: "L020".to_string(),
                        span: None,
                        message: "If condition must be boolean".to_string(),
                        help: None,
                    });
                }

                for stmt in then_block {
                    self.check_statement(stmt)?;
                }

                for stmt in else_block.iter().flatten() {
                    self.check_statement(stmt)?;
                }

                Ok(())
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.check_expression(expr)?;
                }
                Ok(())
            }
            Stmt::Expr(expr) => {
                self.check_expression(expr)?;
                Ok(())
            }
            Stmt::Use(_) => Ok(()),
        }
    }

    fn check_expression(&self, expr: &Expr) -> Result<LumoraType, LumoraError> {
        match expr {
            Expr::Integer(_) => Ok(LumoraType::I32),
            Expr::Float(_) => Ok(LumoraType::F64),
            Expr::Boolean(_) => Ok(LumoraType::Bool),
            Expr::StringLiteral(_) => Ok(LumoraType::String),
            Expr::Identifier(name) => {
                self.variables
                    .get(name)
                    .cloned()
                    .ok_or_else(|| LumoraError::TypeError {
                        code: "L021".to_string(),
                        span: None,
                        message: format!("Undefined variable: {}", name),
                        help: None,
                    })
            }
            Expr::Binary { left, op, right } => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;

                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                        match (left_type, right_type) {
                            (LumoraType::I32, LumoraType::I32) => Ok(LumoraType::I32),
                            (LumoraType::F64, LumoraType::F64) => Ok(LumoraType::F64),
                            _ => Err(LumoraError::TypeError {
                                code: "L022".to_string(),
                                span: None,
                                message: "Arithmetic operations require operands of the same numeric type (i32 or f64)".to_string(),
                                help: None,
                            }),
                        }
                    }
                    BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::Less | BinaryOp::Greater => {
                        if left_type == right_type {
                            Ok(LumoraType::Bool)
                        } else {
                            Err(LumoraError::TypeError {
                                code: "L023".to_string(),
                                span: None,
                                message: "Comparison requires same types".to_string(),
                                help: None,
                            })
                        }
                    }
                }
            }
            Expr::Call { name, args } => {
                if let Some((param_types, return_type)) = self.functions.get(name) {
                    if args.len() != param_types.len() {
                        return Err(LumoraError::TypeError {
                            code: "L024".to_string(),
                            span: None,
                            message: format!(
                                "Function {} expects {} arguments, got {}",
                                name,
                                param_types.len(),
                                args.len()
                            ),
                            help: None,
                        });
                    }

                    for (arg, expected_type) in args.iter().zip(param_types.iter()) {
                        let arg_type = self.check_expression(arg)?;
                        if arg_type != *expected_type {
                            return Err(LumoraError::TypeError {
                                code: "L025".to_string(),
                                span: None,
                                message: format!(
                                    "Argument type mismatch: expected {:?}, found {:?}",
                                    expected_type, arg_type
                                ),
                                help: None,
                            });
                        }
                    }

                    Ok(return_type.clone())
                } else {
                    Err(LumoraError::TypeError {
                        code: "L026".to_string(),
                        span: None,
                        message: format!("Undefined function: {}", name),
                        help: None,
                    })
                }
            }
        }
    }
}
