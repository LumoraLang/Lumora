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
                if value_type == LumoraType::Null {
                    if !matches!(*ty, LumoraType::String | LumoraType::Array(_)) {
                        return Err(LumoraError::TypeError {
                            code: "L019".to_string(),
                            span: None,
                            message: format!(
                                "Type mismatch: cannot assign null to non-pointer type {:?}",
                                ty
                            ),
                            help: None,
                        });
                    }
                } else if *ty != value_type {
                    if *ty == LumoraType::I32 && value_type == LumoraType::I64 {
                        if let Expr::Integer(n) = value {
                            if *n >= i32::MIN as i64 && *n <= i32::MAX as i64 {
                            } else {
                                return Err(LumoraError::TypeError {
                                    code: "L019".to_string(),
                                    span: None,
                                    message: format!(
                                        "Type mismatch: I66 value {} out of range for I32",
                                        n
                                    ),
                                    help: None,
                                });
                            }
                        } else {
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
                    } else {
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
            Stmt::While { condition, body } => {
                let cond_type = self.check_expression(condition)?;
                if cond_type != LumoraType::Bool {
                    return Err(LumoraError::TypeError {
                        code: "L027".to_string(),
                        span: None,
                        message: "While condition must be boolean".to_string(),
                        help: None,
                    });
                }
                for stmt in body {
                    self.check_statement(stmt)?;
                }
                Ok(())
            }
            Stmt::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                self.check_statement(initializer)?;
                let cond_type = self.check_expression(condition)?;
                if cond_type != LumoraType::Bool {
                    return Err(LumoraError::TypeError {
                        code: "L028".to_string(),
                        span: None,
                        message: "For loop condition must be boolean".to_string(),
                        help: None,
                    });
                }
                self.check_expression(increment)?;
                for stmt in body {
                    self.check_statement(stmt)?;
                }
                Ok(())
            }
        }
    }

    fn check_expression(&self, expr: &Expr) -> Result<LumoraType, LumoraError> {
        match expr {
            Expr::Integer(_) => Ok(LumoraType::I64),
            Expr::Float(_) => Ok(LumoraType::F64),
            Expr::Boolean(_) => Ok(LumoraType::Bool),
            Expr::StringLiteral(_) => Ok(LumoraType::String),
            Expr::Null => Ok(LumoraType::Null),
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
                            (LumoraType::I64, LumoraType::I64) => Ok(LumoraType::I64),
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
                        if left_type == right_type
                            || left_type == LumoraType::Null
                            || right_type == LumoraType::Null
                        {
                            Ok(LumoraType::Bool)
                        } else if (left_type == LumoraType::I32 && right_type == LumoraType::I64)
                            || (left_type == LumoraType::I64 && right_type == LumoraType::I32)
                        {
                            Ok(LumoraType::Bool)
                        } else {
                            Err(LumoraError::TypeError {
                                code: "L023".to_string(),
                                span: None,
                                message: "Comparison requires compatible types".to_string(),
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
                            if *expected_type == LumoraType::I32 && arg_type == LumoraType::I64 {
                                if let Expr::Integer(n) = arg {
                                    if *n >= i32::MIN as i64 && *n <= i32::MAX as i64 {
                                    } else {
                                        return Err(LumoraError::TypeError {
                                            code: "L025".to_string(),
                                            span: None,
                                            message: format!(
                                                "Argument type mismatch: I66 value {} out of range for I32",
                                                n
                                            ),
                                            help: None,
                                        });
                                    }
                                } else {
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
                            } else {
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
            Expr::ArrayLiteral(elements) => {
                if elements.is_empty() {
                    return Err(LumoraError::TypeError {
                        code: "L032".to_string(),
                        span: None,
                        message: "Empty array literals are not supported without explicit type annotation.".to_string(),
                        help: None,
                    });
                }
                let first_element_type = self.check_expression(&elements[0])?;
                for element in elements.iter().skip(1) {
                    let element_type = self.check_expression(element)?;
                    if element_type != first_element_type {
                        return Err(LumoraError::TypeError {
                            code: "L033".to_string(),
                            span: None,
                            message: format!(
                                "Array elements must have the same type: expected {:?}, found {:?}",
                                first_element_type, element_type
                            ),
                            help: None,
                        });
                    }
                }
                Ok(LumoraType::Array(Box::new(first_element_type)))
            }
            Expr::ArrayIndex { array, index } => {
                let array_type = self.check_expression(array)?;
                let index_type = self.check_expression(index)?;

                let element_type = match array_type {
                    LumoraType::Array(inner_type) => *inner_type,
                    LumoraType::String => LumoraType::I32,
                    _ => {
                        return Err(LumoraError::TypeError {
                            code: "L034".to_string(),
                            span: None,
                            message: "Cannot index a non-array or non-string type.".to_string(),
                            help: None,
                        });
                    }
                };

                match index_type {
                    LumoraType::I32 | LumoraType::I64 => Ok(element_type),
                    _ => {
                        return Err(LumoraError::TypeError {
                            code: "L035".to_string(),
                            span: None,
                            message: "Array index must be an integer type (i32 or i64)."
                                .to_string(),
                            help: None,
                        });
                    }
                }
            }
            Expr::ArgCount => Ok(LumoraType::I32),
            Expr::GetArg(index_expr) => {
                let index_type = self.check_expression(index_expr)?;
                match index_type {
                    LumoraType::I32 | LumoraType::I64 => Ok(LumoraType::String),
                    _ => {
                        return Err(LumoraError::TypeError {
                            code: "L044".to_string(),
                            span: None,
                            message: "Argument index must be an integer type (i32 or i64)."
                                .to_string(),
                            help: None,
                        });
                    }
                }
            }
            Expr::StringOf(expr) => {
                let expr_type = self.check_expression(expr)?;
                match expr_type {
                    LumoraType::I32 | LumoraType::I64 | LumoraType::F64 | LumoraType::Bool | LumoraType::String => Ok(LumoraType::String),
                    _ => return Err(LumoraError::TypeError {
                        code: "L045".to_string(),
                        span: None,
                        message: format!("Cannot convert type {:?} to string", expr_type),
                        help: None,
                    }),
                }
            }
            Expr::I32Of(expr) => {
                let expr_type = self.check_expression(expr)?;
                match expr_type {
                    LumoraType::I32 | LumoraType::I64 | LumoraType::F64 | LumoraType::Bool | LumoraType::String => Ok(LumoraType::I32),
                    _ => return Err(LumoraError::TypeError {
                        code: "L046".to_string(),
                        span: None,
                        message: format!("Cannot convert type {:?} to i32", expr_type),
                        help: None,
                    }),
                }
            }
            Expr::I64Of(expr) => {
                let expr_type = self.check_expression(expr)?;
                match expr_type {
                    LumoraType::I32 | LumoraType::I64 | LumoraType::F64 | LumoraType::Bool | LumoraType::String => Ok(LumoraType::I64),
                    _ => return Err(LumoraError::TypeError {
                        code: "L047".to_string(),
                        span: None,
                        message: format!("Cannot convert type {:?} to i64", expr_type),
                        help: None,
                    }),
                }
            }
            Expr::BoolOf(expr) => {
                let expr_type = self.check_expression(expr)?;
                match expr_type {
                    LumoraType::I32 | LumoraType::I64 | LumoraType::F64 | LumoraType::Bool | LumoraType::String => Ok(LumoraType::Bool),
                    _ => return Err(LumoraError::TypeError {
                        code: "L048".to_string(),
                        span: None,
                        message: format!("Cannot convert type {:?} to bool", expr_type),
                        help: None,
                    }),
                }
            }
            Expr::F32Of(expr) => {
                let expr_type = self.check_expression(expr)?;
                match expr_type {
                    LumoraType::I32 | LumoraType::I64 | LumoraType::F64 | LumoraType::Bool | LumoraType::String => Ok(LumoraType::F64),
                    _ => return Err(LumoraError::TypeError {
                        code: "L049".to_string(),
                        span: None,
                        message: format!("Cannot convert type {:?} to f32", expr_type),
                        help: None,
                    }),
                }
            }
            Expr::F64Of(expr) => {
                let expr_type = self.check_expression(expr)?;
                match expr_type {
                    LumoraType::I32 | LumoraType::I64 | LumoraType::F64 | LumoraType::Bool | LumoraType::String => Ok(LumoraType::F64),
                    _ => return Err(LumoraError::TypeError {
                        code: "L050".to_string(),
                        span: None,
                        message: format!("Cannot convert type {:?} to f64", expr_type),
                        help: None,
                    }),
                }
            }
        }
    }
}
