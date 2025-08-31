use crate::ast::{
    BinaryOp, Expr, ExternalFunction, Function, LumoraType, Program, Stmt, TopLevelDeclaration,
};
use crate::errors::{LumoraError, Span};
use crate::lexer::{Spanned, Token};

pub struct Parser {
    tokens: Vec<Spanned<Token>>,
    current: usize,
    file_name: String,
    source_code: String,
}

impl Parser {
    pub fn new(tokens: Vec<Spanned<Token>>, file_name: String, source_code: String) -> Self {
        Self {
            tokens,
            current: 0,
            file_name,
            source_code,
        }
    }

    fn peek(&self) -> Option<&Spanned<Token>> {
        self.tokens.get(self.current)
    }

    fn advance(&mut self) -> Option<&Spanned<Token>> {
        if self.current < self.tokens.len() {
            let token = &self.tokens[self.current];
            self.current += 1;
            Some(token)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: &Token) -> Result<(), LumoraError> {
        let current_span_cloned = self.peek().map(|s| s.span.clone());
        let file_name_at_start = self.file_name.clone();
        let source_code_at_start = self.source_code.clone();
        let advanced_token = self.advance();
        match advanced_token {
            Some(spanned_token)
                if std::mem::discriminant(&spanned_token.value)
                    == std::mem::discriminant(expected) =>
            {
                Ok(())
            }
            Some(spanned_token) => Err(LumoraError::ParseError {
                code: "L003".to_string(),
                span: Some(Span::new(
                    spanned_token.span.clone(),
                    file_name_at_start.clone(),
                    &source_code_at_start,
                )),
                message: format!("Expected {:?}, found {:?}", expected, spanned_token.value),
                help: None,
            }),
            None => Err(LumoraError::ParseError {
                code: "L004".to_string(),
                span: current_span_cloned
                    .map(|s| Span::new(s, file_name_at_start, &source_code_at_start)),
                message: "Unexpected end of input".to_string(),
                help: None,
            }),
        }
    }

    pub fn parse(&mut self) -> Result<Program, LumoraError> {
        let mut uses = Vec::new();
        let mut declarations = Vec::new();
        while self.peek().is_some() {
            let peeked_token = self.peek().unwrap();
            match &peeked_token.value {
                Token::Use => {
                    let module_name = self.parse_use_statement()?;
                    uses.push(module_name);
                }
                Token::Exp | Token::Fn => {
                    declarations.push(TopLevelDeclaration::Function(self.parse_function()?));
                }
                Token::Ext => {
                    declarations.push(TopLevelDeclaration::ExternalFunction(
                        self.parse_external_function()?,
                    ));
                }
                _ => {
                    return Err(LumoraError::ParseError {
                        code: "L006".to_string(),
                        span: Some(Span::new(
                            peeked_token.span.clone(),
                            self.file_name.clone(),
                            &self.source_code,
                        )),
                        message: "Expected 'use', 'exp', 'fn', or 'ext'".to_string(),
                        help: None,
                    });
                }
            }
        }

        Ok(Program { uses, declarations })
    }

    fn parse_function(&mut self) -> Result<Function, LumoraError> {
        let is_exported = matches!(self.peek().map(|s| &s.value), Some(Token::Exp));
        if is_exported {
            self.expect(&Token::Exp)?;
        }
        self.expect(&Token::Fn)?;
        let name = match self.advance() {
            Some(spanned_token) => match &spanned_token.value {
                Token::Identifier(name) => name.clone(),
                _ => {
                    return Err(LumoraError::ParseError {
                        code: "L007".to_string(),
                        span: Some(Span::new(
                            spanned_token.span.clone(),
                            self.file_name.clone(),
                            &self.source_code,
                        )),
                        message: "Expected function name (identifier)".to_string(),
                        help: None,
                    });
                }
            },
            None => {
                return Err(LumoraError::ParseError {
                    code: "L007".to_string(),
                    span: None,
                    message: "Unexpected end of input while expecting function name".to_string(),
                    help: None,
                });
            }
        };

        self.expect(&Token::LeftParen)?;
        let mut params = Vec::new();
        while !matches!(self.peek().map(|s| &s.value), Some(Token::RightParen)) {
            let param_name = match self.advance() {
                Some(spanned_token) => match &spanned_token.value {
                    Token::Identifier(name) => name.clone(),
                    _ => {
                        return Err(LumoraError::ParseError {
                            code: "L008".to_string(),
                            span: Some(Span::new(
                                spanned_token.span.clone(),
                                self.file_name.clone(),
                                &self.source_code,
                            )),
                            message: "Expected parameter name (identifier)".to_string(),
                            help: None,
                        });
                    }
                },
                None => {
                    return Err(LumoraError::ParseError {
                        code: "L008".to_string(),
                        span: None,
                        message: "Unexpected end of input while expecting parameter name"
                            .to_string(),
                        help: None,
                    });
                }
            };

            let param_type = self.parse_type()?;
            params.push((param_name, param_type));
            if matches!(self.peek().map(|s| &s.value), Some(Token::Comma)) {
                self.advance();
            }
        }

        self.expect(&Token::RightParen)?;
        self.expect(&Token::Colon)?;
        let return_type = self.parse_type()?;
        self.expect(&Token::LeftBrace)?;
        let mut body = Vec::new();
        while !matches!(self.peek().map(|s| &s.value), Some(Token::RightBrace)) {
            body.push(self.parse_statement()?);
        }

        self.expect(&Token::RightBrace)?;
        Ok(Function {
            is_exported,
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_external_function(&mut self) -> Result<ExternalFunction, LumoraError> {
        self.expect(&Token::Ext)?;
        self.expect(&Token::Fn)?;
        let name = match self.advance() {
            Some(spanned_token) => match &spanned_token.value {
                Token::Identifier(name) => name.clone(),
                _ => {
                    return Err(LumoraError::ParseError {
                        code: "L007".to_string(),
                        span: Some(Span::new(
                            spanned_token.span.clone(),
                            self.file_name.clone(),
                            &self.source_code,
                        )),
                        message: "Expected external function name (identifier)".to_string(),
                        help: None,
                    });
                }
            },
            None => {
                return Err(LumoraError::ParseError {
                    code: "L007".to_string(),
                    span: None,
                    message: "Unexpected end of input while expecting external function name"
                        .to_string(),
                    help: None,
                });
            }
        };

        self.expect(&Token::LeftParen)?;
        let mut params = Vec::new();
        while !matches!(self.peek().map(|s| &s.value), Some(Token::RightParen)) {
            let param_type = self.parse_type()?;
            params.push(param_type);
            if matches!(self.peek().map(|s| &s.value), Some(Token::Comma)) {
                self.advance();
            }
        }

        self.expect(&Token::RightParen)?;
        self.expect(&Token::Colon)?;
        let return_type = self.parse_type()?;
        self.expect(&Token::Semicolon)?;

        Ok(ExternalFunction {
            name,
            params,
            return_type,
        })
    }

    fn parse_type(&mut self) -> Result<LumoraType, LumoraError> {
        match self.advance() {
            Some(spanned_token) => match &spanned_token.value {
                Token::I32Type => Ok(LumoraType::I32),
                Token::I64Type => Ok(LumoraType::I64),
                Token::F64Type => Ok(LumoraType::F64),
                Token::BoolType => Ok(LumoraType::Bool),
                Token::StringType => Ok(LumoraType::String),
                Token::VoidType => Ok(LumoraType::Void),
                _ => Err(LumoraError::ParseError {
                    code: "L009".to_string(),
                    span: Some(Span::new(
                        spanned_token.span.clone(),
                        self.file_name.clone(),
                        &self.source_code,
                    )),
                    message: "Expected a type (e.g., i32, bool)".to_string(),
                    help: None,
                }),
            },
            None => Err(LumoraError::ParseError {
                code: "L009".to_string(),
                span: None,
                message: "Unexpected end of input while expecting a type".to_string(),
                help: None,
            }),
        }
    }

    fn parse_statement(&mut self) -> Result<Stmt, LumoraError> {
        let is_exported = matches!(self.peek().map(|s| &s.value), Some(Token::Exp));
        if is_exported {
            self.advance();
        }

        let current_span = self.peek().map(|s| s.span.clone());
        match self.peek().map(|s| &s.value) {
            Some(Token::Let) => {
                self.advance();
                let name = match self.advance() {
                    Some(spanned_token) => match &spanned_token.value {
                        Token::Identifier(name) => name.clone(),
                        _ => {
                            return Err(LumoraError::ParseError {
                                code: "L010".to_string(),
                                span: Some(Span::new(
                                    spanned_token.span.clone(),
                                    self.file_name.clone(),
                                    &self.source_code,
                                )),
                                message: "Expected variable name (identifier)".to_string(),
                                help: None,
                            });
                        }
                    },
                    None => {
                        return Err(LumoraError::ParseError {
                            code: "L010".to_string(),
                            span: None,
                            message: "Unexpected end of input while expecting variable name"
                                .to_string(),
                            help: None,
                        });
                    }
                };

                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;
                self.expect(&Token::Assign)?;
                let value = self.parse_expression()?;
                self.expect(&Token::Semicolon)?;
                Ok(Stmt::Let {
                    is_exported,
                    name,
                    ty,
                    value,
                })
            }
            Some(Token::If) => {
                if is_exported {
                    return Err(LumoraError::ParseError {
                        code: "L011".to_string(),
                        span: current_span.map(|s| {
                            Span::new(s.clone(), self.file_name.clone(), &self.source_code)
                        }),
                        message: "Cannot export 'if' statements".to_string(),
                        help: None,
                    });
                }
                self.advance();
                let condition = self.parse_expression()?;
                self.expect(&Token::LeftBrace)?;

                let mut then_block = Vec::new();
                while !matches!(self.peek().map(|s| &s.value), Some(Token::RightBrace)) {
                    then_block.push(self.parse_statement()?);
                }
                self.expect(&Token::RightBrace)?;
                let else_block = if matches!(self.peek().map(|s| &s.value), Some(Token::Else)) {
                    self.advance();
                    if matches!(self.peek().map(|s| &s.value), Some(Token::If)) {
                        Some(vec![self.parse_statement()?])
                    } else {
                        self.expect(&Token::LeftBrace)?;
                        let mut else_stmts = Vec::new();
                        while !matches!(self.peek().map(|s| &s.value), Some(Token::RightBrace)) {
                            else_stmts.push(self.parse_statement()?);
                        }
                        self.expect(&Token::RightBrace)?;
                        Some(else_stmts)
                    }
                } else {
                    None
                };

                Ok(Stmt::If {
                    condition,
                    then_block,
                    else_block,
                })
            }
            Some(Token::Return) => {
                if is_exported {
                    return Err(LumoraError::ParseError {
                        code: "L012".to_string(),
                        span: current_span.map(|s| {
                            Span::new(s.clone(), self.file_name.clone(), &self.source_code)
                        }),
                        message: "Cannot export 'return' statements".to_string(),
                        help: None,
                    });
                }
                self.advance();
                let expr = if matches!(self.peek().map(|s| &s.value), Some(Token::Semicolon)) {
                    None
                } else {
                    Some(self.parse_expression()?)
                };
                self.expect(&Token::Semicolon)?;
                Ok(Stmt::Return(expr))
            }
            Some(Token::While) => {
                if is_exported {
                    return Err(LumoraError::ParseError {
                        code: "L013".to_string(),
                        span: current_span.map(|s| {
                            Span::new(s.clone(), self.file_name.clone(), &self.source_code)
                        }),
                        message: "Cannot export 'while' statements".to_string(),
                        help: None,
                    });
                }
                self.parse_while_statement()
            }
            Some(Token::For) => {
                if is_exported {
                    return Err(LumoraError::ParseError {
                        code: "L014".to_string(),
                        span: current_span.map(|s| {
                            Span::new(s.clone(), self.file_name.clone(), &self.source_code)
                        }),
                        message: "Cannot export 'for' statements".to_string(),
                        help: None,
                    });
                }
                self.parse_for_statement()
            }
            _ => {
                if is_exported {
                    return Err(LumoraError::ParseError {
                        code: "L015".to_string(),
                        span: current_span.map(|s| {
                            Span::new(s.clone(), self.file_name.clone(), &self.source_code)
                        }),
                        message: "Cannot export expressions".to_string(),
                        help: None,
                    });
                }
                let expr = self.parse_expression()?;
                self.expect(&Token::Semicolon)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_while_statement(&mut self) -> Result<Stmt, LumoraError> {
        self.expect(&Token::While)?;
        let condition = self.parse_expression()?;
        self.expect(&Token::LeftBrace)?;
        let mut body = Vec::new();
        while !matches!(self.peek().map(|s| &s.value), Some(Token::RightBrace)) {
            body.push(self.parse_statement()?);
        }
        self.expect(&Token::RightBrace)?;
        Ok(Stmt::While { condition, body })
    }

    fn parse_for_statement(&mut self) -> Result<Stmt, LumoraError> {
        self.expect(&Token::For)?;
        self.expect(&Token::LeftParen)?;
        let initializer = Box::new(self.parse_statement()?);
        let condition = self.parse_expression()?;
        self.expect(&Token::Semicolon)?;
        let increment = Box::new(self.parse_expression()?);
        self.expect(&Token::RightParen)?;
        self.expect(&Token::LeftBrace)?;
        let mut body = Vec::new();
        while !matches!(self.peek().map(|s| &s.value), Some(Token::RightBrace)) {
            body.push(self.parse_statement()?);
        }
        self.expect(&Token::RightBrace)?;
        Ok(Stmt::For { initializer, condition, increment, body })
    }

    fn parse_expression(&mut self) -> Result<Expr, LumoraError> {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Result<Expr, LumoraError> {
        let mut expr = self.parse_comparison()?;
        while let Some(op) = self.peek().map(|s| s.value.clone()) {
            let binary_op = match op {
                Token::Equal => BinaryOp::Equal,
                Token::NotEqual => BinaryOp::NotEqual,
                _ => break,
            };

            self.advance();
            let right = self.parse_comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: binary_op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, LumoraError> {
        let mut expr = self.parse_addition()?;
        while let Some(op) = self.peek().map(|s| s.value.clone()) {
            let binary_op = match op {
                Token::Less => BinaryOp::Less,
                Token::Greater => BinaryOp::Greater,
                _ => break,
            };

            self.advance();
            let right = self.parse_addition()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: binary_op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_addition(&mut self) -> Result<Expr, LumoraError> {
        let mut expr = self.parse_multiplication()?;
        while let Some(op) = self.peek().map(|s| s.value.clone()) {
            let binary_op = match op {
                Token::Plus => BinaryOp::Add,
                Token::Minus => BinaryOp::Sub,
                _ => break,
            };

            self.advance();
            let right = self.parse_multiplication()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: binary_op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_multiplication(&mut self) -> Result<Expr, LumoraError> {
        let mut expr = self.parse_primary()?;
        while let Some(op) = self.peek().map(|s| s.value.clone()) {
            let binary_op = match op {
                Token::Multiply => BinaryOp::Mul,
                Token::Divide => BinaryOp::Div,
                _ => break,
            };

            self.advance();
            let right = self.parse_primary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: binary_op,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Expr, LumoraError> {
        let spanned_token = self.advance();
        match spanned_token {
            Some(spanned_token) => match &spanned_token.value {
                Token::Integer(n) => Ok(Expr::Integer(*n)),
                Token::Float(f) => Ok(Expr::Float(*f)),
                Token::True => Ok(Expr::Boolean(true)),
                Token::False => Ok(Expr::Boolean(false)),
                Token::StringLiteral(s) => Ok(Expr::StringLiteral(s.clone())),
                Token::Identifier(name_token) => {
                    let name = name_token.clone();
                    if matches!(self.peek().map(|s| &s.value), Some(Token::LeftParen)) {
                        self.advance();
                        let mut args = Vec::new();

                        while !matches!(self.peek().map(|s| &s.value), Some(Token::RightParen)) {
                            args.push(self.parse_expression()?);
                            if matches!(self.peek().map(|s| &s.value), Some(Token::Comma)) {
                                self.advance();
                            }
                        }

                        self.expect(&Token::RightParen)?;
                        Ok(Expr::Call { name, args })
                    } else {
                        Ok(Expr::Identifier(name))
                    }
                }
                Token::LeftParen => {
                    let expr = self.parse_expression()?;
                    self.expect(&Token::RightParen)?;
                    Ok(expr)
                }
                _ => Err(LumoraError::ParseError {
                    code: "L016".to_string(),
                    span: Some(Span::new(
                        spanned_token.span.clone(),
                        self.file_name.clone(),
                        &self.source_code,
                    )),
                    message: "Expected an expression".to_string(),
                    help: None,
                }),
            },
            None => Err(LumoraError::ParseError {
                code: "L016".to_string(),
                span: None,
                message: "Unexpected end of input while expecting an expression".to_string(),
                help: None,
            }),
        }
    }

    fn parse_use_statement(&mut self) -> Result<String, LumoraError> {
        self.expect(&Token::Use)?;
        let mut module_path = String::new();
        loop {
            let spanned_token = self.advance();
            match spanned_token {
                Some(spanned_token) => match &spanned_token.value {
                    Token::Identifier(name) => {
                        module_path.push_str(name);
                    }
                    _ => {
                        return Err(LumoraError::ParseError {
                            code: "L031".to_string(),
                            span: Some(Span::new(
                                spanned_token.span.clone(),
                                self.file_name.clone(),
                                &self.source_code,
                            )),
                            message: "Expected identifier in use statement".to_string(),
                            help: None,
                        });
                    }
                },
                None => {
                    return Err(LumoraError::ParseError {
                        code: "L031".to_string(),
                        span: None,
                        message: "Unexpected end of input while parsing use statement".to_string(),
                        help: None,
                    });
                }
            }

            if matches!(self.peek().map(|s| &s.value), Some(Token::Dot)) {
                self.advance();
                module_path.push('.');
            } else {
                break;
            }
        }
        self.expect(&Token::Semicolon)?;
        Ok(module_path)
    }
}
