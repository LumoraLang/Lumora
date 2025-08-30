use logos::Logos;
extern crate libc;
use inkwell::Either;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use inkwell::types::{BasicTypeEnum, BasicType, AsTypeRef};
use inkwell::IntPredicate;
use std::collections::HashMap;
use llvm_sys::core::LLVMBuildLoad2;
use inkwell::values::AsValueRef;
use std::env;
use std::fs;
use std::process::Command;
use std::path::Path;
#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("return")]
    Return,
    #[token("use")]
    Use,
    #[token("exp")]
    Exp,
    #[token("i32")]
    I32Type,
    #[token("bool")]
    BoolType,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token("=")]
    Assign,
    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Multiply,
    #[token("/")]
    Divide,
    #[token(".")]
    Dot,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_owned())]
    Identifier(String),
    #[regex(r"-?[0-9]+", |lex| lex.slice().parse().ok())]
    Integer(i32),
}

pub struct Spanned<T> {
    pub value: T,
    pub span: logos::Span,
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Span {
    pub file: String,
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
    pub snippet: Option<String>,
}

impl Span {
    pub fn new(logos_span: logos::Span, file_name: String, source_code: &str) -> Self {
        let (start_line, start_column) = get_line_col(source_code, logos_span.start);
        let (end_line, end_column) = get_line_col(source_code, logos_span.end);
        let snippet = source_code.lines().nth(start_line - 1).map(|s| s.to_string());
        Span {
            file: file_name,
            start_line,
            start_column,
            end_line,
            end_column,
            snippet,
        }
    }
}

fn get_line_col(source: &str, offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    for (i, c) in source.chars().enumerate() {
        if i == offset {
            break;
        }
        if c == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

#[derive(Error, Debug)]
pub enum LumoraError {
    #[error("{code}: {message}")]
    ParseError {
        code: String,
        span: Option<Span>,
        message: String,
        help: Option<String>,
    },
    #[error("{code}: {message}")]
    TypeError {
        code: String,
        span: Option<Span>,
        message: String,
        help: Option<String>,
    },
    #[error("{code}: {message}")]
    CodegenError {
        code: String,
        span: Option<Span>,
        message: String,
        help: Option<String>,
    },
}

impl From<inkwell::builder::BuilderError> for LumoraError {
    fn from(err: inkwell::builder::BuilderError) -> Self {
        LumoraError::CodegenError { code: "L001".to_string(), span: None, message: err.to_string(), help: None }
    }
}



impl From<inkwell::support::LLVMString> for LumoraError {
    fn from(err: inkwell::support::LLVMString) -> Self {
        LumoraError::CodegenError { code: "L002".to_string(), span: None, message: err.to_string(), help: None }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LumoraType {
    I32,
    Bool,
    Void,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(i32),
    Boolean(bool),
    Identifier(String),
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Call {
        name: String,
        args: Vec<Expr>,
    },
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
    Add, Sub, Mul, Div,
    Equal, NotEqual, Less, Greater,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let {
        is_exported: bool,
        name: String,
        ty: LumoraType,
        value: Expr,
    },
    If {
        condition: Expr,
        then_block: Vec<Stmt>,
        else_block: Option<Vec<Stmt>>,
    },
    Return(Option<Expr>),
    Expr(Expr),
    Use(String),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub is_exported: bool,
    pub name: String,
    pub params: Vec<(String, LumoraType)>,
    pub return_type: LumoraType,
    pub body: Vec<Stmt>,
}

#[derive(Debug)]
pub struct Program {
    pub uses: Vec<String>,
    pub functions: Vec<Function>,
}

pub struct Parser {
    tokens: Vec<Spanned<Token>>,
    current: usize,
    file_name: String,
    source_code: String,
}

impl Parser {
    pub fn new(tokens: Vec<Spanned<Token>>, file_name: String, source_code: String) -> Self {
        Self { tokens, current: 0, file_name, source_code }
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
            Some(spanned_token) if std::mem::discriminant(&spanned_token.value) == std::mem::discriminant(expected) => Ok(()),
            Some(spanned_token) => Err(LumoraError::ParseError {
                code: "L003".to_string(),
                span: Some(Span::new(spanned_token.span.clone(), file_name_at_start.clone(), &source_code_at_start)),
                message: format!("Expected {:?}, found {:?}", expected, spanned_token.value),
                help: None,
            }),
            None => Err(LumoraError::ParseError {
                code: "L004".to_string(),
                span: current_span_cloned.map(|s| Span::new(s, file_name_at_start, &source_code_at_start)),
                message: "Unexpected end of input".to_string(),
                help: None,
            }),
        }
    }

    pub fn parse(&mut self) -> Result<Program, LumoraError> {
        let mut uses = Vec::new();
        let mut functions = Vec::new();
        
        while self.peek().is_some() {
            let peeked_token = self.peek().unwrap();
            match &peeked_token.value {
                Token::Use => {
                    let module_name = self.parse_use_statement()?;
                    uses.push(module_name);
                },
                Token::Exp | Token::Fn => {
                    functions.push(self.parse_function()?);
                },
                _ => return Err(LumoraError::ParseError { code: "L006".to_string(), span: Some(Span::new(peeked_token.span.clone(), self.file_name.clone(), &self.source_code)), message: "Expected 'use', 'exp', or 'fn'".to_string(), help: None })
            }
        }
        
        Ok(Program { uses, functions })
    }

    fn parse_function(&mut self) -> Result<Function, LumoraError> {
        let is_exported = matches!(self.peek().map(|s| &s.value), Some(Token::Exp));
        if is_exported {
            self.expect(&Token::Exp)?;
        }
        self.expect(&Token::Fn)?;
        let name = match self.advance() {
            Some(spanned_token) => {
                match &spanned_token.value {
                    Token::Identifier(name) => name.clone(),
                    _ => return Err(LumoraError::ParseError { code: "L007".to_string(), span: Some(Span::new(spanned_token.span.clone(), self.file_name.clone(), &self.source_code)), message: "Expected function name (identifier)".to_string(), help: None }),
                }
            },
            None => return Err(LumoraError::ParseError { code: "L007".to_string(), span: None, message: "Unexpected end of input while expecting function name".to_string(), help: None }),
        };

        self.expect(&Token::LeftParen)?;
        let mut params = Vec::new();
        
        while !matches!(self.peek().map(|s| &s.value), Some(Token::RightParen)) {
            let param_name = match self.advance() {
                Some(spanned_token) => {
                    match &spanned_token.value {
                        Token::Identifier(name) => name.clone(),
                        _ => return Err(LumoraError::ParseError { code: "L008".to_string(), span: Some(Span::new(spanned_token.span.clone(), self.file_name.clone(), &self.source_code)), message: "Expected parameter name (identifier)".to_string(), help: None }),
                    }
                },
                None => return Err(LumoraError::ParseError { code: "L008".to_string(), span: None, message: "Unexpected end of input while expecting parameter name".to_string(), help: None }),
            };
            
            let param_type = self.parse_type()?;
            params.push((param_name, param_type));
            
            if matches!(self.peek().map(|s| &s.value), Some(Token::Comma)) {
                self.advance();
            }
        }
        
        self.expect(&Token::RightParen)?;
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

    fn parse_type(&mut self) -> Result<LumoraType, LumoraError> {
        match self.advance() {
            Some(spanned_token) => {
                match &spanned_token.value {
                    Token::I32Type => Ok(LumoraType::I32),
                    Token::BoolType => Ok(LumoraType::Bool),
                    _ => Err(LumoraError::ParseError { code: "L009".to_string(), span: Some(Span::new(spanned_token.span.clone(), self.file_name.clone(), &self.source_code)), message: "Expected a type (e.g., i32, bool)".to_string(), help: None }),
                }
            },
            None => Err(LumoraError::ParseError { code: "L009".to_string(), span: None, message: "Unexpected end of input while expecting a type".to_string(), help: None }),
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
                    Some(spanned_token) => {
                        match &spanned_token.value {
                            Token::Identifier(name) => name.clone(),
                            _ => return Err(LumoraError::ParseError { code: "L010".to_string(), span: Some(Span::new(spanned_token.span.clone(), self.file_name.clone(), &self.source_code)), message: "Expected variable name (identifier)".to_string(), help: None }),
                        }
                    },
                    None => return Err(LumoraError::ParseError { code: "L010".to_string(), span: None, message: "Unexpected end of input while expecting variable name".to_string(), help: None }),
                };
                
                let ty = self.parse_type()?;
                self.expect(&Token::Assign)?;
                let value = self.parse_expression()?;
                self.expect(&Token::Semicolon)?;
                
                Ok(Stmt::Let { is_exported, name, ty, value })
            }
            Some(Token::If) => {
                if is_exported {
                    return Err(LumoraError::ParseError { code: "L011".to_string(), span: current_span.map(|s| Span::new(s.clone(), self.file_name.clone(), &self.source_code)), message: "Cannot export 'if' statements".to_string(), help: None })
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
                    self.expect(&Token::LeftBrace)?;
                    let mut else_stmts = Vec::new();
                    while !matches!(self.peek().map(|s| &s.value), Some(Token::RightBrace)) {
                        else_stmts.push(self.parse_statement()?);
                    }
                    self.expect(&Token::RightBrace)?;
                    Some(else_stmts)
                } else {
                    None
                };
                
                Ok(Stmt::If { condition, then_block, else_block })
            }
            Some(Token::Return) => {
                if is_exported {
                    return Err(LumoraError::ParseError { code: "L012".to_string(), span: current_span.map(|s| Span::new(s.clone(), self.file_name.clone(), &self.source_code)), message: "Cannot export 'return' statements".to_string(), help: None })
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
            _ => {
                if is_exported {
                    return Err(LumoraError::ParseError { code: "L015".to_string(), span: current_span.map(|s| Span::new(s.clone(), self.file_name.clone(), &self.source_code)), message: "Cannot export expressions".to_string(), help: None })
                }
                let expr = self.parse_expression()?;
                self.expect(&Token::Semicolon)?;
                Ok(Stmt::Expr(expr))
            }
        }
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
            Some(spanned_token) => {
                match &spanned_token.value {
                    Token::Integer(n) => Ok(Expr::Integer(*n)),
                    Token::True => Ok(Expr::Boolean(true)),
                    Token::False => Ok(Expr::Boolean(false)),
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
                    _ => Err(LumoraError::ParseError { code: "L016".to_string(), span: Some(Span::new(spanned_token.span.clone(), self.file_name.clone(), &self.source_code)), message: "Expected an expression".to_string(), help: None }),
                }
            },
            None => Err(LumoraError::ParseError { code: "L016".to_string(), span: None, message: "Unexpected end of input while expecting an expression".to_string(), help: None }),
        }
    }

    fn parse_use_statement(&mut self) -> Result<String, LumoraError> {
        self.expect(&Token::Use)?;
        let mut module_path = String::new();
        loop {
            let spanned_token = self.advance();
            match spanned_token {
                Some(spanned_token) => {
                    match &spanned_token.value {
                        Token::Identifier(name) => {
                            module_path.push_str(name);
                        },
                        _ => return Err(LumoraError::ParseError { code: "L031".to_string(), span: Some(Span::new(spanned_token.span.clone(), self.file_name.clone(), &self.source_code)), message: "Expected identifier in use statement".to_string(), help: None }),
                    }
                },
                None => return Err(LumoraError::ParseError { code: "L031".to_string(), span: None, message: "Unexpected end of input while parsing use statement".to_string(), help: None }),
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

pub struct TypeChecker {
    variables: HashMap<String, LumoraType>,
    functions: HashMap<String, (Vec<LumoraType>, LumoraType)>,
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
            let imported_source = std::fs::read_to_string(&module_path)
                .map_err(|e| LumoraError::ParseError { code: "L017".to_string(), span: None, message: format!("Could not read imported module {}: {}", module_path, e), help: None })?;
            let mut lexer = Token::lexer(&imported_source).spanned();
            let mut tokens = Vec::new();
            while let Some((token_result, span)) = lexer.next() {
                let token = token_result.map_err(|_| LumoraError::ParseError { code: "L018".to_string(), span: Some(Span::new(span.clone(), module_path.clone(), &imported_source)), message: "Lexing error in imported module: Unrecognized token".to_string(), help: None })?;
                tokens.push(Spanned { value: token, span });
            }

            let mut parser = Parser::new(tokens, module_path.clone(), imported_source.clone());
            let imported_program = parser.parse()?;
            let mut imported_type_checker = TypeChecker::new();
            imported_type_checker.check_program(&imported_program)?;
            for function in &imported_program.functions {
                if function.is_exported {
                    let param_types = function.params.iter().map(|(_, ty)| ty.clone()).collect();
                    self.functions.insert(function.name.clone(), (param_types, function.return_type.clone()));
                }
            }
            
        }

        
        for function in &program.functions {
            let param_types = function.params.iter().map(|(_, ty)| ty.clone()).collect();
            self.functions.insert(function.name.clone(), (param_types, function.return_type.clone()));
        }

        for function in &program.functions {
            self.check_function(function)?;
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
            Stmt::Let { is_exported: _, name, ty, value } => {
                let value_type = self.check_expression(value)?;
                if *ty != value_type {
                    return Err(LumoraError::TypeError { code: "L019".to_string(), span: None, message: format!(
                        "Type mismatch: expected {:?}, found {:?}", ty, value_type
                    ), help: None });
                }
                self.variables.insert(name.clone(), ty.clone());
                Ok(())
            }
            Stmt::If { condition, then_block, else_block } => {
                let cond_type = self.check_expression(condition)?;
                if cond_type != LumoraType::Bool {
                    return Err(LumoraError::TypeError { code: "L020".to_string(), span: None, message: "If condition must be boolean".to_string(), help: None });
                }
                
                for stmt in then_block {
                    self.check_statement(stmt)?;
                }
                
                if let Some(else_stmts) = else_block {
                    for stmt in else_stmts {
                        self.check_statement(stmt)?;
                    }
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
            Stmt::Use(_) => {
                Ok(())
            }
        }
    }

    fn check_expression(&self, expr: &Expr) -> Result<LumoraType, LumoraError> {
        match expr {
            Expr::Integer(_) => Ok(LumoraType::I32),
            Expr::Boolean(_) => Ok(LumoraType::Bool),
            Expr::Identifier(name) => {
                self.variables.get(name)
                    .cloned()
                    .ok_or_else(|| LumoraError::TypeError { code: "L021".to_string(), span: None, message: format!("Undefined variable: {}", name), help: None })
            }
            Expr::Binary { left, op, right } => {
                let left_type = self.check_expression(left)?;
                let right_type = self.check_expression(right)?;
                
                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                        if left_type == LumoraType::I32 && right_type == LumoraType::I32 {
                            Ok(LumoraType::I32)
                        } else {
                            Err(LumoraError::TypeError { code: "L022".to_string(), span: None, message: "Arithmetic operations require i32 operands".to_string(), help: None })
                        }
                    }
                    BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::Less | BinaryOp::Greater => {
                        if left_type == right_type {
                            Ok(LumoraType::Bool)
                        } else {
                            Err(LumoraError::TypeError { code: "L023".to_string(), span: None, message: "Comparison requires same types".to_string(), help: None })
                        }
                    }
                }
            }
            Expr::Call { name, args} => {
                if let Some((param_types, return_type)) = self.functions.get(name) {
                    if args.len() != param_types.len() {
                        return Err(LumoraError::TypeError { code: "L024".to_string(), span: None, message: format!(
                            "Function {} expects {} arguments, got {}", 
                            name, param_types.len(), args.len()
                        ), help: None });
                    }
                    
                    for (arg, expected_type) in args.iter().zip(param_types.iter()) {
                        let arg_type = self.check_expression(arg)?;
                        if arg_type != *expected_type {
                            return Err(LumoraError::TypeError { code: "L025".to_string(), span: None, message: format!(
                                "Argument type mismatch: expected {:?}, found {:?}",
                                expected_type, arg_type
                            ), help: None });
                        }
                    }
                    
                    Ok(return_type.clone())
                } else {
                    Err(LumoraError::TypeError { code: "L026".to_string(), span: None, message: format!("Undefined function: {}", name), help: None })
                }
            }
        }
    }
}

pub struct CodeGenerator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, (PointerValue<'ctx>, LumoraType)>,
    functions: HashMap<String, FunctionValue<'ctx>>,
    all_functions: HashMap<String, (Vec<LumoraType>, LumoraType)>,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str, all_functions: HashMap<String, (Vec<LumoraType>, LumoraType)>) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            variables: HashMap::new(),
            functions: HashMap::new(),
            all_functions,
        }
    }

    pub fn generate(&mut self, program: &Program) -> Result<String, LumoraError> {
        for (name, (param_types, return_type)) in &self.all_functions {
            let fn_type = {
                let param_llvm_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> = param_types
                    .iter()
                    .map(|ty| self.type_to_llvm_type(ty).into())
                    .collect();
                match return_type {
                    LumoraType::Void => self.context.void_type().fn_type(&param_llvm_types, false),
                    _ => self.type_to_llvm_type(return_type).fn_type(&param_llvm_types, false),
                }
            };
            let fn_val = self.module.add_function(name, fn_type, None);
            self.functions.insert(name.clone(), fn_val);
        }

        for function in &program.functions {
            self.generate_function(function)?;
        }

        Ok(self.module.to_string())
    }

    

    fn generate_function(&mut self, function: &Function) -> Result<(), LumoraError> {
        let fn_val = *self.functions.get(&function.name).unwrap();
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        self.variables.clear();
        for (i, (name, param_lumora_type)) in function.params.iter().enumerate() {
            let param = fn_val.get_nth_param(i as u32).unwrap();
            let alloca = self.builder.build_alloca(param.get_type(), name)?;
            self.builder.build_store(alloca, param)?;
            self.variables.insert(name.clone(), (alloca, param_lumora_type.clone()));
        }

        for stmt in &function.body {
            self.generate_statement(stmt)?;
        }

        if function.return_type == LumoraType::Void {
            let _ = self.builder.build_return(None);
        }

        Ok(())
    }

    fn generate_statement(&mut self, stmt: &Stmt) -> Result<(), LumoraError> {
        match stmt {
            Stmt::Let { is_exported: _, name, ty, value } => {
                let alloca = self.builder.build_alloca(self.type_to_llvm_type(ty), name)?;
                let val = self.generate_expression(value)?;
                self.builder.build_store(alloca, val)?;
                self.variables.insert(name.clone(), (alloca, ty.clone()));
                Ok(())
            }
            Stmt::If { condition, then_block, else_block } => {
                let cond_val = self.generate_expression(condition)?;
                let fn_val = self.builder.get_insert_block().unwrap().get_parent().unwrap();
                let then_bb = self.context.append_basic_block(fn_val, "then");
                let else_bb = self.context.append_basic_block(fn_val, "else");
                let then_ends_with_return = self.block_ends_with_return(then_block);
                let else_ends_with_return = if let Some(else_stmts) = else_block {
                    self.block_ends_with_return(else_stmts)
                } else {
                    false
                };

                let create_cont_bb = !then_ends_with_return || !else_ends_with_return;
                let cont_bb = if create_cont_bb {
                    Some(self.context.append_basic_block(fn_val, "cont"))
                } else {
                    None
                };

                let _ = self.builder.build_conditional_branch(cond_val.into_int_value(), then_bb, else_bb);
                self.builder.position_at_end(then_bb);
                for stmt in then_block {
                    self.generate_statement(stmt)?;
                }
                if !then_ends_with_return {
                    if let Some(cont) = cont_bb {
                        let _ = self.builder.build_unconditional_branch(cont);
                    }
                }

                self.builder.position_at_end(else_bb);
                if let Some(else_stmts) = else_block {
                    for stmt in else_stmts {
                        self.generate_statement(stmt)?;
                    }
                }
                if !else_ends_with_return {
                    if let Some(cont) = cont_bb {
                        let _ = self.builder.build_unconditional_branch(cont);
                    }
                }

                if let Some(cont) = cont_bb {
                    self.builder.position_at_end(cont);
                }
                Ok(())
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    let val = self.generate_expression(expr)?;
                    let _ = self.builder.build_return(Some(&val));
                } else {
                    let _ = self.builder.build_return(None);
                }
                Ok(())
            }
            Stmt::Expr(expr) => {
                self.generate_expression(expr)?;
                Ok(())
            }
            Stmt::Use(_) => {
                Ok(())
            }
            
        }
    }

    fn generate_expression(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, LumoraError> {
        match expr {
            Expr::Integer(n) => Ok(self.context.i32_type().const_int(*n as u64, true).into()),
            Expr::Boolean(b) => Ok(self.context.bool_type().const_int(*b as u64, false).into()),
            Expr::Identifier(name) => {
                let (ptr, lumora_type) = self.variables.get(name).unwrap();
                let loaded_value_ref = unsafe {
                    LLVMBuildLoad2(self.builder.as_mut_ptr(), self.type_to_llvm_type(&lumora_type).as_type_ref(), ptr.as_value_ref(), name.as_ptr() as *const ::libc::c_char)
                };
                match lumora_type {
                    LumoraType::I32 => Ok(unsafe { inkwell::values::IntValue::new(loaded_value_ref).into() }),
                    LumoraType::Bool => Ok(unsafe { inkwell::values::IntValue::new(loaded_value_ref).into() }),
                    LumoraType::Void => Err(LumoraError::CodegenError { code: "L027".to_string(), span: None, message: "Cannot load void type".to_string(), help: None }),
                }
            }
            Expr::Binary { left, op, right } => {
                let left_val = self.generate_expression(left)?;
                let right_val = self.generate_expression(right)?;
                match op {
                    BinaryOp::Add => Ok(self.builder.build_int_add(left_val.into_int_value(), right_val.into_int_value(), "add")?.into()),
                    BinaryOp::Sub => Ok(self.builder.build_int_sub(left_val.into_int_value(), right_val.into_int_value(), "sub")?.into()),
                    BinaryOp::Mul => Ok(self.builder.build_int_mul(left_val.into_int_value(), right_val.into_int_value(), "mul")?.into()),
                    BinaryOp::Div => Ok(self.builder.build_int_signed_div(left_val.into_int_value(), right_val.into_int_value(), "div")?.into()),
                    BinaryOp::Equal => Ok(self.builder.build_int_compare(IntPredicate::EQ, left_val.into_int_value(), right_val.into_int_value(), "eq")?.into()),
                    BinaryOp::NotEqual => Ok(self.builder.build_int_compare(IntPredicate::NE, left_val.into_int_value(), right_val.into_int_value(), "ne")?.into()),
                    BinaryOp::Less => Ok(self.builder.build_int_compare(IntPredicate::SLT, left_val.into_int_value(), right_val.into_int_value(), "lt")?.into()),
                    BinaryOp::Greater => Ok(self.builder.build_int_compare(IntPredicate::SGT, left_val.into_int_value(), right_val.into_int_value(), "gt")?.into()),
                }
            }
            Expr::Call { name, args} => {
                let fn_val = *self.functions.get(name).unwrap();
                let arg_values: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> = args
                    .iter()
                    .map(|arg| self.generate_expression(arg).map(|val| val.into()))
                    .collect::<Result<_, LumoraError>>()?;

                let call_site_value = self.builder.build_call(fn_val, arg_values.as_slice(), "call");
                match call_site_value?.try_as_basic_value() {
                    Either::Left(basic_value) => Ok(basic_value),
                    Either::Right(_) => Err(LumoraError::CodegenError { code: "L028".to_string(), span: None, message: "Function call returned void".to_string(), help: None }),
                }
            }
        }
    }

    fn type_to_llvm_type(&self, ty: &LumoraType) -> BasicTypeEnum<'ctx> {
        match ty {
            LumoraType::I32 => self.context.i32_type().into(),
            LumoraType::Bool => self.context.bool_type().into(),
            LumoraType::Void => panic!("Void is not a basic type"),
        }
    }

    fn block_ends_with_return(&self, block: &[Stmt]) -> bool {
        if let Some(last_stmt) = block.last() {
            matches!(last_stmt, Stmt::Return(_))
        } else {
            false
        }
    }
}

pub fn compile_lumora(source: &str) -> Result<String, LumoraError> {
    let mut lexer = Token::lexer(source).spanned();
    let mut tokens = Vec::new();
    while let Some((token_result, span)) = lexer.next() {
        let token = token_result.map_err(|_| LumoraError::ParseError { code: "L029".to_string(), span: Some(Span::new(span.clone(), "main_program".to_string(), source)), message: "Lexing error: Unrecognized token".to_string(), help: None })?;
        tokens.push(Spanned { value: token, span });
    }

    let mut parser = Parser::new(tokens, "main_program".to_string(), source.to_string());
    let program = parser.parse()?;
    let mut type_checker = TypeChecker::new();
    type_checker.check_program(&program)?;
    let context = Context::create();
    let mut codegen = CodeGenerator::new(&context, "lumora_program", type_checker.functions);
    let llvm_ir = codegen.generate(&program)?;

    Ok(llvm_ir)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input.lum> [output_executable_name]", args[0]);
        return Ok(())
    }

    let input_file = &args[1];
    let output_name = if args.len() > 2 {
        &args[2]
    } else {
        Path::new(input_file).file_stem().and_then(|s| s.to_str()).unwrap_or("a.out")
    };

    let source_code = fs::read_to_string(input_file)?;
    let mut lexer = Token::lexer(&source_code).spanned();
    let mut tokens = Vec::new();
    while let Some((token_result, span)) = lexer.next() {
        let token = token_result.map_err(|_| LumoraError::ParseError { code: "L030".to_string(), span: Some(Span::new(span.clone(), input_file.clone(), &source_code)), message: "Lexing error: Unrecognized token".to_string(), help: None })?;
        tokens.push(Spanned { value: token, span });
    }

    let mut parser = Parser::new(tokens, input_file.clone(), source_code.clone());
    let program = parser.parse()?;
    let mut type_checker = TypeChecker::new();
    type_checker.check_program(&program)?;
    let mut imported_bc_files = Vec::new();
    for module_name in &program.uses {
        let imported_lum_file = module_name.clone();
        let imported_output_name = format!("imported_{}", module_name);
        let imported_source_code = fs::read_to_string(&imported_lum_file)?;
        let imported_llvm_ir = compile_lumora(&imported_source_code)?;
        let imported_ll_file = format!("{}.ll", imported_output_name);
        fs::write(&imported_ll_file, imported_llvm_ir)?;
        let imported_bc_file = format!("{}.bc", imported_output_name);
        let llvm_as_output = Command::new("llvm-as")
            .arg(&imported_ll_file)
            .arg("-o")
            .arg(&imported_bc_file)
            .output()?;

        if !llvm_as_output.status.success() {
            eprintln!("llvm-as failed for {}: {}", imported_lum_file, String::from_utf8_lossy(&llvm_as_output.stderr));
            return Err("llvm-as compilation failed".into());
        }

        if let Err(e) = fs::remove_file(&imported_ll_file) {
            eprintln!("Warning: Could not delete temporary .ll file {}: {}", imported_ll_file, e);
        }
        imported_bc_files.push(imported_bc_file);
    }

    let context = Context::create();
    let mut codegen = CodeGenerator::new(&context, "lumora_program", type_checker.functions);
    let llvm_ir = codegen.generate(&program)?;
    let ll_file = format!("{}.ll", output_name);
    fs::write(&ll_file, llvm_ir)?;
    let s_file = format!("{}.s", output_name);
    let llc_output = Command::new("llc")
        .arg(&ll_file)
        .arg("-o")
        .arg(&s_file)
        .output()?;
    
    if let Err(e) = fs::remove_file(&ll_file) {
        eprintln!("Warning: Could not delete temporary .ll file {}: {}", ll_file, e);
    }

    if !llc_output.status.success() {
        eprintln!("llc failed: {}", String::from_utf8_lossy(&llc_output.stderr));
        
        if let Err(e) = fs::remove_file(&s_file) {
            eprintln!("Warning: Could not delete temporary .s file {}: {}", s_file, e);
        }
        return Err("llc compilation failed".into());
    }

    let clang_output = Command::new("clang")
        .arg(&s_file)
        .args(&imported_bc_files)
        .arg("-o")
        .arg(output_name)
        .output()?;

    
    if let Err(e) = fs::remove_file(&s_file) {
        eprintln!("Warning: Could not delete temporary .s file {}: {}", s_file, e);
    }

    if !clang_output.status.success() {
        eprintln!("clang failed: {}", String::from_utf8_lossy(&clang_output.stderr));
        return Err("clang compilation failed".into());
    }

    for bc_file in &imported_bc_files {
        if let Err(e) = fs::remove_file(bc_file) {
            eprintln!("Warning: Could not delete temporary .bc file {}: {}", bc_file, e);
        }
    }

    println!("Compilation successful: {} -> {}", input_file, output_name);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_function() {
        let source = r###" 
            fn main() i32 {
                let x i32 = 42;
                return x;
            }
        "###;
        
        let result = compile_lumora(source);
        assert!(result.is_ok());
    }

    #[test]
    fn test_arithmetic() {
        let source = r###" 
            fn add(a i32, b i32) i32 {
                return a + b;
            }
        "###;
        
        let result = compile_lumora(source);
        assert!(result.is_ok());
    }
}
