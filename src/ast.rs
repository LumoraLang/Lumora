use serde::{Deserialize, Serialize};

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
    Add,
    Sub,
    Mul,
    Div,
    Equal,
    NotEqual,
    Less,
    Greater,
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
