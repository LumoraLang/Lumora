use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum LumoraType {
    I32,
    I64,
    F64,
    Bool,
    Void,
    String,
    Array(Box<LumoraType>),
    Null,
}

#[derive(Debug, Clone)]
pub struct ExternalFunction {
    pub name: String,
    pub params: Vec<LumoraType>,
    pub return_type: LumoraType,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    StringLiteral(String),
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
    ArrayLiteral(Vec<Expr>),
    ArrayIndex {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    Null,
    ArgCount,
    GetArg(Box<Expr>),
    StringOf(Box<Expr>),
    I32Of(Box<Expr>),
    I64Of(Box<Expr>),
    BoolOf(Box<Expr>),
    F32Of(Box<Expr>),
    F64Of(Box<Expr>),
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
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },
    For {
        initializer: Box<Stmt>,
        condition: Expr,
        increment: Box<Expr>,
        body: Vec<Stmt>,
    },
}

#[derive(Debug, Clone)]
pub struct Function {
    pub is_exported: bool,
    pub name: String,
    pub params: Vec<(String, LumoraType)>,
    pub return_type: LumoraType,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum TopLevelDeclaration {
    Function(Function),
    ExternalFunction(ExternalFunction),
}

#[derive(Debug)]
pub struct Program {
    pub uses: Vec<String>,
    pub declarations: Vec<TopLevelDeclaration>,
}
