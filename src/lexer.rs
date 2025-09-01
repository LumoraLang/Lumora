use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    #[regex(r"[ \t\n\f]+", logos::skip)]
    #[regex(r"//[^\n]*", logos::skip)]
    #[regex(r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", logos::skip)]
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
    #[token("ext")]
    Ext,
    #[token("stringof")]
    StringOf,
    #[token("i32of")]
    I32Of,
    #[token("i64of")]
    I64Of,
    #[token("boolof")]
    BoolOf,
    #[token("f32of")]
    F32Of,
    #[token("f64of")]
    F64Of,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("void")]
    VoidType,
    #[token("i32")]
    I32Type,
    #[token("i64")]
    I64Type,
    #[token("f32")]
    F32Type,
    #[token("f64")]
    F64Type,
    #[token("bool")]
    BoolType,
    #[token("string")]
    StringType,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("null")]
    Null,
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
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
    #[token(":")]
    Colon,
    #[regex(r"-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", |lex| lex.slice().parse().ok())]
    Float(f32),
    #[regex(r"0x[0-9a-fA-F]+|-?[0-9]+", |lex| {
    let slice = lex.slice();
    if slice.starts_with("0x") {
        i64::from_str_radix(slice.trim_start_matches("0x"), 16).ok()
    } else {
        slice.parse::<i64>().ok()
    }
    })]
    Integer(i64),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_owned())]
    Identifier(String),
    #[regex(r#""([^"\\]|\\.)*"|'([^'\\]|\\.)*'"#, |lex| parse_string_literal(lex))]
    StringLiteral(String),
    Error,
}

fn parse_string_literal(lex: &mut Lexer<Token>) -> String {
    let s = lex.slice();
    if s.len() < 2 {
        return String::new();
    }
    let first = s.chars().next().unwrap();
    let last = s.chars().last().unwrap();
    if (first == '"' && last != '"') || (first == '\'' && last != '\'') {
        return String::new();
    }

    let content = &s[1..s.len() - 1];
    let mut result = String::with_capacity(content.len());
    let mut chars = content.chars().peekable();
    while let Some(c) = chars.next() {
        if c == '\\' {
            if let Some(next) = chars.next() {
                match next {
                    'n' => result.push('\n'),
                    '\\' => result.push('\\'),
                    'e' => result.push('\x1B'),
                    'b' => result.push('\x08'),
                    'r' => result.push('\r'),
                    other => result.push(other),
                }
            }
        } else {
            result.push(c);
        }
    }

    result
}

pub struct Spanned<T> {
    pub value: T,
    pub span: logos::Span,
}

pub fn get_line_col(source: &str, offset: usize) -> (usize, usize) {
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
