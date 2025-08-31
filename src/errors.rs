use serde::{Deserialize, Serialize};
use thiserror::Error;

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
        let (start_line, start_column) = crate::lexer::get_line_col(source_code, logos_span.start);
        let (end_line, end_column) = crate::lexer::get_line_col(source_code, logos_span.end);
        let snippet = source_code
            .lines()
            .nth(start_line - 1)
            .map(|s| s.to_string());
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
    #[error("ConfigurationError: {message}")]
    ConfigurationError {
        message: String,
        help: Option<String>,
    },
}

impl From<inkwell::builder::BuilderError> for LumoraError {
    fn from(err: inkwell::builder::BuilderError) -> Self {
        LumoraError::CodegenError {
            code: "L001".to_string(),
            span: None,
            message: err.to_string(),
            help: None,
        }
    }
}

impl From<inkwell::support::LLVMString> for LumoraError {
    fn from(err: inkwell::support::LLVMString) -> Self {
        LumoraError::CodegenError {
            code: "L002".to_string(),
            span: None,
            message: err.to_string(),
            help: None,
        }
    }
}
