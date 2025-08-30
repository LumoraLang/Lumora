pub mod lexer;
pub mod errors;
pub mod ast;
pub mod parser;
pub mod type_checker;
pub mod codegen;
use logos::Logos;
use inkwell::context::Context;
use crate::lexer::{Token, Spanned};
use crate::errors::{LumoraError, Span};
use crate::parser::Parser;
use crate::type_checker::TypeChecker;
use crate::codegen::CodeGenerator;
pub fn compile_lumora(source: &str) -> Result<String, LumoraError> {
    let mut lexer = Token::lexer(source).spanned();
    let mut tokens = Vec::new();
    while let Some((token_result, span)) = lexer.next() {
        let token = token_result.map_err(|_| LumoraError::ParseError {
            code: "L029".to_string(),
            span: Some(Span::new(span.clone(), "main_program".to_string(), source)),
            message: "Lexing error: Unrecognized token".to_string(),
            help: None,
        })?;
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
