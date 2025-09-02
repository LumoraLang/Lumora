pub mod ast;
pub mod codegen;
pub mod config;
pub mod errors;
pub mod lexer;
pub mod parser;
pub mod pm;
pub mod type_checker;
use crate::codegen::CodeGenerator;
use crate::errors::{LumoraError, Span};
use crate::lexer::{Spanned, Token};
use crate::parser::Parser;
use crate::type_checker::TypeChecker;
use dirs;
use inkwell::context::Context;
use logos::Logos;
use std::env;
use std::path::{Path, PathBuf};
pub fn compile_lumora(source: &str, args: &[String]) -> Result<String, LumoraError> {
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
    let mut codegen = CodeGenerator::new(
        &context,
        "lumora_program",
        type_checker.functions,
        type_checker.struct_definitions,
        args.to_vec(),
    );
    let llvm_ir = codegen.generate(&program)?;

    Ok(llvm_ir)
}

pub fn resolve_module_path(module_name: &str, current_file: &Path) -> Result<PathBuf, LumoraError> {
    let mut search_paths = Vec::new();
    if let Ok(current_dir) = std::env::current_dir() {
        search_paths.push(current_dir);
    }
    if let Some(parent) = current_file.parent() {
        search_paths.push(parent.to_path_buf());
    }
    if let Some(parent) = current_file.parent() {
        search_paths.push(parent.join(".lumora"))
    }
    if let Some(home_dir) = dirs::home_dir() {
        search_paths.push(home_dir.join(".lumora"));
    }
    let project_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    search_paths.push(project_root.clone());
    search_paths.push(project_root.join(".lumora"));
    search_paths.push(PathBuf::from("/usr/share/lumora/"));
    for base_path in search_paths {
        let candidate_path = base_path.join(module_name);
        if candidate_path.exists() {
            return Ok(candidate_path);
        }

        let mut candidate_with_ext = base_path.join(module_name);
        if candidate_with_ext.extension().is_none() {
            candidate_with_ext.set_extension("lum");
            if candidate_with_ext.exists() {
                return Ok(candidate_with_ext);
            }
        }
    }

    Err(LumoraError::ConfigurationError {
        message: format!("Module '{}' not found.", module_name),
        help: Some(
            "Ensure the module exists in the current working directory, the directory of the current file, ~/.lumora/, the project root, the project's .lumora directory, or /usr/share/lumora/."
                .to_string(),
        ),
    })
}
