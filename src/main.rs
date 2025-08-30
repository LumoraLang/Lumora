use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;
use lumora::compile_lumora;
use lumora::errors::LumoraError;
use lumora::lexer::{Token, Spanned};
use lumora::parser::Parser;
use lumora::type_checker::TypeChecker;
use logos::Logos;
fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input.lum> [output_executable_name]", args[0]);
        return Ok(());
    }

    let input_file = &args[1];
    let output_name = if args.len() > 2 {
        &args[2]
    } else {
        Path::new(input_file)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("a.out")
    };

    let source_code = fs::read_to_string(input_file)?;
    let mut lexer = Token::lexer(&source_code).spanned();
    let mut tokens = Vec::new();
    while let Some((token_result, span)) = lexer.next() {
        let token = token_result.map_err(|_| LumoraError::ParseError {
            code: "L030".to_string(),
            span: Some(lumora::errors::Span::new(span.clone(), input_file.clone(), &source_code)),
            message: "Lexing error: Unrecognized token".to_string(),
            help: None,
        })?;
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
            eprintln!(
                "llvm-as failed for {}: {}",
                imported_lum_file,
                String::from_utf8_lossy(&llvm_as_output.stderr)
            );
            return Err("llvm-as compilation failed".into());
        }

        if let Err(e) = fs::remove_file(&imported_ll_file) {
            eprintln!(
                "Warning: Could not delete temporary .ll file {}: {}",
                imported_ll_file, e
            );
        }
        imported_bc_files.push(imported_bc_file);
    }

    let llvm_ir = compile_lumora(&source_code)?;
    let ll_file = format!("{}.ll", output_name);
    fs::write(&ll_file, llvm_ir)?;
    let s_file = format!("{}.s", output_name);
    let llc_output = Command::new("llc")
        .arg(&ll_file)
        .arg("-o")
        .arg(&s_file)
        .output()?;

    if let Err(e) = fs::remove_file(&ll_file) {
        eprintln!(
            "Warning: Could not delete temporary .ll file {}: {}",
            ll_file, e
        );
    }

    if !llc_output.status.success() {
        eprintln!(
            "llc failed: {}",
            String::from_utf8_lossy(&llc_output.stderr)
        );

        if let Err(e) = fs::remove_file(&s_file) {
            eprintln!(
                "Warning: Could not delete temporary .s file {}: {}",
                s_file, e
            );
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
        eprintln!(
            "Warning: Could not delete temporary .s file {}: {}",
            s_file, e
        );
    }

    if !clang_output.status.success() {
        eprintln!(
            "clang failed: {}",
            String::from_utf8_lossy(&clang_output.stderr)
        );
        return Err("clang compilation failed".into());
    }

    for bc_file in &imported_bc_files {
        if let Err(e) = fs::remove_file(bc_file) {
            eprintln!(
                "Warning: Could not delete temporary .bc file {}: {}",
                bc_file, e
            );
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
