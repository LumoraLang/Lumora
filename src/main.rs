use logos::Logos;
use lumora::compile_lumora;
use lumora::config::load_config;
use lumora::errors::LumoraError;
use lumora::lexer::{Spanned, Token};
use lumora::parser::Parser;
use lumora::type_checker::TypeChecker;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input.lum> [output_executable_name]", args[0]);
        return Ok(());
    }

    let config = load_config("lumora.yaml").unwrap_or_else(|e| {
        eprintln!(
            "Warning: Could not load lumora.yaml: {}. Using default configuration.",
            e
        );
        Default::default()
    });

    let output_dir = &config.build_settings.output_dir;
    fs::create_dir_all(output_dir)?;

    let input_file = &args[1];
    let output_name = if args.len() > 2 {
        PathBuf::from(&args[2])
    } else {
        Path::new(input_file)
            .file_stem()
            .and_then(|s| s.to_str())
            .map(|s| output_dir.join(s))
            .unwrap_or_else(|| output_dir.join("a.out"))
    };

    let source_code = fs::read_to_string(input_file)?;
    let mut lexer = Token::lexer(&source_code).spanned();
    let mut tokens = Vec::new();
    while let Some((token_result, span)) = lexer.next() {
        let token = token_result.map_err(|_| LumoraError::ParseError {
            code: "L030".to_string(),
            span: Some(lumora::errors::Span::new(
                span.clone(),
                input_file.clone(),
                &source_code,
            )),
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
        let imported_lum_file = PathBuf::from(module_name);
        let imported_output_name = format!("imported_{}", module_name);
        let imported_source_code = fs::read_to_string(&imported_lum_file)?;
        let imported_llvm_ir = compile_lumora(&imported_source_code)?;
        let imported_ll_file = output_dir.join(format!("{}.ll", imported_output_name));
        fs::write(&imported_ll_file, imported_llvm_ir)?;
        let imported_bc_file = output_dir.join(format!("{}.bc", imported_output_name));
        let llvm_as_output = Command::new("llvm-as")
            .arg(&imported_ll_file)
            .arg("-o")
            .arg(&imported_bc_file)
            .output()?;

        if !llvm_as_output.status.success() {
            eprintln!(
                "llvm-as failed for {}: {}",
                imported_lum_file.display(),
                String::from_utf8_lossy(&llvm_as_output.stderr)
            );
            return Err("llvm-as compilation failed".into());
        }

        if let Err(e) = fs::remove_file(&imported_ll_file) {
            eprintln!(
                "Warning: Could not delete temporary .ll file {}: {}",
                imported_ll_file.display(),
                e
            );
        }
        imported_bc_files.push(imported_bc_file);
    }

    let llvm_ir = compile_lumora(&source_code)?;
    let ll_file = output_dir.join(format!(
        "{}.ll",
        output_name.file_name().unwrap().to_str().unwrap()
    ));
    fs::write(&ll_file, llvm_ir)?;

    let s_file = output_dir.join(format!(
        "{}.s",
        output_name.file_name().unwrap().to_str().unwrap()
    ));
    let mut llc_command = Command::new("llc");
    llc_command
        .arg(&ll_file)
        .arg("-o")
        .arg(&s_file)
        .arg("-relocation-model=pic");

    if !config.build_settings.optimization_level.is_empty() {
        llc_command.arg(format!("-{}", config.build_settings.optimization_level));
    }
    if config.build_settings.debug_info {
        llc_command.arg("--debugify-level=locations");
    }
    if !config.build_settings.target_triple.is_empty() {
        llc_command.arg(format!("-mtriple={}", config.build_settings.target_triple));
    }

    let llc_output = llc_command.output()?;

    if let Err(e) = fs::remove_file(&ll_file) {
        eprintln!(
            "Warning: Could not delete temporary .ll file {}: {}",
            ll_file.display(),
            e
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
                s_file.display(),
                e
            );
        }
        return Err("llc compilation failed".into());
    }

    let mut external_object_files = Vec::new();
    for dep in &config.external_dependencies {
        if let Some(ext) = dep.extension() {
            if ext == "c" || ext == "cpp" {
                let obj_file = output_dir.join(dep.with_extension("o").file_name().unwrap());
                let mut clang_compile_command = Command::new("clang");
                clang_compile_command
                    .arg("-c")
                    .arg(dep)
                    .arg("-o")
                    .arg(&obj_file);

                if !config.build_settings.optimization_level.is_empty() {
                    clang_compile_command
                        .arg(format!("-{}", config.build_settings.optimization_level));
                }
                if config.build_settings.debug_info {
                    clang_compile_command.arg("-g");
                }
                if !config.build_settings.target_triple.is_empty() {
                    clang_compile_command
                        .arg(format!("-target={}", config.build_settings.target_triple));
                }
                clang_compile_command.arg("-fPIE");
                let compile_output = clang_compile_command.output()?;
                if !compile_output.status.success() {
                    eprintln!(
                        "clang compilation of {} failed: {}",
                        dep.display(),
                        String::from_utf8_lossy(&compile_output.stderr)
                    );
                    return Err("External C/C++ compilation failed".into());
                }
                external_object_files.push(obj_file);
            } else if ext == "o" || ext == "a" {
                external_object_files.push(dep.clone());
            } else {
                eprintln!(
                    "Warning: Unsupported external dependency type: {}",
                    dep.display()
                );
            }
        } else {
            eprintln!(
                "Warning: External dependency has no extension: {}",
                dep.display()
            );
        }
    }

    let mut clang_command = Command::new("clang");
    clang_command
        .arg(&s_file)
        .args(&imported_bc_files)
        .args(&external_object_files)
        .arg("-o")
        .arg(&output_name);

    for lib in &config.linker_settings.libraries {
        clang_command.arg(format!("-l{}", lib));
    }
    clang_command.args(&config.linker_settings.flags);

    if !config.build_settings.optimization_level.is_empty() {
        clang_command.arg(format!("-{}", config.build_settings.optimization_level));
    }
    if config.build_settings.debug_info {
        clang_command.arg("-g");
    }
    if !config.build_settings.target_triple.is_empty() {
        clang_command.arg(format!("-target={}", config.build_settings.target_triple));
    }

    let clang_output = clang_command.output()?;

    if let Err(e) = fs::remove_file(&s_file) {
        eprintln!(
            "Warning: Could not delete temporary .s file {}: {}",
            s_file.display(),
            e
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
                bc_file.display(),
                e
            );
        }
    }

    println!(
        "Compilation successful: {} -> {}",
        input_file,
        output_name.display()
    );
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
