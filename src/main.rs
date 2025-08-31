use clap::Parser;
use logos::Logos;
use lumora::compile_lumora;
use lumora::config::{OutputType, load_config};
use lumora::errors::LumoraError;
use lumora::lexer::{Spanned, Token};
use lumora::parser::Parser as LumoraParser;
use lumora::type_checker::TypeChecker;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Path to the input .lum file
    input_file: PathBuf,

    /// Name of the output executable or library
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Path to the lumora.yaml configuration file
    #[arg(short, long, value_name = "FILE")]
    config: Option<PathBuf>,

    /// Optimization level (e.g., O0, O1, O2, O3, Os, Oz)
    #[arg(short = 'O', long, value_name = "LEVEL")]
    optimize: Option<String>,

    /// Include debug information
    #[arg(short, long)]
    debug: bool,

    /// Target triple for compilation (e.g., x86_64-unknown-linux-gnu)
    #[arg(short, long, value_name = "TRIPLE")]
    target: Option<String>,

    /// Type of output to generate (executable, shared, static)
    #[arg(long, value_enum, default_value_t = OutputType::Executable)]
    output_type: OutputType,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    let config = if let Some(config_path) = cli.config {
        load_config(&config_path.to_string_lossy()).unwrap_or_else(|e| {
            eprintln!(
                "Warning: Could not load config file {}: {}. Using default configuration.",
                config_path.display(),
                e
            );
            Default::default()
        })
    } else {
        load_config("lumora.yaml").unwrap_or_else(|e| {
            eprintln!(
                "Warning: Could not load lumora.yaml: {}. Using default configuration.",
                e
            );
            Default::default()
        })
    };

    let output_dir = &config.build_settings.output_dir;
    fs::create_dir_all(output_dir)?;

    let input_file = &cli.input_file;
    let output_name = if let Some(output_path) = cli.output {
        output_path
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
                input_file.to_string_lossy().to_string(),
                &source_code,
            )),
            message: "Lexing error: Unrecognized token".to_string(),
            help: None,
        })?;
        tokens.push(Spanned { value: token, span });
    }

    let mut parser = LumoraParser::new(
        tokens,
        input_file.to_string_lossy().to_string(),
        source_code.clone(),
    );
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

    let o_file = output_dir.join(format!(
        "{}.o",
        output_name.file_name().unwrap().to_str().unwrap()
    ));
    let mut llc_command = Command::new("llc");
    llc_command
        .arg(&ll_file)
        .arg("-o")
        .arg(&o_file)
        .arg("-filetype=obj")
        .arg("-relocation-model=pic");

    let effective_optimization_level = cli.optimize.as_ref().or_else(|| {
        if config.build_settings.optimization_level.is_empty() {
            None
        } else {
            Some(&config.build_settings.optimization_level)
        }
    });

    if let Some(opt_level) = effective_optimization_level {
        llc_command.arg(format!("-{}", opt_level));
    }

    if cli.debug || config.build_settings.debug_info {
        llc_command.arg("--debugify-level=locations");
    }

    let effective_target_triple = cli.target.as_ref().or_else(|| {
        if config.build_settings.target_triple.is_empty() {
            None
        } else {
            Some(&config.build_settings.target_triple)
        }
    });

    if let Some(target_triple) = effective_target_triple {
        llc_command.arg(format!("-mtriple={}", target_triple));
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

        if let Err(e) = fs::remove_file(&o_file) {
            eprintln!(
                "Warning: Could not delete temporary .o file {}: {}",
                o_file.display(),
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

                let effective_optimization_level = cli.optimize.as_ref().or_else(|| {
                    if config.build_settings.optimization_level.is_empty() {
                        None
                    } else {
                        Some(&config.build_settings.optimization_level)
                    }
                });

                if let Some(opt_level) = effective_optimization_level {
                    clang_compile_command.arg(format!("-{}", opt_level));
                }

                if cli.debug || config.build_settings.debug_info {
                    clang_compile_command.arg("-g");
                }

                let effective_target_triple = cli.target.as_ref().or_else(|| {
                    if config.build_settings.target_triple.is_empty() {
                        None
                    } else {
                        Some(&config.build_settings.target_triple)
                    }
                });

                if let Some(target_triple) = effective_target_triple {
                    clang_compile_command.arg(format!("-target={}", target_triple));
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

    let final_output_name = match cli.output_type {
        OutputType::Executable => output_name.clone(),
        OutputType::SharedLibrary => output_name.with_extension("so"),
        OutputType::StaticLibrary => output_name.with_extension("a"),
    };

    let link_result = match cli.output_type {
        OutputType::Executable | OutputType::SharedLibrary => {
            let mut clang_command = Command::new("clang");
            clang_command
                .arg(&o_file)
                .args(&imported_bc_files)
                .args(&external_object_files)
                .arg("-o")
                .arg(&final_output_name);

            if let OutputType::SharedLibrary = cli.output_type {
                clang_command.arg("-shared");
            }

            for lib in &config.linker_settings.libraries {
                clang_command.arg(format!("-l{}", lib));
            }
            clang_command.args(&config.linker_settings.flags);

            let effective_optimization_level = cli.optimize.as_ref().or_else(|| {
                if config.build_settings.optimization_level.is_empty() {
                    None
                } else {
                    Some(&config.build_settings.optimization_level)
                }
            });

            if let Some(opt_level) = effective_optimization_level {
                clang_command.arg(format!("-{}", opt_level));
            }

            if cli.debug || config.build_settings.debug_info {
                clang_command.arg("-g");
            }

            let effective_target_triple = cli.target.as_ref().or_else(|| {
                if config.build_settings.target_triple.is_empty() {
                    None
                } else {
                    Some(&config.build_settings.target_triple)
                }
            });

            if let Some(target_triple) = effective_target_triple {
                clang_command.arg(format!("-target={}", target_triple));
            }

            clang_command.output()?
        }
        OutputType::StaticLibrary => {
            let mut ar_command = Command::new("ar");
            ar_command
                .arg("rcs")
                .arg(&final_output_name)
                .arg(&o_file)
                .args(&external_object_files);

            ar_command.output()?
        }
    };

    if let Err(e) = fs::remove_file(&o_file) {
        eprintln!(
            "Warning: Could not delete temporary .o file {}: {}",
            o_file.display(),
            e
        );
    }

    if !link_result.status.success() {
        eprintln!(
            "Linking failed: {}",
            String::from_utf8_lossy(&link_result.stderr)
        );
        return Err("Linking failed".into());
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
        input_file.display(),
        final_output_name.display()
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
