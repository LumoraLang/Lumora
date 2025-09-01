use crate::LumoraError;
use crate::config::{Dependency, load_config};
use clap::{Parser, Subcommand};
use dirs;
use indicatif::{ProgressBar, ProgressStyle};
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct PmArgs {
    #[command(subcommand)]
    pub command: PmCommands,
}

#[derive(Debug, Subcommand)]
pub enum PmCommands {
    /// List installed dependencies
    List,
    /// Add a new dependency
    Add {
        /// Repository to add (e.g., githubuser/repo, gl:gitlabuser/repo, git:https://custom.git/repo.git)
        repository: String,
        /// Install for the current user (~/.lumora)
        #[arg(long)]
        user: bool,
        /// Install globally (/usr/share/lumora) (requires root)
        #[arg(long)]
        global: bool,
    },
    /// Remove a dependency
    Rm {
        /// Name of the dependency to remove
        name: String,
        /// Remove from current user's dependencies (~/.lumora)
        #[arg(long)]
        user: bool,
        /// Remove from global dependencies (/usr/share/lumora) (requires root)
        #[arg(long)]
        global: bool,
    },
    /// Install dependencies from lumora.yaml
    Install,
}

pub fn handle_pm_command(command: PmCommands) -> Result<(), LumoraError> {
    match command {
        PmCommands::List => {
            println!("Listing dependencies...");
            list_dependencies()?;
        }
        PmCommands::Add {
            repository,
            user,
            global,
        } => {
            println!(
                "Adding dependency: {} (user: {}, global: {})",
                repository, user, global
            );
            add_dependency(repository, user, global)?;
        }
        PmCommands::Rm { name, user, global } => {
            println!(
                "Removing dependency: {} (user: {}, global: {})",
                name, user, global
            );
            remove_dependency(name, user, global)?;
        }
        PmCommands::Install => {
            println!("Installing dependencies from config...");
            install_dependencies_from_config()?;
        }
    }
    Ok(())
}

fn get_local_dir() -> PathBuf {
    PathBuf::from(".lumora")
}

fn get_user_dir() -> Option<PathBuf> {
    dirs::home_dir().map(|p| p.join(".lumora"))
}

fn get_global_dir() -> PathBuf {
    PathBuf::from("/usr/share/lumora")
}

fn list_dependencies() -> Result<(), LumoraError> {
    println!(
        "\nLocal dependencies ({}/):\n----------------------------------",
        get_local_dir().display()
    );
    print_dir_contents(&get_local_dir())?;

    if let Some(user_dir) = get_user_dir() {
        println!(
            "\nUser dependencies ({}/):\n----------------------------------",
            user_dir.display()
        );
        print_dir_contents(&user_dir)?;
    }

    println!(
        "\nGlobal dependencies ({}/):\n----------------------------------",
        get_global_dir().display()
    );
    print_dir_contents(&get_global_dir())?;

    Ok(())
}

fn print_dir_contents(dir: &Path) -> Result<(), LumoraError> {
    if !dir.exists() {
        println!("  (Directory does not exist)");
        return Ok(());
    }
    if !dir.is_dir() {
        println!("  (Not a directory)");
        return Ok(());
    }

    let mut entries: Vec<_> = fs::read_dir(dir)
        .map_err(|e| LumoraError::ConfigurationError {
            message: format!("Failed to read directory {}: {}", dir.display(), e),
            help: None,
        })?
        .filter_map(|entry| entry.ok())
        .collect();

    entries.sort_by_key(|entry| entry.file_name());

    if entries.is_empty() {
        println!("  (No dependencies found)");
    } else {
        for entry in entries {
            let path = entry.path();
            if path.is_dir() {
                let dep_name = path
                    .file_name()
                    .unwrap_or_default()
                    .to_string_lossy()
                    .to_string();
                let nested_config_path = path.join("lumora.yaml");
                match load_config(&nested_config_path.to_string_lossy()) {
                    Ok(cfg) => {
                        println!("  - {} (v{})", cfg.project_name, cfg.version);
                        println!(
                            "    Source: {}",
                            cfg.dependencies
                                .first()
                                .map_or("N/A".to_string(), |d| d.source.clone())
                        );
                        println!("    Path: {}", path.display());
                    }
                    Err(_) => {
                        println!("  - {} (No lumora.yaml found or invalid)", dep_name);
                        println!("    Path: {}", path.display());
                    }
                }
            } else {
                println!(
                    "  - {}",
                    path.file_name().unwrap_or_default().to_string_lossy()
                );
            }
        }
    }
    Ok(())
}

fn add_dependency(repository: String, user: bool, global: bool) -> Result<(), LumoraError> {
    let (source_str, repo_name) = parse_repository_string(&repository)?;
    let install_dir = get_install_dir(user, global)?;
    let target_path = install_dir.join(&repo_name);

    if target_path.exists() {
        return Err(LumoraError::ConfigurationError {
            message: format!(
                "Dependency '{}' already exists at {}.",
                repo_name,
                target_path.display()
            ),
            help: Some(
                "Consider running 'lumora pm rm' first if you want to re-add it.".to_string(),
            ),
        });
    }

    let spinner_style = ProgressStyle::with_template("{spinner:.green} {msg}")
        .unwrap()
        .tick_chars(r"-\/ ");

    if source_str.starts_with("dir:") {
        let source_path = PathBuf::from(source_str.trim_start_matches("dir:"));
        let pb = ProgressBar::new_spinner();
        pb.set_style(spinner_style.clone());
        pb.set_message(format!(
            "Copying local directory {} to {}...",
            source_path.display(),
            target_path.display()
        ));
        pb.enable_steady_tick(Duration::from_millis(100));

        let output = Command::new("cp")
            .arg("-r")
            .arg(&source_path)
            .arg(&target_path)
            .output()
            .map_err(|e| LumoraError::ConfigurationError {
                message: format!("Failed to copy local directory: {}", e),
                help: Some("Ensure you have read permissions for the source and write permissions for the destination.".to_string()),
            })?;

        pb.finish_and_clear();

        if !output.status.success() {
            return Err(LumoraError::ConfigurationError {
                message: format!(
                    "Failed to copy local directory: {}",
                    String::from_utf8_lossy(&output.stderr)
                ),
                help: None,
            });
        }
        println!(
            "Successfully added local dependency '{}' to {}.",
            repo_name,
            target_path.display()
        );
    } else {
        let pb = ProgressBar::new_spinner();
        pb.set_style(spinner_style.clone());
        pb.set_message(format!(
            "Cloning {} into {}...",
            source_str,
            target_path.display()
        ));
        pb.enable_steady_tick(Duration::from_millis(100));

        let output = Command::new("git")
            .arg("clone")
            .arg(&source_str)
            .arg(&target_path)
            .output()
            .map_err(|e| LumoraError::ConfigurationError {
                message: format!("Failed to execute git clone: {}", e),
                help: Some("Ensure git is installed and in your PATH.".to_string()),
            })?;

        pb.finish_and_clear();

        if !output.status.success() {
            return Err(LumoraError::ConfigurationError {
                message: format!(
                    "Git clone failed: {}",
                    String::from_utf8_lossy(&output.stderr)
                ),
                help: None,
            });
        }

        let pb = ProgressBar::new_spinner();
        pb.set_style(spinner_style.clone());
        pb.set_message(format!("Updating submodules for {}...", repo_name));
        pb.enable_steady_tick(Duration::from_millis(100));

        let output = Command::new("git")
            .arg("submodule")
            .arg("update")
            .arg("--init")
            .arg("--recursive")
            .current_dir(&target_path)
            .output()
            .map_err(|e| LumoraError::ConfigurationError {
                message: format!("Failed to execute git submodule update: {}", e),
                help: Some("Ensure git is installed and in your PATH.".to_string()),
            })?;

        pb.finish_and_clear();

        if !output.status.success() {
            eprintln!(
                "Warning: Git submodule update failed for {}: {}",
                repo_name,
                String::from_utf8_lossy(&output.stderr)
            );
        }
        println!(
            "Successfully added git dependency '{}' to {}.",
            repo_name,
            target_path.display()
        );
    }

    let output = Command::new("git")
        .arg("rev-parse")
        .arg("HEAD")
        .current_dir(&target_path)
        .output()
        .map_err(|e| LumoraError::ConfigurationError {
            message: format!("Failed to get git commit hash: {}", e),
            help: Some("Ensure git is installed and in your PATH.".to_string()),
        })?;

    let version = if output.status.success() {
        String::from_utf8_lossy(&output.stdout).trim().to_string()
    } else {
        "unknown".to_string()
    };

    let mut config = load_config("lumora.yaml").unwrap_or_default();
    let new_dependency = Dependency {
        name: repo_name.clone(),
        source: repository,
        path: target_path.clone(),
        version,
    };

    config.dependencies.push(new_dependency);
    let config_content =
        serde_yaml::to_string(&config).map_err(|e| LumoraError::ConfigurationError {
            message: format!("Failed to serialize config to YAML: {}", e),
            help: None,
        })?;

    fs::write("lumora.yaml", config_content).map_err(|e| LumoraError::ConfigurationError {
        message: format!("Failed to write lumora.yaml: {}", e),
        help: None,
    })?;

    let mut visited: HashSet<PathBuf> = HashSet::new();
    visited.insert(target_path.clone());
    install_nested_dependencies(&target_path, &mut visited, "  ")?;

    Ok(())
}

fn parse_repository_string(repo_string: &str) -> Result<(String, String), LumoraError> {
    if repo_string.starts_with("git:") {
        let url = repo_string.trim_start_matches("git:").to_string();
        let name = url
            .split('/')
            .last()
            .unwrap_or("unknown")
            .trim_end_matches(".git")
            .to_string();
        Ok((url, name))
    } else if repo_string.starts_with("gl:") {
        let parts: Vec<&str> = repo_string
            .trim_start_matches("gl:")
            .splitn(2, '/')
            .collect();
        if parts.len() == 2 {
            let url = format!("https://gitlab.com/{}/{}.git", parts[0], parts[1]);
            let name = parts[1].to_string();
            Ok((url, name))
        } else {
            Err(LumoraError::ConfigurationError {
                message: format!("Invalid GitLab repository string: {}", repo_string),
                help: Some("Expected format: gl:username/repo".to_string()),
            })
        }
    } else if repo_string.starts_with("dir:") {
        let path_str = repo_string.trim_start_matches("dir:").to_string();
        let path = PathBuf::from(&path_str);
        if !path.exists() {
            return Err(LumoraError::ConfigurationError {
                message: format!("Local directory does not exist: {}", path_str),
                help: None,
            });
        }
        let name = path
            .file_name()
            .unwrap_or_default()
            .to_string_lossy()
            .to_string();
        Ok((path_str, name))
    } else if repo_string.contains('/') {
        let parts: Vec<&str> = repo_string.splitn(2, '/').collect();
        if parts.len() == 2 {
            let url = format!("https://github.com/{}/{}.git", parts[0], parts[1]);
            let name = parts[1].to_string();
            Ok((url, name))
        } else {
            Err(LumoraError::ConfigurationError {
                message: format!("Invalid GitHub repository string: {}", repo_string),
                help: Some("Expected format: username/repo".to_string()),
            })
        }
    } else {
        Err(LumoraError::ConfigurationError {
            message: format!("Invalid repository string: {}", repo_string),
            help: Some("Expected format: username/repo, gl:username/repo, git:https://url.git, or dir:/path/to/local/repo".to_string()),
        })
    }
}

fn get_install_dir(user: bool, global: bool) -> Result<PathBuf, LumoraError> {
    if user && global {
        return Err(LumoraError::ConfigurationError {
            message: "Cannot specify both --user and --global.".to_string(),
            help: None,
        });
    }

    if user {
        get_user_dir().ok_or_else(|| LumoraError::ConfigurationError {
            message: "Could not determine user home directory.".to_string(),
            help: None,
        })
    } else if global {
        Ok(get_global_dir())
    } else {
        Ok(get_local_dir())
    }
}

fn remove_dependency(name: String, user: bool, global: bool) -> Result<(), LumoraError> {
    let install_dir = get_install_dir(user, global)?;
    let target_path = install_dir.join(&name);
    if !target_path.exists() {
        return Err(LumoraError::ConfigurationError {
            message: format!(
                "Dependency '{}' not found at {}.",
                name,
                target_path.display()
            ),
            help: None,
        });
    }

    println!("Removing {} from {}...", name, target_path.display());
    fs::remove_dir_all(&target_path).map_err(|e| LumoraError::ConfigurationError {
        message: format!(
            "Failed to remove directory {}: {}",
            target_path.display(),
            e
        ),
        help: Some("Ensure you have write permissions.".to_string()),
    })?;

    let mut config = load_config("lumora.yaml").unwrap_or_default();
    let initial_len = config.dependencies.len();
    config
        .dependencies
        .retain(|dep| dep.name != name || dep.path != target_path);
    if config.dependencies.len() == initial_len {
        eprintln!(
            "Warning: Dependency '{}' not found in lumora.yaml. Only removed from filesystem.",
            name
        );
    } else {
        let config_content =
            serde_yaml::to_string(&config).map_err(|e| LumoraError::ConfigurationError {
                message: format!("Failed to serialize config to YAML: {}", e),
                help: None,
            })?;

        fs::write("lumora.yaml", config_content).map_err(|e| LumoraError::ConfigurationError {
            message: format!("Failed to write lumora.yaml: {}", e),
            help: None,
        })?;
    }
    println!("Successfully removed dependency '{}'.", name);
    Ok(())
}

fn install_nested_dependencies(
    repo_path: &Path,
    visited: &mut HashSet<PathBuf>,
    indent: &str,
) -> Result<(), LumoraError> {
    let nested_config_path = repo_path.join("lumora.yaml");
    if !nested_config_path.exists() {
        return Ok(());
    }

    if visited.contains(repo_path) {
        return Err(LumoraError::ConfigurationError {
            message: format!("Circular dependency detected: {}", repo_path.display()),
            help: Some(
                "Review the lumora.yaml files in the dependency chain to break the cycle."
                    .to_string(),
            ),
        });
    }
    visited.insert(repo_path.to_path_buf());

    println!(
        "{}{}Found nested lumora.yaml in {}. Installing its dependencies...",
        indent,
        indent,
        repo_path.display()
    );
    let nested_config = load_config(&nested_config_path.to_string_lossy()).map_err(|e| {
        LumoraError::ConfigurationError {
            message: format!(
                "Failed to load nested lumora.yaml from {}: {}",
                nested_config_path.display(),
                e
            ),
            help: None,
        }
    })?;

    let next_indent = format!("{}  ", indent);

    let spinner_style = ProgressStyle::with_template("{spinner:.green} {msg}")
        .unwrap()
        .tick_chars(r"-\/ ");

    for dep in nested_config.dependencies {
        let pb = ProgressBar::new_spinner();
        pb.set_style(spinner_style.clone());
        pb.set_message(format!(
            "{}{}Nested dependency: {} from {}",
            next_indent, next_indent, dep.name, dep.source
        ));
        pb.enable_steady_tick(Duration::from_millis(100));

        let parent_lumora_dir = repo_path.join(".lumora");
        fs::create_dir_all(&parent_lumora_dir).map_err(|e| LumoraError::ConfigurationError {
            message: format!(
                "Could not create directory {}: {}",
                parent_lumora_dir.display(),
                e
            ),
            help: None,
        })?;

        let (repo_url, repo_name) = parse_repository_string(&dep.source)?;
        let target_path = parent_lumora_dir.join(&repo_name);

        if dep.source.starts_with("dir:") {
            pb.finish_and_clear();
            println!(
                "{}{}Skipping git operations for local dependency '{}'.",
                next_indent, next_indent, dep.name
            );
        } else if target_path.exists() {
            pb.set_message(format!(
                "{}{}Nested dependency '{}' already exists. Pulling latest changes...",
                next_indent, next_indent, dep.name
            ));
            let output = Command::new("git")
                .arg("pull")
                .current_dir(&target_path)
                .output()
                .map_err(|e| LumoraError::ConfigurationError {
                    message: format!("Failed to execute git pull for {}: {}", dep.name, e),
                    help: Some("Ensure git is installed and in your PATH.".to_string()),
                })?;

            pb.finish_and_clear();

            if !output.status.success() {
                eprintln!(
                    "{}{}Warning: Git pull failed for {}: {}",
                    next_indent,
                    next_indent,
                    dep.name,
                    String::from_utf8_lossy(&output.stderr)
                );
            }
        } else {
            pb.set_message(format!(
                "{}{}Cloning {} into {}...",
                next_indent,
                next_indent,
                repo_url,
                target_path.display()
            ));
            let output = Command::new("git")
                .arg("clone")
                .arg(&repo_url)
                .arg(&target_path)
                .output()
                .map_err(|e| LumoraError::ConfigurationError {
                    message: format!("Failed to execute git clone for {}: {}", dep.name, e),
                    help: Some("Ensure git is installed and in your PATH.".to_string()),
                })?;

            pb.finish_and_clear();

            if !output.status.success() {
                return Err(LumoraError::ConfigurationError {
                    message: format!(
                        "{}{}Git clone failed for {}: {}",
                        next_indent,
                        next_indent,
                        dep.name,
                        String::from_utf8_lossy(&output.stderr)
                    ),
                    help: None,
                });
            }
        }

        if !dep.source.starts_with("dir:") {
            let pb_sub = ProgressBar::new_spinner();
            pb_sub.set_style(spinner_style.clone());
            pb_sub.set_message(format!(
                "{}{}Updating submodules for {}...",
                next_indent, next_indent, dep.name
            ));
            pb_sub.enable_steady_tick(Duration::from_millis(100));

            let output = Command::new("git")
                .arg("submodule")
                .arg("update")
                .arg("--init")
                .arg("--recursive")
                .current_dir(&target_path)
                .output()
                .map_err(|e| LumoraError::ConfigurationError {
                    message: format!(
                        "Failed to execute git submodule update for {}: {}",
                        dep.name, e
                    ),
                    help: Some("Ensure git is installed and in your PATH.".to_string()),
                })?;

            pb_sub.finish_and_clear();

            if !output.status.success() {
                eprintln!(
                    "{}{}Warning: Git submodule update failed for {}: {}",
                    next_indent,
                    next_indent,
                    dep.name,
                    String::from_utf8_lossy(&output.stderr)
                );
            }
        }

        install_nested_dependencies(&target_path, visited, &next_indent)?;
    }

    visited.remove(repo_path);
    Ok(())
}

fn install_dependencies_from_config() -> Result<(), LumoraError> {
    let config = load_config("lumora.yaml").unwrap_or_default();

    if config.dependencies.is_empty() {
        println!("No dependencies found in lumora.yaml.");
        return Ok(());
    }

    let mut visited: HashSet<PathBuf> = HashSet::new();

    let spinner_style = ProgressStyle::with_template("{spinner:.green} {msg}")
        .unwrap()
        .tick_chars(r"-\/ ");

    for dep in config.dependencies {
        let pb = ProgressBar::new_spinner();
        pb.set_style(spinner_style.clone());
        pb.set_message(format!(
            "Installing dependency: {} from {}",
            dep.name, dep.source
        ));
        pb.enable_steady_tick(Duration::from_millis(100));

        let target_path = dep.path;
        let repo_url = parse_repository_string(&dep.source)?.0;

        if dep.source.starts_with("dir:") {
            pb.finish_and_clear();
            println!(
                "Skipping git operations for local dependency '{}'.",
                dep.name
            );
        } else if target_path.exists() {
            pb.set_message(format!(
                "Dependency '{}' already exists at {}. Pulling latest changes...",
                dep.name,
                target_path.display()
            ));
            let output = Command::new("git")
                .arg("pull")
                .current_dir(&target_path)
                .output()
                .map_err(|e| LumoraError::ConfigurationError {
                    message: format!("Failed to execute git pull for {}: {}", dep.name, e),
                    help: Some("Ensure git is installed and in your PATH.".to_string()),
                })?;

            pb.finish_and_clear();

            if !output.status.success() {
                eprintln!(
                    "Warning: Git pull failed for {}: {}",
                    dep.name,
                    String::from_utf8_lossy(&output.stderr)
                );
            }
        } else {
            pb.set_message(format!(
                "Cloning {} into {}...",
                repo_url,
                target_path.display()
            ));
            let output = Command::new("git")
                .arg("clone")
                .arg(&repo_url)
                .arg(&target_path)
                .output()
                .map_err(|e| LumoraError::ConfigurationError {
                    message: format!("Failed to execute git clone for {}: {}", dep.name, e),
                    help: Some("Ensure git is installed and in your PATH.".to_string()),
                })?;

            pb.finish_and_clear();

            if !output.status.success() {
                return Err(LumoraError::ConfigurationError {
                    message: format!(
                        "Git clone failed for {}: {}",
                        dep.name,
                        String::from_utf8_lossy(&output.stderr)
                    ),
                    help: None,
                });
            }
        }

        if !dep.source.starts_with("dir:") {
            let pb_sub = ProgressBar::new_spinner();
            pb_sub.set_style(spinner_style.clone());
            pb_sub.set_message(format!("Updating submodules for {}...", dep.name));
            pb_sub.enable_steady_tick(Duration::from_millis(100));

            let output = Command::new("git")
                .arg("submodule")
                .arg("update")
                .arg("--init")
                .arg("--recursive")
                .current_dir(&target_path)
                .output()
                .map_err(|e| LumoraError::ConfigurationError {
                    message: format!(
                        "Failed to execute git submodule update for {}: {}",
                        dep.name, e
                    ),
                    help: Some("Ensure git is installed and in your PATH.".to_string()),
                })?;

            pb_sub.finish_and_clear();

            if !output.status.success() {
                eprintln!(
                    "Warning: Git submodule update failed for {}: {}",
                    dep.name,
                    String::from_utf8_lossy(&output.stderr)
                );
            }
        }

        install_nested_dependencies(&target_path, &mut visited, "  ")?;
    }

    println!("All dependencies installed.");
    Ok(())
}
