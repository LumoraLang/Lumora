use crate::LumoraError;
use crate::config::{Dependency, LockedDependency, LockFile, load_config, load_lockfile, save_lockfile};
use clap::{Parser, Subcommand};
use dirs;
use indicatif::{ProgressBar, ProgressStyle};
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;
use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
use std::io::Write;
use cli_table::{print_stdout, Table, WithTitle};

fn print_colored(text: &str, color: Color, bold: bool) -> Result<(), LumoraError> {
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    stdout.set_color(ColorSpec::new().set_fg(Some(color)).set_bold(bold)).map_err(|e| LumoraError::ConfigurationError {
        message: format!("Failed to set color: {}", e),
        help: None,
    })?;
    write!(&mut stdout, "{}", text).map_err(|e| LumoraError::ConfigurationError {
        message: format!("Failed to write colored text: {}", e),
        help: None,
    })?;
    stdout.reset().map_err(|e| LumoraError::ConfigurationError {
        message: format!("Failed to reset color: {}", e),
        help: None,
    })?;
    Ok(())
}

fn println_colored(text: &str, color: Color, bold: bool) -> Result<(), LumoraError> {
    let mut stdout = StandardStream::stdout(ColorChoice::Auto);
    stdout.set_color(ColorSpec::new().set_fg(Some(color)).set_bold(bold)).map_err(|e| LumoraError::ConfigurationError {
        message: format!("Failed to set color: {}", e),
        help: None,
    })?;
    writeln!(&mut stdout, "{}", text).map_err(|e| LumoraError::ConfigurationError {
        message: format!("Failed to write colored text: {}", e),
        help: None,
    })?;
    stdout.reset().map_err(|e| LumoraError::ConfigurationError {
        message: format!("Failed to reset color: {}", e),
        help: None,
    })?;
    Ok(())
}

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
            println_colored("Listing dependencies...", Color::Cyan, false)?;
            list_dependencies()?;
        }
        PmCommands::Add {
            repository,
            user,
            global,
        } => {
            println_colored(
                &format!(
                    "Adding dependency: {} (user: {}, global: {})",
                    repository, user, global
                ),
                Color::Cyan,
                false,
            )?;
            add_dependency(repository, user, global)?;
        }
        PmCommands::Rm { name, user, global } => {
            println_colored(
                &format!(
                    "Removing dependency: {} (user: {}, global: {})",
                    name, user, global
                ),
                Color::Cyan,
                false,
            )?;
            remove_dependency(name, user, global)?;
        }
        PmCommands::Install => {
            println_colored("Installing dependencies from config...", Color::Cyan, false)?;
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
    let lockfile = load_lockfile("lumora.lock").unwrap_or_default();

    println_colored(
        &format!("\nLocal dependencies ({})", get_local_dir().display()),
        Color::Cyan,
        true,
    )?;
    println_colored("----------------------------------", Color::Cyan, false)?;
    print_dir_contents(&get_local_dir(), &lockfile)?;

    if let Some(user_dir) = get_user_dir() {
        println_colored(
            &format!("\nUser dependencies ({})", user_dir.display()),
            Color::Cyan,
            true,
        )?;
        println_colored("----------------------------------", Color::Cyan, false)?;
        print_dir_contents(&user_dir, &lockfile)?;
    }

    println_colored(
        &format!("\nGlobal dependencies ({})", get_global_dir().display()),
        Color::Cyan,
        true,
    )?;
    println_colored("----------------------------------", Color::Cyan, false)?;
    print_dir_contents(&get_global_dir(), &lockfile)?;

    Ok(())
}

#[derive(Table, Debug)]
struct DependencyRow {
    #[table(title = "Name")]
    name: String,
    #[table(title = "Version")]
    version: String,
    #[table(title = "Source")]
    source: String,
    #[table(title = "Path")]
    path: String,
}

fn print_dir_contents(dir: &Path, lockfile: &LockFile) -> Result<(), LumoraError> {
    if !dir.exists() {
        println_colored("  (Directory does not exist)", Color::Yellow, false)?;
        return Ok(())
    }
    if !dir.is_dir() {
        println_colored("  (Not a directory)", Color::Yellow, false)?;
        return Ok(())
    }

    let mut entries: Vec<_> = fs::read_dir(dir)
        .map_err(|e| LumoraError::ConfigurationError {
            message: format!("Failed to read directory {}: {}", dir.display(), e),
            help: None,
        })?.
        filter_map(|entry| entry.ok()).
        collect();

    entries.sort_by_key(|entry| entry.file_name());
    if entries.is_empty() {
        println_colored("  (No dependencies found)", Color::Yellow, false)?;
    } else {
        let mut rows = Vec::new();
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
                        let mut version_info = format!("v{}", cfg.version);
                        let mut source_info = "N/A".to_string(); // Default to N/A

                        if let Some(locked_dep) = lockfile
                            .dependencies
                            .iter()
                            .find(|ld| ld.name == cfg.project_name)
                        {
                            source_info = locked_dep.source.clone();
                            version_info = locked_dep.version.clone(); // Use locked version for display
                        }

                        if let Some(dep) = cfg.dependencies.first() {
                            if !dep.version_req.is_empty() {
                                version_info = format!("req: {}", dep.version_req);
                            }
                        }
                        rows.push(DependencyRow {
                            name: cfg.project_name,
                            version: version_info,
                            source: source_info,
                            path: path.display().to_string(),
                        });
                    }
                    Err(_) => {
                        rows.push(DependencyRow {
                            name: format!("{} (No lumora.yaml found or invalid)", dep_name),
                            version: "N/A".to_string(),
                            source: "N/A".to_string(),
                            path: path.display().to_string(),
                        });
                    }
                }
            } else {
                rows.push(DependencyRow {
                    name: path.file_name().unwrap_or_default().to_string_lossy().to_string(),
                    version: "File".to_string(),
                    source: "N/A".to_string(),
                    path: path.display().to_string(),
                });
            }
        }
        print_stdout(rows.with_title()).map_err(|e| LumoraError::ConfigurationError {
            message: format!("Failed to print table: {}", e),
            help: None,
        })?;
    }
    Ok(())
}


fn add_dependency(repository: String, user: bool, global: bool) -> Result<(), LumoraError> {
    let (source_str, repo_name, version_req) = parse_repository_string(&repository)?;
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

    let mut resolved_version = "unknown".to_string();

    if source_str.starts_with("dir:") {
        let source_path = PathBuf::from(source_str.trim_start_matches("dir:"));
        let pb = ProgressBar::new_spinner();
        pb.set_style(spinner_style.clone());
        pb.set_message(format!(
            "{} Copying local directory {} to {}...",
            "📦",
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
        println_colored(
            &format!(
                "Successfully added local dependency '{}' to {}.",
                repo_name,
                target_path.display()
            ),
            Color::Green,
            false,
        )?;
    } else {
        resolved_version = resolve_git_version(&source_str, &version_req)?;

        let pb = ProgressBar::new_spinner();
        pb.set_style(spinner_style.clone());
        pb.set_message(format!(
            "{} Cloning {} (version: {}) into {}...",
            "🚀",
            source_str,
            resolved_version,
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

        let pb_checkout = ProgressBar::new_spinner();
        pb_checkout.set_style(spinner_style.clone());
        pb_checkout.set_message(format!("{} Checking out version {} for {}...", "🔍", resolved_version, repo_name));
        pb_checkout.enable_steady_tick(Duration::from_millis(100));
        let output_checkout = Command::new("git")
            .arg("checkout")
            .arg(&resolved_version)
            .current_dir(&target_path)
            .output()
            .map_err(|e| LumoraError::ConfigurationError {
                message: format!("Failed to execute git checkout: {}", e),
                help: Some("Ensure git is installed and in your PATH.".to_string()),
            })?;
        pb_checkout.finish_and_clear();
        if !output_checkout.status.success() {
            return Err(LumoraError::ConfigurationError {
                message: format!(
                    "Git checkout failed for version {}: {}",
                    resolved_version,
                    String::from_utf8_lossy(&output_checkout.stderr)
                ),
                help: None,
            });
        }


        let pb = ProgressBar::new_spinner();
        pb.set_style(spinner_style.clone());
        pb.set_message(format!("{} Updating submodules for {}...", "🔗", repo_name));
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
            println_colored(
                &format!(
                    "Warning: Git submodule update failed for {}: {}",
                    repo_name,
                    String::from_utf8_lossy(&output.stderr)
                ),
                Color::Yellow,
                false,
            )?;
        }
        println_colored(
            &format!(
                "Successfully added git dependency '{}' to {}.",
                repo_name,
                target_path.display()
            ),
            Color::Green,
            false,
        )?;
    }

    let mut config = load_config("lumora.yaml").unwrap_or_default();
    let new_dependency = Dependency {
        name: repo_name.clone(),
        source: repository.clone(),
        path: target_path.to_string_lossy().into_owned(),
        version: resolved_version.clone(),
        version_req,
    };

    config.dependencies.push(new_dependency);
    let c_files_glob = target_path
        .join("native/**/*.c")
        .to_string_lossy()
        .into_owned();
    let o_files_glob = target_path
        .join("native/**/*.o")
        .to_string_lossy()
        .into_owned();
    config.external_dependencies.push(c_files_glob);
    config.external_dependencies.push(o_files_glob);
    let config_content =
        serde_yaml::to_string(&config).map_err(|e| LumoraError::ConfigurationError {
            message: format!("Failed to serialize config to YAML: {}", e),
            help: None,
        })?;

    fs::write("lumora.yaml", config_content).map_err(|e| LumoraError::ConfigurationError {
        message: format!("Failed to write lumora.yaml: {}", e),
        help: None,
    })?;

    let mut lockfile = load_lockfile("lumora.lock").unwrap_or_default();
    let new_locked_dependency = LockedDependency {
        name: repo_name.clone(),
        source: repository,
        path: target_path.to_string_lossy().into_owned(),
        version: resolved_version,
    };
    lockfile.dependencies.push(new_locked_dependency);
    save_lockfile("lumora.lock", &lockfile).map_err(|e| LumoraError::ConfigurationError {
        message: format!("Failed to write lumora.lock: {}", e),
        help: None,
    })?;

    let mut visited: HashSet<PathBuf> = HashSet::new();
    install_nested_dependencies(&target_path, &mut visited, "  ")?;
    Ok(())
}

fn parse_repository_string(repo_string: &str) -> Result<(String, String, String), LumoraError> {
    let parts: Vec<&str> = repo_string.splitn(2, '@').collect();
    let (base_repo_string, version_req) = if parts.len() == 2 {
        (parts[0], parts[1].to_string())
    } else {
        (repo_string, String::new())
    };

    if base_repo_string.starts_with("git:") {
        let url = base_repo_string.trim_start_matches("git:").to_string();
        let name = url
            .split('/')
            .last()
            .unwrap_or("unknown")
            .trim_end_matches(".git")
            .to_string();
        Ok((url, name, version_req))
    } else if base_repo_string.starts_with("gl:") {
        let parts: Vec<&str> = base_repo_string
            .trim_start_matches("gl:")
            .splitn(2, '/')
            .collect();
        if parts.len() == 2 {
            let url = format!("https://gitlab.com/{}/{}.git", parts[0], parts[1]);
            let name = parts[1].to_string();
            Ok((url, name, version_req))
        } else {
            Err(LumoraError::ConfigurationError {
                message: format!("Invalid GitLab repository string: {}", base_repo_string),
                help: Some("Expected format: gl:username/repo".to_string()),
            })
        }
    } else if base_repo_string.starts_with("dir:") {
        let path_str = base_repo_string.trim_start_matches("dir:").to_string();
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
        Ok((path_str, name, version_req))
    } else if base_repo_string.contains('/') {
        let parts: Vec<&str> = base_repo_string.splitn(2, '/').collect();
        if parts.len() == 2 {
            let url = format!("https://github.com/{}/{}.git", parts[0], parts[1]);
            let name = parts[1].to_string();
            Ok((url, name, version_req))
        } else {
            Err(LumoraError::ConfigurationError {
                message: format!("Invalid GitHub repository string: {}", base_repo_string),
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

fn resolve_git_version(
    repo_url: &str,
    version_req: &str,
) -> Result<String, LumoraError> {
    if version_req.is_empty() || version_req == "latest" {
        let output = Command::new("git")
            .arg("ls-remote")
            .arg("--heads")
            .arg(repo_url)
            .arg("main")
            .output()
            .map_err(|e| LumoraError::ConfigurationError {
                message: format!("Failed to execute git ls-remote for main branch: {}", e),
                help: Some("Ensure git is installed and in your PATH.".to_string()),
            })?;

        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            if let Some(line) = stdout.lines().next() {
                return Ok(line.split_whitespace().next().unwrap_or_default().to_string());
            }
        }

        let output = Command::new("git")
            .arg("ls-remote")
            .arg("--heads")
            .arg(repo_url)
            .arg("master")
            .output()
            .map_err(|e| LumoraError::ConfigurationError {
                message: format!("Failed to execute git ls-remote for master branch: {}", e),
                help: Some("Ensure git is installed and in your PATH.".to_string()),
            })?;

        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            if let Some(line) = stdout.lines().next() {
                return Ok(line.split_whitespace().next().unwrap_or_default().to_string());
            }
        }

        return Err(LumoraError::ConfigurationError {
            message: format!(
                "Could not resolve 'main' or 'master' branch for repository: {}",
                repo_url
            ),
            help: None,
        });
    }

    let output = Command::new("git")
        .arg("ls-remote")
        .arg("--tags")
        .arg("--heads")
        .arg(repo_url)
        .arg(version_req)
        .output()
        .map_err(|e| LumoraError::ConfigurationError {
            message: format!("Failed to execute git ls-remote for version {}: {}", version_req, e),
            help: Some("Ensure git is installed and in your PATH.".to_string()),
        })?;

    if output.status.success() {
        let stdout = String::from_utf8_lossy(&output.stdout);
        if let Some(line) = stdout.lines().next() {
            return Ok(line.split_whitespace().next().unwrap_or_default().to_string());
        }
    }

    Err(LumoraError::ConfigurationError {
        message: format!(
            "Could not resolve version '{}' for repository: {}",
            version_req,
            repo_url
        ),
        help: Some("Ensure the version, tag, or branch exists.".to_string()),
    })
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

    println_colored(
        &format!("Removing {} from {}...", name, target_path.display()),
        Color::Cyan,
        false,
    )?;
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
        .retain(|dep| dep.name != name || dep.path != target_path.to_string_lossy().into_owned());
    let c_files_glob_to_remove = target_path
        .join("native/**/*.c")
        .to_string_lossy()
        .into_owned();
    let o_files_glob_to_remove = target_path
        .join("native/**/*.o")
        .to_string_lossy()
        .into_owned();
    config
        .external_dependencies
        .retain(|path| path != &c_files_glob_to_remove && path != &o_files_glob_to_remove);

    if config.dependencies.len() == initial_len {
        println_colored(
            &format!(
                "Warning: Dependency '{}' not found in lumora.yaml. Only removed from filesystem.",
                name
            ),
            Color::Yellow,
            false,
        )?;
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

    let mut lockfile = load_lockfile("lumora.lock").unwrap_or_default();
    let initial_lock_len = lockfile.dependencies.len();
    lockfile
        .dependencies
        .retain(|dep| dep.name != name || dep.path != target_path.to_string_lossy().into_owned());

    if lockfile.dependencies.len() != initial_lock_len {
        save_lockfile("lumora.lock", &lockfile).map_err(|e| LumoraError::ConfigurationError {
            message: format!("Failed to write lumora.lock: {}", e),
            help: None,
        })?;
    } else {
        println_colored(
            &format!(
                "Warning: Dependency '{}' not found in lumora.lock.",
                name
            ),
            Color::Yellow,
            false,
        )?;
    }

    println_colored(
        &format!("Successfully removed dependency '{}'.", name),
        Color::Green,
        false,
    )?;
    Ok(())
}

fn install_nested_dependencies(
    repo_path: &Path,
    visited: &mut HashSet<PathBuf>,
    indent: &str,
) -> Result<(), LumoraError> {
    let nested_config_path = repo_path.join("lumora.yaml");
    if !nested_config_path.exists() {
        return Ok(())
    }

    let canonical_repo_path =
        repo_path
            .canonicalize()
            .map_err(|e| LumoraError::ConfigurationError {
                message: format!("Failed to canonicalize path {}: {}", repo_path.display(), e),
                help: None,
            })?;

    if visited.contains(&canonical_repo_path) {
        return Err(LumoraError::ConfigurationError {
            message: format!("Circular dependency detected: {}", repo_path.display()),
            help: Some(
                "Review the lumora.yaml files in the dependency chain to break the cycle."
                    .to_string(),
            ),
        });
    }
    visited.insert(canonical_repo_path.clone());
        println_colored(
        &format!(
            "{}{}Found nested lumora.yaml in {}. Installing its dependencies...",
            indent,
            indent,
            repo_path.display()
        ),
        Color::Yellow,
        false,
    )?;
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
            "{}{}📦 Nested dependency: {} from {}",
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

        let (repo_url, repo_name, version_req) = parse_repository_string(&dep.source)?;
        let target_path = parent_lumora_dir.join(&repo_name);
        if dep.source.starts_with("dir:") {
            pb.finish_and_clear();
            println_colored(
                &format!(
                    "{}{}Skipping git operations for local dependency '{}'.",
                    next_indent, next_indent, dep.name
                ),
                Color::Yellow,
                false,
            )?;
        } else {
            let resolved_version = resolve_git_version(&repo_url, &version_req)?;
            if target_path.exists() {
                pb.set_message(format!(
                    "{}{}🔍 Nested dependency '{}' already exists. Checking out {}...",
                    next_indent, next_indent, dep.name, resolved_version
                ));
                let output = Command::new("git")
                    .arg("checkout")
                    .arg(&resolved_version)
                    .current_dir(&target_path)
                    .output()
                    .map_err(|e| LumoraError::ConfigurationError {
                        message: format!("Failed to execute git checkout for {}: {}", dep.name, e),
                        help: Some("Ensure git is installed and in your PATH.".to_string()),
                    })?;

                pb.finish_and_clear();
                if !output.status.success() {
                    println_colored(
                        &format!(
                            "{}{}Warning: Git checkout failed for {}: {}",
                            next_indent,
                            next_indent,
                            dep.name,
                            String::from_utf8_lossy(&output.stderr)
                        ),
                        Color::Yellow,
                        false,
                    )?;
                }
            } else {
                pb.set_message(format!(
                    "{}{}🚀 Cloning {} (version: {}) into {}...",
                    next_indent,
                    next_indent,
                    repo_url,
                    resolved_version,
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

                let pb_checkout = ProgressBar::new_spinner();
                pb_checkout.set_style(spinner_style.clone());
                pb_checkout.set_message(format!("{} Checking out version {} for {}...", "🔍", resolved_version, repo_name));
                pb_checkout.enable_steady_tick(Duration::from_millis(100));
                let output_checkout = Command::new("git")
                    .arg("checkout")
                    .arg(&resolved_version)
                    .current_dir(&target_path)
                    .output()
                    .map_err(|e| LumoraError::ConfigurationError {
                        message: format!("Failed to execute git checkout: {}", e),
                        help: Some("Ensure git is installed and in your PATH.".to_string()),
                    })?;
                pb_checkout.finish_and_clear();
                if !output_checkout.status.success() {
                    return Err(LumoraError::ConfigurationError {
                        message: format!(
                            "Git checkout failed for version {}: {}",
                            resolved_version,
                            String::from_utf8_lossy(&output_checkout.stderr)
                        ),
                        help: None,
                    });
                }
            }

            let pb_sub = ProgressBar::new_spinner();
            pb_sub.set_style(spinner_style.clone());
            pb_sub.set_message(format!(
                "{}{}🔗 Updating submodules for {}...",
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
                println_colored(
                    &format!(
                        "{}{}Warning: Git submodule update failed for {}: {}",
                        next_indent,
                        next_indent,
                        dep.name,
                        String::from_utf8_lossy(&output.stderr)
                    ),
                    Color::Yellow,
                    false,
                )?;
            }
        }

        install_nested_dependencies(&target_path, visited, &next_indent)?;
    }

    visited.remove(&canonical_repo_path);
    Ok(())
}

    

fn install_dependencies_from_config() -> Result<(), LumoraError> {
    let config = load_config("lumora.yaml").unwrap_or_default();
    if config.dependencies.is_empty() {
        println_colored("No dependencies found in lumora.yaml.", Color::Yellow, false)?;
        return Ok(())
    }

    let mut lockfile = load_lockfile("lumora.lock").unwrap_or_default();
    let mut updated_lockfile_entries: Vec<LockedDependency> = Vec::new();

    let mut visited: HashSet<PathBuf> = HashSet::new();
    let spinner_style = ProgressStyle::with_template("{spinner:.green} {msg}")
        .unwrap()
        .tick_chars(r"-\/ ");

    for dep in config.dependencies {
        let pb = ProgressBar::new_spinner();
        pb.set_style(spinner_style.clone());
        pb.set_message(format!(
            "{} Installing dependency: {} from {}",
            "📦",
            dep.name, dep.source
        ));
        pb.enable_steady_tick(Duration::from_millis(100));
        let target_path_buf = PathBuf::from(&dep.path);
        let (repo_url, _, version_req) = parse_repository_string(&dep.source)?;

        let mut resolved_version = String::new();
        
        if !dep.source.starts_with("dir:") {
            if let Some(locked_dep) = lockfile
                .dependencies
                .iter()
                .find(|ld| ld.name == dep.name && ld.source == dep.source)
            {
                resolved_version = locked_dep.version.clone();
                
                pb.set_message(format!(
                    "{} Using locked version {} for dependency:{}",
                    "🔒",
                    resolved_version, dep.name
                ));
            } else {
                resolved_version = resolve_git_version(&repo_url, &version_req)?;
                pb.set_message(format!(
                    "{} Resolved version {} for dependency:{}",
                    "✅",
                    resolved_version, dep.name
                ));
            }
        }

        if dep.source.starts_with("dir:") {
            pb.finish_and_clear();
            println_colored(
                &format!(
                    "Skipping git operations for local dependency '{}'.",
                    dep.name
                ),
                Color::Yellow,
                false,
            )?;
        } else if target_path_buf.exists() {
            pb.set_message(format!(
                "{} Dependency '{}' already exists at {}. Checking out {}...",
                "🔍",
                dep.name,
                target_path_buf.display(),
                resolved_version
            ));
            let output = Command::new("git")
                .arg("checkout")
                .arg(&resolved_version)
                .current_dir(&target_path_buf)
                .output()
                .map_err(|e| LumoraError::ConfigurationError {
                    message: format!("Failed to execute git checkout for {}: {}", dep.name, e),
                    help: Some("Ensure git is installed and in your PATH.".to_string()),
                })?;

            pb.finish_and_clear();
            if !output.status.success() {
                println_colored(
                    &format!(
                        "Warning: Git checkout failed for {}: {}",
                        dep.name,
                        String::from_utf8_lossy(&output.stderr)
                    ),
                    Color::Yellow,
                    false,
                )?;
            }
        } else {
            pb.set_message(format!(
                "{} Cloning {} (version: {}) into {}...",
                "🚀",
                repo_url,
                resolved_version,
                target_path_buf.display()
            ));
            let output = Command::new("git")
                .arg("clone")
                .arg(&repo_url)
                .arg(&target_path_buf)
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

            let pb_checkout = ProgressBar::new_spinner();
            pb_checkout.set_style(spinner_style.clone());
            pb_checkout.set_message(format!("{} Checking out version {} for {}...", "🔍", resolved_version, dep.name));
            pb_checkout.enable_steady_tick(Duration::from_millis(100));
            let output_checkout = Command::new("git")
                .arg("checkout")
                .arg(&resolved_version)
                .current_dir(&target_path_buf)
                .output()
                .map_err(|e| LumoraError::ConfigurationError {
                    message: format!("Failed to execute git checkout: {}", e),
                    help: Some("Ensure git is installed and in your PATH.".to_string()),
                })?;
            pb_checkout.finish_and_clear();
            if !output_checkout.status.success() {
                return Err(LumoraError::ConfigurationError {
                    message: format!(
                        "Git checkout failed for version {}: {}",
                        resolved_version,
                        String::from_utf8_lossy(&output_checkout.stderr)
                    ),
                    help: None,
                });
            }
        }

        if !dep.source.starts_with("dir:") {
            let pb_sub = ProgressBar::new_spinner();
            pb_sub.set_style(spinner_style.clone());
            pb_sub.set_message(format!("{} Updating submodules for {}...", "🔗", dep.name));
            pb_sub.enable_steady_tick(Duration::from_millis(100));
            let output = Command::new("git")
                .arg("submodule")
                .arg("update")
                .arg("--init")
                .arg("--recursive")
                .current_dir(&target_path_buf)
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
                println_colored(
                    &format!(
                        "Warning: Git submodule update failed for {}: {}",
                        dep.name,
                        String::from_utf8_lossy(&output.stderr)
                    ),
                    Color::Yellow,
                    false,
                )?;
            }
        }

        updated_lockfile_entries.push(LockedDependency {
            name: dep.name.clone(),
            source: dep.source.clone(),
            path: target_path_buf.to_string_lossy().into_owned(),
            version: resolved_version,
        });

        install_nested_dependencies(&target_path_buf, &mut visited, "  ")?;
    }

    lockfile.dependencies = updated_lockfile_entries;
    save_lockfile("lumora.lock", &lockfile).map_err(|e| LumoraError::ConfigurationError {
        message: format!("Failed to write lumora.lock: {}", e),
        help: None,
    })?;

    println_colored("All dependencies installed.", Color::Green, false)?;
    Ok(())
}
