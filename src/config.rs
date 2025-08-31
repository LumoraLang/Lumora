use clap::ValueEnum;
use serde::{Deserialize, Serialize};
use std::fs;
use std::io;
use std::path::PathBuf;

#[derive(Debug, Deserialize, Serialize, Default, Clone, ValueEnum)]
pub enum OutputType {
    #[default]
    Executable,
    SharedLibrary,
    StaticLibrary,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct BuildSettings {
    #[serde(default = "default_output_dir")]
    pub output_dir: PathBuf,
    #[serde(default = "default_optimization_level")]
    pub optimization_level: String,
    #[serde(default = "default_debug_info")]
    pub debug_info: bool,
    #[serde(default)]
    pub target_triple: String,
    #[serde(default)]
    pub output_type: OutputType,
}

impl Default for BuildSettings {
    fn default() -> Self {
        Self {
            output_dir: default_output_dir(),
            optimization_level: default_optimization_level(),
            debug_info: default_debug_info(),
            target_triple: String::new(),
            output_type: OutputType::default(),
        }
    }
}

fn default_output_dir() -> PathBuf {
    "build".into()
}
fn default_optimization_level() -> String {
    "O0".to_string()
}
fn default_debug_info() -> bool {
    true
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LinkerSettings {
    #[serde(default)]
    pub libraries: Vec<String>,
    #[serde(default)]
    pub flags: Vec<String>,
}

impl Default for LinkerSettings {
    fn default() -> Self {
        Self {
            libraries: Vec::new(),
            flags: Vec::new(),
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct TestSettings {
    #[serde(default = "default_run_tests_on_build")]
    pub run_tests_on_build: bool,
    #[serde(default = "default_test_dir")]
    pub test_dir: PathBuf,
}

impl Default for TestSettings {
    fn default() -> Self {
        Self {
            run_tests_on_build: default_run_tests_on_build(),
            test_dir: default_test_dir(),
        }
    }
}

fn default_run_tests_on_build() -> bool {
    true
}
fn default_test_dir() -> PathBuf {
    "tests".into()
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LumoraConfig {
    #[serde(default = "default_project_name")]
    pub project_name: String,
    #[serde(default = "default_version")]
    pub version: String,
    #[serde(default)]
    pub authors: Vec<String>,
    #[serde(default = "default_description")]
    pub description: String,

    #[serde(default)]
    pub build_settings: BuildSettings,
    #[serde(default)]
    pub source_files: Vec<String>,
    #[serde(default)]
    pub external_dependencies: Vec<PathBuf>,
    #[serde(default)]
    pub linker_settings: LinkerSettings,
    #[serde(default)]
    pub test_settings: TestSettings,
}

impl Default for LumoraConfig {
    fn default() -> Self {
        Self {
            project_name: default_project_name(),
            version: default_version(),
            authors: Vec::new(),
            description: default_description(),
            build_settings: BuildSettings::default(),
            source_files: vec!["src/**/*.lum".to_string(), "test.lum".to_string()],
            external_dependencies: Vec::new(),
            linker_settings: LinkerSettings::default(),
            test_settings: TestSettings::default(),
        }
    }
}

fn default_project_name() -> String {
    "default_lumora_project".to_string()
}
fn default_version() -> String {
    "0.1.0".to_string()
}
fn default_description() -> String {
    "A Lumora project.".to_string()
}

pub fn load_config(path: &str) -> Result<LumoraConfig, io::Error> {
    let content = fs::read_to_string(path)?;
    let config: LumoraConfig = serde_yaml::from_str(&content).map_err(|e| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("Failed to parse lumora.yaml: {}", e),
        )
    })?;
    Ok(config)
}
