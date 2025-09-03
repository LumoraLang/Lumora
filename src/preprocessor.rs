use std::collections::HashMap;
use crate::errors::LumoraError;

pub struct Preprocessor {
    definitions: HashMap<String, String>,
    conditional_stack: Vec<bool>,
}

impl Preprocessor {
    pub fn new() -> Self {
        Preprocessor {
            definitions: HashMap::new(),
            conditional_stack: vec![true],
        }
    }

    fn parse_directive(&self, directive: &str) -> Result<Directive, LumoraError> {
        let parts: Vec<&str> = directive.split_whitespace().collect();
        if parts.is_empty() {
            return Err(LumoraError::PreprocessorError {
                code: "P001".to_string(),
                span: None,
                message: "Empty directive".to_string(),
                help: Some("Directives must have content.".to_string()),
            });
        }
        if parts[0].is_empty() {
            return Err(LumoraError::PreprocessorError {
                code: "P012".to_string(),
                span: None,
                message: "Directive starts with whitespace".to_string(),
                help: Some("Remove leading whitespace from directive.".to_string()),
            });
        }
        match parts[0] {
            "define" => {
                if parts.len() < 2 {
                    return Err(LumoraError::PreprocessorError {
                        code: "P002".to_string(),
                        span: None,
                        message: "define directive missing name".to_string(),
                        help: Some("Use ~define NAME VALUE~".to_string()),
                    });
                }
                if parts[1].is_empty() {
                    return Err(LumoraError::PreprocessorError {
                        code: "P013".to_string(),
                        span: None,
                        message: "define directive name is empty".to_string(),
                        help: Some("Provide a valid name for define.".to_string()),
                    });
                }
                let name = parts[1];
                let value = if parts.len() > 2 {
                    parts[2..].join(" ").trim_matches('"').to_string()
                } else {
                    String::new()
                };
                Ok(Directive::Define(name.to_string(), value))
            }
            "undef" => {
                if parts.len() != 2 {
                    return Err(LumoraError::PreprocessorError {
                        code: "P003".to_string(),
                        span: None,
                        message: "undef directive requires exactly one name".to_string(),
                        help: Some("Use ~undef NAME~".to_string()),
                    });
                }
                if parts[1].is_empty() {
                    return Err(LumoraError::PreprocessorError {
                        code: "P014".to_string(),
                        span: None,
                        message: "undef directive name is empty".to_string(),
                        help: Some("Provide a valid name for undef.".to_string()),
                    });
                }
                Ok(Directive::Undef(parts[1].to_string()))
            }
            "if" => {
                if parts.len() < 2 {
                    return Err(LumoraError::PreprocessorError {
                        code: "P004".to_string(),
                        span: None,
                        message: "if directive missing condition".to_string(),
                        help: Some("Use ~if CONDITION~ or ~if VAR == VALUE~".to_string()),
                    });
                }
                let condition = parts[1..].join(" ");
                if condition.is_empty() {
                    return Err(LumoraError::PreprocessorError {
                        code: "P015".to_string(),
                        span: None,
                        message: "if directive condition is empty".to_string(),
                        help: Some("Provide a valid condition for if.".to_string()),
                    });
                }
                if condition.contains("==") {
                    let eq_parts: Vec<&str> = condition.splitn(2, "==").collect();
                    if eq_parts.len() == 2 {
                        let var = eq_parts[0].trim();
                        let val = eq_parts[1].trim().trim_matches('"');
                        if var.is_empty() {
                            return Err(LumoraError::PreprocessorError {
                                code: "P016".to_string(),
                                span: None,
                                message: "if directive variable is empty".to_string(),
                                help: Some("Provide a valid variable for if ==.".to_string()),
                            });
                        }
                        Ok(Directive::IfEqual(var.to_string(), val.to_string()))
                    } else {
                        Err(LumoraError::PreprocessorError {
                            code: "P005".to_string(),
                            span: None,
                            message: "Invalid if condition".to_string(),
                            help: Some("Use ~if VAR == VALUE~".to_string()),
                        })
                    }
                } else if condition.contains("!=") {
                    let neq_parts: Vec<&str> = condition.splitn(2, "!=").collect();
                    if neq_parts.len() == 2 {
                        let var = neq_parts[0].trim();
                        let val = neq_parts[1].trim().trim_matches('"');
                        if var.is_empty() {
                            return Err(LumoraError::PreprocessorError {
                                code: "P017".to_string(),
                                span: None,
                                message: "if directive variable is empty".to_string(),
                                help: Some("Provide a valid variable for if !=.".to_string()),
                            });
                        }
                        Ok(Directive::IfNotEqual(var.to_string(), val.to_string()))
                    } else {
                        Err(LumoraError::PreprocessorError {
                            code: "P018".to_string(),
                            span: None,
                            message: "Invalid if condition".to_string(),
                            help: Some("Use ~if VAR != VALUE~".to_string()),
                        })
                    }
                } else {
                    Ok(Directive::IfDefined(condition))
                }
            }
            "ifdef" => {
                if parts.len() != 2 {
                    return Err(LumoraError::PreprocessorError {
                        code: "P006".to_string(),
                        span: None,
                        message: "ifdef directive requires exactly one name".to_string(),
                        help: Some("Use ~ifdef NAME~".to_string()),
                    });
                }
                if parts[1].is_empty() {
                    return Err(LumoraError::PreprocessorError {
                        code: "P019".to_string(),
                        span: None,
                        message: "ifdef directive name is empty".to_string(),
                        help: Some("Provide a valid name for ifdef.".to_string()),
                    });
                }
                Ok(Directive::IfDefined(parts[1].to_string()))
            }
            "ifndef" => {
                if parts.len() != 2 {
                    return Err(LumoraError::PreprocessorError {
                        code: "P007".to_string(),
                        span: None,
                        message: "ifndef directive requires exactly one name".to_string(),
                        help: Some("Use ~ifndef NAME~".to_string()),
                    });
                }
                if parts[1].is_empty() {
                    return Err(LumoraError::PreprocessorError {
                        code: "P020".to_string(),
                        span: None,
                        message: "ifndef directive name is empty".to_string(),
                        help: Some("Provide a valid name for ifndef.".to_string()),
                    });
                }
                Ok(Directive::IfNotDefined(parts[1].to_string()))
            }
            "elif" => {
                if parts.len() < 2 {
                    return Err(LumoraError::PreprocessorError {
                        code: "P008".to_string(),
                        span: None,
                        message: "elif directive missing condition".to_string(),
                        help: Some("Use ~elif CONDITION~".to_string()),
                    });
                }
                let condition = parts[1..].join(" ");
                if condition.is_empty() {
                    return Err(LumoraError::PreprocessorError {
                        code: "P021".to_string(),
                        span: None,
                        message: "elif directive condition is empty".to_string(),
                        help: Some("Provide a valid condition for elif.".to_string()),
                    });
                }
                Ok(Directive::Elif(condition))
            }
            "else" => {
                if parts.len() != 1 {
                    return Err(LumoraError::PreprocessorError {
                        code: "P009".to_string(),
                        span: None,
                        message: "else directive takes no arguments".to_string(),
                        help: Some("Use ~else~".to_string()),
                    });
                }
                Ok(Directive::Else)
            }
            "endif" => {
                if parts.len() != 1 {
                    return Err(LumoraError::PreprocessorError {
                        code: "P010".to_string(),
                        span: None,
                        message: "endif directive takes no arguments".to_string(),
                        help: Some("Use ~endif~".to_string()),
                    });
                }
                Ok(Directive::Endif)
            }
            _ => Err(LumoraError::PreprocessorError {
                code: "P011".to_string(),
                span: None,
                message: format!("Unknown directive: {}", parts[0]),
                help: Some("Supported directives: define, undef, if, ifdef, ifndef, elif, else, endif".to_string()),
            }),
        }
    }

    fn evaluate_condition(&self, condition: &str) -> bool {
        if condition.contains("==") {
            let parts: Vec<&str> = condition.splitn(2, "==").collect();
            if parts.len() == 2 {
                let var = parts[0].trim();
                let val = parts[1].trim().trim_matches('"');
                return self.definitions.get(var).map(|v| v == val).unwrap_or(false);
            }
            return false;
        } else if condition.contains("!=") {
            let parts: Vec<&str> = condition.splitn(2, "!=").collect();
            if parts.len() == 2 {
                let var = parts[0].trim();
                let val = parts[1].trim().trim_matches('"');
                return self.definitions.get(var).map(|v| v != val).unwrap_or(true);
            }
            return false;
        } else if condition.contains("<=") {
            let parts: Vec<&str> = condition.splitn(2, "<=").collect();
            if parts.len() == 2 {
                let var = parts[0].trim();
                let val = parts[1].trim().trim_matches('"');
                return self.definitions.get(var).map(|v| v.as_str() <= val).unwrap_or(false);
            }
            return false;
        } else if condition.contains(">=") {
            let parts: Vec<&str> = condition.splitn(2, ">=").collect();
            if parts.len() == 2 {
                let var = parts[0].trim();
                let val = parts[1].trim().trim_matches('"');
                return self.definitions.get(var).map(|v| v.as_str() >= val).unwrap_or(false);
            }
            return false;
        } else if condition.contains("<") {
            let parts: Vec<&str> = condition.splitn(2, "<").collect();
            if parts.len() == 2 {
                let var = parts[0].trim();
                let val = parts[1].trim().trim_matches('"');
                return self.definitions.get(var).map(|v| v.as_str() < val).unwrap_or(false);
            }
            return false;
        } else if condition.contains(">") {
            let parts: Vec<&str> = condition.splitn(2, ">").collect();
            if parts.len() == 2 {
                let var = parts[0].trim();
                let val = parts[1].trim().trim_matches('"');
                return self.definitions.get(var).map(|v| v.as_str() > val).unwrap_or(false);
            }
            return false;
        } else if condition.contains("&&") {
            let parts: Vec<&str> = condition.splitn(2, "&&").collect();
            if parts.len() == 2 {
                return self.evaluate_condition(parts[0].trim()) && self.evaluate_condition(parts[1].trim());
            }
            return false;
        } else if condition.contains("||") {
            let parts: Vec<&str> = condition.splitn(2, "||").collect();
            if parts.len() == 2 {
                return self.evaluate_condition(parts[0].trim()) || self.evaluate_condition(parts[1].trim());
            }
            return false;
        } else if condition.starts_with("defined ") {
            let var = condition.trim_start_matches("defined ").trim();
            return self.definitions.contains_key(var);
        } else if condition.starts_with("!defined ") {
            let var = condition.trim_start_matches("!defined ").trim();
            return !self.definitions.contains_key(var);
        } else {
            return self.definitions.contains_key(condition);
        }
    }

    pub fn preprocess(&mut self, input: &str) -> Result<String, LumoraError> {
        let mut filtered_lines = Vec::new();
        let lines: Vec<&str> = input.lines().collect();
        let mut i = 0;
        while i < lines.len() {
            let line = lines[i];
            let trimmed_line = line.trim();
            if trimmed_line.starts_with("~") && trimmed_line.ends_with("~") {
                let directive_str = trimmed_line.trim_start_matches('~').trim_end_matches('~').trim();
                if directive_str.contains(";") {
                    let parts: Vec<&str> = directive_str.splitn(2, ';').collect();
                    if parts.len() == 2 {
                        let condition_str = parts[0].trim();
                        let inner_directive_str = parts[1].trim();
                        if condition_str.starts_with("if ") {
                            let condition = condition_str[3..].trim();
                            let current_active = *self.conditional_stack.last().unwrap_or(&true);
                            if current_active && self.evaluate_condition(condition) {
                                let inner_directive = self.parse_directive(inner_directive_str)?;
                                match inner_directive {
                                    Directive::Define(name, value) => {
                                        self.definitions.insert(name, value);
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                } else {
                    let directive = self.parse_directive(directive_str)?;
                    match directive {
                        Directive::Define(name, value) => {
                            if *self.conditional_stack.last().unwrap_or(&true) {
                                self.definitions.insert(name, value);
                            }
                        }
                        Directive::Undef(name) => {
                            if *self.conditional_stack.last().unwrap_or(&true) {
                                self.definitions.remove(&name);
                            }
                        }
                        Directive::IfEqual(var, val) => {
                            let current_active = *self.conditional_stack.last().unwrap_or(&true);
                            if current_active {
                                let defined = self.definitions.get(&var).map(|v| v == &val).unwrap_or(false);
                                self.conditional_stack.push(defined);
                            } else {
                                self.conditional_stack.push(false);
                            }
                        }
                        Directive::IfNotEqual(var, val) => {
                            let current_active = *self.conditional_stack.last().unwrap_or(&true);
                            if current_active {
                                let defined = self.definitions.get(&var).map(|v| v != &val).unwrap_or(true);
                                self.conditional_stack.push(defined);
                            } else {
                                self.conditional_stack.push(false);
                            }
                        }
                        Directive::IfDefined(var) => {
                            let current_active = *self.conditional_stack.last().unwrap_or(&true);
                            if current_active {
                                let defined = self.definitions.contains_key(&var);
                                self.conditional_stack.push(defined);
                            } else {
                                self.conditional_stack.push(false);
                            }
                        }
                        Directive::IfNotDefined(var) => {
                            let current_active = *self.conditional_stack.last().unwrap_or(&true);
                            if current_active {
                                let defined = !self.definitions.contains_key(&var);
                                self.conditional_stack.push(defined);
                            } else {
                                self.conditional_stack.push(false);
                            }
                        }
                        Directive::Elif(condition) => {
                            let last = self.conditional_stack.pop().unwrap_or(false);
                            let parent_active = *self.conditional_stack.last().unwrap_or(&true);
                            if parent_active && !last {
                                if self.evaluate_condition(&condition) {
                                    self.conditional_stack.push(true);
                                } else {
                                    self.conditional_stack.push(false);
                                }
                            } else {
                                self.conditional_stack.push(false);
                            }
                        }
                        Directive::Else => {
                            let last = self.conditional_stack.pop().unwrap_or(false);
                            let parent_active = *self.conditional_stack.last().unwrap_or(&true);
                            let new_state = parent_active && !last;
                            self.conditional_stack.push(new_state);
                        }
                        Directive::Endif => {
                            self.conditional_stack.pop();
                            if self.conditional_stack.is_empty() {
                                self.conditional_stack.push(true);
                            }
                        }
                    }
                }
            } else {
                let include = *self.conditional_stack.last().unwrap_or(&true);
                if include {
                    let mut processed_line = line.to_string();
                    let mut iterations = 0;
                    while iterations < 10 && processed_line.contains('~') {
                        let mut changed = false;
                        for (name, value) in &self.definitions {
                            let pattern = format!("~{}~", name);
                            if processed_line.contains(&pattern) {
                                processed_line = processed_line.replace(&pattern, value);
                                changed = true;
                            }
                        }
                        if !changed {
                            break;
                        }
                        iterations += 1;
                    }
                    filtered_lines.push(processed_line);
                }
            }
            i += 1;
        }

        let preprocessed_output = filtered_lines.join("\n");
        Ok(preprocessed_output)
    }
}

enum Directive {
    Define(String, String),
    Undef(String),
    IfEqual(String, String),
    IfNotEqual(String, String),
    IfDefined(String),
    IfNotDefined(String),
    Elif(String),
    Else,
    Endif,
}
