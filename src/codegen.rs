extern crate libc;
use crate::ast::StructDefinition;
use crate::ast::{
    BinaryOp, Expr, Function, LumoraType, Program, Stmt, TopLevelDeclaration, UnaryOp,
};
use crate::errors::LumoraError;
use inkwell::Either;
use inkwell::IntPredicate;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::types::{AsTypeRef, BasicType, BasicTypeEnum};
use inkwell::values::AsValueRef;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue};
use llvm_sys::core::LLVMBuildLoad2;
use std::collections::HashMap;
pub struct CodeGenerator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, (PointerValue<'ctx>, LumoraType)>,
    functions: HashMap<String, FunctionValue<'ctx>>,
    all_functions: HashMap<String, (Vec<LumoraType>, LumoraType)>,
    struct_definitions: HashMap<String, StructDefinition>,
    enum_definitions: HashMap<String, Vec<String>>,
    bb_counter: usize,
    args: Vec<String>,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module_name: &str,
        all_functions: HashMap<String, (Vec<LumoraType>, LumoraType)>,
        struct_definitions: HashMap<String, StructDefinition>,
        enum_definitions: HashMap<String, Vec<String>>,
        args: Vec<String>,
    ) -> Self {
        let module = context.create_module(module_name);
        let i8_ptr_type = context.ptr_type(0.into());
        let i64_type = context.i64_type();
        let strlen_type = i64_type.fn_type(&[i8_ptr_type.into()], false);
        module.add_function("strlen", strlen_type, Some(Linkage::External));
        let malloc_type = i8_ptr_type.fn_type(&[i64_type.into()], false);
        module.add_function("malloc", malloc_type, Some(Linkage::External));
        let strcpy_type = i8_ptr_type.fn_type(&[i8_ptr_type.into(), i8_ptr_type.into()], false);
        module.add_function("strcpy", strcpy_type, Some(Linkage::External));
        let strcat_type = i8_ptr_type.fn_type(&[i8_ptr_type.into(), i8_ptr_type.into()], false);
        module.add_function("strcat", strcat_type, Some(Linkage::External));
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            variables: HashMap::new(),
            functions: HashMap::new(),
            all_functions,
            struct_definitions,
            enum_definitions,
            bb_counter: 0,
            args,
        }
    }

    pub fn generate(&mut self, program: &Program) -> Result<String, LumoraError> {
        for (name, (param_types, return_type)) in &self.all_functions {
            let fn_type = {
                if name == "main" {
                    let i32_type = self.context.i32_type();
                    let i8_ptr_type = self.context.ptr_type(0.into());
                    i32_type.fn_type(&[i32_type.into(), i8_ptr_type.into()], false)
                } else {
                    let param_llvm_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
                        param_types
                            .iter()
                            .map(|ty| self.type_to_llvm_type(ty).into())
                            .collect();
                    match return_type {
                        LumoraType::Void => {
                            self.context.void_type().fn_type(&param_llvm_types, false)
                        }
                        _ => self
                            .type_to_llvm_type(return_type)
                            .fn_type(&param_llvm_types, false),
                    }
                }
            };
            let fn_val = self.module.add_function(name, fn_type, None);
            self.functions.insert(name.clone(), fn_val);
        }

        for declaration in &program.declarations {
            match declaration {
                TopLevelDeclaration::Function(function) => {
                    self.generate_function(function)?;
                }
                TopLevelDeclaration::ExternalFunction(_ext_func) => {}
                TopLevelDeclaration::StructDefinition(struct_def) => {
                    let struct_type = self.context.opaque_struct_type(&struct_def.name);
                    let mut field_types = Vec::new();
                    for (_, field_lumora_type) in &struct_def.fields {
                        field_types.push(self.type_to_llvm_type(field_lumora_type));
                    }
                    struct_type.set_body(&field_types, false);
                }
                TopLevelDeclaration::EnumDefinition(_) => {}
            }
        }

        Ok(self.module.to_string())
    }

    fn generate_function(&mut self, function: &Function) -> Result<(), LumoraError> {
        let fn_val = *self.functions.get(&function.name).unwrap();
        let entry = self.context.append_basic_block(fn_val, "entry");
        self.builder.position_at_end(entry);
        self.variables.clear();
        for (i, (name, param_lumora_type)) in function.params.iter().enumerate() {
            let param = fn_val.get_nth_param(i as u32).unwrap();
            let alloca = self.builder.build_alloca(param.get_type(), name)?;
            self.builder.build_store(alloca, param)?;
            self.variables
                .insert(name.clone(), (alloca, param_lumora_type.clone()));
        }

        for stmt in &function.body {
            self.generate_statement(stmt)?;
        }

        if function.return_type == LumoraType::Void {
            let _ = self.builder.build_return(None);
        }

        Ok(())
    }

    fn generate_statement(&mut self, stmt: &Stmt) -> Result<(), LumoraError> {
        match stmt {
            Stmt::Let {
                is_exported: _,
                name,
                ty,
                value,
            } => {
                let resolved_ty = self.resolve_lumora_type(ty);
                let alloca = self
                    .builder
                    .build_alloca(self.type_to_llvm_type(&resolved_ty), name)?;
                let val = self.generate_expression(value)?;

                if let LumoraType::Struct(_) = resolved_ty {
                    let struct_llvm_type = self.type_to_llvm_type(&resolved_ty);
                    let loaded_struct = self.builder.build_load(
                        struct_llvm_type,
                        val.into_pointer_value(),
                        "load_struct_for_let",
                    )?;
                    self.builder.build_store(alloca, loaded_struct)?;
                } else {
                    self.builder.build_store(alloca, val)?;
                }

                self.variables.insert(name.clone(), (alloca, resolved_ty));
                Ok(())
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                let cond_val = self.generate_expression(condition)?;
                let fn_val = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();

                let current_bb_id = self.bb_counter;
                self.bb_counter += 1;
                let then_bb = self
                    .context
                    .append_basic_block(fn_val, &format!("then{}", current_bb_id));
                let else_bb = self
                    .context
                    .append_basic_block(fn_val, &format!("else{}", current_bb_id));
                let cont_bb = self
                    .context
                    .append_basic_block(fn_val, &format!("cont{}", current_bb_id));

                let _ = self.builder.build_conditional_branch(
                    cond_val.into_int_value(),
                    then_bb,
                    else_bb,
                );

                self.builder.position_at_end(then_bb);
                for stmt in then_block {
                    self.generate_statement(stmt)?;
                }
                if !self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_some()
                {
                    let _ = self.builder.build_unconditional_branch(cont_bb);
                }
                self.builder.position_at_end(else_bb);
                if let Some(else_stmts) = else_block {
                    for stmt in else_stmts {
                        self.generate_statement(stmt)?;
                    }
                }
                if !self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_some()
                {
                    let _ = self.builder.build_unconditional_branch(cont_bb);
                }
                self.builder.position_at_end(cont_bb);
                Ok(())
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    let val = self.generate_expression(expr)?;
                    let fn_val = self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap();
                    let function_return_type = self
                        .all_functions
                        .get(&fn_val.get_name().to_str().unwrap().to_string())
                        .unwrap()
                        .1
                        .clone();

                    let returned_expr_type = self.type_of_expression(expr)?;

                    let final_val =
                        self.build_cast(val, &returned_expr_type, &function_return_type)?;
                    let _ = self.builder.build_return(Some(&final_val));
                } else {
                    let _ = self.builder.build_return(None);
                }
                Ok(())
            }
            Stmt::Expr(expr) => {
                self.generate_expression(expr)?;
                Ok(())
            }
            Stmt::Use(_) => Ok(()),
            Stmt::While { condition, body } => {
                let fn_val = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let loop_header_bb = self.context.append_basic_block(fn_val, "loop_header");
                let loop_body_bb = self.context.append_basic_block(fn_val, "loop_body");
                let loop_exit_bb = self.context.append_basic_block(fn_val, "loop_exit");
                self.builder.build_unconditional_branch(loop_header_bb)?;
                self.builder.position_at_end(loop_header_bb);
                let cond_val = self.generate_expression(condition)?;
                self.builder.build_conditional_branch(
                    cond_val.into_int_value(),
                    loop_body_bb,
                    loop_exit_bb,
                )?;

                self.builder.position_at_end(loop_body_bb);
                for stmt in body {
                    self.generate_statement(stmt)?;
                }
                self.builder.build_unconditional_branch(loop_header_bb)?;
                self.builder.position_at_end(loop_exit_bb);
                Ok(())
            }
            Stmt::For {
                initializer,
                condition,
                increment,
                body,
            } => {
                self.generate_statement(initializer)?;
                let fn_val = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_parent()
                    .unwrap();
                let loop_header_bb = self.context.append_basic_block(fn_val, "for_loop_header");
                let loop_body_bb = self.context.append_basic_block(fn_val, "for_loop_body");
                let loop_exit_bb = self.context.append_basic_block(fn_val, "for_loop_exit");
                self.builder.build_unconditional_branch(loop_header_bb)?;
                self.builder.position_at_end(loop_header_bb);
                let cond_val = self.generate_expression(condition)?;
                self.builder.build_conditional_branch(
                    cond_val.into_int_value(),
                    loop_body_bb,
                    loop_exit_bb,
                )?;

                self.builder.position_at_end(loop_body_bb);
                for stmt in body {
                    self.generate_statement(stmt)?;
                }
                self.generate_expression(increment)?;
                self.builder.build_unconditional_branch(loop_header_bb)?;
                self.builder.position_at_end(loop_exit_bb);
                Ok(())
            }
        }
    }

    fn generate_string_binary_op(
        &mut self,
        left_val: BasicValueEnum<'ctx>,
        right_val: BasicValueEnum<'ctx>,
        left_lumora_type: LumoraType,
        right_lumora_type: LumoraType,
        op: &BinaryOp,
    ) -> Result<BasicValueEnum<'ctx>, LumoraError> {
        let i8_ptr_type = self.context.ptr_type(0.into());
        let i64_type = self.context.i64_type();
        match op {
            BinaryOp::Add => {
                if left_lumora_type == LumoraType::String && right_lumora_type == LumoraType::String
                {
                    let strlen_fn = self.module.get_function("strlen").unwrap();
                    let strcpy_fn = self.module.get_function("strcpy").unwrap();
                    let strcat_fn = self.module.get_function("strcat").unwrap();
                    let malloc_fn = self.module.get_function("malloc").unwrap();
                    let left_str = left_val.into_pointer_value();
                    let right_str = right_val.into_pointer_value();
                    let left_len = self
                        .builder
                        .build_call(strlen_fn, &[left_str.into()], "left_len")?
                        .try_as_basic_value()
                        .left()
                        .unwrap()
                        .into_int_value();
                    let right_len = self
                        .builder
                        .build_call(strlen_fn, &[right_str.into()], "right_len")?
                        .try_as_basic_value()
                        .left()
                        .unwrap()
                        .into_int_value();
                    let total_len = self
                        .builder
                        .build_int_add(left_len, right_len, "total_len")?;
                    let total_len_plus_null = self.builder.build_int_add(
                        total_len,
                        i64_type.const_int(1, false),
                        "total_len_plus_null",
                    )?;
                    let new_str_ptr = self
                        .builder
                        .build_call(malloc_fn, &[total_len_plus_null.into()], "new_str_ptr")?
                        .try_as_basic_value()
                        .left()
                        .unwrap()
                        .into_pointer_value();
                    self.builder.build_call(
                        strcpy_fn,
                        &[new_str_ptr.into(), left_str.into()],
                        "strcpy_call",
                    )?;
                    self.builder.build_call(
                        strcat_fn,
                        &[new_str_ptr.into(), right_str.into()],
                        "strcat_call",
                    )?;
                    Ok(new_str_ptr.into())
                } else {
                    return Err(LumoraError::CodegenError {
                        code: "L030".to_string(),
                        span: None,
                        message: "String addition requires both operands to be strings".to_string(),
                        help: None,
                    });
                }
            }
            BinaryOp::NullCoalescing => {
                return Err(LumoraError::CodegenError {
                    code: "L057".to_string(),
                    span: None,
                    message: "Null coalescing operator not supported for string types".to_string(),
                    help: None,
                });
            }
            BinaryOp::Mul => {
                let (str_val, int_val) = if left_lumora_type == LumoraType::String {
                    (left_val.into_pointer_value(), right_val.into_int_value())
                } else {
                    (right_val.into_pointer_value(), left_val.into_int_value())
                };

                let strlen_fn = self.module.get_function("strlen").unwrap();
                let malloc_fn = self.module.get_function("malloc").unwrap();
                let strcpy_fn = self.module.get_function("strcpy").unwrap();
                let str_len = self
                    .builder
                    .build_call(strlen_fn, &[str_val.into()], "str_len")?
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_int_value();
                let total_len = self.builder.build_int_mul(str_len, int_val, "total_len")?;
                let total_len_plus_null = self.builder.build_int_add(
                    total_len,
                    i64_type.const_int(1, false),
                    "total_len_plus_null",
                )?;
                let new_str_ptr = self
                    .builder
                    .build_call(malloc_fn, &[total_len_plus_null.into()], "new_str_ptr")?
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_pointer_value();
                self.builder
                    .build_store(new_str_ptr, self.context.i8_type().const_int(0, false))?;
                let loop_bb = self.context.append_basic_block(
                    self.builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap(),
                    "loop_bb",
                );
                let after_loop_bb = self.context.append_basic_block(
                    self.builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap(),
                    "after_loop_bb",
                );
                let counter_alloca = self.builder.build_alloca(i64_type, "counter_alloca")?;
                self.builder
                    .build_store(counter_alloca, i64_type.const_int(0, false))?;
                self.builder.build_unconditional_branch(loop_bb)?;
                self.builder.position_at_end(loop_bb);
                let current_counter = self
                    .builder
                    .build_load(i64_type, counter_alloca, "current_counter")?
                    .into_int_value();
                let loop_cond = self.builder.build_int_compare(
                    IntPredicate::SLT,
                    current_counter,
                    int_val,
                    "loop_cond",
                )?;
                self.builder.build_conditional_branch(
                    loop_cond,
                    {
                        let current_pos_ptr = self
                            .builder
                            .build_call(strlen_fn, &[new_str_ptr.into()], "current_pos_ptr")?
                            .try_as_basic_value()
                            .left()
                            .unwrap()
                            .into_int_value();
                        let current_pos_gep = unsafe {
                            self.builder
                                .build_gep(
                                    i8_ptr_type,
                                    new_str_ptr,
                                    &[current_pos_ptr.into()],
                                    "current_pos_gep",
                                )
                                .unwrap()
                        };
                        self.builder.build_call(
                            strcpy_fn,
                            &[current_pos_gep.into(), str_val.into()],
                            "strcpy_loop",
                        )?;

                        let next_counter = self.builder.build_int_add(
                            current_counter,
                            i64_type.const_int(1, false),
                            "next_counter",
                        )?;
                        self.builder.build_store(counter_alloca, next_counter)?;
                        loop_bb
                    },
                    after_loop_bb,
                )?;

                self.builder.position_at_end(after_loop_bb);
                Ok(new_str_ptr.into())
            }
            _ => Err(LumoraError::CodegenError {
                code: "L064".to_string(),
                span: None,
                message: format!("Unsupported binary operation for string types: {:?}", op),
                help: None,
            }),
        }
    }

    fn generate_expression(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, LumoraError> {
        match expr {
            Expr::Integer(n) => Ok(self.context.i64_type().const_int(*n as u64, true).into()),
            Expr::Float(f) => Ok(self.context.f32_type().const_float(*f as f64).into()),
            Expr::Boolean(b) => Ok(self.context.bool_type().const_int(*b as u64, false).into()),
            Expr::Null => Ok(self.context.ptr_type(0.into()).const_null().into()),
            Expr::Dereference(expr) => {
                let ptr_val = self.generate_expression(expr)?.into_pointer_value();
                let expr_type = self.type_of_expression(expr)?;
                let inner_type = match expr_type {
                    LumoraType::Pointer(inner) => *inner,
                    LumoraType::NullablePointer(inner) => *inner,
                    _ => {
                        return Err(LumoraError::CodegenError {
                            code: "L061".to_string(),
                            span: None,
                            message: format!(
                                "Attempted to dereference non-pointer type: {:?}",
                                expr_type
                            ),
                            help: None,
                        });
                    }
                };
                let llvm_inner_type = self.type_to_llvm_type(&inner_type);
                Ok(self
                    .builder
                    .build_load(llvm_inner_type, ptr_val, "deref_load")?
                    .into())
            }
            Expr::StringLiteral(s) => {
                if s.is_empty() {
                    Ok(self.context.ptr_type(0.into()).const_null().into())
                } else {
                    let i8_type = self.context.i8_type();
                    let string_len = s.len() as u32 + 1;
                    let string_type = i8_type.array_type(string_len);
                    let string_constant = self.context.const_string(s.as_bytes(), true);
                    let global_string =
                        self.module
                            .add_global(string_type, Some(0.into()), "str_literal");
                    global_string.set_constant(true);
                    global_string.set_initializer(&string_constant);
                    global_string.set_linkage(Linkage::Private);
                    Ok(global_string.as_pointer_value().into())
                }
            }
            Expr::Identifier(name) => {
                let (ptr, lumora_type) =
                    self.variables
                        .get(name)
                        .ok_or_else(|| LumoraError::CodegenError {
                            code: "L038".to_string(),
                            span: None,
                            message: format!("Undefined variable: {}", name),
                            help: None,
                        })?;
                let loaded_value_ref = unsafe {
                    LLVMBuildLoad2(
                        self.builder.as_mut_ptr(),
                        self.type_to_llvm_type(&lumora_type).as_type_ref(),
                        ptr.as_value_ref(),
                        name.as_ptr() as *const ::libc::c_char,
                    )
                };
                match lumora_type {
                    LumoraType::I32 => {
                        Ok(unsafe { inkwell::values::IntValue::new(loaded_value_ref).into() })
                    }
                    LumoraType::I64 => {
                        Ok(unsafe { inkwell::values::IntValue::new(loaded_value_ref).into() })
                    }
                    LumoraType::F64 => {
                        Ok(unsafe { inkwell::values::FloatValue::new(loaded_value_ref).into() })
                    }
                    LumoraType::F32 => {
                        Ok(unsafe { inkwell::values::BasicValueEnum::new(loaded_value_ref) })
                    }
                    LumoraType::Bool => {
                        Ok(unsafe { inkwell::values::IntValue::new(loaded_value_ref).into() })
                    }
                    LumoraType::String => {
                        Ok(unsafe { inkwell::values::PointerValue::new(loaded_value_ref).into() })
                    }
                    LumoraType::Array(_) => {
                        Ok(unsafe { inkwell::values::PointerValue::new(loaded_value_ref).into() })
                    }
                    LumoraType::Void => Err(LumoraError::CodegenError {
                        code: "L027".to_string(),
                        span: None,
                        message: "Cannot load void type".to_string(),
                        help: None,
                    }),
                    LumoraType::Null => {
                        Ok(unsafe { inkwell::values::PointerValue::new(loaded_value_ref).into() })
                    }
                    LumoraType::Pointer(_) | LumoraType::NullablePointer(_) => {
                        Ok(unsafe { inkwell::values::PointerValue::new(loaded_value_ref).into() })
                    }
                    LumoraType::Struct(_) => Ok((*ptr).into()),
                    LumoraType::Enum(_) => {
                        Ok(unsafe { inkwell::values::IntValue::new(loaded_value_ref).into() })
                    }
                }
            }
            Expr::Binary { left, op, right } => {
                let left_val = self.generate_expression(left)?;
                let right_val = self.generate_expression(right)?;
                let left_lumora_type = self.type_of_expression(left)?;
                let right_lumora_type = self.type_of_expression(right)?;

                if matches!(op, BinaryOp::NullCoalescing) {
                    let inner_type = match left_lumora_type {
                        LumoraType::NullablePointer(inner) => *inner,
                        _ => {
                            return Err(LumoraError::CodegenError {
                                code: "L058".to_string(),
                                span: None,
                                message: "Null coalescing operator can only be used with nullable pointers".to_string(),
                                help: None,
                            });
                        }
                    };

                    let fn_val = self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_parent()
                        .unwrap();

                    let is_null_bb = self.context.append_basic_block(fn_val, "is_null");
                    let is_not_null_bb = self.context.append_basic_block(fn_val, "is_not_null");
                    let merge_bb = self.context.append_basic_block(fn_val, "merge");

                    let is_null_cond = self
                        .builder
                        .build_is_null(left_val.into_pointer_value(), "is_null_cond")?;

                    self.builder.build_conditional_branch(
                        is_null_cond,
                        is_null_bb,
                        is_not_null_bb,
                    )?;

                    self.builder.position_at_end(is_not_null_bb);
                    let loaded_left_val = self.builder.build_load(
                        self.type_to_llvm_type(&inner_type),
                        left_val.into_pointer_value(),
                        "loaded_left_val",
                    )?;
                    let left_val_not_null = loaded_left_val;
                    self.builder.build_unconditional_branch(merge_bb)?;

                    self.builder.position_at_end(is_null_bb);
                    let right_val_is_null = right_val;
                    self.builder.build_unconditional_branch(merge_bb)?;

                    self.builder.position_at_end(merge_bb);
                    let phi = self
                        .builder
                        .build_phi(self.type_to_llvm_type(&inner_type), "null_coalescing_phi")?;
                    phi.add_incoming(&[
                        (&left_val_not_null, is_not_null_bb),
                        (&right_val_is_null, is_null_bb),
                    ]);
                    Ok(phi.as_basic_value())
                } else if left_lumora_type == LumoraType::String
                    || right_lumora_type == LumoraType::String
                {
                    self.generate_string_binary_op(
                        left_val,
                        right_val,
                        left_lumora_type,
                        right_lumora_type,
                        op,
                    )
                } else if left_val.is_int_value() && right_val.is_int_value() {
                    let mut left_int_val = left_val.into_int_value();
                    let mut right_int_val = right_val.into_int_value();
                    if left_lumora_type == LumoraType::I32 && right_lumora_type == LumoraType::I64 {
                        left_int_val = self.builder.build_int_s_extend(
                            left_int_val,
                            self.context.i64_type(),
                            "sext_i32_to_i64",
                        )?;
                    } else if left_lumora_type == LumoraType::I64
                        && right_lumora_type == LumoraType::I32
                    {
                        right_int_val = self.builder.build_int_s_extend(
                            right_int_val,
                            self.context.i64_type(),
                            "sext_i32_to_i64",
                        )?;
                    }

                    match op {
                        BinaryOp::Add => Ok(self
                            .builder
                            .build_int_add(left_int_val, right_int_val, "add")?
                            .into()),
                        BinaryOp::Sub => Ok(self
                            .builder
                            .build_int_sub(left_int_val, right_int_val, "sub")?
                            .into()),
                        BinaryOp::Mul => Ok(self
                            .builder
                            .build_int_mul(left_int_val, right_int_val, "mul")?
                            .into()),
                        BinaryOp::Div => Ok(self
                            .builder
                            .build_int_signed_div(left_int_val, right_int_val, "div")?
                            .into()),
                        BinaryOp::Equal => Ok(self
                            .builder
                            .build_int_compare(IntPredicate::EQ, left_int_val, right_int_val, "eq")?
                            .into()),
                        BinaryOp::NullCoalescing => {
                            let fn_val = self
                                .builder
                                .get_insert_block()
                                .unwrap()
                                .get_parent()
                                .unwrap();

                            let is_null_bb = self.context.append_basic_block(fn_val, "is_null");
                            let is_not_null_bb =
                                self.context.append_basic_block(fn_val, "is_not_null");
                            let merge_bb = self.context.append_basic_block(fn_val, "merge");

                            let is_null_cond = self
                                .builder
                                .build_is_null(left_val.into_pointer_value(), "is_null_cond")?;

                            self.builder.build_conditional_branch(
                                is_null_cond,
                                is_null_bb,
                                is_not_null_bb,
                            )?;

                            self.builder.position_at_end(is_not_null_bb);
                            let left_val_not_null = left_val;
                            self.builder.build_unconditional_branch(merge_bb)?;

                            self.builder.position_at_end(is_null_bb);
                            let right_val_is_null = right_val;
                            self.builder.build_unconditional_branch(merge_bb)?;

                            self.builder.position_at_end(merge_bb);
                            let phi = self.builder.build_phi(
                                self.type_to_llvm_type(&self.type_of_expression(left)?),
                                "null_coalescing_phi",
                            )?;
                            phi.add_incoming(&[
                                (&left_val_not_null, is_not_null_bb),
                                (&right_val_is_null, is_null_bb),
                            ]);
                            Ok(phi.as_basic_value())
                        }
                        BinaryOp::NotEqual => Ok(self
                            .builder
                            .build_int_compare(IntPredicate::NE, left_int_val, right_int_val, "ne")?
                            .into()),
                        BinaryOp::Less => Ok(self
                            .builder
                            .build_int_compare(
                                IntPredicate::SLT,
                                left_int_val,
                                right_int_val,
                                "lt",
                            )?
                            .into()),
                        BinaryOp::Greater => Ok(self
                            .builder
                            .build_int_compare(
                                IntPredicate::SGT,
                                left_int_val,
                                right_int_val,
                                "gt",
                            )?
                            .into()),
                    }
                } else if left_val.is_float_value() && right_val.is_float_value() {
                    match op {
                        BinaryOp::Add => Ok(self
                            .builder
                            .build_float_add(
                                left_val.into_float_value(),
                                right_val.into_float_value(),
                                "fadd",
                            )?
                            .into()),
                        BinaryOp::Sub => Ok(self
                            .builder
                            .build_float_sub(
                                left_val.into_float_value(),
                                right_val.into_float_value(),
                                "fsub",
                            )?
                            .into()),
                        BinaryOp::Mul => Ok(self
                            .builder
                            .build_float_mul(
                                left_val.into_float_value(),
                                right_val.into_float_value(),
                                "fmul",
                            )?
                            .into()),
                        BinaryOp::Div => Ok(self
                            .builder
                            .build_float_div(
                                left_val.into_float_value(),
                                right_val.into_float_value(),
                                "fdiv",
                            )?
                            .into()),
                        BinaryOp::Equal => Ok(self
                            .builder
                            .build_float_compare(
                                inkwell::FloatPredicate::OEQ,
                                left_val.into_float_value(),
                                right_val.into_float_value(),
                                "feq",
                            )?
                            .into()),
                        BinaryOp::NotEqual => Ok(self
                            .builder
                            .build_float_compare(
                                inkwell::FloatPredicate::ONE,
                                left_val.into_float_value(),
                                right_val.into_float_value(),
                                "fne",
                            )?
                            .into()),
                        BinaryOp::Less => Ok(self
                            .builder
                            .build_float_compare(
                                inkwell::FloatPredicate::OLT,
                                left_val.into_float_value(),
                                right_val.into_float_value(),
                                "flt",
                            )?
                            .into()),
                        BinaryOp::Greater => Ok(self
                            .builder
                            .build_float_compare(
                                inkwell::FloatPredicate::OGT,
                                left_val.into_float_value(),
                                right_val.into_float_value(),
                                "fgt",
                            )?
                            .into()),
                        BinaryOp::NullCoalescing => {
                            let fn_val = self
                                .builder
                                .get_insert_block()
                                .unwrap()
                                .get_parent()
                                .unwrap();

                            let is_null_bb = self.context.append_basic_block(fn_val, "is_null");
                            let is_not_null_bb =
                                self.context.append_basic_block(fn_val, "is_not_null");
                            let merge_bb = self.context.append_basic_block(fn_val, "merge");

                            let is_null_cond = self
                                .builder
                                .build_is_null(left_val.into_pointer_value(), "is_null_cond")?;

                            self.builder.build_conditional_branch(
                                is_null_cond,
                                is_null_bb,
                                is_not_null_bb,
                            )?;

                            self.builder.position_at_end(is_not_null_bb);
                            let left_val_not_null = left_val;
                            self.builder.build_unconditional_branch(merge_bb)?;

                            self.builder.position_at_end(is_null_bb);
                            let right_val_is_null = right_val;
                            self.builder.build_unconditional_branch(merge_bb)?;

                            self.builder.position_at_end(merge_bb);
                            let phi = self.builder.build_phi(
                                self.type_to_llvm_type(&self.type_of_expression(left)?),
                                "null_coalescing_phi",
                            )?;
                            phi.add_incoming(&[
                                (&left_val_not_null, is_not_null_bb),
                                (&right_val_is_null, is_null_bb),
                            ]);
                            Ok(phi.as_basic_value())
                        }
                    }
                } else if (left_lumora_type == LumoraType::Null
                    || right_lumora_type == LumoraType::Null)
                    && (matches!(op, BinaryOp::Equal) || matches!(op, BinaryOp::NotEqual))
                {
                    let (val1, type1) = (left_val, left_lumora_type);
                    let (val2, type2) = (right_val, right_lumora_type);
                    let result = if type1 == LumoraType::Null {
                        if val2.is_pointer_value() {
                            if matches!(op, BinaryOp::Equal) {
                                self.builder
                                    .build_is_null(val2.into_pointer_value(), "is_null")?
                            } else {
                                self.builder
                                    .build_is_not_null(val2.into_pointer_value(), "is_not_null")?
                            }
                        } else if val2.is_int_value() {
                            let zero = val2.into_int_value().get_type().const_int(0, false);
                            if matches!(op, BinaryOp::Equal) {
                                self.builder.build_int_compare(
                                    IntPredicate::EQ,
                                    val2.into_int_value(),
                                    zero,
                                    "eq_null",
                                )?
                            } else {
                                self.builder.build_int_compare(
                                    IntPredicate::NE,
                                    val2.into_int_value(),
                                    zero,
                                    "ne_null",
                                )?
                            }
                        } else if val2.is_float_value() {
                            let zero = val2.into_float_value().get_type().const_float(0.0);
                            if matches!(op, BinaryOp::Equal) {
                                self.builder.build_float_compare(
                                    inkwell::FloatPredicate::OEQ,
                                    val2.into_float_value(),
                                    zero,
                                    "eq_null_f",
                                )?
                            } else {
                                self.builder.build_float_compare(
                                    inkwell::FloatPredicate::ONE,
                                    val2.into_float_value(),
                                    zero,
                                    "ne_null_f",
                                )?
                            }
                        } else {
                            return Err(LumoraError::CodegenError {
                                code: "L030".to_string(),
                                span: None,
                                message: "Unsupported type for comparison with null".to_string(),
                                help: None,
                            });
                        }
                    } else if type2 == LumoraType::Null {
                        if val1.is_pointer_value() {
                            if matches!(op, BinaryOp::Equal) {
                                self.builder
                                    .build_is_null(val1.into_pointer_value(), "is_null")?
                            } else {
                                self.builder
                                    .build_is_not_null(val1.into_pointer_value(), "is_not_null")?
                            }
                        } else if val1.is_int_value() {
                            let zero = val1.into_int_value().get_type().const_int(0, false);
                            if matches!(op, BinaryOp::Equal) {
                                self.builder.build_int_compare(
                                    IntPredicate::EQ,
                                    val1.into_int_value(),
                                    zero,
                                    "eq_null",
                                )?
                            } else {
                                self.builder.build_int_compare(
                                    IntPredicate::NE,
                                    val1.into_int_value(),
                                    zero,
                                    "ne_null",
                                )?
                            }
                        } else if val1.is_float_value() {
                            let zero = val1.into_float_value().get_type().const_float(0.0);
                            if matches!(op, BinaryOp::Equal) {
                                self.builder.build_float_compare(
                                    inkwell::FloatPredicate::OEQ,
                                    val1.into_float_value(),
                                    zero,
                                    "eq_null_f",
                                )?
                            } else {
                                self.builder.build_float_compare(
                                    inkwell::FloatPredicate::ONE,
                                    val1.into_float_value(),
                                    zero,
                                    "ne_null_f",
                                )?
                            }
                        } else {
                            return Err(LumoraError::CodegenError {
                                code: "L030".to_string(),
                                span: None,
                                message: "Unsupported type for comparison with null".to_string(),
                                help: None,
                            });
                        }
                    } else {
                        return Err(LumoraError::CodegenError {
                            code: "L030".to_string(),
                            span: None,
                            message: "Binary operation operands must be of the same numeric type (i32 or f64)".to_string(),
                            help: None,
                        });
                    };
                    Ok(result.into())
                } else {
                    Err(LumoraError::CodegenError {
                        code: "L030".to_string(),
                        span: None,
                        message: "Binary operation operands must be of the same numeric type (i32 or f64)".to_string(),
                        help: None,
                    })
                }
            }
            Expr::Call { name, args } => {
                let fn_val =
                    self.module
                        .get_function(name)
                        .ok_or_else(|| LumoraError::CodegenError {
                            code: "L026".to_string(),
                            span: None,
                            message: format!("Undefined function: {}", name),
                            help: None,
                        })?;
                let arg_values: Vec<inkwell::values::BasicMetadataValueEnum<'ctx>> = args
                    .iter()
                    .map(|arg| self.generate_expression(arg).map(|val| val.into()))
                    .collect::<Result<_, LumoraError>>()?;

                let call_site_value =
                    self.builder
                        .build_call(fn_val, arg_values.as_slice(), "call");

                let (_, lumora_return_type) =
                    self.all_functions
                        .get(name)
                        .ok_or_else(|| LumoraError::CodegenError {
                            code: "L041".to_string(),
                            span: None,
                            message: format!(
                                "Function '{}' not found in all_functions during codegen",
                                name
                            ),
                            help: None,
                        })?;
                match call_site_value?.try_as_basic_value() {
                    Either::Left(basic_value) => Ok(basic_value),
                    Either::Right(_) => {
                        if *lumora_return_type == LumoraType::Void {
                            Ok(self.context.i64_type().const_int(0, false).into())
                        } else {
                            Err(LumoraError::CodegenError {
                                code: "L028".to_string(),
                                span: None,
                                message: format!(
                                    "Function {} returned void but expected a value of type {:?}",
                                    name, lumora_return_type
                                ),
                                help: None,
                            })
                        }
                    }
                }
            }
            Expr::ArrayLiteral(elements) => {
                let element_type = self.type_of_expression(expr)?;
                let LumoraType::Array(inner_type) = element_type else {
                    return Err(LumoraError::CodegenError {
                        code: "L036".to_string(),
                        span: None,
                        message: "Expected array type for array literal".to_string(),
                        help: None,
                    });
                };
                let llvm_element_type = self.type_to_llvm_type(&inner_type);
                let array_type = llvm_element_type.array_type(elements.len() as u32);
                let array_alloca = self.builder.build_alloca(array_type, "array_literal")?;

                for (i, element_expr) in elements.iter().enumerate() {
                    let element_val = self.generate_expression(element_expr)?;
                    let index = self.context.i64_type().const_int(i as u64, false);
                    let ptr = unsafe {
                        self.builder
                            .build_gep(
                                llvm_element_type,
                                array_alloca,
                                &[self.context.i64_type().const_int(0, false), index],
                                "array_element_ptr",
                            )
                            .unwrap()
                    };
                    self.builder.build_store(ptr, element_val)?;
                }
                Ok(array_alloca.into())
            }
            Expr::ArrayIndex { array, index } => {
                let array_ptr = self.generate_expression(array)?.into_pointer_value();
                let index_val = self.generate_expression(index)?.into_int_value();

                let array_type = self.type_of_expression(array)?;
                let (llvm_element_type, result_type) = match array_type {
                    LumoraType::Array(inner_type) => {
                        (self.type_to_llvm_type(&inner_type), *inner_type)
                    }
                    LumoraType::String => (self.context.i8_type().into(), LumoraType::I32),
                    _ => {
                        return Err(LumoraError::CodegenError {
                            code: "L037".to_string(),
                            span: None,
                            message: "Cannot index a non-array or non-string type.".to_string(),
                            help: None,
                        });
                    }
                };

                let ptr = unsafe {
                    self.builder
                        .build_gep(
                            llvm_element_type,
                            array_ptr,
                            &[self.context.i64_type().const_int(0, false), index_val],
                            "element_ptr",
                        )
                        .unwrap()
                };
                let loaded_val = self
                    .builder
                    .build_load(llvm_element_type, ptr, "load_element")?;

                if result_type == LumoraType::I32
                    && loaded_val.is_int_value()
                    && loaded_val.into_int_value().get_type().get_bit_width() == 8
                {
                    Ok(self
                        .builder
                        .build_int_s_extend(
                            loaded_val.into_int_value(),
                            self.context.i32_type(),
                            "sext_i8_to_i32",
                        )?
                        .into())
                } else {
                    Ok(loaded_val.into())
                }
            }
            Expr::ArgCount => {
                let main_fn = self.module.get_function("main").unwrap();
                let argc = main_fn.get_nth_param(0).unwrap().into_int_value();
                Ok(argc.into())
            }
            Expr::GetArg(index_expr) => {
                let main_fn = self.module.get_function("main").unwrap();
                let argv = main_fn.get_nth_param(1).unwrap().into_pointer_value();
                let index_val = self.generate_expression(index_expr)?.into_int_value();

                let arg_ptr = unsafe {
                    self.builder.build_gep(
                        self.context.ptr_type(0.into()),
                        argv,
                        &[index_val],
                        "arg_ptr",
                    )?
                };
                let loaded_arg = self.builder.build_load(
                    self.context.ptr_type(0.into()),
                    arg_ptr,
                    "loaded_arg",
                )?;
                Ok(loaded_arg.into())
            }
            Expr::StringOf(expr) => {
                let val = self.generate_expression(expr)?;
                let expr_type = self.type_of_expression(expr)?;
                match expr_type {
                    LumoraType::I32 => {
                        let i32_val = val.into_int_value();
                        let format_str = self
                            .builder
                            .build_global_string_ptr("%d\0", "fmt_i32_str")?;
                        let sprintf_fn = self.module.get_function("sprintf").unwrap_or_else(|| {
                            let fn_type = self.context.i32_type().fn_type(
                                &[
                                    self.context.ptr_type(0.into()).into(),
                                    self.context.ptr_type(0.into()).into(),
                                ],
                                true,
                            );
                            self.module.add_function("sprintf", fn_type, None)
                        });
                        let buffer = self.builder.build_array_alloca(
                            self.context.i8_type(),
                            self.context.i32_type().const_int(20, false),
                            "str_buffer",
                        )?;
                        let _ = self.builder.build_call(
                            sprintf_fn,
                            &[
                                buffer.into(),
                                format_str.as_pointer_value().into(),
                                i32_val.into(),
                            ],
                            "sprintf_call",
                        );
                        Ok(buffer.into())
                    }
                    LumoraType::I64 => {
                        let i64_val = val.into_int_value();
                        let format_str = self
                            .builder
                            .build_global_string_ptr("%lld\0", "fmt_i64_str")?;
                        let sprintf_fn = self.module.get_function("sprintf").unwrap_or_else(|| {
                            let fn_type = self.context.i32_type().fn_type(
                                &[
                                    self.context.ptr_type(0.into()).into(),
                                    self.context.ptr_type(0.into()).into(),
                                ],
                                true,
                            );
                            self.module.add_function("sprintf", fn_type, None)
                        });
                        let buffer = self.builder.build_array_alloca(
                            self.context.i8_type(),
                            self.context.i32_type().const_int(20, false),
                            "str_buffer",
                        )?;
                        let _ = self.builder.build_call(
                            sprintf_fn,
                            &[
                                buffer.into(),
                                format_str.as_pointer_value().into(),
                                i64_val.into(),
                            ],
                            "sprintf_call",
                        );
                        Ok(buffer.into())
                    }
                    LumoraType::F64 => {
                        let f64_val = val.into_float_value();
                        let format_str = self
                            .builder
                            .build_global_string_ptr("%f\0", "fmt_f64_str")?;
                        let sprintf_fn = self.module.get_function("sprintf").unwrap_or_else(|| {
                            let fn_type = self.context.i32_type().fn_type(
                                &[
                                    self.context.ptr_type(0.into()).into(),
                                    self.context.ptr_type(0.into()).into(),
                                ],
                                true,
                            );
                            self.module.add_function("sprintf", fn_type, None)
                        });
                        let buffer = self.builder.build_array_alloca(
                            self.context.i8_type(),
                            self.context.i32_type().const_int(30, false),
                            "str_buffer",
                        )?;
                        let _ = self.builder.build_call(
                            sprintf_fn,
                            &[
                                buffer.into(),
                                format_str.as_pointer_value().into(),
                                f64_val.into(),
                            ],
                            "sprintf_call",
                        );
                        Ok(buffer.into())
                    }
                    LumoraType::F32 => {
                        let f32_val = val.into_float_value();
                        let f64_val = self.builder.build_float_ext(
                            f32_val,
                            self.context.f64_type(),
                            "f32_to_f64",
                        )?;
                        let format_str = self
                            .builder
                            .build_global_string_ptr("%f\0", "fmt_f32_str")?;
                        let sprintf_fn = self.module.get_function("sprintf").unwrap_or_else(|| {
                            let fn_type = self.context.i32_type().fn_type(
                                &[
                                    self.context.ptr_type(0.into()).into(),
                                    self.context.ptr_type(0.into()).into(),
                                ],
                                true,
                            );
                            self.module.add_function("sprintf", fn_type, None)
                        });
                        let buffer = self.builder.build_array_alloca(
                            self.context.i8_type(),
                            self.context.i32_type().const_int(30, false),
                            "str_buffer",
                        )?;
                        let _ = self.builder.build_call(
                            sprintf_fn,
                            &[
                                buffer.into(),
                                format_str.as_pointer_value().into(),
                                f64_val.into(),
                            ],
                            "sprintf_call",
                        );
                        Ok(buffer.into())
                    }
                    LumoraType::Bool => {
                        let bool_val = val.into_int_value();
                        let true_str =
                            self.builder.build_global_string_ptr("true\0", "true_str")?;
                        let false_str = self
                            .builder
                            .build_global_string_ptr("false\0", "false_str")?;
                        let result_ptr = self.builder.build_select(
                            bool_val,
                            true_str.as_basic_value_enum(),
                            false_str.as_basic_value_enum(),
                            "bool_to_str",
                        )?;
                        Ok(result_ptr.into())
                    }
                    LumoraType::String => Ok(val),
                    _ => Err(LumoraError::CodegenError {
                        code: "L051".to_string(),
                        span: None,
                        message: format!("Unsupported type for string conversion: {:?}", expr_type),
                        help: None,
                    }),
                }
            }
            Expr::I32Of(expr) => {
                let val = self.generate_expression(expr)?;
                let expr_type = self.type_of_expression(expr)?;
                match expr_type {
                    LumoraType::I32 => Ok(val),
                    LumoraType::I64 => Ok(self
                        .builder
                        .build_int_truncate(
                            val.into_int_value(),
                            self.context.i32_type(),
                            "trunc_i64_to_i32",
                        )?
                        .into()),
                    LumoraType::F64 => Ok(self
                        .builder
                        .build_float_to_signed_int(
                            val.into_float_value(),
                            self.context.i32_type(),
                            "f64_to_i32",
                        )?
                        .into()),
                    LumoraType::F32 => Ok(self
                        .builder
                        .build_float_to_signed_int(
                            val.into_float_value(),
                            self.context.i32_type(),
                            "f32_to_i32",
                        )?
                        .into()),
                    LumoraType::Bool => Ok(self
                        .builder
                        .build_int_z_extend(
                            val.into_int_value(),
                            self.context.i32_type(),
                            "bool_to_i32",
                        )?
                        .into()),
                    LumoraType::String => {
                        let str_val = val.into_pointer_value();
                        let atoi_fn = self.module.get_function("atoi").unwrap_or_else(|| {
                            let fn_type = self
                                .context
                                .i32_type()
                                .fn_type(&[self.context.ptr_type(0.into()).into()], false);
                            self.module.add_function("atoi", fn_type, None)
                        });
                        let call_result =
                            self.builder
                                .build_call(atoi_fn, &[str_val.into()], "atoi_call");
                        Ok(call_result?.try_as_basic_value().left().unwrap())
                    }
                    _ => Err(LumoraError::CodegenError {
                        code: "L052".to_string(),
                        span: None,
                        message: format!("Unsupported type for i32 conversion: {:?}", expr_type),
                        help: None,
                    }),
                }
            }
            Expr::I64Of(expr) => {
                let val = self.generate_expression(expr)?;
                let expr_type = self.type_of_expression(expr)?;
                match expr_type {
                    LumoraType::I32 => Ok(self
                        .builder
                        .build_int_s_extend(
                            val.into_int_value(),
                            self.context.i64_type(),
                            "sext_i32_to_i64",
                        )?
                        .into()),
                    LumoraType::I64 => Ok(val),
                    LumoraType::F64 => Ok(self
                        .builder
                        .build_float_to_signed_int(
                            val.into_float_value(),
                            self.context.i64_type(),
                            "f64_to_i64",
                        )?
                        .into()),
                    LumoraType::F32 => Ok(self
                        .builder
                        .build_float_to_signed_int(
                            val.into_float_value(),
                            self.context.i64_type(),
                            "f32_to_i64",
                        )?
                        .into()),
                    LumoraType::Bool => Ok(self
                        .builder
                        .build_int_z_extend(
                            val.into_int_value(),
                            self.context.i64_type(),
                            "bool_to_i64",
                        )?
                        .into()),
                    LumoraType::String => {
                        let str_val = val.into_pointer_value();
                        let atoll_fn = self.module.get_function("atoll").unwrap_or_else(|| {
                            let fn_type = self
                                .context
                                .i64_type()
                                .fn_type(&[self.context.ptr_type(0.into()).into()], false);
                            self.module.add_function("atoll", fn_type, None)
                        });
                        let call_result =
                            self.builder
                                .build_call(atoll_fn, &[str_val.into()], "atoll_call");
                        Ok(call_result?.try_as_basic_value().left().unwrap())
                    }
                    _ => Err(LumoraError::CodegenError {
                        code: "L053".to_string(),
                        span: None,
                        message: format!("Unsupported type for i64 conversion: {:?}", expr_type),
                        help: None,
                    }),
                }
            }
            Expr::BoolOf(expr) => {
                let val = self.generate_expression(expr)?;
                let expr_type = self.type_of_expression(expr)?;
                match expr_type {
                    LumoraType::I32 | LumoraType::I64 => Ok(self
                        .builder
                        .build_int_compare(
                            IntPredicate::NE,
                            val.into_int_value(),
                            val.into_int_value().get_type().const_int(0, false),
                            "int_to_bool",
                        )?
                        .into()),
                    LumoraType::F64 => Ok(self
                        .builder
                        .build_float_compare(
                            inkwell::FloatPredicate::ONE,
                            val.into_float_value(),
                            val.into_float_value().get_type().const_float(0.0),
                            "float_to_bool",
                        )?
                        .into()),
                    LumoraType::F32 => Ok(self
                        .builder
                        .build_float_compare(
                            inkwell::FloatPredicate::ONE,
                            val.into_float_value(),
                            val.into_float_value().get_type().const_float(0.0),
                            "f32_to_bool",
                        )?
                        .into()),
                    LumoraType::Bool => Ok(val),
                    LumoraType::String => {
                        let str_val = val.into_pointer_value();
                        let strlen_fn = self.module.get_function("strlen").unwrap_or_else(|| {
                            let fn_type = self
                                .context
                                .i64_type()
                                .fn_type(&[self.context.ptr_type(0.into()).into()], false);
                            self.module.add_function("strlen", fn_type, None)
                        });
                        let call_result =
                            self.builder
                                .build_call(strlen_fn, &[str_val.into()], "strlen_call");
                        Ok(self
                            .builder
                            .build_int_compare(
                                IntPredicate::NE,
                                call_result?
                                    .try_as_basic_value()
                                    .left()
                                    .unwrap()
                                    .into_int_value(),
                                self.context.i64_type().const_int(0, false),
                                "str_to_bool",
                            )?
                            .into())
                    }
                    _ => Err(LumoraError::CodegenError {
                        code: "L054".to_string(),
                        span: None,
                        message: format!("Unsupported type for bool conversion: {:?}", expr_type),
                        help: None,
                    }),
                }
            }
            Expr::F32Of(expr) => {
                let val = self.generate_expression(expr)?;
                let expr_type = self.type_of_expression(expr)?;
                match expr_type {
                    LumoraType::I32 => Ok(self
                        .builder
                        .build_signed_int_to_float(
                            val.into_int_value(),
                            self.context.f32_type(),
                            "i32_to_f32",
                        )?
                        .into()),
                    LumoraType::I64 => Ok(self
                        .builder
                        .build_signed_int_to_float(
                            val.into_int_value(),
                            self.context.f32_type(),
                            "i64_to_f32",
                        )?
                        .into()),
                    LumoraType::F64 => Ok(self
                        .builder
                        .build_float_trunc(
                            val.into_float_value(),
                            self.context.f32_type(),
                            "f64_to_f32",
                        )?
                        .into()),
                    LumoraType::Bool => Ok(self
                        .builder
                        .build_signed_int_to_float(
                            val.into_int_value(),
                            self.context.f32_type(),
                            "bool_to_f32",
                        )?
                        .into()),
                    LumoraType::String => {
                        let str_val = val.into_pointer_value();
                        let atof_fn = self.module.get_function("atof").unwrap_or_else(|| {
                            let fn_type = self
                                .context
                                .f64_type()
                                .fn_type(&[self.context.ptr_type(0.into()).into()], false);
                            self.module.add_function("atof", fn_type, None)
                        });
                        let call_result =
                            self.builder
                                .build_call(atof_fn, &[str_val.into()], "atof_call");
                        Ok(self
                            .builder
                            .build_float_trunc(
                                call_result?
                                    .try_as_basic_value()
                                    .left()
                                    .unwrap()
                                    .into_float_value(),
                                self.context.f32_type(),
                                "f64_to_f32_from_atof",
                            )?
                            .into())
                    }
                    _ => Err(LumoraError::CodegenError {
                        code: "L055".to_string(),
                        span: None,
                        message: format!("Unsupported type for f32 conversion: {:?}", expr_type),
                        help: None,
                    }),
                }
            }
            Expr::F64Of(expr) => {
                let val = self.generate_expression(expr)?;
                let expr_type = self.type_of_expression(expr)?;
                match expr_type {
                    LumoraType::I32 => Ok(self
                        .builder
                        .build_signed_int_to_float(
                            val.into_int_value(),
                            self.context.f64_type(),
                            "i32_to_f64",
                        )?
                        .into()),
                    LumoraType::I64 => Ok(self
                        .builder
                        .build_signed_int_to_float(
                            val.into_int_value(),
                            self.context.f64_type(),
                            "i64_to_f64",
                        )?
                        .into()),
                    LumoraType::F64 => Ok(val),
                    LumoraType::Bool => Ok(self
                        .builder
                        .build_signed_int_to_float(
                            val.into_int_value(),
                            self.context.f64_type(),
                            "bool_to_f64",
                        )?
                        .into()),
                    LumoraType::String => {
                        let str_val = val.into_pointer_value();
                        let atof_fn = self.module.get_function("atof").unwrap_or_else(|| {
                            let fn_type = self
                                .context
                                .f64_type()
                                .fn_type(&[self.context.ptr_type(0.into()).into()], false);
                            self.module.add_function("atof", fn_type, None)
                        });
                        let call_result =
                            self.builder
                                .build_call(atof_fn, &[str_val.into()], "atof_call");
                        Ok(call_result?.try_as_basic_value().left().unwrap())
                    }
                    _ => Err(LumoraError::CodegenError {
                        code: "L056".to_string(),
                        span: None,
                        message: format!("Unsupported type for f64 conversion: {:?}", expr_type),
                        help: None,
                    }),
                }
            }
            Expr::StructLiteral { name, fields } => {
                let struct_type =
                    self.module
                        .get_struct_type(name)
                        .ok_or_else(|| LumoraError::CodegenError {
                            code: "L072".to_string(),
                            span: None,
                            message: format!(
                                "Struct type '{}' not found in LLVM module during codegen",
                                name
                            ),
                            help: None,
                        })?;
                let struct_alloca = self.builder.build_alloca(
                    struct_type.as_basic_type_enum(),
                    &format!("{}_literal", name),
                )?;

                let struct_def = self
                    .struct_definitions
                    .get(name)
                    .ok_or_else(|| LumoraError::CodegenError {
                        code: "L073".to_string(),
                        span: None,
                        message: format!("Struct definition '{}' not found during codegen", name),
                        help: None,
                    })?
                    .clone();

                for (i, (field_name, field_expr)) in fields.iter().enumerate() {
                    let field_val = self.generate_expression(field_expr)?;
                    let field_lumora_type = &struct_def.fields[i].1;
                    let casted_field_val = self.build_cast(
                        field_val,
                        &self.type_of_expression(field_expr)?,
                        field_lumora_type,
                    )?;
                    let field_ptr = self.builder.build_struct_gep(
                        struct_type,
                        struct_alloca,
                        i as u32,
                        field_name,
                    )?;
                    let value_to_store = if let LumoraType::Struct(_) = field_lumora_type {
                        let struct_llvm_type = self.type_to_llvm_type(field_lumora_type);
                        self.builder.build_load(
                            struct_llvm_type,
                            casted_field_val.into_pointer_value(),
                            &format!("load_struct_for_field_{}", field_name),
                        )?
                    } else {
                        casted_field_val
                    };
                    self.builder.build_store(field_ptr, value_to_store)?;
                }
                Ok(struct_alloca.into())
            }
            Expr::FieldAccess { target, field_name } => {
                let target_val = self.generate_expression(target)?;
                let target_type = self.type_of_expression(target)?;

                match target_type {
                    LumoraType::Struct(struct_name) => {
                        let struct_type = self.module.get_struct_type(&struct_name).ok_or_else(
                            || LumoraError::CodegenError {
                                code: "L074".to_string(),
                                span: None,
                                message: format!(
                                    "Struct type '{}' not found in LLVM module during field access",
                                    struct_name
                                ),
                                help: None,
                            },
                        )?;
                        let struct_def =
                            self.struct_definitions.get(&struct_name).ok_or_else(|| {
                                LumoraError::CodegenError {
                                    code: "L075".to_string(),
                                    span: None,
                                    message: format!(
                                        "Struct definition '{}' not found during field access",
                                        struct_name
                                    ),
                                    help: None,
                                }
                            })?;

                        let field_index = struct_def
                            .fields
                            .iter()
                            .position(|(name, _)| name == field_name)
                            .ok_or_else(|| LumoraError::CodegenError {
                                code: "L076".to_string(),
                                span: None,
                                message: format!(
                                    "Field '{}' not found in struct '{}' during field access",
                                    field_name, struct_name
                                ),
                                help: None,
                            })?;

                        let field_ptr = self.builder.build_struct_gep(
                            struct_type,
                            target_val.into_pointer_value(),
                            field_index as u32,
                            field_name,
                        )?;
                        let field_lumora_type = &struct_def.fields[field_index].1;
                        if let LumoraType::Struct(_) = field_lumora_type {
                            Ok(field_ptr.into())
                        } else {
                            Ok(self
                                .builder
                                .build_load(
                                    self.type_to_llvm_type(field_lumora_type),
                                    field_ptr,
                                    &format!("{}.{}", struct_name, field_name),
                                )?
                                .into())
                        }
                    }
                    _ => Err(LumoraError::CodegenError {
                        code: "L067".to_string(),
                        span: None,
                        message: format!(
                            "Cannot access field '{}' on non-struct type {:?}",
                            field_name, target_type
                        ),
                        help: None,
                    }),
                }
            }
            Expr::EnumVariant {
                enum_name,
                variant_name,
            } => {
                if let Some(variants) = self.enum_definitions.get(enum_name) {
                    if let Some(index) = variants.iter().position(|v| v == variant_name) {
                        Ok(self
                            .context
                            .i64_type()
                            .const_int(index as u64, false)
                            .into())
                    } else {
                        Err(LumoraError::CodegenError {
                            code: "L077".to_string(),
                            span: None,
                            message: format!(
                                "Variant '{}' not found in enum '{}' during codegen",
                                variant_name, enum_name
                            ),
                            help: None,
                        })
                    }
                } else {
                    Err(LumoraError::CodegenError {
                        code: "L078".to_string(),
                        span: None,
                        message: format!("Enum '{}' not found during codegen", enum_name),
                        help: None,
                    })
                }
            }
            Expr::Unary { op, right } => {
                let right_val = self.generate_expression(right)?;
                let right_type = self.type_of_expression(right)?;

                match op {
                    UnaryOp::Negate => match right_type {
                        LumoraType::I32 | LumoraType::I64 => Ok(self
                            .builder
                            .build_int_neg(right_val.into_int_value(), "neg")?
                            .into()),
                        LumoraType::F32 | LumoraType::F64 => Ok(self
                            .builder
                            .build_float_neg(right_val.into_float_value(), "fneg")?
                            .into()),
                        _ => Err(LumoraError::CodegenError {
                            code: "L059".to_string(),
                            span: None,
                            message: format!("Unsupported type for negation: {:?}", right_type),
                            help: None,
                        }),
                    },
                    UnaryOp::Not => match right_type {
                        LumoraType::Bool => Ok(self
                            .builder
                            .build_not(right_val.into_int_value(), "not")?
                            .into()),
                        _ => Err(LumoraError::CodegenError {
                            code: "L060".to_string(),
                            span: None,
                            message: format!("Unsupported type for logical NOT: {:?}", right_type),
                            help: None,
                        }),
                    },
                    UnaryOp::AddressOf => {
                        let val = self.generate_expression(right)?;
                        let val_type = self.type_of_expression(right)?;

                        let alloca = self
                            .builder
                            .build_alloca(self.type_to_llvm_type(&val_type), "temp_addr_of")?;
                        self.builder.build_store(alloca, val)?;

                        Ok(alloca.into())
                    }
                }
            }
        }
    }
    fn resolve_lumora_type(&self, ty: &LumoraType) -> LumoraType {
        match ty {
            LumoraType::Struct(name) => {
                if self.enum_definitions.contains_key(name) {
                    LumoraType::Enum(name.clone())
                } else {
                    LumoraType::Struct(name.clone())
                }
            }
            _ => ty.clone(),
        }
    }

    fn type_to_llvm_type(&self, ty: &LumoraType) -> BasicTypeEnum<'ctx> {
        let resolved_ty = self.resolve_lumora_type(ty);
        match resolved_ty {
            LumoraType::I32 => self.context.i32_type().into(),
            LumoraType::I64 => self.context.i64_type().into(),
            LumoraType::F64 => self.context.f64_type().into(),
            LumoraType::F32 => self.context.f32_type().into(),
            LumoraType::Bool => self.context.bool_type().into(),
            LumoraType::String => self.context.ptr_type(0.into()).into(),
            LumoraType::Void => {
                panic!("Void is not a basic type and cannot be converted to BasicTypeEnum")
            }
            LumoraType::Array(inner_type) => self
                .type_to_llvm_type(&inner_type)
                .into_pointer_type()
                .into(),
            LumoraType::Null => self.context.ptr_type(0.into()).into(),
            LumoraType::Pointer(inner_type) => self
                .type_to_llvm_type(&inner_type)
                .into_pointer_type()
                .into(),
            LumoraType::NullablePointer(inner_type) => self
                .type_to_llvm_type(&inner_type)
                .into_pointer_type()
                .into(),
            LumoraType::Struct(name) => self.module.get_struct_type(&name).unwrap().into(),
            LumoraType::Enum(_) => self.context.i64_type().into(),
        }
    }

    fn type_of_expression(&self, expr: &Expr) -> Result<LumoraType, LumoraError> {
        match expr {
            Expr::Integer(_) => Ok(LumoraType::I64),
            Expr::Float(_) => Ok(LumoraType::F32),
            Expr::Boolean(_) => Ok(LumoraType::Bool),
            Expr::StringLiteral(_) => Ok(LumoraType::String),
            Expr::Null => Ok(LumoraType::Null),
            Expr::Dereference(expr) => {
                let expr_type = self.type_of_expression(expr)?;
                match expr_type {
                    LumoraType::Pointer(inner_type) => Ok(*inner_type),
                    LumoraType::NullablePointer(inner_type) => Ok(*inner_type),
                    _ => Err(LumoraError::CodegenError {
                        code: "L062".to_string(),
                        span: None,
                        message: format!(
                            "Cannot get type of dereferenced non-pointer: {:?}",
                            expr_type
                        ),
                        help: None,
                    }),
                }
            }
            Expr::Identifier(name) => self
                .variables
                .get(name)
                .map(|(_, ty)| ty.clone())
                .ok_or_else(|| LumoraError::CodegenError {
                    code: "L038".to_string(),
                    span: None,
                    message: format!("Undefined variable: {}", name),
                    help: None,
                }),
            Expr::Binary { left, op, right } => {
                let left_type = self.type_of_expression(left)?;
                let right_type = self.type_of_expression(right)?;

                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => {
                        match (left_type, right_type) {
                            (LumoraType::I32, LumoraType::I32) => Ok(LumoraType::I32),
                            (LumoraType::I64, LumoraType::I64) => Ok(LumoraType::I64),
                            (LumoraType::F64, LumoraType::F64) => Ok(LumoraType::F64),
                            _ => Err(LumoraError::CodegenError {
                                code: "L039".to_string(),
                                span: None,
                                message: "Arithmetic operations require operands of the same numeric type (i32 or f64)".to_string(),
                                help: None,
                            }),
                        }
                    }
                    BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::Less | BinaryOp::Greater => {
                        if left_type == right_type {
                            Ok(LumoraType::Bool)
                        } else if (left_type == LumoraType::I32 && right_type == LumoraType::I64)
                            || (left_type == LumoraType::I64 && right_type == LumoraType::I32)
                        {
                            Ok(LumoraType::Bool)
                        } else {
                            Err(LumoraError::CodegenError {
                                code: "L040".to_string(),
                                span: None,
                                message: "Comparison requires same types".to_string(),
                                help: None,
                            })
                        }
                    }
                    BinaryOp::NullCoalescing => {
                        match left_type {
                            LumoraType::NullablePointer(inner_type) => Ok(*inner_type),
                            LumoraType::Null => Ok(right_type),
                            _ => Err(LumoraError::CodegenError {
                                code: "L063".to_string(),
                                span: None,
                                message: "Null coalescing operator can only be used with nullable pointers or null".to_string(),
                                help: None,
                            }),
                        }
                    }
                }
            }
            Expr::Call { name, args: _ } => self
                .all_functions
                .get(name)
                .map(|(_, return_type)| return_type.clone())
                .ok_or_else(|| LumoraError::CodegenError {
                    code: "L041".to_string(),
                    span: None,
                    message: format!("Undefined function: {}", name),
                    help: None,
                }),
            Expr::ArrayLiteral(elements) => {
                if elements.is_empty() {
                    return Err(LumoraError::CodegenError {
                        code: "L042".to_string(),
                        span: None,
                        message: "Empty array literals are not supported without explicit type annotation.".to_string(),
                        help: None,
                    });
                }
                let first_element_type = self.type_of_expression(&elements[0])?;
                Ok(LumoraType::Array(Box::new(first_element_type)))
            }
            Expr::ArrayIndex { array, index: _ } => {
                let array_type = self.type_of_expression(array)?;
                let LumoraType::Array(inner_type) = array_type else {
                    return Err(LumoraError::CodegenError {
                        code: "L043".to_string(),
                        span: None,
                        message: "Cannot index a non-array type.".to_string(),
                        help: None,
                    });
                };
                Ok(*inner_type)
            }
            Expr::ArgCount => Ok(LumoraType::I32),
            Expr::GetArg(_) => Ok(LumoraType::String),
            Expr::StringOf(_) => Ok(LumoraType::String),
            Expr::I32Of(_) => Ok(LumoraType::I32),
            Expr::I64Of(_) => Ok(LumoraType::I64),
            Expr::BoolOf(_) => Ok(LumoraType::Bool),
            Expr::F32Of(_) => Ok(LumoraType::F64),
            Expr::F64Of(_) => Ok(LumoraType::F64),
            Expr::StructLiteral { name, fields: _ } => Ok(LumoraType::Struct(name.clone())),
            Expr::FieldAccess { target, field_name } => {
                let target_type = self.type_of_expression(target)?;
                match target_type {
                    LumoraType::Struct(struct_name) => {
                        let struct_def =
                            self.struct_definitions.get(&struct_name).ok_or_else(|| {
                                LumoraError::CodegenError {
                                    code: "L071".to_string(),
                                    span: None,
                                    message: format!(
                                        "Struct '{}' not found during codegen",
                                        struct_name
                                    ),
                                    help: None,
                                }
                            })?;
                        struct_def
                            .fields
                            .iter()
                            .find(|(name, _)| name == field_name)
                            .map(|(_, ty)| ty.clone())
                            .ok_or_else(|| LumoraError::CodegenError {
                                code: "L068".to_string(),
                                span: None,
                                message: format!(
                                    "Field '{}' not found in struct '{}'",
                                    field_name, struct_name
                                ),
                                help: None,
                            })
                    }
                    _ => Err(LumoraError::CodegenError {
                        code: "L069".to_string(),
                        span: None,
                        message: format!(
                            "Cannot access field '{}' on non-struct type {:?}",
                            field_name, target_type
                        ),
                        help: None,
                    }),
                }
            }
            Expr::EnumVariant {
                enum_name,
                variant_name: _,
            } => Ok(LumoraType::Enum(enum_name.clone())),
            Expr::Unary { op, right } => {
                let right_type = self.type_of_expression(right)?;
                match op {
                    UnaryOp::Negate => match right_type {
                        LumoraType::I32 | LumoraType::I64 | LumoraType::F32 | LumoraType::F64 => {
                            Ok(right_type)
                        }
                        _ => Err(LumoraError::CodegenError {
                            code: "L059".to_string(),
                            span: None,
                            message: format!("Unsupported type for negation: {:?}", right_type),
                            help: None,
                        }),
                    },
                    UnaryOp::Not => match right_type {
                        LumoraType::Bool => Ok(LumoraType::Bool),
                        _ => Err(LumoraError::CodegenError {
                            code: "L060".to_string(),
                            span: None,
                            message: format!("Unsupported type for logical NOT: {:?}", right_type),
                            help: None,
                        }),
                    },
                    UnaryOp::AddressOf => Ok(LumoraType::Pointer(Box::new(right_type))),
                }
            }
        }
    }

    fn build_cast(
        &self,
        value: BasicValueEnum<'ctx>,
        from_type: &LumoraType,
        to_type: &LumoraType,
    ) -> Result<BasicValueEnum<'ctx>, LumoraError> {
        if from_type == to_type {
            return Ok(value);
        }

        match (from_type, to_type) {
            (LumoraType::I64, LumoraType::I32) => Ok(self
                .builder
                .build_int_truncate(
                    value.into_int_value(),
                    self.context.i32_type(),
                    "trunc_i64_to_i32",
                )?
                .into()),
            (LumoraType::I32, LumoraType::I64) => Ok(self
                .builder
                .build_int_s_extend(
                    value.into_int_value(),
                    self.context.i64_type(),
                    "sext_i32_to_i64",
                )?
                .into()),
            (LumoraType::F32, LumoraType::F64) => Ok(self
                .builder
                .build_float_ext(
                    value.into_float_value(),
                    self.context.f64_type(),
                    "f32_to_f64",
                )?
                .into()),
            (LumoraType::F64, LumoraType::F32) => Ok(self
                .builder
                .build_float_trunc(
                    value.into_float_value(),
                    self.context.f32_type(),
                    "f64_to_f32",
                )?
                .into()),
            _ => Err(LumoraError::CodegenError {
                code: "L070".to_string(),
                span: None,
                message: format!("Unsupported cast from {:?} to {:?}", from_type, to_type),
                help: None,
            }),
        }
    }
}
