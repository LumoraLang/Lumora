extern crate libc;
use crate::ast::{BinaryOp, Expr, Function, LumoraType, Program, Stmt, TopLevelDeclaration};
use crate::errors::LumoraError;
use inkwell::Either;
use inkwell::IntPredicate;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::module::Module;
use inkwell::types::{AsTypeRef, BasicType, BasicTypeEnum};
use inkwell::values::AsValueRef;
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use llvm_sys::core::LLVMBuildLoad2;
use std::collections::HashMap;

pub struct CodeGenerator<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, (PointerValue<'ctx>, LumoraType)>,
    functions: HashMap<String, FunctionValue<'ctx>>,
    all_functions: HashMap<String, (Vec<LumoraType>, LumoraType)>,
    bb_counter: usize,
}

impl<'ctx> CodeGenerator<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module_name: &str,
        all_functions: HashMap<String, (Vec<LumoraType>, LumoraType)>,
    ) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        Self {
            context,
            module,
            builder,
            variables: HashMap::new(),
            functions: HashMap::new(),
            all_functions,
            bb_counter: 0,
        }
    }

    pub fn generate(&mut self, program: &Program) -> Result<String, LumoraError> {
        for (name, (param_types, return_type)) in &self.all_functions {
            let fn_type = {
                let param_llvm_types: Vec<inkwell::types::BasicMetadataTypeEnum<'ctx>> =
                    param_types
                        .iter()
                        .map(|ty| self.type_to_llvm_type(ty).into())
                        .collect();
                match return_type {
                    LumoraType::Void => self.context.void_type().fn_type(&param_llvm_types, false),
                    _ => self
                        .type_to_llvm_type(return_type)
                        .fn_type(&param_llvm_types, false),
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
                let alloca = self
                    .builder
                    .build_alloca(self.type_to_llvm_type(ty), name)?;
                let val = self.generate_expression(value)?;
                self.builder.build_store(alloca, val)?;
                self.variables.insert(name.clone(), (alloca, ty.clone()));
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

                let _ = self.builder.build_conditional_branch(
                    cond_val.into_int_value(),
                    then_bb,
                    else_bb,
                );

                self.builder.position_at_end(then_bb);
                for stmt in then_block {
                    self.generate_statement(stmt)?;
                }
                let then_block_terminated = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_some();

                self.builder.position_at_end(else_bb);
                if let Some(else_stmts) = else_block {
                    for stmt in else_stmts {
                        self.generate_statement(stmt)?;
                    }
                }
                let else_block_terminated = self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_some();

                let create_cont_bb = !then_block_terminated || !else_block_terminated;
                let cont_bb = if create_cont_bb {
                    Some(
                        self.context
                            .append_basic_block(fn_val, &format!("cont{}", current_bb_id)),
                    )
                } else {
                    None
                };

                if !then_block_terminated {
                    if let Some(cont) = cont_bb {
                        self.builder.position_at_end(then_bb);
                        let _ = self.builder.build_unconditional_branch(cont);
                    }
                }

                if !else_block_terminated {
                    if let Some(cont) = cont_bb {
                        self.builder.position_at_end(else_bb);
                        let _ = self.builder.build_unconditional_branch(cont);
                    }
                }

                if let Some(cont) = cont_bb {
                    self.builder.position_at_end(cont);
                }
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

                    let final_val = if returned_expr_type == LumoraType::I64
                        && function_return_type == LumoraType::I32
                    {
                        self.builder
                            .build_int_truncate(
                                val.into_int_value(),
                                self.context.i32_type(),
                                "trunc_i64_to_i32",
                            )?
                            .into()
                    } else {
                        val
                    };
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

    fn generate_expression(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, LumoraError> {
        match expr {
            Expr::Integer(n) => Ok(self.context.i64_type().const_int(*n as u64, true).into()),
            Expr::Float(f) => Ok(self.context.f64_type().const_float(*f).into()),
            Expr::Boolean(b) => Ok(self.context.bool_type().const_int(*b as u64, false).into()),
            Expr::Null => Ok(self.context.ptr_type(0.into()).const_null().into()),
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
                let (ptr, lumora_type) = self.variables.get(name).unwrap();
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
                }
            }
            Expr::Binary { left, op, right } => {
                let left_val = self.generate_expression(left)?;
                let right_val = self.generate_expression(right)?;

                let left_lumora_type = self.type_of_expression(left)?;
                let right_lumora_type = self.type_of_expression(right)?;

                if left_val.is_int_value() && right_val.is_int_value() {
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

                let (_, lumora_return_type) = self.all_functions.get(name).unwrap();
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
        }
    }

    fn type_to_llvm_type(&self, ty: &LumoraType) -> BasicTypeEnum<'ctx> {
        match ty {
            LumoraType::I32 => self.context.i32_type().into(),
            LumoraType::I64 => self.context.i64_type().into(),
            LumoraType::F64 => self.context.f64_type().into(),
            LumoraType::Bool => self.context.bool_type().into(),
            LumoraType::String => self.context.ptr_type(0.into()).into(),
            LumoraType::Void => {
                panic!("Void is not a basic type and cannot be converted to BasicTypeEnum")
            }
            LumoraType::Array(inner_type) => self
                .type_to_llvm_type(inner_type)
                .into_pointer_type()
                .into(),
            LumoraType::Null => self.context.ptr_type(0.into()).into(),
        }
    }

    fn type_of_expression(&self, expr: &Expr) -> Result<LumoraType, LumoraError> {
        match expr {
            Expr::Integer(_) => Ok(LumoraType::I64),
            Expr::Float(_) => Ok(LumoraType::F64),
            Expr::Boolean(_) => Ok(LumoraType::Bool),
            Expr::StringLiteral(_) => Ok(LumoraType::String),
            Expr::Null => Ok(LumoraType::Null),
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
        }
    }
}
