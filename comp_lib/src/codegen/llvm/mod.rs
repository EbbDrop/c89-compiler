use crate::{diagnostic::AggregateResult, ir};

pub fn build_from_ir(ir: &ir::Root, filename: &str, source: &str) -> AggregateResult<String> {
    llvm_ir_builder::build(ir, filename, source)
}

mod llvm_ir_builder {
    use crate::{
        diagnostic::AggregateResult,
        ir::{self, ctype},
    };
    use lir::{ty::BitSize, AddressSignificance, Linkage};
    use llvm_ir::{
        self as lir,
        convert::{Into_, TryInto_},
        ty::Type,
        value::Value,
    };
    use std::collections::HashMap;

    type Result<T> = std::result::Result<T, String>;

    pub fn build(ir: &ir::Root, filename: &str, source: &str) -> AggregateResult<String> {
        let mut module = lir::Module::new("main".to_owned());
        module.set_source_filename(filename.to_owned());

        match MainBuilder::new(&mut module, source).build_from_ir(ir) {
            Ok(function) => {
                module
                    .define_function_named("main".into(), function)
                    .unwrap();
                // TODO: let compiler front-end determine the fmt opts
                AggregateResult::new_ok(format!("{}", module.display(lir::FmtOpts::default())))
            }
            Err(msg) => panic!("ICE: {msg}"),
        }
    }

    struct MainBuilder<'m, 's> {
        module: &'m mut lir::Module,
        function: lir::FunctionDefinitionBuilder,
        // Maps ir ItemId to the ptr we got from Alloca
        symbol_table: HashMap<ir::table::ItemId, lir::value::Register<lir::ty::Pointer>>,
        printf: (lir::FunctionDeclarationHandle, lir::constant::Pointer),
        printf_int: lir::constant::Pointer,
        printf_float: lir::constant::Pointer,
        printf_ptr: lir::constant::Pointer,
        continue_label_stack: Vec<lir::constant::Label>,
        break_label_stack: Vec<lir::constant::Label>,
        source: &'s str,
    }

    impl<'m, 's> MainBuilder<'m, 's> {
        fn new(module: &'m mut lir::Module, source: &'s str) -> Self {
            let function =
                lir::FunctionDeclaration::new(lir::ty::I16.into()).to_definition_builder();

            let printf = {
                let mut printf = lir::FunctionDeclaration::new(lir::ty::I32.into());
                printf.add_param(lir::ty::Pointer::new_literal().build());
                printf.is_vararg = true;
                printf.linkage = Linkage::External;
                printf.address_significance = Some(AddressSignificance::Unnamed);
                module
                    .declare_function_named("printf".into(), printf)
                    .unwrap()
            };

            let mut declare_printf_template = |name: &str, template: &str| {
                let constant = lir::constant::Array::new_char_array_from_str(template);
                let global_var = lir::GlobalVarDefinition::new_constant(constant)
                    .with_linkage(Linkage::Private)
                    .with_address_significance(AddressSignificance::Unnamed);
                module
                    .define_global_var_named(name.into(), global_var)
                    .unwrap()
            };

            let (_, printf_int) = declare_printf_template(".printf_int", "%i\n\0");
            let (_, printf_float) = declare_printf_template(".printf_float", "%f\n\0");
            let (_, printf_ptr) = declare_printf_template(".printf_ptr", "%p\n\0");

            Self {
                module,
                function,
                symbol_table: HashMap::new(),
                printf,
                printf_int,
                printf_float,
                printf_ptr,
                continue_label_stack: Vec::new(),
                break_label_stack: Vec::new(),
                source,
            }
        }

        fn build_from_ir(mut self, root: &ir::Root) -> Result<lir::FunctionDefinition> {
            self.allocate_items(&root.table)?;
            self.add_block(&root.global)?;
            Ok(self.function.build())
        }

        fn allocate_items(&mut self, table: &ir::table::Table) -> Result<()> {
            for (item_id, item) in table.iter() {
                let comment = format!(
                    " allocation of: {} {}",
                    item.ty,
                    &self.source[std::ops::Range::from(item.original_span)]
                );
                self.function.add_comment(comment);

                let ty = self.ctype_to_llvm_type(&item.ty);

                let ptr = self
                    .function
                    .add_instruction(lir::instruction::Alloca { ty, amount: None })?;

                self.symbol_table.insert(item_id, ptr.into_());
            }
            Ok(())
        }

        fn get_symbol(
            &mut self,
            item_id: &ir::table::ItemId,
        ) -> Option<lir::value::Register<lir::ty::Pointer>> {
            self.symbol_table.get(item_id).cloned()
        }

        fn add_block(&mut self, block: &ir::BlockNode) -> Result<()> {
            for stmt_node in &block.stmts {
                self.add_stmt_node(stmt_node)?;
            }
            Ok(())
        }

        fn add_stmt_node(&mut self, stmt_node: &ir::StmtNode) -> Result<()> {
            if let Some(comment) = &stmt_node.comments {
                self.function.add_comment(comment.clone());
            }
            for line in self.source[std::ops::Range::from(stmt_node.span)].lines() {
                self.function.add_comment(format!(";; {line}"));
            }
            match &stmt_node.stmt {
                ir::Stmt::Expr(node) => {
                    self.add_expr_node(node)?;
                }
                ir::Stmt::Printf(node) => self.add_printf(node)?,
                ir::Stmt::IfStmt(node) => self.add_if_stmt_node(node)?,
                ir::Stmt::SwitchStmt(node) => self.add_switch_stmt_node(node)?,
                ir::Stmt::LoopStmt(node) => self.add_loop_stmt_node(node)?,
                ir::Stmt::Break => self.add_break_stmt_node()?,
                ir::Stmt::Continue => self.add_continue_stmt_node()?,
                ir::Stmt::Return(node) => self.add_return_stmt_node(node)?,
            };
            Ok(())
        }

        fn add_if_stmt_node(&mut self, if_stmt_node: &ir::IfStmtNode) -> Result<()> {
            // Create a label for the block where execution continues after either executing the
            // if-branch or the else-branch.
            let label_end = self.function.declare_block();
            let label_if_branch = self.function.declare_block();
            // Either the label of the else-branch, or `label_end`.
            let dest_false = match if_stmt_node.else_branch {
                Some(_) => self.function.declare_block(),
                None => label_end.clone(),
            };

            let cond = {
                let res = self.add_expr_node(&if_stmt_node.condition)?;
                self.add_truthy_check(res)?
            };

            self.function.terminate_and_start_declared_block(
                lir::instruction::BranchConditional {
                    cond,
                    dest_true: label_if_branch.clone(),
                    dest_false: dest_false.clone(),
                },
                label_if_branch,
            )?;

            // Add if branch
            self.add_block(&if_stmt_node.if_branch)?;

            // Add optional else branch
            if let Some(else_branch) = &if_stmt_node.else_branch {
                self.function.jump_start_declared_block(dest_false)?;
                self.add_block(else_branch)?;
            }

            self.function.jump_start_declared_block(label_end)?;

            Ok(())
        }

        fn add_switch_stmt_node(&mut self, switch_stmt_node: &ir::SwitchStmtNode) -> Result<()> {
            let control_value: lir::value::Integer =
                self.add_expr_node(&switch_stmt_node.expr)?.try_into()?;
            let control_value_ty = control_value.ty();

            let mut branches: Vec<(lir::constant::Integer, lir::constant::Label)> = Vec::new();
            let mut default_target: Option<lir::constant::Label> = None;

            let mut bodies: Vec<(lir::constant::Label, &ir::BlockNode)> = Vec::new();

            let mut last_alias = None;
            for case in &switch_stmt_node.cases {
                let body = match &case.data {
                    ir::SwitchStmtCase::Case { body, .. } => body,
                    ir::SwitchStmtCase::Default { body } => body,
                };
                let label = if body.stmts.is_empty() {
                    last_alias
                        .get_or_insert_with(|| self.function.declare_block())
                        .clone()
                } else {
                    last_alias
                        .take()
                        .unwrap_or_else(|| self.function.declare_block())
                };
                match &case.data {
                    ir::SwitchStmtCase::Case { label: value, .. } => {
                        let int_const = lir::constant::Integer::new::<lir::ty::Integer>(
                            control_value_ty.clone(),
                            *value,
                        );
                        branches.push((int_const, label.clone()));
                    }
                    ir::SwitchStmtCase::Default { .. } => default_target = Some(label.clone()),
                }
                if !body.stmts.is_empty() {
                    bodies.push((label, body));
                }
            }

            if bodies.is_empty() {
                return Ok(());
            }

            let label_end = last_alias
                .take()
                .unwrap_or_else(|| self.function.declare_block());

            self.function.terminate_block(lir::instruction::Switch {
                value: control_value,
                default_dest: default_target.take().unwrap_or(label_end.clone()),
                branches,
            })?;

            self.break_label_stack.push(label_end.clone());

            for (label, block_node) in bodies {
                self.function.jump_start_declared_block(label)?;
                self.add_block(block_node)?;
            }

            self.function.jump_start_declared_block(label_end)?;

            self.break_label_stack.pop().unwrap();

            Ok(())
        }

        fn add_loop_stmt_node(&mut self, loop_stmt_node: &ir::LoopStmtNode) -> Result<()> {
            let label_loop_start = self.function.jump_start_block()?;

            let label_break = self.function.declare_block();
            let label_continue = match &loop_stmt_node.continuation {
                Some(_) => self.function.declare_block(),
                None => label_loop_start.clone(),
            };

            if let Some(condition_node) = &loop_stmt_node.condition {
                let cond = {
                    let res = self.add_expr_node(condition_node)?;
                    self.add_truthy_check(res)?
                };
                let label_body_start = self.function.declare_block();
                self.function.terminate_and_start_declared_block(
                    lir::instruction::BranchConditional {
                        cond,
                        dest_true: label_body_start.clone(),
                        dest_false: label_break.clone(),
                    },
                    label_body_start,
                )?;
            }

            // NOTE: this needs to be set *after* executing the condition. Technically this
            // shouldn't make a difference since the condition can never contain a jump statement.
            self.continue_label_stack.push(label_continue.clone());
            self.break_label_stack.push(label_break.clone());

            self.add_block(&loop_stmt_node.body)?;

            if let Some(continuation_node) = &loop_stmt_node.continuation {
                self.function.jump_start_declared_block(label_continue)?;
                self.add_expr_node(continuation_node)?;
            }

            self.function
                .terminate_block_with_branch_to(label_loop_start)?;

            self.function.jump_start_declared_block(label_break)?;

            self.continue_label_stack.pop().unwrap();
            self.break_label_stack.pop().unwrap();

            Ok(())
        }

        fn add_break_stmt_node(&mut self) -> Result<()> {
            self.function.terminate_block_with_branch_to(
                self.break_label_stack
                    .last()
                    .expect("break statements can only appear within loops")
                    .clone(),
            )
        }

        fn add_continue_stmt_node(&mut self) -> Result<()> {
            self.function.terminate_block_with_branch_to(
                self.continue_label_stack
                    .last()
                    .expect("continue statements can only appear within loops")
                    .clone(),
            )
        }

        fn add_return_stmt_node(&mut self, expr_node: &Option<ir::ExprNode>) -> Result<()> {
            match expr_node {
                Some(expr_node) => {
                    let return_value = self.add_expr_node(expr_node)?;
                    self.function
                        .terminate_block(lir::instruction::Return(return_value))
                }
                None => self.function.terminate_block(lir::instruction::ReturnVoid),
            }
        }

        fn add_printf(&mut self, expr_node: &ir::ExprNode) -> Result<()> {
            let value: lir::value::Primitive = self.add_expr_node(expr_node)?.try_into()?;

            let str_pointer = match &value {
                lir::value::Primitive::Integer(_) => self.printf_int.clone(),
                lir::value::Primitive::FloatingPoint(_) => self.printf_float.clone(),
                lir::value::Primitive::Pointer(_) => self.printf_ptr.clone(),
            };

            let (fn_handle, fn_pointer) = self.printf.clone();

            self.function
                .add_maybe_yielding_instruction(lir::instruction::Call {
                    calling_conv: Default::default(),
                    fn_ty: self.module.get_function_declaration(fn_handle).ty(),
                    fn_pointer: fn_pointer.into(),
                    fn_args: vec![str_pointer.into(), value.into()],
                })
                .map(|_| ())
        }

        fn add_expr_node(&mut self, expr_node: &ir::ExprNode) -> Result<lir::value::Element> {
            use ir::Expr as E;
            Ok(match &expr_node.expr {
                E::Constant(constant) => self.add_constant_expr_node(expr_node, constant)?.into(),
                E::Cast(node) => self.add_cast_expr_node(expr_node, node)?,
                E::UnaryArith(op, node) => self.add_unary_expr_node(expr_node, op, node)?,
                E::LvalueDeref(lv_node) => self.add_dereference_lvalue_node(lv_node)?,
                E::PostfixInc(lv_node) => self.add_postfix_inc_expr_node(lv_node)?.into(),
                E::PostfixDec(lv_node) => self.add_postfix_dec_expr_node(lv_node)?.into(),
                E::PrefixInc(lv_node) => self.add_prefix_inc_expr_node(lv_node)?.into(),
                E::PrefixDec(lv_node) => self.add_prefix_dec_expr_node(lv_node)?.into(),
                E::Reference(lv_node) => self.add_reference_lvalue_node(lv_node)?.into(),
                E::Binary(lhs, op, rhs) => self.add_binary_expr_node(expr_node, op, lhs, rhs)?,
                E::Relation(lhs, op, rhs) => {
                    self.add_relation_expr_node(expr_node, op, lhs, rhs)?.into()
                }
                E::LogicalAnd(lhs, rhs) => {
                    self.add_logical_and_expr_node(expr_node, lhs, rhs)?.into()
                }
                E::LogicalOr(lhs, rhs) => {
                    self.add_logical_or_expr_node(expr_node, lhs, rhs)?.into()
                }
                E::Assign(lv_node, rhs_node) => self.add_assign_expr_node(lv_node, rhs_node)?,
            })
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Dereference
        ////////////////////////////////////////////////////////////////////////////////////////////

        /// Loads the value that the specified lvalue refers to.
        fn add_dereference_lvalue_node(
            &mut self,
            lvalue_node: &ir::LvalueExprNode,
        ) -> Result<lir::value::Element> {
            let ty = self.ctype_to_llvm_type(&lvalue_node.ty);
            let pointer = self.add_reference_lvalue_node(lvalue_node)?;
            self.add_load_from_ptr(pointer, ty).map(Into::into)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Reference
        ////////////////////////////////////////////////////////////////////////////////////////////

        /// Gives the pointer that corresponds to the lvalue.
        fn add_reference_lvalue_node(
            &mut self,
            lvalue_node: &ir::LvalueExprNode,
        ) -> Result<lir::value::Pointer> {
            match &lvalue_node.expr {
                ir::LvalueExpr::Ident(item_id) => Ok(self
                    .get_symbol(item_id)
                    .expect("all used identifiers should have been allocated already")
                    .into()),
                ir::LvalueExpr::Dereference(inner) => {
                    let element = self.add_expr_node(inner)?;
                    Ok(element.try_into()?)
                }
            }
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Assign
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_assign_expr_node(
            &mut self,
            lvalue_node: &ir::LvalueExprNode,
            rhs_node: &ir::ExprNode,
        ) -> Result<lir::value::Element> {
            // Left-hand side of the assignment is executed first
            let pointer = self.add_reference_lvalue_node(lvalue_node)?;
            let rhs = self.add_expr_node(rhs_node)?;
            self.function
                .add_void_instruction(lir::instruction::Store {
                    value: rhs.clone(),
                    pointer,
                })?;
            Ok(rhs)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Constant
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_constant_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            constant: &ir::Constant,
        ) -> Result<lir::constant::Element> {
            Ok(match constant {
                ir::Constant::Integer(v) => lir::constant::Integer::new(
                    match outer_node.ty {
                        ctype::CType::Scalar(ctype::Scalar::Arithmetic(a)) => {
                            lir::ty::Integer::new_literal(a.size_in_bits())?
                        }
                        ctype::CType::Scalar(ctype::Scalar::Pointer(_, _)) => unreachable!(),
                    },
                    *v,
                )
                .into(),
                ir::Constant::Float(f) => match outer_node.ty {
                    ctype::CType::Scalar(ctype::Scalar::Arithmetic(a)) => match a {
                        ctype::Arithmetic::Float => {
                            lir::constant::FloatingPoint::new_float(*f as f32)
                        }
                        ctype::Arithmetic::Double => lir::constant::FloatingPoint::new_double(*f),
                        ctype::Arithmetic::LongDouble => {
                            lir::constant::FloatingPoint::new_double(*f)
                        }
                        _ => unreachable!(),
                    },
                    ctype::CType::Scalar(ctype::Scalar::Pointer(_, _)) => unreachable!(),
                }
                .into(),
                ir::Constant::String(string) => {
                    // CType of string literals is always a char array pointer
                    let mut char_array = string.clone();
                    char_array.push(0u8); // C strings always implicitly end with a NULL byte
                    let constant = lir::constant::Array::new_char_array(char_array);
                    // C string literals are constant and identical string literals may be merged.
                    let global_var = lir::GlobalVarDefinition::new_constant(constant)
                        .with_linkage(lir::Linkage::Private)
                        .with_address_significance(lir::AddressSignificance::Unnamed);
                    self.module.define_global_var(global_var).1.into()
                }
            })
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Cast
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_cast_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            inner_node: &ir::ExprNode,
        ) -> Result<lir::value::Element> {
            let to_ty = self.ctype_to_llvm_type(&outer_node.ty);
            let is_to_type_signed = self.is_ctype_signed(&outer_node.ty)?;
            let is_from_type_signed = self.is_ctype_signed(&inner_node.ty)?;
            let inner = self.add_expr_node(inner_node)?;

            use lir::ty;
            use lir::value::{Element, Primitive, Single};
            let cast_instruction: lir::instruction::YieldingInstruction = match inner {
                Element::Single(Single::Primitive(from)) => {
                    let ty::Element::Single(ty::Single::Primitive(to)) = to_ty else { unreachable!("primitive's should only be casted to other primitive's") };
                    match (from, to) {
                        (Primitive::Integer(value), ty::Primitive::Integer(to_ty)) => {
                            return self
                                .add_int_cast(value, to_ty, is_from_type_signed)
                                .map(Into::into);
                        }
                        (Primitive::Integer(value), ty::Primitive::FloatingPoint(to_ty)) => {
                            match is_from_type_signed {
                                true => {
                                    lir::instruction::cast::SiToFp { value, to_ty }.try_into_()?
                                }
                                false => {
                                    lir::instruction::cast::UiToFp { value, to_ty }.try_into_()?
                                }
                            }
                        }
                        (Primitive::Integer(value), ty::Primitive::Pointer(to_ty)) => {
                            lir::instruction::cast::IntToPtr { value, to_ty }.try_into_()?
                        }
                        (Primitive::FloatingPoint(value), ty::Primitive::Integer(to_ty)) => {
                            match is_to_type_signed {
                                true => {
                                    lir::instruction::cast::FpToSi { value, to_ty }.try_into_()?
                                }
                                false => {
                                    lir::instruction::cast::FpToUi { value, to_ty }.try_into_()?
                                }
                            }
                        }
                        (Primitive::FloatingPoint(value), ty::Primitive::FloatingPoint(to_ty)) => {
                            match value.ty().bit_size().cmp(&to_ty.bit_size()) {
                                std::cmp::Ordering::Less => {
                                    lir::instruction::cast::FpExt { value, to_ty }.try_into_()?
                                }
                                std::cmp::Ordering::Equal => return Ok(value.into()),
                                std::cmp::Ordering::Greater => {
                                    lir::instruction::cast::FpTrunc { value, to_ty }.try_into_()?
                                }
                            }
                        }
                        (Primitive::FloatingPoint(_), ty::Primitive::Pointer(_)) => unreachable!(),
                        (Primitive::Pointer(value), ty::Primitive::Integer(to_ty)) => {
                            lir::instruction::cast::PtrToInt { value, to_ty }.try_into_()?
                        }
                        (Primitive::Pointer(_), ty::Primitive::FloatingPoint(_)) => unreachable!(),
                        (Primitive::Pointer(value), ty::Primitive::Pointer(to_ty)) => {
                            if value.ty().equiv_to(&to_ty) {
                                return Ok(value.into());
                            } else {
                                unreachable!()
                            }
                        }
                    }
                }
                Element::Single(Single::Vector(_)) => unreachable!(),
                Element::Aggregate(_) => todo!(),
            };
            Ok(self
                .function
                .add_instruction(cast_instruction)?
                .try_into()?)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Unary
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_unary_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            op: &ir::UnaryOp,
            inner_node: &ir::ExprNode,
        ) -> Result<lir::value::Element> {
            let inner = self.add_expr_node(inner_node)?;
            match op {
                ir::UnaryOp::Neg => self
                    .add_unary_neg_expr_node(inner.try_into()?)
                    .map(Into::into),
                ir::UnaryOp::BitNot => self
                    .add_unary_bit_not_expr_node(inner.try_into()?)
                    .map(Into::into),
                ir::UnaryOp::Not => self
                    .add_unary_not_expr_node(outer_node, inner.try_into()?)
                    .map(Into::into),
            }
        }

        fn add_unary_neg_expr_node(
            &mut self,
            inner: lir::value::Primitive,
        ) -> Result<lir::value::Primitive> {
            use lir::value::Primitive;
            match inner {
                Primitive::Integer(operand2) => self
                    .function
                    .add_instruction(lir::instruction::binary_op::Int {
                        operator: lir::instruction::BinaryIntOp::Sub,
                        operand1: lir::constant::Integer::new(operand2.ty(), 0).into(),
                        operand2,
                    })
                    .map(Into::into),
                Primitive::FloatingPoint(operand) => self
                    .function
                    .add_instruction(lir::instruction::unary_op::Fp {
                        operator: lir::instruction::UnaryFpOp::Fneg,
                        operand,
                    })
                    .map(Into::into),
                Primitive::Pointer(_) => todo!(),
            }
        }

        fn add_unary_bit_not_expr_node(
            &mut self,
            inner: lir::value::Integer,
        ) -> Result<lir::value::Integer> {
            self.function
                .add_instruction(lir::instruction::binary_op::Int {
                    operator: lir::instruction::BinaryIntOp::Xor,
                    operand1: lir::constant::Integer::new(inner.ty(), -1).into(),
                    operand2: inner,
                })
                .map(Into::into)
        }

        fn add_unary_not_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            inner: lir::value::Single,
        ) -> Result<lir::value::Integer> {
            let result = self.add_falsy_check(inner.into())?;
            self.add_cast_from_bool_to_integer_ctype(result, &outer_node.ty)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Binary
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_binary_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            op: &ir::BinaryOp,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> Result<lir::value::Element> {
            use lir::instruction::BinaryIntOp;
            Ok(match op {
                ir::BinaryOp::Mul => self.add_binary_mul_expr_node(lhs_node, rhs_node)?.into(),
                ir::BinaryOp::Div => self
                    .add_binary_div_expr_node(outer_node, lhs_node, rhs_node)?
                    .into(),
                ir::BinaryOp::Rem => self
                    .add_binary_rem_expr_node(outer_node, lhs_node, rhs_node)?
                    .into(),
                ir::BinaryOp::Add => self.add_binary_add_expr_node(lhs_node, rhs_node)?.into(),
                ir::BinaryOp::Sub => {
                    self.add_binary_sub_expr_node(outer_node, lhs_node, rhs_node)?
                }
                ir::BinaryOp::ShiftLeft => self
                    .add_binary_integer_instruction(BinaryIntOp::Shl, lhs_node, rhs_node)?
                    .into(),
                ir::BinaryOp::ShiftRight => {
                    // NOTE: The shift right operation in C is implementation defined as to whether it it is
                    // logical or arithmetic. This implementation uses logical shift right.
                    self.add_binary_integer_instruction(BinaryIntOp::Lshr, lhs_node, rhs_node)?
                        .into()
                }
                ir::BinaryOp::Bitwise(bitwise_op) => match bitwise_op {
                    ir::BitwiseOp::And => self
                        .add_binary_integer_instruction(BinaryIntOp::And, lhs_node, rhs_node)?
                        .into(),
                    ir::BitwiseOp::Or => self
                        .add_binary_integer_instruction(BinaryIntOp::Or, lhs_node, rhs_node)?
                        .into(),
                    ir::BitwiseOp::Xor => self
                        .add_binary_integer_instruction(BinaryIntOp::Xor, lhs_node, rhs_node)?
                        .into(),
                },
            })
        }

        fn add_binary_add_expr_node(
            &mut self,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> Result<lir::value::Primitive> {
            let lhs = self.add_expr_node(lhs_node)?.try_into()?;
            let rhs = self.add_expr_node(rhs_node)?.try_into()?;

            use lir::value::Primitive;
            match (lhs, rhs) {
                (Primitive::Integer(operand1), Primitive::Integer(operand2)) => self
                    .function
                    .add_instruction(lir::instruction::binary_op::Int {
                        operator: lir::instruction::BinaryIntOp::Add,
                        operand1,
                        operand2,
                    })
                    .map(Into::into),
                (Primitive::FloatingPoint(operand1), Primitive::FloatingPoint(operand2)) => self
                    .function
                    .add_instruction(lir::instruction::binary_op::Fp {
                        operator: lir::instruction::BinaryFpOp::Fadd,
                        operand1,
                        operand2,
                    })
                    .map(Into::into),
                (Primitive::Integer(index), Primitive::Pointer(pointer)) => self
                    .function
                    .add_instruction(lir::instruction::GetElementPtr {
                        ty: self.ctype_ptr_inner_to_llvm_type(&rhs_node.ty)?,
                        pointer,
                        indices: vec![index],
                    })
                    .map(Into::into),
                (Primitive::Pointer(pointer), Primitive::Integer(index)) => self
                    .function
                    .add_instruction(lir::instruction::GetElementPtr {
                        ty: self.ctype_ptr_inner_to_llvm_type(&lhs_node.ty)?,
                        pointer,
                        indices: vec![index],
                    })
                    .map(Into::into),
                _ => unreachable!(),
            }
        }

        fn add_binary_sub_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> Result<lir::value::Element> {
            let lhs = self.add_expr_node(lhs_node)?.try_into()?;
            let rhs = self.add_expr_node(rhs_node)?.try_into()?;

            use lir::value::Primitive;
            match (lhs, rhs) {
                (Primitive::Integer(operand1), Primitive::Integer(operand2)) => self
                    .function
                    .add_instruction(lir::instruction::binary_op::Int {
                        operator: lir::instruction::BinaryIntOp::Sub,
                        operand1,
                        operand2,
                    })
                    .map(Into::into),
                (Primitive::FloatingPoint(operand1), Primitive::FloatingPoint(operand2)) => self
                    .function
                    .add_instruction(lir::instruction::binary_op::Fp {
                        operator: lir::instruction::BinaryFpOp::Fsub,
                        operand1,
                        operand2,
                    })
                    .map(Into::into),
                (Primitive::Pointer(pointer), Primitive::Integer(integer)) => {
                    let neg_index =
                        self.function
                            .add_instruction(lir::instruction::binary_op::Int {
                                operator: lir::instruction::BinaryIntOp::Sub,
                                operand1: lir::constant::Integer::new(integer.ty(), 0).into(),
                                operand2: integer,
                            })?;
                    self.function
                        .add_instruction(lir::instruction::GetElementPtr {
                            ty: self.ctype_ptr_inner_to_llvm_type(&lhs_node.ty)?,
                            pointer,
                            indices: vec![neg_index.into()],
                        })
                        .map(Into::into)
                }
                (Primitive::Pointer(ptr1), Primitive::Pointer(ptr2)) => {
                    let to_ty: lir::ty::Integer =
                        self.ctype_to_llvm_type(&outer_node.ty).try_into()?;
                    let operand1 =
                        self.function
                            .add_instruction(lir::instruction::cast::PtrToInt {
                                value: ptr1,
                                to_ty: to_ty.clone(),
                            })?;
                    let operand2 = self
                        .function
                        .add_instruction(lir::instruction::cast::PtrToInt { value: ptr2, to_ty })?;
                    let byte_diff =
                        self.function
                            .add_instruction(lir::instruction::binary_op::Int {
                                operator: lir::instruction::BinaryIntOp::Sub,
                                operand1,
                                operand2,
                            })?;
                    let ptr_size = self.retrieve_ptr_size(byte_diff.ty())?;
                    let element_diff =
                        self.function
                            .add_instruction(lir::instruction::binary_op::Int {
                                operator: lir::instruction::BinaryIntOp::Sdiv,
                                operand1: byte_diff.into(),
                                operand2: ptr_size,
                            })?;
                    let out_ty: lir::ty::Integer =
                        self.ctype_to_llvm_type(&outer_node.ty).try_into()?;
                    self.add_int_cast(element_diff.into(), out_ty, true)
                        .map(Into::into)
                }
                _ => unreachable!(),
            }
        }

        fn add_binary_mul_expr_node(
            &mut self,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> Result<lir::value::Primitive> {
            let lhs = self.add_expr_node(lhs_node)?.try_into()?;
            let rhs = self.add_expr_node(rhs_node)?.try_into()?;

            use lir::value::Primitive;
            match (lhs, rhs) {
                (Primitive::Integer(operand1), Primitive::Integer(operand2)) => self
                    .function
                    .add_instruction(lir::instruction::binary_op::Int {
                        operator: lir::instruction::BinaryIntOp::Mul,
                        operand1,
                        operand2,
                    })
                    .map(Into::into),
                (Primitive::FloatingPoint(operand1), Primitive::FloatingPoint(operand2)) => self
                    .function
                    .add_instruction(lir::instruction::binary_op::Fp {
                        operator: lir::instruction::BinaryFpOp::Fmul,
                        operand1,
                        operand2,
                    })
                    .map(Into::into),
                _ => unreachable!(),
            }
        }

        fn add_binary_div_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> Result<lir::value::Primitive> {
            let lhs = self.add_expr_node(lhs_node)?.try_into()?;
            let rhs = self.add_expr_node(rhs_node)?.try_into()?;

            use lir::value::Primitive;
            match (lhs, rhs) {
                (Primitive::Integer(operand1), Primitive::Integer(operand2)) => self
                    .function
                    .add_instruction(lir::instruction::binary_op::Int {
                        operator: match outer_node.ty {
                            ctype::CType::Scalar(ctype::Scalar::Arithmetic(a)) => {
                                match a.is_signed() {
                                    true => lir::instruction::BinaryIntOp::Sdiv,
                                    false => lir::instruction::BinaryIntOp::Udiv,
                                }
                            }
                            _ => unreachable!(),
                        },
                        operand1,
                        operand2,
                    })
                    .map(Into::into),
                (Primitive::FloatingPoint(operand1), Primitive::FloatingPoint(operand2)) => self
                    .function
                    .add_instruction(lir::instruction::binary_op::Fp {
                        operator: lir::instruction::BinaryFpOp::Fdiv,
                        operand1,
                        operand2,
                    })
                    .map(Into::into),
                _ => unreachable!(),
            }
        }

        fn add_binary_rem_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> Result<lir::value::Primitive> {
            let lhs = self.add_expr_node(lhs_node)?.try_into()?;
            let rhs = self.add_expr_node(rhs_node)?.try_into()?;

            use lir::value::Primitive;
            match (lhs, rhs) {
                (Primitive::Integer(operand1), Primitive::Integer(operand2)) => self
                    .function
                    .add_instruction(lir::instruction::binary_op::Int {
                        operator: match outer_node.ty {
                            ctype::CType::Scalar(ctype::Scalar::Arithmetic(a)) => {
                                match a.is_signed() {
                                    true => lir::instruction::BinaryIntOp::Srem,
                                    false => lir::instruction::BinaryIntOp::Urem,
                                }
                            }
                            _ => unreachable!(),
                        },
                        operand1,
                        operand2,
                    })
                    .map(Into::into),
                (Primitive::FloatingPoint(operand1), Primitive::FloatingPoint(operand2)) => self
                    .function
                    .add_instruction(lir::instruction::binary_op::Fp {
                        operator: lir::instruction::BinaryFpOp::Frem,
                        operand1,
                        operand2,
                    })
                    .map(Into::into),
                _ => unreachable!(),
            }
        }

        fn add_binary_integer_instruction(
            &mut self,
            operator: lir::instruction::BinaryIntOp,
            lhs: &ir::ExprNode,
            rhs: &ir::ExprNode,
        ) -> Result<lir::value::Integer> {
            let operand1 = self.add_expr_node(lhs)?.try_into()?;
            let operand2 = self.add_expr_node(rhs)?.try_into()?;

            self.function
                .add_instruction(lir::instruction::binary_op::Int::<lir::value::Integer> {
                    operator,
                    operand1,
                    operand2,
                })
                .map(Into::into)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Relation
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_relation_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            relation_op: &ir::RelationOp,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> Result<lir::value::Integer> {
            let lhs = self.add_expr_node(lhs_node)?.try_into()?;
            let rhs = self.add_expr_node(rhs_node)?.try_into()?;

            use lir::value::Primitive;
            let boolean = match (lhs, rhs) {
                (Primitive::Integer(operand1), Primitive::Integer(operand2)) => {
                    let signed = self.is_ctype_signed(&lhs_node.ty)?;
                    let operator = match (relation_op, signed) {
                        (ir::RelationOp::Eq, _) => lir::instruction::IcmpCond::Eq,
                        (ir::RelationOp::Ne, _) => lir::instruction::IcmpCond::Ne,
                        (ir::RelationOp::Lt, true) => lir::instruction::IcmpCond::Slt,
                        (ir::RelationOp::Gt, true) => lir::instruction::IcmpCond::Sgt,
                        (ir::RelationOp::Ge, true) => lir::instruction::IcmpCond::Sge,
                        (ir::RelationOp::Le, true) => lir::instruction::IcmpCond::Sle,
                        (ir::RelationOp::Lt, false) => lir::instruction::IcmpCond::Ult,
                        (ir::RelationOp::Gt, false) => lir::instruction::IcmpCond::Ugt,
                        (ir::RelationOp::Ge, false) => lir::instruction::IcmpCond::Uge,
                        (ir::RelationOp::Le, false) => lir::instruction::IcmpCond::Ule,
                    };
                    self.function
                        .add_instruction(lir::instruction::compare::Int {
                            operator,
                            operand1,
                            operand2,
                        })
                }
                (Primitive::FloatingPoint(operand1), Primitive::FloatingPoint(operand2)) => {
                    let operator = match relation_op {
                        ir::RelationOp::Eq => lir::instruction::FcmpCond::Oeq,
                        ir::RelationOp::Ne => lir::instruction::FcmpCond::One,
                        ir::RelationOp::Lt => lir::instruction::FcmpCond::Olt,
                        ir::RelationOp::Gt => lir::instruction::FcmpCond::Ogt,
                        ir::RelationOp::Ge => lir::instruction::FcmpCond::Oge,
                        ir::RelationOp::Le => lir::instruction::FcmpCond::Ole,
                    };
                    self.function
                        .add_instruction(lir::instruction::compare::Fp {
                            operator,
                            operand1,
                            operand2,
                        })
                }
                _ => unreachable!(),
            };

            self.add_cast_from_bool_to_integer_ctype(boolean?.into(), &outer_node.ty)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Logical and
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_logical_and_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> Result<lir::value::Integer> {
            // Create a label placeholder for the block were execution continues hereafter
            let label_end = self.function.declare_block();
            // Create a label placeholder for the block that will execute rhs (will only be branched
            // to of the lhs was truthy)
            let label_execute_rhs = self.function.declare_block();

            // Execute & check lhs
            let lhs_result = {
                let res = self.add_expr_node(lhs_node)?;
                self.add_truthy_check(res)?
            };

            // The label of the block containing the last instruction of the computation of the lhs,
            // that will end with the conditional branch specified below.
            let label_end_lhs = self.function.get_or_set_block_label();

            // Skip the execution of the rhs if the lhs is falsy
            self.function.terminate_and_start_declared_block(
                lir::instruction::BranchConditional {
                    cond: lhs_result,
                    dest_true: label_execute_rhs.clone(),
                    dest_false: label_end.clone(),
                },
                label_execute_rhs,
            )?;

            // Execute & check rhs
            let rhs_result = {
                let res = self.add_expr_node(rhs_node)?;
                self.add_truthy_check(res)?
            };

            // The label of the block containing the last instructionn of the computation of the
            // rhs, that will end with the unconditional branch specified below.
            let label_end_rhs = self.function.get_or_set_block_label();

            self.function.jump_start_declared_block(label_end)?;

            let result = self.function.add_instruction(lir::instruction::Phi {
                head: (lir::constant::Boolean::new(false).into(), label_end_lhs),
                tail: vec![(rhs_result, label_end_rhs)],
            })?;

            self.add_cast_from_bool_to_integer_ctype(result.into(), &outer_node.ty)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Logical or
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_logical_or_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> Result<lir::value::Integer> {
            // Create a label placeholder for the block were execution continues hereafter
            let label_end = self.function.declare_block();
            // Create a label placeholder for the block that will execute rhs (will only be branched
            // to of the lhs was falsy)
            let label_execute_rhs = self.function.declare_block();

            // Execute & check lhs
            let lhs_result = {
                let res = self.add_expr_node(lhs_node)?;
                self.add_truthy_check(res)?
            };

            // The label of the block containing the last instruction of the computation of the lhs,
            // that will end with the conditional branch specified below.
            let label_end_lhs = self.function.get_or_set_block_label();

            // Skip the execution of the rhs if the lhs is truthy
            self.function.terminate_and_start_declared_block(
                lir::instruction::BranchConditional {
                    cond: lhs_result,
                    dest_true: label_end.clone(),
                    dest_false: label_execute_rhs.clone(),
                },
                label_execute_rhs,
            )?;

            // Execute & check rhs
            let rhs_result = {
                let res = self.add_expr_node(rhs_node)?;
                self.add_truthy_check(res)?
            };

            // The label of the block containing the last instructionn of the computation of the
            // rhs, that will end with the unconditional branch specified below.
            let label_end_rhs = self.function.get_or_set_block_label();

            self.function.jump_start_declared_block(label_end)?;

            let result = self.function.add_instruction(lir::instruction::Phi {
                head: (lir::constant::Boolean::new(true).into(), label_end_lhs),
                tail: vec![(rhs_result, label_end_rhs)],
            })?;

            self.add_cast_from_bool_to_integer_ctype(result.into(), &outer_node.ty)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Prefix increment & decrement, Postfix increment & decrement
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_prefix_inc_expr_node(
            &mut self,
            lvalue_node: &ir::LvalueExprNode,
        ) -> Result<lir::value::Primitive> {
            use lir::instruction::{BinaryFpOp as FpOp, BinaryIntOp as IntOp};
            self.add_prepostfix_incdec_expr_node(lvalue_node, IntOp::Add, 1, FpOp::Fadd, 1., 1)
                .map(|(_, new_value)| new_value)
        }

        fn add_prefix_dec_expr_node(
            &mut self,
            lvalue_node: &ir::LvalueExprNode,
        ) -> Result<lir::value::Primitive> {
            use lir::instruction::{BinaryFpOp as FpOp, BinaryIntOp as IntOp};
            self.add_prepostfix_incdec_expr_node(lvalue_node, IntOp::Sub, 1, FpOp::Fsub, 1., -1)
                .map(|(_, new_value)| new_value)
        }

        fn add_postfix_inc_expr_node(
            &mut self,
            lvalue_node: &ir::LvalueExprNode,
        ) -> Result<lir::value::Primitive> {
            use lir::instruction::{BinaryFpOp as FpOp, BinaryIntOp as IntOp};
            self.add_prepostfix_incdec_expr_node(lvalue_node, IntOp::Add, 1, FpOp::Fadd, 1., 1)
                .map(|(value, _)| value)
        }

        fn add_postfix_dec_expr_node(
            &mut self,
            lvalue_node: &ir::LvalueExprNode,
        ) -> Result<lir::value::Primitive> {
            use lir::instruction::{BinaryFpOp as FpOp, BinaryIntOp as IntOp};
            self.add_prepostfix_incdec_expr_node(lvalue_node, IntOp::Sub, 1, FpOp::Fsub, 1., -1)
                .map(|(value, _)| value)
        }

        fn add_prepostfix_incdec_expr_node(
            &mut self,
            lvalue_node: &ir::LvalueExprNode,
            int_op: lir::instruction::BinaryIntOp,
            int_constant: i128,
            fp_op: lir::instruction::BinaryFpOp,
            fp_constant: f32,
            ptr_offset: i128,
        ) -> Result<(lir::value::Primitive, lir::value::Primitive)> {
            let value_ty: lir::ty::Primitive =
                self.ctype_to_llvm_type(&lvalue_node.ty).try_into()?;
            let pointer = self.add_reference_lvalue_node(lvalue_node)?;
            let value: lir::value::Primitive =
                self.add_load_from_ptr(pointer.clone(), value_ty)?.into();

            use lir::value::Primitive;
            let new_value: lir::value::Primitive = match value.clone() {
                Primitive::Integer(integer) => {
                    let operand2 = lir::constant::Integer::new(integer.ty(), int_constant).into();
                    self.function
                        .add_instruction(lir::instruction::binary_op::Int {
                            operator: int_op,
                            operand1: integer,
                            operand2,
                        })?
                        .into()
                }
                Primitive::FloatingPoint(fp) => {
                    let operand2 = match fp.ty() {
                        lir::ty::FloatingPoint::Float(_) => {
                            lir::constant::FloatingPoint::new_float(fp_constant).into()
                        }
                        lir::ty::FloatingPoint::Double(_) => {
                            lir::constant::FloatingPoint::new_double(fp_constant as _).into()
                        }
                        _ => unreachable!(),
                    };
                    self.function
                        .add_instruction(lir::instruction::binary_op::Fp {
                            operator: fp_op,
                            operand1: fp,
                            operand2,
                        })?
                        .into()
                }
                Primitive::Pointer(pointer) => self
                    .function
                    .add_instruction(lir::instruction::GetElementPtr {
                        ty: self.ctype_ptr_inner_to_llvm_type(&lvalue_node.ty)?,
                        pointer,
                        indices: vec![lir::constant::Integer::new(lir::ty::I32, ptr_offset).into()],
                    })?
                    .into(),
            };

            self.function
                .add_void_instruction(lir::instruction::Store {
                    value: new_value.clone(),
                    pointer,
                })?;

            Ok((value, new_value))
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Utility methods
        ////////////////////////////////////////////////////////////////////////////////////////////

        /// Adds a cast instruction from the boolean to the llvm type corresponding to the ctype.
        /// Doesn't insert a cast if the ctype is `1` bit wide.
        ///
        /// Assumes a non-integer ctype is unreachable.
        ///
        /// Returns the boolean value from the inserted cast, or the original boolean if no cast was
        /// inserted.
        fn add_cast_from_bool_to_integer_ctype(
            &mut self,
            boolean: lir::value::Boolean,
            ctype: &ctype::CType,
        ) -> Result<lir::value::Integer> {
            match ctype {
                ctype::CType::Scalar(ctype::Scalar::Arithmetic(a)) => {
                    let target_bit_size = a.size_in_bits();
                    if target_bit_size != 1 {
                        return self
                            .function
                            .add_instruction(lir::instruction::cast::ZextInt {
                                value: boolean,
                                to_ty: lir::ty::Integer::new_literal(target_bit_size)?,
                            })
                            .map(Into::into);
                    }
                }
                ctype::CType::Scalar(ctype::Scalar::Pointer(_, _)) => unreachable!(),
            }
            Ok(boolean.into())
        }

        fn add_int_cast(
            &mut self,
            value: lir::value::Integer,
            to_ty: lir::ty::Integer,
            sign_extend: bool,
        ) -> Result<lir::value::Integer> {
            match value.ty().bit_size().cmp(&to_ty.bit_size()) {
                std::cmp::Ordering::Less => match sign_extend {
                    true => self
                        .function
                        .add_instruction(lir::instruction::cast::SextInt { value, to_ty })
                        .map(Into::into),
                    false => self
                        .function
                        .add_instruction(lir::instruction::cast::ZextInt { value, to_ty })
                        .map(Into::into),
                },
                std::cmp::Ordering::Equal => Ok(value),
                std::cmp::Ordering::Greater => self
                    .function
                    .add_instruction(lir::instruction::cast::TruncInt { value, to_ty })
                    .map(Into::into),
            }
        }

        /// Returns an `i1` which is `true` if the inner is truthy (according to c).
        fn add_truthy_check(&mut self, inner: lir::value::Element) -> Result<lir::value::Boolean> {
            let inner_ty = inner.ty();

            if inner_ty.equiv_to(&lir::ty::I1.into()) {
                return Ok(inner.try_into().expect(
                    "integers with type i1 should be losslessly convertible to boolean values",
                ));
            }

            use lir::value::{Element, Primitive, Single};
            match inner {
                Element::Single(single) => match single {
                    Single::Primitive(primitive) => match primitive {
                        Primitive::Integer(integer) => self
                            .function
                            .add_instruction(lir::instruction::compare::Int {
                                operator: lir::instruction::IcmpCond::Ne,
                                operand1: lir::constant::Integer::new(integer.ty(), 0).into(),
                                operand2: integer,
                            })
                            .map(Into::into),
                        Primitive::FloatingPoint(fp) => self
                            .function
                            .add_instruction(lir::instruction::compare::Fp {
                                // unordered neq, because NaN is truthy
                                operator: lir::instruction::FcmpCond::Une,
                                operand1: lir::constant::FloatingPoint::zero_typed(fp.ty()).into(),
                                operand2: fp,
                            })
                            .map(Into::into),
                        Primitive::Pointer(ptr) => self
                            .function
                            .add_instruction(lir::instruction::compare::Ptr {
                                operator: lir::instruction::IcmpCond::Ne,
                                operand1: lir::constant::Pointer::NULL.into(),
                                operand2: ptr,
                            })
                            .map(Into::into),
                    },
                    Single::Vector(_) => unreachable!(),
                },
                Element::Aggregate(_) => todo!(),
            }
        }

        /// Returns an `i1` which is `true` if the inner is falsy (according to c).
        ///
        /// Prefer `add_truthy_check` if possible, because it doesn't have to insert an instruction
        /// if the inner type is already `i1`.
        fn add_falsy_check(&mut self, inner: lir::value::Element) -> Result<lir::value::Boolean> {
            use lir::value::{Element, Primitive, Single};
            let cmp_instr: lir::instruction::YieldingInstruction = match inner {
                Element::Single(single) => match single {
                    Single::Primitive(primitive) => match primitive {
                        Primitive::Integer(integer) => lir::instruction::compare::Int {
                            operator: lir::instruction::IcmpCond::Eq,
                            operand1: lir::constant::Integer::new(integer.ty(), 0).into(),
                            operand2: integer,
                        }
                        .try_into_()?,
                        Primitive::FloatingPoint(fp) => lir::instruction::compare::Fp {
                            // unordered neq, because NaN is truthy
                            operator: lir::instruction::FcmpCond::Oeq,
                            operand1: lir::constant::FloatingPoint::zero_typed(fp.ty()).into(),
                            operand2: fp,
                        }
                        .try_into_()?,
                        Primitive::Pointer(ptr) => lir::instruction::compare::Ptr {
                            operator: lir::instruction::IcmpCond::Eq,
                            operand1: lir::constant::Pointer::NULL.into(),
                            operand2: ptr,
                        }
                        .try_into_()?,
                    },
                    Single::Vector(_) => unreachable!(),
                },
                Element::Aggregate(_) => todo!(),
            };
            Ok(self.function.add_instruction(cmp_instr)?.try_into()?)
        }

        fn retrieve_ptr_size(&mut self, size_ty: lir::ty::Integer) -> Result<lir::value::Integer> {
            let size_as_ptr = self
                .function
                .add_instruction(lir::instruction::GetElementPtr {
                    ty: lir::ty::Pointer::new_literal().build(),
                    pointer: lir::constant::Pointer::NULL.into(),
                    indices: vec![lir::constant::Integer::new(lir::ty::I1, 1).into()],
                })?;
            self.function
                .add_instruction(lir::instruction::cast::PtrToInt {
                    value: size_as_ptr,
                    to_ty: size_ty,
                })
                .map(Into::into)
        }

        /// Adds an llvm instruction that loads data of the type `ty` at the address `pointer`.
        fn add_load_from_ptr<T: lir::ty::ElementType>(
            &mut self,
            pointer: lir::value::Pointer,
            ty: T,
        ) -> Result<lir::value::Register<T>> {
            self.function
                .add_instruction(lir::instruction::Load { ty, pointer })
        }

        fn ctype_to_llvm_type(&self, ctype: &ctype::CType) -> lir::ty::Element {
            match ctype {
                ctype::CType::Scalar(scalar) => match scalar {
                    ctype::Scalar::Arithmetic(arithmetic) => match arithmetic {
                        ctype::Arithmetic::Float => lir::ty::Float::new_literal().into(),
                        ctype::Arithmetic::Double => lir::ty::Double::new_literal().into(),
                        ctype::Arithmetic::LongDouble => lir::ty::Double::new_literal().into(),
                        ctype::Arithmetic::Char
                        | ctype::Arithmetic::SignedChar
                        | ctype::Arithmetic::SignedShortInt
                        | ctype::Arithmetic::SignedInt
                        | ctype::Arithmetic::SignedLongInt
                        | ctype::Arithmetic::UnsignedChar
                        | ctype::Arithmetic::UnsignedShortInt
                        | ctype::Arithmetic::UnsignedInt
                        | ctype::Arithmetic::UnsignedLongInt => lir::ty::Integer::new_literal(
                            arithmetic.size_in_bits(),
                        )
                        .expect("all c integer bit sizes should be valid llvm integer bit sizes")
                        .into(),
                    },
                    ctype::Scalar::Pointer(_, _) => lir::ty::Pointer::new_literal().build().into(),
                },
            }
        }

        /// Converts a ctype that is known to be a pointer to the llvm equivalent of the type it
        /// points to. Returns an Err(()) if the ctype is not a ptr.
        fn ctype_ptr_inner_to_llvm_type(&self, ctype: &ctype::CType) -> Result<lir::ty::Element> {
            match ctype {
                ctype::CType::Scalar(scalar) => match scalar {
                    ctype::Scalar::Arithmetic(_) => {
                        Err("cannot retrieve inner type of non-pointer type")?
                    }
                    ctype::Scalar::Pointer(inner, _) => Ok(self.ctype_to_llvm_type(inner)),
                },
            }
        }

        /// Treats ptrs as unsigned, will return Err for aggregates
        fn is_ctype_signed(&self, ctype: &ir::ctype::CType) -> Result<bool> {
            match ctype {
                ctype::CType::Scalar(ctype::Scalar::Arithmetic(a)) => Ok(a.is_signed()),
                ctype::CType::Scalar(ctype::Scalar::Pointer(_, _)) => Ok(false),
            }
        }
    }
}
