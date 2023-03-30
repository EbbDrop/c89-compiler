use crate::{diagnostic::AggregateResult, ir};

pub fn build_from_ir(ir: &ir::Root, filename: &str, source: &str) -> AggregateResult<String> {
    llvm_ir_builder::build(ir, filename, source)
}

mod llvm_ir_builder {
    use crate::{
        diagnostic::AggregateResult,
        ir::{self, ctype},
    };
    use llvm_ir::{self as lir, IntoTyped, TryIntoTyped};
    use std::collections::HashMap;

    pub fn build(ir: &ir::Root, filename: &str, source: &str) -> AggregateResult<String> {
        let mut module =
            lir::Module::new("main".to_owned().try_into().unwrap(), filename.to_owned());

        let main_id = module
            .add_global_identifier("main".to_owned().try_into().unwrap())
            .unwrap();

        let main_builder =
            lir::FunctionDefinitionBuilder::new(main_id, lir::ty::Void.into()).start_body();

        match FunctionBuilder::new(&mut module, main_builder, source).build_from_ir(ir) {
            Ok(()) => AggregateResult::new_ok(format!("{}", module)),
            Err(()) => panic!("ICE"),
        }
    }

    type LlvmResult = lir::Result<lir::TypedValue>;

    struct FunctionBuilder<'m, 's> {
        module: &'m mut lir::Module,
        function: Option<lir::BasicBlockBuilder>,
        // Maps ir ItemId to the ptr (as TypedValue) we got from Alloca
        symbol_table: HashMap<ir::table::ItemId, lir::TypedValue>,
        printf_int: lir::id::GlobalName,
        printf_float: lir::id::GlobalName,
        printf_ptr: lir::id::GlobalName,
        source: &'s str,
    }

    impl<'m, 's> FunctionBuilder<'m, 's> {
        fn new(
            module: &'m mut lir::Module,
            handle: lir::FunctionBodyBuilder,
            source: &'s str,
        ) -> Self {
            let printf_int = module
                .add_global_identifier(lir::id::name(".printf_int"))
                .unwrap();
            let printf_float = module
                .add_global_identifier(lir::id::name(".printf_float"))
                .unwrap();
            let printf_ptr = module
                .add_global_identifier(lir::id::name(".printf_ptr"))
                .unwrap();
            Self {
                module,
                function: Some(handle.start_block()),
                symbol_table: HashMap::new(),
                printf_int,
                printf_float,
                printf_ptr,
                source,
            }
        }

        fn function_ref(&self) -> &lir::BasicBlockBuilder {
            self.function.as_ref().unwrap()
        }

        fn function_mut(&mut self) -> &mut lir::BasicBlockBuilder {
            self.function.as_mut().unwrap()
        }

        fn build_from_ir(mut self, root: &ir::Root) -> Result<(), ()> {
            self.allocate_items(&root.table)?;

            self.add_block(&root.global)?;

            // TEMP: for now always finish with a `ret void`
            // The below will panic if the function's return type isn't void.
            let function_definition = self
                .function
                .take()
                .unwrap()
                .terminate_block(lir::RawTerminatorInstruction::ReturnVoid.try_into()?)?
                .build()?;

            self.module.define_function(function_definition);

            Ok(())
        }

        fn allocate_items(&mut self, table: &ir::table::Table) -> Result<(), ()> {
            for (item_id, item) in table.iter() {
                let comment = format!(
                    " allocation of: {} {}",
                    item.ty,
                    &self.source[std::ops::Range::from(item.original_span)]
                );
                self.function_mut().add_comment(comment);
                let ty = self.ctype_to_llvm_type(&item.ty);
                let alloc_instr = lir::RawYieldingInstruction::Alloca { ty, amount: None };
                let ptr = self
                    .function_mut()
                    .add_yielding_instruction(alloc_instr.try_into()?)?;
                self.symbol_table.insert(item_id, ptr);
            }
            Ok(())
        }

        fn get_symbol(&mut self, item_id: &ir::table::ItemId) -> LlvmResult {
            self.symbol_table.get(item_id).cloned().ok_or(lir::Error)
        }

        fn add_block(&mut self, block: &ir::Block) -> Result<(), ()> {
            for stmt_node in &block.0 {
                self.add_stmt_node(stmt_node)?;
            }
            Ok(())
        }

        fn add_stmt_node(&mut self, stmt_node: &ir::StmtNode) -> Result<(), ()> {
            if let Some(comment) = &stmt_node.comments {
                self.function_mut().add_comment(comment.clone());
            }
            for line in self.source[std::ops::Range::from(stmt_node.span)].lines() {
                self.function_mut().add_comment(format!(";; {line}"));
            }
            match &stmt_node.stmt {
                ir::Stmt::Expr(expr_node) => {
                    self.add_expr_node(expr_node)?;
                }
                ir::Stmt::Printf(expr_node) => {
                    self.add_fprint(expr_node)?;
                }
            };
            Ok(())
        }

        fn add_fprint(&mut self, expr_node: &ir::ExprNode) -> LlvmResult {
            let value = self.add_expr_node(expr_node)?;
            let str_id = match value.ty().to_type_cat() {
                lir::ty::TypeCat::Void => return Err(lir::Error),
                lir::ty::TypeCat::Integer => self.printf_int.clone(),
                lir::ty::TypeCat::FloatingPoint => self.printf_float.clone(),
                lir::ty::TypeCat::Ptr => self.printf_ptr.clone(),
            };
            let printf_call = lir::RawYieldingInstruction::Printf {
                args: vec![str_id.into_typed(lir::ty::Ptr.into()), value],
            };
            self.function_mut()
                .add_yielding_instruction(printf_call.try_into()?)
        }

        fn add_expr_node(&mut self, expr_node: &ir::ExprNode) -> LlvmResult {
            match &expr_node.expr {
                ir::Expr::Constant(constant) => self.add_constant_expr_node(expr_node, constant),
                ir::Expr::Cast(node) => self.add_cast_expr_node(expr_node, node),
                ir::Expr::UnaryArith(op, node) => self.add_unary_expr_node(expr_node, op, node),
                ir::Expr::LvalueDeref(lvalue_node) => self.add_dereference_lvalue_node(lvalue_node),
                ir::Expr::PostfixInc(lvalue_node) => self.add_postfix_inc_expr_node(lvalue_node),
                ir::Expr::PostfixDec(lvalue_node) => self.add_postfix_dec_expr_node(lvalue_node),
                ir::Expr::PrefixInc(lvalue_node) => self.add_prefix_inc_expr_node(lvalue_node),
                ir::Expr::PrefixDec(lvalue_node) => self.add_prefix_dec_expr_node(lvalue_node),
                ir::Expr::Reference(lvalue_node) => self.add_reference_lvalue_node(lvalue_node),
                ir::Expr::Binary(lhs, op, rhs) => {
                    self.add_binary_expr_node(expr_node, op, lhs, rhs)
                }
                ir::Expr::Relation(lhs, op, rhs) => {
                    self.add_relation_expr_node(expr_node, op, lhs, rhs)
                }
                ir::Expr::LogicalAnd(lhs, rhs) => {
                    self.add_logical_and_expr_node(expr_node, lhs, rhs)
                }
                ir::Expr::LogicalOr(lhs, rhs) => self.add_logical_or_expr_node(expr_node, lhs, rhs),
                ir::Expr::Assign(lvalue_node, rhs_node) => {
                    self.add_assign_expr_node(lvalue_node, rhs_node)
                }
            }
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Dereference
        ////////////////////////////////////////////////////////////////////////////////////////////

        /// Loads the value that the specified lvalue refers to.
        fn add_dereference_lvalue_node(&mut self, lvalue_node: &ir::LvalueExprNode) -> LlvmResult {
            let ty = self.ctype_to_llvm_type(&lvalue_node.ty);
            let pointer = self.add_reference_lvalue_node(lvalue_node)?;
            self.add_load_from_ptr(pointer, ty)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Reference
        ////////////////////////////////////////////////////////////////////////////////////////////

        /// Gives the pointer that corresponds to the lvalue.
        fn add_reference_lvalue_node(&mut self, lvalue_node: &ir::LvalueExprNode) -> LlvmResult {
            match &lvalue_node.expr {
                ir::LvalueExpr::Ident(item_id) => self.get_symbol(item_id),
                ir::LvalueExpr::Dereference(inner) => self.add_expr_node(inner),
            }
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Assign
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_assign_expr_node(
            &mut self,
            lvalue_node: &ir::LvalueExprNode,
            rhs_node: &ir::ExprNode,
        ) -> LlvmResult {
            // Left-hand side of the assignment is executed first
            let lvalue = self.add_reference_lvalue_node(lvalue_node)?;
            let rhs = self.add_expr_node(rhs_node)?;
            let store_instr = lir::RawYieldlessInstruction::Store {
                value: rhs.clone(),
                pointer: lvalue,
            };
            self.function_mut()
                .add_yieldless_instruction(store_instr.try_into()?);
            Ok(rhs)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Constant
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_constant_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            constant: &ir::Constant,
        ) -> LlvmResult {
            match *constant {
                ir::Constant::Integer(v) => {
                    lir::Constant::Integer(v).try_into_typed(match outer_node.ty {
                        ctype::CType::Scalar(ctype::Scalar::Arithmetic(a)) => {
                            lir::ty::Int(a.size_in_bits()).into()
                        }
                        ctype::CType::Scalar(ctype::Scalar::Pointer(_, _)) => unreachable!(),
                    })
                }
                ir::Constant::Float(f) => {
                    lir::Constant::FloatingPoint(f).try_into_typed(match outer_node.ty {
                        ctype::CType::Scalar(ctype::Scalar::Arithmetic(a)) => match a {
                            ctype::Arithmetic::Float => lir::ty::Float.into(),
                            ctype::Arithmetic::Double => lir::ty::Double.into(),
                            ctype::Arithmetic::LongDouble => lir::ty::Double.into(),
                            _ => unreachable!(),
                        },
                        ctype::CType::Scalar(ctype::Scalar::Pointer(_, _)) => unreachable!(),
                    })
                }
            }
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Cast
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_cast_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            inner_node: &ir::ExprNode,
        ) -> LlvmResult {
            let to_ty = self.ctype_to_llvm_type(&outer_node.ty);
            let is_to_type_signed = self.is_ctype_signed(&outer_node.ty)?;
            let is_from_type_signed = self.is_ctype_signed(&inner_node.ty)?;
            let inner_tyval = self.add_expr_node(inner_node)?;
            use lir::ty::Type;
            let cast_op = match (inner_tyval.ty(), to_ty) {
                (Type::Int(f), Type::Int(t)) => match f.bits().cmp(&t.bits()) {
                    std::cmp::Ordering::Less => match is_from_type_signed {
                        true => lir::CastOp::Sext,
                        false => lir::CastOp::Zext,
                    },
                    std::cmp::Ordering::Equal => return Ok(inner_tyval),
                    std::cmp::Ordering::Greater => lir::CastOp::Trunc,
                },
                (Type::Int(_), Type::Float(_) | Type::Double(_)) => match is_from_type_signed {
                    true => lir::CastOp::Sitofp,
                    false => lir::CastOp::Uitofp,
                },
                (Type::Int(_), Type::Ptr(_)) => return self.cast_int_to_prt(inner_tyval, to_ty),
                (Type::Float(_) | Type::Double(_), Type::Int(_)) => match is_to_type_signed {
                    true => lir::CastOp::Fptosi,
                    false => lir::CastOp::Fptoui,
                },
                (Type::Float(_), Type::Float(_)) => return Ok(inner_tyval),
                (Type::Double(_), Type::Double(_)) => return Ok(inner_tyval),
                (Type::Ptr(_), Type::Ptr(_)) => return Ok(inner_tyval),
                (Type::Float(_), Type::Double(_)) => lir::CastOp::Fpext,
                (Type::Double(_), Type::Float(_)) => lir::CastOp::Fptrunc,
                (Type::Ptr(_), Type::Int(_)) => return self.cast_ptr_to_int(inner_tyval, to_ty),
                (Type::Float(_) | Type::Double(_), Type::Ptr(_)) => unreachable!(),
                (Type::Ptr(_), Type::Float(_) | Type::Double(_)) => unreachable!(),
                (Type::Void(_), _) | (_, Type::Void(_)) => unreachable!(),
            };

            let instr = lir::RawYieldingInstruction::Cast {
                op: cast_op,
                value: inner_tyval,
                to_ty,
            };

            self.function_mut()
                .add_yielding_instruction(instr.try_into()?)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Unary
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_unary_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            op: &ir::UnaryOp,
            inner_node: &ir::ExprNode,
        ) -> LlvmResult {
            let inner_tyval = self.add_expr_node(inner_node)?;
            match op {
                ir::UnaryOp::Neg => self.add_unary_neg_expr_node(inner_tyval),
                ir::UnaryOp::BitNot => self.add_unary_bit_not_expr_node(inner_tyval),
                ir::UnaryOp::Not => self.add_unary_not_expr_node(outer_node, inner_tyval),
            }
        }

        fn add_unary_neg_expr_node(&mut self, inner: lir::TypedValue) -> LlvmResult {
            use lir::ty::TypeCat;
            let instr = match inner.ty().to_type_cat() {
                TypeCat::Integer => lir::RawYieldingInstruction::BinaryOp {
                    op: lir::BinaryOp::Sub,
                    op1: lir::Constant::Integer(0).try_into_typed(inner.ty())?,
                    op2: inner,
                },
                TypeCat::FloatingPoint => lir::RawYieldingInstruction::UnaryOp {
                    op: lir::UnaryOp::Fneg,
                    op1: inner,
                },
                TypeCat::Ptr | TypeCat::Void => unreachable!(),
            };
            self.function_mut()
                .add_yielding_instruction(instr.try_into()?)
        }

        fn add_unary_bit_not_expr_node(&mut self, inner: lir::TypedValue) -> LlvmResult {
            let instr = lir::RawYieldingInstruction::BinaryOp {
                op: lir::BinaryOp::Xor,
                op1: lir::Constant::Integer(-1).try_into_typed(inner.ty())?,
                op2: inner,
            };
            self.function_mut()
                .add_yielding_instruction(instr.try_into()?)
        }

        fn add_unary_not_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            inner: lir::TypedValue,
        ) -> LlvmResult {
            let result = self.add_falsy_check(inner)?;
            self.add_cast_from_bool_to_arithmetic_ctype(result, &outer_node.ty)
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
        ) -> LlvmResult {
            match op {
                ir::BinaryOp::Mul => self.add_binary_mul_expr_node(lhs_node, rhs_node),
                ir::BinaryOp::Div => self.add_binary_div_expr_node(outer_node, lhs_node, rhs_node),
                ir::BinaryOp::Rem => self.add_binary_rem_expr_node(outer_node, lhs_node, rhs_node),
                ir::BinaryOp::Add => self.add_binary_add_expr_node(lhs_node, rhs_node),
                ir::BinaryOp::Sub => self.add_binary_sub_expr_node(outer_node, lhs_node, rhs_node),
                ir::BinaryOp::ShiftLeft => {
                    self.add_binary_integer_instruction(lir::BinaryOp::Shl, lhs_node, rhs_node)
                }
                ir::BinaryOp::ShiftRight => {
                    // NOTE: The shift right operation in C is implementation defined as to whether it it is
                    // logical or arithmetic. This implementation uses logical shift right.
                    self.add_binary_integer_instruction(lir::BinaryOp::Lshr, lhs_node, rhs_node)
                }
                ir::BinaryOp::Bitwise(bitwise_op) => match bitwise_op {
                    ir::BitwiseOp::And => {
                        self.add_binary_integer_instruction(lir::BinaryOp::And, lhs_node, rhs_node)
                    }
                    ir::BitwiseOp::Or => {
                        self.add_binary_integer_instruction(lir::BinaryOp::Or, lhs_node, rhs_node)
                    }
                    ir::BitwiseOp::Xor => {
                        self.add_binary_integer_instruction(lir::BinaryOp::Xor, lhs_node, rhs_node)
                    }
                },
            }
        }

        fn add_binary_add_expr_node(
            &mut self,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> LlvmResult {
            let lhs = self.add_expr_node(lhs_node)?;
            let rhs = self.add_expr_node(rhs_node)?;

            use lir::ty::TypeCat;
            let instr = match (lhs.ty().to_type_cat(), rhs.ty().to_type_cat()) {
                (TypeCat::Integer, TypeCat::Integer) => lir::RawYieldingInstruction::BinaryOp {
                    op: lir::BinaryOp::Add,
                    op1: lhs,
                    op2: rhs,
                },
                (TypeCat::FloatingPoint, TypeCat::FloatingPoint) => {
                    lir::RawYieldingInstruction::BinaryOp {
                        op: lir::BinaryOp::Fadd,
                        op1: lhs,
                        op2: rhs,
                    }
                }
                (TypeCat::Integer, TypeCat::Ptr) => lir::RawYieldingInstruction::GetElementPtr {
                    ty: self.ctype_ptr_inner_to_llvm_type(&rhs_node.ty)?,
                    ptrval: rhs,
                    index: lhs,
                },
                (TypeCat::Ptr, TypeCat::Integer) => lir::RawYieldingInstruction::GetElementPtr {
                    ty: self.ctype_ptr_inner_to_llvm_type(&lhs_node.ty)?,
                    ptrval: lhs,
                    index: rhs,
                },
                _ => unreachable!(),
            };

            self.function_mut()
                .add_yielding_instruction(instr.try_into()?)
        }

        fn add_binary_sub_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> LlvmResult {
            let lhs = self.add_expr_node(lhs_node)?;
            let rhs = self.add_expr_node(rhs_node)?;

            use lir::ty::TypeCat;
            let instr = match (lhs.ty().to_type_cat(), rhs.ty().to_type_cat()) {
                (TypeCat::Integer, TypeCat::Integer) => lir::RawYieldingInstruction::BinaryOp {
                    op: lir::BinaryOp::Sub,
                    op1: lhs,
                    op2: rhs,
                },
                (TypeCat::FloatingPoint, TypeCat::FloatingPoint) => {
                    lir::RawYieldingInstruction::BinaryOp {
                        op: lir::BinaryOp::Fsub,
                        op1: lhs,
                        op2: rhs,
                    }
                }
                (TypeCat::Ptr, TypeCat::Ptr) => {
                    let int_ty = self.ctype_to_llvm_type(&outer_node.ty);
                    let op1 = self.cast_ptr_to_int(lhs, int_ty)?;
                    let op2 = self.cast_ptr_to_int(rhs, int_ty)?;
                    let sub_instr = lir::RawYieldingInstruction::BinaryOp {
                        op: lir::BinaryOp::Sub,
                        op1,
                        op2,
                    };
                    let bit_diff = self
                        .function_mut()
                        .add_yielding_instruction(sub_instr.try_into()?)?;
                    lir::RawYieldingInstruction::BinaryOp {
                        op: lir::BinaryOp::Sdiv,
                        op1: bit_diff,
                        op2: lir::Constant::Integer(
                            self.ctype_ptr_inner_to_llvm_type(&lhs_node.ty)?
                                .bit_size()
                                .to_bytes_exact()
                                .unwrap()
                                .into(),
                        )
                        .try_into_typed(int_ty)?,
                    }
                }
                (TypeCat::Ptr, TypeCat::Integer) => {
                    let neg_index = self.function_mut().add_yielding_instruction(
                        lir::RawYieldingInstruction::BinaryOp {
                            op: lir::BinaryOp::Sub,
                            op1: lir::Constant::Integer(0).try_into_typed(rhs.ty())?,
                            op2: rhs,
                        }
                        .try_into()?,
                    )?;
                    lir::RawYieldingInstruction::GetElementPtr {
                        ty: self.ctype_ptr_inner_to_llvm_type(&lhs_node.ty)?,
                        ptrval: lhs,
                        index: neg_index,
                    }
                }
                _ => unreachable!(),
            };

            self.function_mut()
                .add_yielding_instruction(instr.try_into()?)
        }

        fn add_binary_mul_expr_node(
            &mut self,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> LlvmResult {
            let op1 = self.add_expr_node(lhs_node)?;
            let op2 = self.add_expr_node(rhs_node)?;

            use lir::ty::TypeCat;
            let op = match (op1.ty().to_type_cat(), op2.ty().to_type_cat()) {
                (TypeCat::Integer, TypeCat::Integer) => lir::BinaryOp::Mul,
                (TypeCat::FloatingPoint, TypeCat::FloatingPoint) => lir::BinaryOp::Fmul,
                _ => unreachable!(),
            };

            let instr = lir::RawYieldingInstruction::BinaryOp { op, op1, op2 };

            self.function_mut()
                .add_yielding_instruction(instr.try_into()?)
        }

        fn add_binary_div_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> LlvmResult {
            let op1 = self.add_expr_node(lhs_node)?;
            let op2 = self.add_expr_node(rhs_node)?;

            use lir::ty::TypeCat;
            let op = match (op1.ty().to_type_cat(), op2.ty().to_type_cat()) {
                (TypeCat::Integer, TypeCat::Integer) => match outer_node.ty {
                    ctype::CType::Scalar(ctype::Scalar::Arithmetic(a)) => match a.is_signed() {
                        true => lir::BinaryOp::Sdiv,
                        false => lir::BinaryOp::Udiv,
                    },
                    _ => unreachable!(),
                },
                (TypeCat::FloatingPoint, TypeCat::FloatingPoint) => lir::BinaryOp::Fdiv,
                _ => unreachable!(),
            };

            let instr = lir::RawYieldingInstruction::BinaryOp { op, op1, op2 };

            self.function_mut()
                .add_yielding_instruction(instr.try_into()?)
        }

        fn add_binary_rem_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> LlvmResult {
            let op1 = self.add_expr_node(lhs_node)?;
            let op2 = self.add_expr_node(rhs_node)?;

            use lir::ty::TypeCat;
            let op = match (op1.ty().to_type_cat(), op2.ty().to_type_cat()) {
                (TypeCat::Integer, TypeCat::Integer) => match outer_node.ty {
                    ctype::CType::Scalar(ctype::Scalar::Arithmetic(a)) => match a.is_signed() {
                        true => lir::BinaryOp::Srem,
                        false => lir::BinaryOp::Urem,
                    },
                    _ => unreachable!(),
                },
                (TypeCat::FloatingPoint, TypeCat::FloatingPoint) => lir::BinaryOp::Frem,
                _ => unreachable!(),
            };

            let instr = lir::RawYieldingInstruction::BinaryOp { op, op1, op2 };

            self.function_mut()
                .add_yielding_instruction(instr.try_into()?)
        }

        fn add_binary_integer_instruction(
            &mut self,
            op: lir::BinaryOp,
            lhs: &ir::ExprNode,
            rhs: &ir::ExprNode,
        ) -> LlvmResult {
            let op1 = self.add_expr_node(lhs)?;
            let op2 = self.add_expr_node(rhs)?;
            let instr = lir::RawYieldingInstruction::BinaryOp { op, op1, op2 };
            self.function_mut()
                .add_yielding_instruction(instr.try_into()?)
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
        ) -> LlvmResult {
            let op1 = self.add_expr_node(lhs_node)?;
            let op2 = self.add_expr_node(rhs_node)?;

            use lir::ty::TypeCat;
            let op = match (op1.ty().to_type_cat(), op2.ty().to_type_cat()) {
                (TypeCat::Integer, TypeCat::Integer) | (TypeCat::Ptr, TypeCat::Ptr) => {
                    let signed = self.is_ctype_signed(&lhs_node.ty)?;
                    lir::CompareOp::Icmp(match (relation_op, signed) {
                        (ir::RelationOp::Eq, _) => lir::IcmpCond::Eq,
                        (ir::RelationOp::Ne, _) => lir::IcmpCond::Ne,
                        (ir::RelationOp::Lt, true) => lir::IcmpCond::Slt,
                        (ir::RelationOp::Gt, true) => lir::IcmpCond::Sgt,
                        (ir::RelationOp::Ge, true) => lir::IcmpCond::Sge,
                        (ir::RelationOp::Le, true) => lir::IcmpCond::Sle,
                        (ir::RelationOp::Lt, false) => lir::IcmpCond::Ult,
                        (ir::RelationOp::Gt, false) => lir::IcmpCond::Ugt,
                        (ir::RelationOp::Ge, false) => lir::IcmpCond::Uge,
                        (ir::RelationOp::Le, false) => lir::IcmpCond::Ule,
                    })
                }
                (TypeCat::FloatingPoint, TypeCat::FloatingPoint) => {
                    lir::CompareOp::Fcmp(match relation_op {
                        ir::RelationOp::Eq => lir::FcmpCond::Oeq,
                        ir::RelationOp::Ne => lir::FcmpCond::One,
                        ir::RelationOp::Lt => lir::FcmpCond::Olt,
                        ir::RelationOp::Gt => lir::FcmpCond::Ogt,
                        ir::RelationOp::Ge => lir::FcmpCond::Oge,
                        ir::RelationOp::Le => lir::FcmpCond::Ole,
                    })
                }
                _ => unreachable!(),
            };

            let cmp_instr = lir::RawYieldingInstruction::Compare { op, op1, op2 };

            let result = self
                .function
                .as_mut()
                .unwrap()
                .add_yielding_instruction(cmp_instr.try_into()?)?;

            self.add_cast_from_bool_to_arithmetic_ctype(result, &outer_node.ty)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Logical and
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_logical_and_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> LlvmResult {
            // Create a label placeholder for the block were execution continues hereafter
            let label_continue = self.function_mut().create_label_placeholder();
            // Create a label placeholder for the block that will execute rhs (will only be branched
            // to of the lhs was falsy)
            let label_execute_rhs = self.function_mut().create_label_placeholder();

            // Execute & check lhs
            let lhs_result = {
                let res = self.add_expr_node(lhs_node)?;
                self.add_truthy_check(res)?
            };

            // Skip the execution of the rhs if the lhs is falsy
            let branch_instr = lir::RawTerminatorInstruction::BranchConditional {
                cond: lhs_result,
                if_dest: label_execute_rhs.clone().into(),
                else_dest: label_continue.clone().into(),
            };

            // The label of the block containing the last instruction of the computation of the lhs,
            // that will end with the conditional branch specified above.
            let label_end_lhs = self.function_ref().current_block_label();

            self.function = Some(
                self.function
                    .take()
                    .unwrap()
                    .terminate_block(branch_instr.try_into()?)?
                    .start_block_with_label(label_execute_rhs.into())?,
            );

            // Execute & check rhs
            let rhs_result = {
                let res = self.add_expr_node(rhs_node)?;
                self.add_truthy_check(res)?
            };

            // The label of the block containing the last instructionn of the computation of the
            // rhs, that will end with the unconditional branch specified below.
            let label_end_rhs = self.function_ref().current_block_label();

            self.function = Some(
                self.function
                    .take()
                    .unwrap()
                    .terminate_block(
                        lir::RawTerminatorInstruction::BranchUnconditional(
                            label_continue.clone().into(),
                        )
                        .try_into()?,
                    )?
                    .start_block_with_label(label_continue.into())?,
            );

            let result = self.function_mut().add_yielding_instruction(
                lir::RawYieldingInstruction::Phi {
                    args: vec![
                        (
                            lir::Constant::Boolean(false).try_into_typed(lir::ty::I1.into())?,
                            label_end_lhs,
                        ),
                        (rhs_result, label_end_rhs),
                    ],
                }
                .try_into()?,
            )?;

            self.add_cast_from_bool_to_arithmetic_ctype(result, &outer_node.ty)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Logical or
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_logical_or_expr_node(
            &mut self,
            outer_node: &ir::ExprNode,
            lhs_node: &ir::ExprNode,
            rhs_node: &ir::ExprNode,
        ) -> LlvmResult {
            // Create a label placeholder for the block were execution continues hereafter
            let label_continue = self.function_mut().create_label_placeholder();
            // Create a label placeholder for the block that will execute rhs (will only be branched
            // to of the lhs was falsy)
            let label_execute_rhs = self.function_mut().create_label_placeholder();

            // Execute & check lhs
            let lhs_result = {
                let res = self.add_expr_node(lhs_node)?;
                self.add_truthy_check(res)?
            };

            // Skip the execution of the rhs if the lhs is truthy
            let branch_instr = lir::RawTerminatorInstruction::BranchConditional {
                cond: lhs_result,
                if_dest: label_continue.clone().into(),
                else_dest: label_execute_rhs.clone().into(),
            };

            // The label of the block containing the last instruction of the computation of the lhs,
            // that will end with the conditional branch specified above.
            let label_end_lhs = self.function_ref().current_block_label();

            self.function = Some(
                self.function
                    .take()
                    .unwrap()
                    .terminate_block(branch_instr.try_into()?)?
                    .start_block_with_label(label_execute_rhs.into())?,
            );

            // Execute & check rhs
            let rhs_result = {
                let res = self.add_expr_node(rhs_node)?;
                self.add_truthy_check(res)?
            };

            // The label of the block containing the last instructionn of the computation of the
            // rhs, that will end with the unconditional branch specified below.
            let label_end_rhs = self.function_ref().current_block_label();

            self.function = Some(
                self.function
                    .take()
                    .unwrap()
                    .terminate_block(
                        lir::RawTerminatorInstruction::BranchUnconditional(
                            label_continue.clone().into(),
                        )
                        .try_into()?,
                    )?
                    .start_block_with_label(label_continue.into())?,
            );

            let result = self.function_mut().add_yielding_instruction(
                lir::RawYieldingInstruction::Phi {
                    args: vec![
                        (
                            lir::Constant::Boolean(true).try_into_typed(lir::ty::I1.into())?,
                            label_end_lhs,
                        ),
                        (rhs_result, label_end_rhs),
                    ],
                }
                .try_into()?,
            )?;

            self.add_cast_from_bool_to_arithmetic_ctype(result, &outer_node.ty)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Prefix increment
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_prefix_inc_expr_node(&mut self, lvalue_node: &ir::LvalueExprNode) -> LlvmResult {
            let value_ty = self.ctype_to_llvm_type(&lvalue_node.ty);
            let pointer = self.add_reference_lvalue_node(lvalue_node)?;
            let value = self.add_load_from_ptr(pointer.clone(), value_ty)?;

            use lir::ty::TypeCat;
            let inc_instr = match value.ty().to_type_cat() {
                TypeCat::Integer => lir::RawYieldingInstruction::BinaryOp {
                    op: lir::BinaryOp::Add,
                    op1: lir::Constant::Integer(1).try_into_typed(value.ty())?,
                    op2: value,
                },
                TypeCat::FloatingPoint => lir::RawYieldingInstruction::BinaryOp {
                    op: lir::BinaryOp::Fadd,
                    op1: lir::Constant::FloatingPoint(1.).try_into_typed(value.ty())?,
                    op2: value,
                },
                TypeCat::Ptr => lir::RawYieldingInstruction::GetElementPtr {
                    ty: self.ctype_to_llvm_type(&lvalue_node.ty),
                    ptrval: value,
                    index: lir::Constant::Integer(1).try_into_typed(lir::ty::I32.into())?,
                },
                _ => unreachable!(),
            };

            let incremented_value = self
                .function_mut()
                .add_yielding_instruction(inc_instr.try_into()?)?;

            let store_instr = lir::RawYieldlessInstruction::Store {
                value: incremented_value.clone(),
                pointer,
            };

            self.function_mut()
                .add_yieldless_instruction(store_instr.try_into()?);

            Ok(incremented_value)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Prefix decrement
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_prefix_dec_expr_node(&mut self, lvalue_node: &ir::LvalueExprNode) -> LlvmResult {
            let value_ty = self.ctype_to_llvm_type(&lvalue_node.ty);
            let pointer = self.add_reference_lvalue_node(lvalue_node)?;
            let value = self.add_load_from_ptr(pointer.clone(), value_ty)?;

            use lir::ty::TypeCat;
            let inc_instr = match value.ty().to_type_cat() {
                TypeCat::Integer => lir::RawYieldingInstruction::BinaryOp {
                    op: lir::BinaryOp::Sub,
                    op1: value,
                    op2: lir::Constant::Integer(1).try_into_typed(value_ty)?,
                },
                TypeCat::FloatingPoint => lir::RawYieldingInstruction::BinaryOp {
                    op: lir::BinaryOp::Fsub,
                    op1: value,
                    op2: lir::Constant::FloatingPoint(1.).try_into_typed(value_ty)?,
                },
                TypeCat::Ptr => lir::RawYieldingInstruction::GetElementPtr {
                    ty: self.ctype_to_llvm_type(&lvalue_node.ty),
                    ptrval: value,
                    index: lir::Constant::Integer(-1).try_into_typed(lir::ty::I32.into())?,
                },
                _ => unreachable!(),
            };

            let decremented_value = self
                .function_mut()
                .add_yielding_instruction(inc_instr.try_into()?)?;

            let store_instr = lir::RawYieldlessInstruction::Store {
                value: decremented_value.clone(),
                pointer,
            };

            self.function_mut()
                .add_yieldless_instruction(store_instr.try_into()?);

            Ok(decremented_value)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Postfix increment
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_postfix_inc_expr_node(&mut self, lvalue_node: &ir::LvalueExprNode) -> LlvmResult {
            let value_ty = self.ctype_to_llvm_type(&lvalue_node.ty);
            let pointer = self.add_reference_lvalue_node(lvalue_node)?;
            let value = self.add_load_from_ptr(pointer.clone(), value_ty)?;

            use lir::ty::TypeCat;
            let inc_instr = match value.ty().to_type_cat() {
                TypeCat::Integer => lir::RawYieldingInstruction::BinaryOp {
                    op: lir::BinaryOp::Add,
                    op1: lir::Constant::Integer(1).try_into_typed(value.ty())?,
                    op2: value.clone(),
                },
                TypeCat::FloatingPoint => lir::RawYieldingInstruction::BinaryOp {
                    op: lir::BinaryOp::Fadd,
                    op1: lir::Constant::FloatingPoint(1.).try_into_typed(value.ty())?,
                    op2: value.clone(),
                },
                TypeCat::Ptr => lir::RawYieldingInstruction::GetElementPtr {
                    ty: self.ctype_to_llvm_type(&lvalue_node.ty),
                    ptrval: value.clone(),
                    index: lir::Constant::Integer(1).try_into_typed(lir::ty::I32.into())?,
                },
                _ => unreachable!(),
            };

            let incremented_value = self
                .function_mut()
                .add_yielding_instruction(inc_instr.try_into()?)?;

            let store_instr = lir::RawYieldlessInstruction::Store {
                value: incremented_value,
                pointer,
            };

            self.function_mut()
                .add_yieldless_instruction(store_instr.try_into()?);

            Ok(value)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Postfix decrement
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn add_postfix_dec_expr_node(&mut self, lvalue_node: &ir::LvalueExprNode) -> LlvmResult {
            let value_ty = self.ctype_to_llvm_type(&lvalue_node.ty);
            let pointer = self.add_reference_lvalue_node(lvalue_node)?;
            let value = self.add_load_from_ptr(pointer.clone(), value_ty)?;

            use lir::ty::TypeCat;
            let inc_instr = match value.ty().to_type_cat() {
                TypeCat::Integer => lir::RawYieldingInstruction::BinaryOp {
                    op: lir::BinaryOp::Sub,
                    op1: value.clone(),
                    op2: lir::Constant::Integer(1).try_into_typed(value_ty)?,
                },
                TypeCat::FloatingPoint => lir::RawYieldingInstruction::BinaryOp {
                    op: lir::BinaryOp::Fsub,
                    op1: value.clone(),
                    op2: lir::Constant::FloatingPoint(1.).try_into_typed(value_ty)?,
                },
                TypeCat::Ptr => lir::RawYieldingInstruction::GetElementPtr {
                    ty: self.ctype_to_llvm_type(&lvalue_node.ty),
                    ptrval: value.clone(),
                    index: lir::Constant::Integer(-1).try_into_typed(lir::ty::I32.into())?,
                },
                _ => unreachable!(),
            };

            let decremented_value = self
                .function_mut()
                .add_yielding_instruction(inc_instr.try_into()?)?;

            let store_instr = lir::RawYieldlessInstruction::Store {
                value: decremented_value,
                pointer,
            };

            self.function_mut()
                .add_yieldless_instruction(store_instr.try_into()?);

            Ok(value)
        }

        ////////////////////////////////////////////////////////////////////////////////////////////
        // Utility methods
        ////////////////////////////////////////////////////////////////////////////////////////////

        fn cast_ptr_to_int(&mut self, ptr: lir::TypedValue, int_ty: lir::ty::Type) -> LlvmResult {
            self.function_mut().add_yielding_instruction(
                lir::RawYieldingInstruction::Cast {
                    op: lir::CastOp::Ptrtoint,
                    value: ptr,
                    to_ty: int_ty,
                }
                .try_into()?,
            )
        }

        fn cast_int_to_prt(&mut self, int: lir::TypedValue, ptr_ty: lir::ty::Type) -> LlvmResult {
            self.function_mut().add_yielding_instruction(
                lir::RawYieldingInstruction::Cast {
                    op: lir::CastOp::Inttoptr,
                    value: int,
                    to_ty: ptr_ty,
                }
                .try_into()?,
            )
        }

        /// Adds a cast instruction from the boolean to the llvm type corresponding to the ctype.
        /// Doesn't insert a cast if the ctype is `1` bit wide.
        ///
        /// Assumes a non-arithmetic ctype is unreachable.
        ///
        /// Returns the tyval from the inserted cast, or the original bool_tyval if no cast was
        /// inserted.
        fn add_cast_from_bool_to_arithmetic_ctype(
            &mut self,
            bool_tyval: lir::TypedValue,
            ctype: &ctype::CType,
        ) -> LlvmResult {
            match ctype {
                ctype::CType::Scalar(ctype::Scalar::Arithmetic(a)) => {
                    let target_bit_size = a.size_in_bits();
                    if target_bit_size != 1 {
                        let cast_instr = lir::RawYieldingInstruction::Cast {
                            op: lir::CastOp::Zext,
                            value: bool_tyval,
                            to_ty: lir::ty::Int(target_bit_size).into(),
                        };
                        return self
                            .function
                            .as_mut()
                            .unwrap()
                            .add_yielding_instruction(cast_instr.try_into()?);
                    }
                }
                ctype::CType::Scalar(ctype::Scalar::Pointer(_, _)) => unreachable!(),
            }
            Ok(bool_tyval)
        }

        /// Returns an `i1` which is `true` if the inner is truthy (according to c).
        fn add_truthy_check(&mut self, inner: lir::TypedValue) -> LlvmResult {
            if inner.ty() == lir::ty::I1.into() {
                return Ok(inner);
            }
            use lir::ty::TypeCat;
            let cmp_instr = match inner.ty().to_type_cat() {
                TypeCat::Void => unreachable!(),
                TypeCat::Integer => lir::RawYieldingInstruction::Compare {
                    op: lir::CompareOp::Icmp(lir::IcmpCond::Ne),
                    op1: lir::Constant::Integer(0).try_into_typed(inner.ty())?,
                    op2: inner,
                },
                TypeCat::FloatingPoint => lir::RawYieldingInstruction::Compare {
                    // unordered neq, because NaN is truthy
                    op: lir::CompareOp::Fcmp(lir::FcmpCond::Une),
                    op1: lir::Constant::FloatingPoint(0.).try_into_typed(inner.ty())?,
                    op2: inner,
                },
                TypeCat::Ptr => lir::RawYieldingInstruction::Compare {
                    op: lir::CompareOp::Icmp(lir::IcmpCond::Ne),
                    op1: lir::Constant::NullPointer.try_into_typed(inner.ty())?,
                    op2: inner,
                },
            };
            self.function_mut()
                .add_yielding_instruction(cmp_instr.try_into()?)
        }

        /// Returns an `i1` which is `true` if the inner is falsy (according to c).
        ///
        /// Prefer `add_truthy_check` if possible, because it doesn't have to insert an instruction
        /// if the inner type is already `i1`.
        fn add_falsy_check(&mut self, inner: lir::TypedValue) -> LlvmResult {
            use lir::ty::TypeCat;
            let cmp_instr = match inner.ty().to_type_cat() {
                TypeCat::Void => unreachable!(),
                TypeCat::Integer => lir::RawYieldingInstruction::Compare {
                    op: lir::CompareOp::Icmp(lir::IcmpCond::Eq),
                    op1: lir::Constant::Integer(0).try_into_typed(inner.ty())?,
                    op2: inner,
                },
                TypeCat::FloatingPoint => lir::RawYieldingInstruction::Compare {
                    // ordered eq, because NaN is truthy
                    op: lir::CompareOp::Fcmp(lir::FcmpCond::Oeq),
                    op1: lir::Constant::FloatingPoint(0.).try_into_typed(inner.ty())?,
                    op2: inner,
                },
                TypeCat::Ptr => lir::RawYieldingInstruction::Compare {
                    op: lir::CompareOp::Icmp(lir::IcmpCond::Eq),
                    op1: lir::Constant::NullPointer.try_into_typed(inner.ty())?,
                    op2: inner,
                },
            };
            self.function_mut()
                .add_yielding_instruction(cmp_instr.try_into()?)
        }

        fn add_load_from_ptr(&mut self, pointer: lir::TypedValue, ty: lir::ty::Type) -> LlvmResult {
            let load_instr = lir::RawYieldingInstruction::Load { ty, pointer }.try_into()?;
            self.function_mut().add_yielding_instruction(load_instr)
        }

        fn ctype_to_llvm_type(&self, ctype: &ctype::CType) -> lir::ty::Type {
            match ctype {
                ctype::CType::Scalar(scalar) => match scalar {
                    ctype::Scalar::Arithmetic(arithmetic) => match arithmetic {
                        ctype::Arithmetic::Float => lir::ty::Float.into(),
                        ctype::Arithmetic::Double => lir::ty::Double.into(),
                        ctype::Arithmetic::LongDouble => lir::ty::Double.into(),
                        ctype::Arithmetic::Char
                        | ctype::Arithmetic::SignedChar
                        | ctype::Arithmetic::SignedShortInt
                        | ctype::Arithmetic::SignedInt
                        | ctype::Arithmetic::SignedLongInt
                        | ctype::Arithmetic::UnsignedChar
                        | ctype::Arithmetic::UnsignedShortInt
                        | ctype::Arithmetic::UnsignedInt
                        | ctype::Arithmetic::UnsignedLongInt => {
                            lir::ty::Int(arithmetic.size_in_bits()).into()
                        }
                    },
                    ctype::Scalar::Pointer(_, _) => lir::ty::Ptr.into(),
                },
            }
        }

        /// Converts a ctype that is known to be a pointer to the llvm equivalent of the type it
        /// points to. Returns an Err(()) if the ctype is not a ptr.
        fn ctype_ptr_inner_to_llvm_type(&self, ctype: &ctype::CType) -> Result<lir::ty::Type, ()> {
            match ctype {
                ctype::CType::Scalar(scalar) => match scalar {
                    ctype::Scalar::Arithmetic(_) => Err(()),
                    ctype::Scalar::Pointer(inner, _) => Ok(self.ctype_to_llvm_type(inner)),
                },
            }
        }

        /// Treats ptrs as unsigned, will return Err for aggregates
        fn is_ctype_signed(&self, ctype: &ir::ctype::CType) -> Result<bool, ()> {
            match ctype {
                ctype::CType::Scalar(ctype::Scalar::Arithmetic(a)) => Ok(a.is_signed()),
                ctype::CType::Scalar(ctype::Scalar::Pointer(_, _)) => Ok(false),
            }
        }
    }
}
