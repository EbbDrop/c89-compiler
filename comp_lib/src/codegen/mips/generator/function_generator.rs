use std::collections::HashSet;

use super::{
    mips_value::{MipsLvalue, MipsValue},
    util::{self, RegType},
};
use crate::{
    codegen::mips::generator::expr_generator::BinaryExprGenerator,
    ir::{self, ctype, ctype::CType},
};
use mips_ir as mir;
use mir::BlockRef;

pub struct FunctionGenerator<'g, 'i, 's> {
    root_generator: &'g mut super::Generator<'i, 's>,
    function: mir::Function,
    ir: &'i ir::FunctionNode,
    register_types: ir::table::Table<util::RegType>,
    reference_values: HashSet<ir::table::ItemId>,
    next_reg: u32,
    next_float_reg: u32,
    next_double_reg: u32,
}

#[derive(Debug)]
pub struct Builder {
    pub bb: mir::BBBuilder,
    pub var_registers: ir::table::Table<mir::AnyReg>,
    pub continue_label: Option<mir::BlockLabel>,
    pub break_label: Option<mir::BlockLabel>,
}

impl Builder {
    fn get_all_registers(&self) -> Vec<mir::AnyReg> {
        self.var_registers.to_vec()
    }

    fn create_block_ref(&self, label: mir::BlockLabel) -> BlockRef {
        mir::BlockRef::new(label, self.get_all_registers())
    }
}

impl<'g, 'i, 's> FunctionGenerator<'g, 'i, 's> {
    pub fn new(
        root_generator: &'g mut super::Generator<'i, 's>,
        ident: &str,
        ir_function: &'i ir::FunctionNode,
    ) -> Self {
        let mut reference_values = HashSet::new();

        let register_types = ir_function.table.map_with_id(|id, item| {
            if item.needs_address {
                reference_values.insert(id);
                RegType::Int
            } else {
                util::ctype_reg_type(&item.ty)
            }
        });

        let params_info = ir_function
            .params
            .iter()
            .map(|param| util::ctype_props(&param.ty))
            .collect();

        Self {
            root_generator,
            function: mir::Function::new(ident.into(), params_info),
            ir: ir_function,
            register_types,
            reference_values,
            next_reg: 0,
            next_float_reg: 0,
            next_double_reg: 0,
        }
    }

    pub fn generate(mut self) -> mir::Function {
        let entry_point_label = self.function.create_block_label();

        let var_registers = self
            .register_types
            .clone()
            .map(|reg| self.new_register_of_type(*reg));

        // Go throu all the variables of witch a address will be used later in the function
        // and mark them as an ReferenceRegister in the MIPS mir.
        for id in &self.reference_values {
            let props = util::ctype_props(&self.ir.table.get(*id).ty);

            let register = match var_registers.get(*id) {
                mir::AnyReg::R(reg) => *reg,
                mir::AnyReg::F(_) => {
                    unreachable!("ICE: created a float register to hold a pointer")
                }
            };

            let ref_reg = mir::ReferenceRegister {
                register,
                stack_info: props,
            };
            self.function.add_reference_register(ref_reg);
        }

        let params: Vec<_> = self
            .ir
            .params
            .iter()
            .map(|param| match param.ident {
                Some(id) => *var_registers.get(id),
                None => self.new_register_of_type(util::ctype_reg_type(&param.ty)),
            })
            .collect();

        let bbbuilder = self.function.start_block(entry_point_label.clone(), params);
        let mut builder = Builder {
            bb: bbbuilder,
            var_registers,
            continue_label: None,
            break_label: None,
        };

        if let Some(block) = &self.ir.body {
            builder = self.add_ir_block_node(builder, block);
        }

        self.function
            .add_block(builder.bb.terminate(mir::term::virt::return_(None)));
        self.function.set_entry_block(entry_point_label.id());
        self.function
    }

    /// Creates a new builder with arguments for all the items, taking the continue and break
    /// labels from the given `prev_builder`
    fn create_new_builder(&mut self, prev_builder: &Builder) -> (mir::BlockLabel, Builder) {
        let registers = self
            .register_types
            .clone()
            .map(|reg| self.new_register_of_type(*reg));

        let new_block_builder = self.function.start_new_block(registers.to_vec());
        let label = new_block_builder.label().clone();

        (
            label,
            Builder {
                bb: new_block_builder,
                var_registers: registers,
                continue_label: prev_builder.continue_label.clone(),
                break_label: prev_builder.break_label.clone(),
            },
        )
    }

    /// Creates a new builder with arguments for all the items, taking the continue and break
    /// labels from the given `prev_builder`
    fn create_builder_with_label(
        &mut self,
        label: mir::BlockLabel,
        prev_builder: &Builder,
    ) -> Builder {
        let registers = self
            .register_types
            .clone()
            .map(|reg| self.new_register_of_type(*reg));

        let new_block_builder = self.function.start_block(label, registers.to_vec());

        Builder {
            bb: new_block_builder,
            var_registers: registers,
            continue_label: prev_builder.continue_label.clone(),
            break_label: prev_builder.break_label.clone(),
        }
    }

    fn add_ir_block_node(&mut self, mut builder: Builder, block_node: &ir::BlockNode) -> Builder {
        for stmt_node in &block_node.stmts {
            builder = self.add_ir_stmt_node(builder, stmt_node);
        }
        builder
    }

    fn add_ir_stmt_node(&mut self, builder: Builder, stmt_node: &ir::StmtNode) -> Builder {
        // TODO: add comments
        match &stmt_node.stmt {
            ir::Stmt::Expr(node) => self.add_ir_expr_node(builder, node).0,
            ir::Stmt::IfStmt(node) => self.add_ir_if(builder, node),
            ir::Stmt::SwitchStmt(_) => todo!(),
            ir::Stmt::LoopStmt(node) => self.add_ir_loop(builder, node),
            ir::Stmt::Break => self.add_ir_break(builder),
            ir::Stmt::Continue => self.add_ir_continue(builder),
            ir::Stmt::Return(node) => self.add_ir_return(builder, node.as_ref()),
        }
    }

    pub fn add_ir_expr_node(
        &mut self,
        mut builder: Builder,
        expr_node: &ir::ExprNode,
    ) -> (Builder, MipsValue) {
        use ir::Expr as E;
        match &expr_node.expr {
            E::LvalueDeref(expr) => self.add_ir_lvalue_deref(builder, expr, &expr_node.ty),
            E::Constant(constant) => {
                let value = self.add_ir_constant(&mut builder, constant, &expr_node.ty);
                (builder, value)
            }
            E::FunctionCall(name, arguments) => {
                self.add_ir_function_call(builder, name, arguments, &expr_node.ty)
            }
            E::PostfixInc(_) => todo!(),
            E::PostfixDec(_) => todo!(),
            E::PrefixInc(_) => todo!(),
            E::PrefixDec(_) => todo!(),
            E::Reference(expr) => self.add_ir_reference(builder, expr),
            E::UnaryArith(_, _) => todo!(),
            E::Binary(left, op, right) => {
                let generator = BinaryExprGenerator {
                    function_generator: self,
                    left,
                    right,
                    to_type: &expr_node.ty,
                };
                match op {
                    ir::BinaryOp::Mul => generator.add_ir_mul(builder),
                    ir::BinaryOp::Div => generator.add_ir_div(builder),
                    ir::BinaryOp::Rem => generator.add_ir_rem(builder),
                    ir::BinaryOp::Add => generator.add_ir_add(builder),
                    ir::BinaryOp::Sub => generator.add_ir_sub(builder),
                    ir::BinaryOp::ShiftLeft => generator.add_ir_shift_left(builder),
                    ir::BinaryOp::ShiftRight => generator.add_ir_shift_right(builder),
                    ir::BinaryOp::Bitwise(op) => generator.add_ir_bitwise(builder, op),
                }
            }
            E::Relation(_, _, _) => todo!(),
            E::LogicalAnd(_, _) => todo!(),
            E::LogicalOr(_, _) => todo!(),
            E::Assign(to_value, from_value) => self.add_ir_assign(builder, to_value, from_value),
            E::Cast(from) => self.cast(builder, from, &expr_node.ty),
        }
    }

    fn add_ir_lvalue_deref(
        &mut self,
        builder: Builder,
        expr: &ir::LvalueExprNode,
        to_type: &CType,
    ) -> (Builder, MipsValue) {
        let (mut builder, value) = self.add_ir_lvalue_node(builder, expr);
        let value = match value {
            MipsLvalue::Address(reg) => {
                if matches!(&expr.ty, CType::Aggregate(ctype::Aggregate::Array(_))) {
                    return (builder, reg.into());
                }
                match util::ctype_floating_fmt(to_type) {
                    Some(mir::FFmt::S) => {
                        let freg = self.new_float_register();
                        builder
                            .bb
                            .add_instruction(mir::instr::load_word_to_fpu(freg, reg, 0));
                        freg.into()
                    }
                    Some(mir::FFmt::D) => {
                        let freg = self.new_double_register();
                        builder
                            .bb
                            .add_instruction(mir::instr::load_doubleword_to_fpu(freg, reg, 0));
                        freg.into()
                    }
                    None => {
                        let out_reg = self.new_register();

                        let props = util::ctype_props(to_type);
                        let inst = match (props.size as u32, props.signed) {
                            (mir::size::BYTE, false) => mir::instr::load_byte_u(out_reg, reg, 0),
                            (mir::size::BYTE, true) => mir::instr::load_byte_s(out_reg, reg, 0),
                            (mir::size::HALF, false) => mir::instr::load_half_u(out_reg, reg, 0),
                            (mir::size::HALF, true) => mir::instr::load_half_s(out_reg, reg, 0),
                            (mir::size::WORD, _) => mir::instr::load_word(out_reg, reg, 0),
                            (other_size, _) => {
                                unreachable!("ICE: unexpected size for lvalue deref: {other_size}")
                            }
                        };
                        builder.bb.add_instruction(inst);
                        out_reg.into()
                    }
                }
            }
            MipsLvalue::Reg(id) => (*builder.var_registers.get(id)).into(),
        };
        (builder, value)
    }

    fn add_ir_constant(
        &mut self,
        builder: &mut Builder,
        constant: &ir::Constant,
        ty: &CType,
    ) -> MipsValue {
        use ctype::{Arithmetic, Scalar};
        match constant {
            &ir::Constant::Integer(value) => {
                // Too many instructions sign extend so it's handier to make sure the value always
                // fits in a signed 16-bit value.
                match i16::try_from(value) {
                    Ok(imm) => MipsValue::Imm(imm as u16),
                    Err(_) => {
                        let reg_upper = self.new_register();
                        let out_reg = self.new_register();
                        builder.bb.add_instruction(mir::instr::load_upper(
                            reg_upper,
                            (value >> 16) as u16,
                        ));
                        builder.bb.add_instruction(mir::instr::or_imm(
                            out_reg,
                            reg_upper,
                            value as u16,
                        ));
                        out_reg.into()
                    }
                }
            }
            &ir::Constant::Float(value) => match ty {
                CType::Scalar(Scalar::Arithmetic(Arithmetic::Float)) => {
                    let label = self.root_generator.add_float_constant(value as f32).clone();
                    self.load_float(builder, label).into()
                }
                CType::Scalar(Scalar::Arithmetic(Arithmetic::Double)) => {
                    let label = self.root_generator.add_double_constant(value).clone();
                    self.load_double(builder, label).into()
                }
                _ => panic!("ICE: float constants must have floating-point type"),
            },
            ir::Constant::String(_) => todo!(),
        }
    }

    fn load_float(&mut self, builder: &mut Builder, label: mir::Label) -> mir::FReg {
        let reg = self.new_register();
        let freg = self.new_float_register();
        builder
            .bb
            .add_instruction(mir::instr::pseudo::load_address(reg, label));
        builder
            .bb
            .add_instruction(mir::instr::load_word_to_fpu(freg, reg, 0));
        freg
    }

    fn load_double(&mut self, builder: &mut Builder, label: mir::Label) -> mir::FReg {
        let reg = self.new_register();
        let freg = self.new_double_register();
        builder
            .bb
            .add_instruction(mir::instr::pseudo::load_address(reg, label));
        builder
            .bb
            .add_instruction(mir::instr::load_doubleword_to_fpu(freg, reg, 0));
        freg
    }

    fn add_ir_function_call(
        &mut self,
        mut builder: Builder,
        name: &str,
        arguments: &[ir::ExprNode],
        to_type: &CType,
    ) -> (Builder, MipsValue) {
        let mut reg_arguments = Vec::with_capacity(arguments.len());
        for argument in arguments {
            let (b, value) = self.add_ir_expr_node(builder, argument);
            builder = b;
            let reg = self.value_into_reg(&mut builder, value, &argument.ty);
            let stack_info = util::ctype_props(&argument.ty);
            reg_arguments.push((reg, stack_info));
        }

        let return_reg = match to_type {
            CType::Void => None,
            other => Some(self.new_register_of_type(util::ctype_reg_type(other))),
        };

        builder.bb.add_instruction(mir::instr::virt::function_call(
            name.into(),
            return_reg,
            reg_arguments,
        ));

        let value = match return_reg {
            Some(reg) => reg.into(),
            None => {
                // This value will not be used, if the lower_ast step did its job correctly.
                MipsValue::Imm(0)
            }
        };

        (builder, value)
    }

    fn add_ir_reference(
        &mut self,
        builder: Builder,
        expr: &ir::LvalueExprNode,
    ) -> (Builder, MipsValue) {
        let (builder, value) = self.add_ir_lvalue_node(builder, expr);
        match value {
            MipsLvalue::Address(reg) => (builder, reg.into()),
            MipsLvalue::Reg(_) => {
                unreachable!("ICE: created a register lvalue, but still needed to get address")
            }
        }
    }

    fn add_ir_assign(
        &mut self,
        builder: Builder,
        to_node: &ir::LvalueExprNode,
        from_node: &ir::ExprNode,
    ) -> (Builder, MipsValue) {
        let (builder, to) = self.add_ir_lvalue_node(builder, to_node);
        let (mut builder, from) = self.add_ir_expr_node(builder, from_node);

        let from = self.value_into_reg(&mut builder, from, &from_node.ty);

        match to {
            MipsLvalue::Address(ptr_reg) => {
                let size = util::ctype_props(&from_node.ty).size as u32;
                let inst = match from {
                    mir::AnyReg::R(from_reg) => match size {
                        mir::size::BYTE => mir::instr::store_byte(from_reg, ptr_reg, 0),
                        mir::size::HALF => mir::instr::store_half(from_reg, ptr_reg, 0),
                        mir::size::WORD => mir::instr::store_word(from_reg, ptr_reg, 0),
                        _ => unreachable!("ICE: storing value with invalid size"),
                    },
                    mir::AnyReg::F(from_reg) => match util::ctype_floating_fmt(&from_node.ty) {
                        Some(mir::FFmt::S) => mir::instr::store_word_from_fpu(from_reg, ptr_reg, 0),
                        Some(mir::FFmt::D) => {
                            mir::instr::store_doubleword_from_fpu(from_reg, ptr_reg, 0)
                        }
                        None => unreachable!(),
                    },
                };
                builder.bb.add_instruction(inst);
            }
            MipsLvalue::Reg(id) => {
                *builder.var_registers.get_mut(id) = from;
            }
        };
        (builder, from.into())
    }

    fn cast(
        &mut self,
        builder: Builder,
        from: &ir::ExprNode,
        to_type: &CType,
    ) -> (Builder, MipsValue) {
        let from_ty = &from.ty;
        let (mut builder, from) = self.add_ir_expr_node(builder, from);

        let out_reg = match from {
            MipsValue::Imm(value) => match util::ctype_floating_fmt(to_type) {
                Some(to_fmt) => {
                    let reg = self.imm_to_reg(&mut builder, value, from_ty);

                    let int_in_float_reg = self.new_float_register();
                    builder
                        .bb
                        .add_instruction(mir::instr::move_to_fpu(reg, int_in_float_reg));

                    let out_reg = self.new_ffmt_register(to_fmt);
                    builder.bb.add_instruction(mir::instr::convert_from_word(
                        to_fmt,
                        out_reg,
                        int_in_float_reg,
                    ));
                    MipsValue::FReg(out_reg)
                }
                _ => {
                    match to_type {
                        CType::Scalar(ctype::Scalar::Pointer(_)) => {
                            self.imm_to_reg(&mut builder, value, from_ty).into()
                        }
                        CType::Scalar(ctype::Scalar::Arithmetic(_)) => {
                            // The other steps can all deal with imm's of arbritrary size
                            MipsValue::Imm(value)
                        }
                        _ => unreachable!("ICE: invalid to cast type"),
                    }
                }
            },
            MipsValue::Reg(reg) => match util::ctype_floating_fmt(to_type) {
                Some(to_fmt) => {
                    let int_in_float_reg = self.new_float_register();
                    builder
                        .bb
                        .add_instruction(mir::instr::move_to_fpu(reg, int_in_float_reg));

                    let out_reg = self.new_ffmt_register(to_fmt);
                    builder.bb.add_instruction(mir::instr::convert_from_word(
                        to_fmt,
                        out_reg,
                        int_in_float_reg,
                    ));
                    MipsValue::FReg(out_reg)
                }
                None => {
                    let from_size = util::ctype_props(from_ty).size;
                    let to_size = util::ctype_props(to_type).size;
                    match (from_size as u32, to_size as u32) {
                        (mir::size::HALF, mir::size::BYTE) | (mir::size::WORD, mir::size::BYTE) => {
                            // The value anded to only get the last relevant bytes. This can give
                            // strange behavior if a negative value is to big for its type, but
                            // this is fine as this is imlementation defined anyway.
                            let out_reg = self.new_register();
                            builder
                                .bb
                                .add_instruction(mir::instr::and_imm(out_reg, reg, 0xff));
                            out_reg.into()
                        }
                        (mir::size::WORD, mir::size::HALF) => {
                            let out_reg = self.new_register();
                            builder
                                .bb
                                .add_instruction(mir::instr::and_imm(out_reg, reg, 0xffff));
                            out_reg.into()
                        }
                        (mir::size::BYTE, mir::size::HALF)
                        | (mir::size::BYTE, mir::size::WORD)
                        | (mir::size::HALF, mir::size::WORD) => {
                            // Don't have to do anything for conversions to bigger types
                            // since the smaller types will still be stored in a WORD sized
                            // register.
                            reg.into()
                        }
                        (mir::size::BYTE, mir::size::BYTE)
                        | (mir::size::HALF, mir::size::HALF)
                        | (mir::size::WORD, mir::size::WORD) => {
                            // Don't have to do anythig for conversions between types with
                            // the same size.
                            reg.into()
                        }
                        _ => unreachable!(),
                    }
                }
            },
            MipsValue::FReg(reg) => {
                match (
                    util::ctype_floating_fmt(to_type),
                    util::ctype_floating_fmt(from_ty),
                ) {
                    (Some(to_fmt), Some(from_fmt)) if to_fmt == from_fmt => MipsValue::FReg(reg),
                    (Some(to_fmt), Some(from_fmt)) => {
                        let out_reg = self.new_ffmt_register(to_fmt);

                        builder
                            .bb
                            .add_instruction(mir::instr::convert(to_fmt, from_fmt, out_reg, reg));

                        MipsValue::FReg(out_reg)
                    }
                    (_, Some(from_fmt)) => {
                        let integer_in_freg = self.new_float_register();
                        builder.bb.add_instruction(mir::instr::convert_to_word(
                            from_fmt,
                            integer_in_freg,
                            reg,
                        ));

                        let out_reg = self.new_register();
                        builder
                            .bb
                            .add_instruction(mir::instr::move_from_fpu(out_reg, integer_in_freg));

                        MipsValue::Reg(out_reg)
                    }
                    _ => unreachable!(),
                }
            }
        };
        (builder, out_reg)
    }

    fn add_ir_lvalue_node(
        &mut self,
        builder: Builder,
        expr: &ir::LvalueExprNode,
    ) -> (Builder, MipsLvalue) {
        match &expr.expr {
            ir::LvalueExpr::Ident(id) => self.add_ir_ident(builder, id),
            ir::LvalueExpr::GlobalIdent(name) => self.add_ir_global_ident(builder, name),
            ir::LvalueExpr::Dereference(expr) => self.add_ir_dereference(builder, expr),
        }
    }

    fn add_ir_ident(&mut self, builder: Builder, id: &ir::table::ItemId) -> (Builder, MipsLvalue) {
        let value = if self.reference_values.contains(id) {
            MipsLvalue::Address(match builder.var_registers.get(*id) {
                mir::AnyReg::R(reg) => *reg,
                mir::AnyReg::F(_) => unreachable!("ICE: used float register to store ptr"),
            })
        } else {
            MipsLvalue::Reg(*id)
        };
        (builder, value)
    }

    fn add_ir_global_ident(&mut self, mut builder: Builder, name: &str) -> (Builder, MipsLvalue) {
        let reg = self.new_register();

        builder
            .bb
            .add_instruction(mir::instr::pseudo::load_address(reg, name.into()));

        (builder, MipsLvalue::Address(reg))
    }

    fn add_ir_dereference(
        &mut self,
        builder: Builder,
        expr: &ir::ExprNode,
    ) -> (Builder, MipsLvalue) {
        let (builder, value) = self.add_ir_expr_node(builder, expr);
        let reg = match value {
            MipsValue::Reg(reg) => reg,
            _ => unreachable!(),
        };
        (builder, MipsLvalue::Address(reg))
    }

    /// Creates a terminator that will jump to the falsy branch if cond is 0, and otherwise fall
    /// through to the truthy branch.
    fn make_condition(
        &mut self,
        builder: &mut Builder,
        cond: mir::AnyReg,
        truthy: mir::BlockLabel,
        falsy: mir::BlockLabel,
    ) -> mir::Terminator {
        let truthy_ref = builder.create_block_ref(truthy);
        let falsy_ref = builder.create_block_ref(falsy);
        match cond {
            mir::AnyReg::R(reg) => {
                mir::term::branch_if(mir::BCond::Eq, reg, mir::Reg::ZERO, falsy_ref, truthy_ref)
            }
            mir::AnyReg::F(_reg) => todo!(),
        }
    }

    fn add_ir_if(&mut self, builder: Builder, node: &ir::IfStmtNode) -> Builder {
        let (mut start_builder, cond_value) = self.add_ir_expr_node(builder, &node.condition);
        let cond_value = self.value_into_reg(&mut start_builder, cond_value, &node.condition.ty);

        let end_label = self.function.create_block_label();

        let (if_case_label, if_case_builder) = self.create_new_builder(&start_builder);
        let if_case_builder = self.add_ir_block_node(if_case_builder, &node.if_branch);

        let falsy_label = if let Some(else_brache) = &node.else_branch {
            let (else_case_label, else_case_builder) = self.create_new_builder(&start_builder);
            let else_case_builder = self.add_ir_block_node(else_case_builder, else_brache);

            let to_end_ref = mir::term::jump(else_case_builder.create_block_ref(end_label.clone()));
            self.function
                .add_block(else_case_builder.bb.terminate(to_end_ref));

            else_case_label
        } else {
            end_label.clone()
        };

        let to_end_ref = mir::term::jump(if_case_builder.create_block_ref(end_label.clone()));
        self.function
            .add_block(if_case_builder.bb.terminate(to_end_ref));

        let end_builder = self.create_builder_with_label(end_label, &start_builder);

        let branching_terminator =
            self.make_condition(&mut start_builder, cond_value, if_case_label, falsy_label);
        self.function
            .add_block(start_builder.bb.terminate(branching_terminator));

        end_builder
    }

    pub fn add_ir_loop(&mut self, builder: Builder, node: &ir::LoopStmtNode) -> Builder {
        let (start_label, start_builder) = self.create_new_builder(&builder);

        let end_label = self.function.create_block_label();

        let mut body_builder = if let Some(condition_node) = &node.condition {
            let (mut start_builder, cond_value) =
                self.add_ir_expr_node(start_builder, condition_node);
            let cond_value =
                self.value_into_reg(&mut start_builder, cond_value, &condition_node.ty);

            let (body_label, body_builder) = self.create_new_builder(&start_builder);

            let branching_terminator = self.make_condition(
                &mut start_builder,
                cond_value,
                body_label,
                end_label.clone(),
            );
            self.function
                .add_block(start_builder.bb.terminate(branching_terminator));

            body_builder
        } else {
            start_builder
        };

        let continuation_label = match &node.continuation {
            Some(_) => self.function.create_block_label(),
            None => start_label.clone(),
        };

        body_builder.continue_label = Some(continuation_label.clone());
        body_builder.break_label = Some(end_label.clone());

        let body_builder = self.add_ir_block_node(body_builder, &node.body);

        if let Some(continuation_node) = &node.continuation {
            let continuation_builder =
                self.create_builder_with_label(continuation_label.clone(), &builder);
            let continuation_builder = self
                .add_ir_expr_node(continuation_builder, continuation_node)
                .0;

            let continuation_terminator =
                mir::term::jump(continuation_builder.create_block_ref(start_label.clone()));
            self.function
                .add_block(continuation_builder.bb.terminate(continuation_terminator));
        }

        let terminator = mir::term::jump(body_builder.create_block_ref(continuation_label));
        self.function
            .add_block(body_builder.bb.terminate(terminator));

        let end_builder = self.create_builder_with_label(end_label, &builder);

        let to_start = mir::term::jump(builder.create_block_ref(start_label));
        self.function.add_block(builder.bb.terminate(to_start));

        end_builder
    }

    pub fn add_ir_break(&mut self, builder: Builder) -> Builder {
        let new_builder = self.create_new_builder(&builder).1;

        let term_ref = builder
            .break_label
            .clone()
            .expect("ICE: break while not in loop/switch");
        let terminator = mir::term::jump(builder.create_block_ref(term_ref));
        self.function.add_block(builder.bb.terminate(terminator));

        new_builder
    }

    pub fn add_ir_continue(&mut self, builder: Builder) -> Builder {
        let new_builder = self.create_new_builder(&builder).1;

        let term_ref = builder
            .continue_label
            .clone()
            .expect("ICE: continue while not in loop");
        let terminator = mir::term::jump(builder.create_block_ref(term_ref));
        self.function.add_block(builder.bb.terminate(terminator));

        new_builder
    }

    fn add_ir_return(&mut self, builder: Builder, node: Option<&ir::ExprNode>) -> Builder {
        let (builder, reg) = match node {
            Some(node) => {
                let (mut builder, value) = self.add_ir_expr_node(builder, node);
                let reg = self.value_into_reg(&mut builder, value, &node.ty);
                (builder, Some(reg))
            }
            None => (builder, None),
        };

        let new_builder = self.create_new_builder(&builder).1;

        self.function
            .add_block(builder.bb.terminate(mir::term::virt::return_(reg)));

        new_builder
    }

    pub fn imm_to_reg(&mut self, builder: &mut Builder, value: u16, ty: &CType) -> mir::Reg {
        let reg = self.new_register();
        match ty {
            CType::Scalar(ctype::Scalar::Arithmetic(arith)) => {
                if arith.is_signed() {
                    builder
                        .bb
                        .add_instruction(mir::instr::add_u_imm(reg, mir::Reg::ZERO, value));
                } else {
                    builder
                        .bb
                        .add_instruction(mir::instr::or_imm(reg, mir::Reg::ZERO, value));
                }
            }

            _ => panic!("ICE: non-arithmetic immediate"),
        }
        reg
    }

    fn value_into_reg(
        &mut self,
        builder: &mut Builder,
        value: MipsValue,
        value_type: &CType,
    ) -> mir::AnyReg {
        match value {
            MipsValue::Imm(imm) => self.imm_to_reg(builder, imm, value_type).into(),
            MipsValue::Reg(reg) => reg.into(),
            MipsValue::FReg(reg) => reg.into(),
        }
    }

    fn new_register_of_type(&mut self, reg_type: util::RegType) -> mir::AnyReg {
        match reg_type {
            util::RegType::Int => self.new_register().into(),
            util::RegType::Single => self.new_float_register().into(),
            util::RegType::Double => self.new_double_register().into(),
        }
    }

    pub fn new_register(&mut self) -> mir::Reg {
        let n = self.next_reg;
        self.next_reg += 1;
        mir::Reg::Virtual(n)
    }

    pub fn new_ffmt_register(&mut self, fmt: mir::FFmt) -> mir::FReg {
        match fmt {
            mir::FFmt::S => self.new_float_register(),
            mir::FFmt::D => self.new_double_register(),
        }
    }

    pub fn new_float_register(&mut self) -> mir::FReg {
        let n = self.next_float_reg;
        self.next_float_reg += 1;
        mir::FReg::VirtualSingle(n)
    }

    pub fn new_double_register(&mut self) -> mir::FReg {
        let n = self.next_double_reg;
        self.next_double_reg += 1;
        mir::FReg::VirtualDouble(n)
    }
}
