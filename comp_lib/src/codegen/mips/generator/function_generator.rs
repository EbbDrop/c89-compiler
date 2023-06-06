use std::collections::HashSet;

use super::{
    expr_generator::{BinaryExprGenerator, UnaryExprGenerator},
    mips_value::{MipsCondOrValue, MipsLvalue, MipsValue, StackedMipsValue},
    util::{self, RegType},
};
use crate::ir::{self, ctype, ctype::CType};
use mips_ir as mir;
use mir::BlockRef;

pub struct FunctionGenerator<'g, 'i, 's> {
    pub root_generator: &'g mut super::Generator<'i, 's>,
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
    pub expr_res_stack: Vec<(mir::AnyReg, RegType)>,
    pub continue_label: Option<mir::BlockId>,
    pub break_label: Option<mir::BlockId>,
}

#[derive(Debug, Clone, Copy)]
pub enum IncDecType {
    Prefix,
    Postfix,
}

#[derive(Debug, Clone, Copy)]
pub enum LogicalBuilderType {
    And,
    Or,
}

impl Builder {
    fn get_all_registers(&self) -> Vec<mir::AnyReg> {
        self.var_registers
            .items()
            .cloned()
            .chain(self.expr_res_stack.iter().map(|(reg, _)| *reg))
            .collect()
    }

    fn create_block_ref(&self, label: mir::BlockId) -> BlockRef {
        mir::BlockRef::new(label, self.get_all_registers())
    }

    pub fn push_to_expr_stack(&mut self, value: MipsValue, ty: &CType) -> StackedMipsValue {
        let reg = match value {
            MipsValue::Imm(value) => return StackedMipsValue::Imm(value),
            MipsValue::Reg(reg) => reg.into(),
            MipsValue::FReg(freg) => freg.into(),
        };
        self.expr_res_stack.push((reg, util::ctype_reg_type(ty)));
        StackedMipsValue::InStack
    }

    pub fn pop_off_expr_stack(&mut self, value: StackedMipsValue) -> MipsValue {
        match value {
            StackedMipsValue::Imm(value) => MipsValue::Imm(value),
            StackedMipsValue::InStack => {
                let reg = self.expr_res_stack.pop().unwrap().0;
                reg.into()
            }
        }
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

        let bbbuilder = self.function.start_entry_block(params);
        let mut builder = Builder {
            bb: bbbuilder,
            var_registers,
            expr_res_stack: Vec::new(),
            continue_label: None,
            break_label: None,
        };

        if let Some(block) = &self.ir.body {
            builder = self.add_ir_block_node(builder, block);
        }

        let implicit_return_value = match &self.ir.return_type {
            CType::Scalar(scalar) => match scalar {
                ctype::Scalar::Arithmetic(arithmetic) => match arithmetic.is_floating() {
                    true => None, // implicit floating point returns are undefined
                    false => Some(mir::Reg::ZERO.into()),
                },
                ctype::Scalar::Pointer(_) => Some(mir::Reg::ZERO.into()),
            },
            CType::Aggregate(_) => {
                unreachable!("aggregate values cannot be returned from functions")
            }
            CType::Void => None,
        };
        self.function.add_block(
            builder
                .bb
                .terminate(mir::term::virt::return_(implicit_return_value)),
        );
        self.function
    }

    /// Creates a new builder with arguments for all the items, taking the continue and break
    /// labels from the given `prev_builder`
    fn create_new_builder(&mut self, prev_builder: &Builder) -> (mir::BlockId, Builder) {
        let label = self.function.create_block_label();
        (label, self.create_builder_with_label(label, prev_builder))
    }

    /// Creates a new builder with arguments for all the items, taking the continue and break
    /// labels from the given `prev_builder`
    fn create_builder_with_label(
        &mut self,
        label: mir::BlockId,
        prev_builder: &Builder,
    ) -> Builder {
        let registers = self
            .register_types
            .clone()
            .map(|reg| self.new_register_of_type(*reg));

        let expr_regs: Vec<_> = prev_builder
            .expr_res_stack
            .iter()
            .map(|(_, reg_type)| (self.new_register_of_type(*reg_type), *reg_type))
            .collect();

        let new_block_builder = self.function.start_block(
            label,
            registers
                .items()
                .cloned()
                .chain(expr_regs.iter().map(|(reg, _)| *reg))
                .collect(),
        );

        Builder {
            bb: new_block_builder,
            var_registers: registers,
            expr_res_stack: expr_regs,
            continue_label: prev_builder.continue_label,
            break_label: prev_builder.break_label,
        }
    }

    fn add_ir_block_node(&mut self, mut builder: Builder, block_node: &ir::BlockNode) -> Builder {
        for stmt_node in &block_node.stmts {
            builder = self.add_ir_stmt_node(builder, stmt_node);
        }
        builder
    }

    fn add_ir_stmt_node(&mut self, mut builder: Builder, stmt_node: &ir::StmtNode) -> Builder {
        if let Some(coments) = stmt_node.comments.clone() {
            builder.bb.add_instruction(mir::instr::comment(coments))
        }
        for line in self.root_generator.source[std::ops::Range::from(stmt_node.span)].lines() {
            builder
                .bb
                .add_instruction(mir::instr::comment(format!("## {line}")))
        }
        match &stmt_node.stmt {
            ir::Stmt::Expr(node) => self.add_ir_expr_node(builder, node).0,
            ir::Stmt::IfStmt(node) => self.add_ir_if(builder, node),
            ir::Stmt::SwitchStmt(node) => self.add_ir_switch(builder, node),
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
    ) -> (Builder, MipsCondOrValue) {
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
            E::PostfixInc(node) => {
                self.add_ir_post_pre_inc_dec(builder, node, 1, IncDecType::Postfix)
            }
            E::PostfixDec(node) => {
                self.add_ir_post_pre_inc_dec(builder, node, -1, IncDecType::Postfix)
            }
            E::PrefixInc(node) => {
                self.add_ir_post_pre_inc_dec(builder, node, 1, IncDecType::Prefix)
            }
            E::PrefixDec(node) => {
                self.add_ir_post_pre_inc_dec(builder, node, -1, IncDecType::Prefix)
            }
            E::Reference(expr) => self.add_ir_reference(builder, expr),
            E::UnaryArith(op, expr) => {
                let generator = UnaryExprGenerator {
                    function_generator: self,
                    expr,
                    to_type: &expr_node.ty,
                };
                match op {
                    ir::UnaryOp::Neg => generator.add_ir_neg(builder),
                    ir::UnaryOp::BitNot => generator.add_ir_bitnot(builder),
                    ir::UnaryOp::Not => generator.add_ir_not(builder),
                }
            }
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
            E::Relation(left, op, right) => {
                let generator = BinaryExprGenerator {
                    function_generator: self,
                    left,
                    right,
                    to_type: &expr_node.ty,
                };
                match op {
                    ir::RelationOp::Eq => generator.add_ir_rel_eq(builder),
                    ir::RelationOp::Ne => generator.add_ir_rel_ne(builder),
                    ir::RelationOp::Lt => generator.add_ir_rel_lt(builder),
                    ir::RelationOp::Gt => generator.add_ir_rel_gt(builder),
                    ir::RelationOp::Ge => generator.add_ir_rel_ge(builder),
                    ir::RelationOp::Le => generator.add_ir_rel_le(builder),
                }
            }
            E::LogicalAnd(left, right) => {
                self.add_ir_logical_and(builder, left, right, LogicalBuilderType::And)
            }
            E::LogicalOr(left, right) => {
                self.add_ir_logical_and(builder, left, right, LogicalBuilderType::Or)
            }
            E::Assign(to_value, from_value) => self.add_ir_assign(builder, to_value, from_value),
            E::Cast(from) => self.cast(builder, from, &expr_node.ty),
        }
    }

    fn add_ir_lvalue_deref(
        &mut self,
        builder: Builder,
        expr: &ir::LvalueExprNode,
        to_type: &CType,
    ) -> (Builder, MipsCondOrValue) {
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
    ) -> MipsCondOrValue {
        use ctype::{Arithmetic, Scalar};
        match constant {
            &ir::Constant::Integer(value) => {
                // Too many instructions sign extend so it's handier to make sure the value always
                // fits in a signed 16-bit value.
                match i16::try_from(value) {
                    Ok(imm) => MipsCondOrValue::Value(MipsValue::Imm(imm as u16)),
                    Err(_) => self.load_int_constant(builder, value).into(),
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
            ir::Constant::String(data) => {
                let label = self.root_generator.add_string_constant(data).clone();
                let reg = self.new_register();
                builder
                    .bb
                    .add_instruction(mir::instr::pseudo::load_address(reg, label));
                reg.into()
            }
        }
    }

    /// Will only load the lowwer 32 bits of the value into the register
    pub fn load_int_constant(&mut self, builder: &mut Builder, value: i128) -> mir::Reg {
        // or_imm zero extends so its fine to use a u16 here.
        match u16::try_from(value) {
            Ok(value) => {
                let out_reg = self.new_register();
                builder
                    .bb
                    .add_instruction(mir::instr::or_imm(out_reg, mir::Reg::ZERO, value));
                out_reg
            }
            Err(_) => {
                let reg_upper = self.new_register();
                let out_reg = self.new_register();
                builder
                    .bb
                    .add_instruction(mir::instr::load_upper(reg_upper, (value >> 16) as u16));
                builder
                    .bb
                    .add_instruction(mir::instr::or_imm(out_reg, reg_upper, value as u16));
                out_reg
            }
        }
    }

    pub fn load_float(&mut self, builder: &mut Builder, label: mir::Label) -> mir::FReg {
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

    pub fn load_double(&mut self, builder: &mut Builder, label: mir::Label) -> mir::FReg {
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
    ) -> (Builder, MipsCondOrValue) {
        let mut reg_arguments = Vec::with_capacity(arguments.len());
        for argument in arguments {
            let (b, value) = self.add_ir_expr_node(builder, argument);
            let (b, value) = self.cond_to_value(b, value);
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
                MipsCondOrValue::Value(MipsValue::Imm(0))
            }
        };

        (builder, value)
    }

    fn lvalue_operation<OPI, OPF>(
        &mut self,
        builder: Builder,
        node: &ir::LvalueExprNode,
        op_int: OPI,
        op_float: OPF,
    ) -> (Builder, MipsCondOrValue)
    where
        OPI: FnOnce(&mut Self, mir::Reg, &mut Builder) -> (mir::Reg, MipsCondOrValue),
        OPF: FnOnce(&mut Self, mir::FReg, mir::FFmt, &mut Builder) -> (mir::FReg, MipsCondOrValue),
    {
        let (mut builder, lvalue) = self.add_ir_lvalue_node(builder, node);

        let ffmt = util::ctype_floating_fmt(&node.ty);
        let value = match lvalue {
            MipsLvalue::Address(ptr_reg) => match ffmt {
                Some(ffmt) => {
                    let old_reg = self.new_ffmt_register(ffmt);

                    let inst = match ffmt {
                        mir::FFmt::S => mir::instr::load_word_to_fpu(old_reg, ptr_reg, 0),
                        mir::FFmt::D => mir::instr::load_doubleword_to_fpu(old_reg, ptr_reg, 0),
                    };
                    builder.bb.add_instruction(inst);

                    let (new_reg, value) = op_float(self, old_reg, ffmt, &mut builder);

                    let inst = match ffmt {
                        mir::FFmt::S => mir::instr::store_word_from_fpu(new_reg, ptr_reg, 0),
                        mir::FFmt::D => mir::instr::store_doubleword_from_fpu(new_reg, ptr_reg, 0),
                    };
                    builder.bb.add_instruction(inst);

                    value
                }

                None => {
                    let props = util::ctype_props(&node.ty);

                    let old_reg = self.new_register();

                    let inst = match (props.size as u32, props.signed) {
                        (mir::size::BYTE, false) => mir::instr::load_byte_u(old_reg, ptr_reg, 0),
                        (mir::size::BYTE, true) => mir::instr::load_byte_s(old_reg, ptr_reg, 0),
                        (mir::size::HALF, false) => mir::instr::load_half_u(old_reg, ptr_reg, 0),
                        (mir::size::HALF, true) => mir::instr::load_half_s(old_reg, ptr_reg, 0),
                        (mir::size::WORD, _) => mir::instr::load_word(old_reg, ptr_reg, 0),
                        (other_size, _) => {
                            unreachable!("ICE: unexpected size for lvalue operation: {other_size}")
                        }
                    };
                    builder.bb.add_instruction(inst);

                    let (new_value_reg, value) = op_int(self, old_reg, &mut builder);

                    let inst = match props.size as u32 {
                        mir::size::BYTE => mir::instr::store_byte(new_value_reg, ptr_reg, 0),
                        mir::size::HALF => mir::instr::store_half(new_value_reg, ptr_reg, 0),
                        mir::size::WORD => mir::instr::store_word(new_value_reg, ptr_reg, 0),
                        _ => unreachable!("ICE: storing value with invalid size"),
                    };
                    builder.bb.add_instruction(inst);

                    value
                }
            },
            MipsLvalue::Reg(id) => {
                let old_reg = builder.var_registers.get(id);
                match old_reg {
                    mir::AnyReg::R(reg) => {
                        let (new_value_reg, value) = op_int(self, *reg, &mut builder);
                        *builder.var_registers.get_mut(id) = new_value_reg.into();
                        value
                    }
                    mir::AnyReg::F(freg) => {
                        let ffmt = ffmt.unwrap();
                        let (new_value_freg, value) = op_float(self, *freg, ffmt, &mut builder);
                        *builder.var_registers.get_mut(id) = new_value_freg.into();
                        value
                    }
                }
            }
        };
        (builder, value)
    }

    fn add_ir_post_pre_inc_dec(
        &mut self,
        builder: Builder,
        node: &ir::LvalueExprNode,
        dir: i16,
        op_type: IncDecType,
    ) -> (Builder, MipsCondOrValue) {
        let pointer_size = match &node.ty {
            CType::Scalar(ctype::Scalar::Pointer(ctype::Pointer { inner, .. })) => {
                let size = util::ctype_props(inner).size;
                Some(size)
            }
            _ => None,
        };

        self.lvalue_operation(
            builder,
            node,
            |s, in_reg, builder| {
                let new_reg = s.new_register();
                match pointer_size {
                    Some(size) => {
                        if let Some(offset) = i16::try_from(size)
                            .ok()
                            .and_then(|size| dir.checked_mul(size))
                        {
                            builder.bb.add_instruction(mir::instr::add_u_imm(
                                new_reg,
                                in_reg,
                                offset as u16,
                            ));
                        } else {
                            let offset_reg = s.load_int_constant(builder, size as i128);
                            builder
                                .bb
                                .add_instruction(mir::instr::add_u(new_reg, in_reg, offset_reg));
                        }
                    }
                    None => {
                        builder
                            .bb
                            .add_instruction(mir::instr::add_u_imm(new_reg, in_reg, dir as u16));
                    }
                }
                match op_type {
                    IncDecType::Postfix => (new_reg, in_reg.into()),
                    IncDecType::Prefix => (new_reg, new_reg.into()),
                }
            },
            |s, in_reg, ffmt, builder| {
                let dir_reg = match ffmt {
                    mir::FFmt::S => {
                        let label = s.root_generator.add_float_constant(dir as f32).clone();
                        s.load_float(builder, label)
                    }
                    mir::FFmt::D => {
                        let label = s.root_generator.add_double_constant(dir as f64).clone();
                        s.load_double(builder, label)
                    }
                };
                let new_reg = s.new_ffmt_register(ffmt);
                builder
                    .bb
                    .add_instruction(mir::instr::add(ffmt, new_reg, in_reg, dir_reg));
                match op_type {
                    IncDecType::Postfix => (new_reg, in_reg.into()),
                    IncDecType::Prefix => (new_reg, new_reg.into()),
                }
            },
        )
    }

    fn add_ir_reference(
        &mut self,
        builder: Builder,
        expr: &ir::LvalueExprNode,
    ) -> (Builder, MipsCondOrValue) {
        let (builder, value) = self.add_ir_lvalue_node(builder, expr);
        match value {
            MipsLvalue::Address(reg) => (builder, reg.into()),
            MipsLvalue::Reg(_) => {
                unreachable!("ICE: created a register lvalue, but still needed to get address")
            }
        }
    }

    fn add_ir_logical_and(
        &mut self,
        builder: Builder,
        left_node: &ir::ExprNode,
        right_node: &ir::ExprNode,
        l_type: LogicalBuilderType,
    ) -> (Builder, MipsCondOrValue) {
        let (end_lable, mut end_builder) = self.create_new_builder(&builder);
        let out_reg = self.new_register();
        end_builder.bb.add_argument(out_reg.into());

        let mut value_setter = |value: u16| {
            let (label, mut builder) = self.create_new_builder(&builder);
            let out_reg = self.new_register();
            builder
                .bb
                .add_instruction(mir::instr::or_imm(out_reg, mir::Reg::ZERO, value));
            let mut to_end = builder.create_block_ref(end_lable);
            to_end.arguments.push(out_reg.into());
            self.function
                .add_block(builder.bb.terminate(mir::term::jump(to_end)));
            label
        };

        let set_one_label = value_setter(1);
        let set_zero_label = value_setter(0);

        let right_eval_label = self.function.create_block_label();
        let (mut builder, left) = self.add_ir_expr_node(builder, left_node);

        let (truthy, falsy) = match l_type {
            LogicalBuilderType::And => (right_eval_label, set_zero_label),
            LogicalBuilderType::Or => (set_one_label, right_eval_label),
        };
        let branch = self.make_condition(&mut builder, left, &left_node.ty, truthy, falsy);

        let right_eval_builder = self.create_builder_with_label(right_eval_label, &builder);

        self.function.add_block(builder.bb.terminate(branch));

        let (mut right_eval_builder, right) = self.add_ir_expr_node(right_eval_builder, right_node);
        let branch = self.make_condition(
            &mut right_eval_builder,
            right,
            &right_node.ty,
            set_one_label,
            set_zero_label,
        );
        self.function
            .add_block(right_eval_builder.bb.terminate(branch));

        (end_builder, out_reg.into())
    }

    fn add_ir_assign(
        &mut self,
        builder: Builder,
        to_node: &ir::LvalueExprNode,
        from_node: &ir::ExprNode,
    ) -> (Builder, MipsCondOrValue) {
        let (builder, to) = self.add_ir_lvalue_node(builder, to_node);
        let (builder, from) = self.add_ir_expr_node(builder, from_node);

        let (mut builder, from) = self.cond_to_value(builder, from);
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
    ) -> (Builder, MipsCondOrValue) {
        let from_ty = &from.ty;
        let (builder, from) = self.add_ir_expr_node(builder, from);
        let (mut builder, from) = self.cond_to_value(builder, from);

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
                    out_reg.into()
                }
                _ => match to_type {
                    CType::Scalar(ctype::Scalar::Pointer(_)) => {
                        self.imm_to_reg(&mut builder, value, from_ty).into()
                    }
                    CType::Scalar(ctype::Scalar::Arithmetic(_)) => {
                        let to_size = util::ctype_props(to_type).size;
                        let value = if to_size == mir::size::BYTE as u128 {
                            value & 0xff
                        } else {
                            value
                        };
                        MipsCondOrValue::Value(MipsValue::Imm(value))
                    }
                    _ => unreachable!("ICE: invalid to cast type"),
                },
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
                    out_reg.into()
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
                        (_, 0) => {
                            // Casting to void, this value will not be usd later so its fine to
                            // just return the from reg.
                            reg.into()
                        }
                        (from, to) => {
                            unreachable!("Unexpected sizes in cast. Form: {from}, to: {to}")
                        }
                    }
                }
            },
            MipsValue::FReg(reg) => {
                match (
                    util::ctype_floating_fmt(to_type),
                    util::ctype_floating_fmt(from_ty),
                ) {
                    (Some(to_fmt), Some(from_fmt)) if to_fmt == from_fmt => reg.into(),
                    (Some(to_fmt), Some(from_fmt)) => {
                        let out_reg = self.new_ffmt_register(to_fmt);

                        builder
                            .bb
                            .add_instruction(mir::instr::convert(to_fmt, from_fmt, out_reg, reg));

                        out_reg.into()
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

                        out_reg.into()
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
        let MipsCondOrValue::Value(MipsValue::Reg(reg)) = value else {
            unreachable!()
        };
        (builder, MipsLvalue::Address(reg))
    }

    /// Creates a terminator that will jump to the falsy branch if cond is 0, and otherwise fall
    /// through to the truthy branch.
    fn make_condition(
        &mut self,
        builder: &mut Builder,
        cond: MipsCondOrValue,
        value_type: &CType,
        truthy: mir::BlockId,
        falsy: mir::BlockId,
    ) -> mir::Terminator {
        let truthy_ref = builder.create_block_ref(truthy);
        let falsy_ref = builder.create_block_ref(falsy);
        match cond {
            MipsCondOrValue::FloatCond { inverse } => {
                mir::term::branch_if_f_cond(!inverse, falsy_ref, truthy_ref)
            }
            MipsCondOrValue::Value(MipsValue::Reg(reg)) => {
                mir::term::branch_if(mir::BCond::Eq, reg, mir::Reg::ZERO, falsy_ref, truthy_ref)
            }
            MipsCondOrValue::Value(MipsValue::FReg(reg)) => {
                self.add_float_zero_eq_check(builder, reg, value_type);
                mir::term::branch_if_f_cond(false, falsy_ref, truthy_ref)
            }
            MipsCondOrValue::Value(MipsValue::Imm(value)) => match value {
                0 => mir::term::jump(falsy_ref),
                _ => mir::term::jump(truthy_ref),
            },
        }
    }

    fn add_ir_if(&mut self, builder: Builder, node: &ir::IfStmtNode) -> Builder {
        let (mut start_builder, cond_value) = self.add_ir_expr_node(builder, &node.condition);

        let end_label = self.function.create_block_label();

        let (if_case_label, if_case_builder) = self.create_new_builder(&start_builder);
        let if_case_builder = self.add_ir_block_node(if_case_builder, &node.if_branch);

        let falsy_label = if let Some(else_brache) = &node.else_branch {
            let (else_case_label, else_case_builder) = self.create_new_builder(&start_builder);
            let else_case_builder = self.add_ir_block_node(else_case_builder, else_brache);

            let to_end_ref = mir::term::jump(else_case_builder.create_block_ref(end_label));
            self.function
                .add_block(else_case_builder.bb.terminate(to_end_ref));

            else_case_label
        } else {
            end_label
        };

        let to_end_ref = mir::term::jump(if_case_builder.create_block_ref(end_label));
        self.function
            .add_block(if_case_builder.bb.terminate(to_end_ref));

        let end_builder = self.create_builder_with_label(end_label, &start_builder);

        let branching_terminator = self.make_condition(
            &mut start_builder,
            cond_value,
            &node.condition.ty,
            if_case_label,
            falsy_label,
        );
        self.function
            .add_block(start_builder.bb.terminate(branching_terminator));

        end_builder
    }

    fn add_ir_switch(&mut self, builder: Builder, node: &ir::SwitchStmtNode) -> Builder {
        let (builder, value) = self.add_ir_expr_node(builder, &node.expr);
        let (mut builder, value) = self.cond_to_value(builder, value);
        let value = match self.value_into_reg(&mut builder, value, &node.expr.ty) {
            mir::AnyReg::R(reg) => reg,
            mir::AnyReg::F(_) => unreachable!("ICE: non integer switch case"),
        };

        let (end_label, end_builder) = self.create_new_builder(&builder);

        let mut cases_builder: Option<Builder> = None;
        let mut switch_builder_and_expr_reg = (builder, value);

        let mut default_label = None;

        for case in &node.cases {
            let cur_label = self.function.create_block_label();

            let cur_builder = if let Some(prev_builder) = cases_builder {
                let cur_builder = self.create_builder_with_label(cur_label, &prev_builder);

                let jump_to_cur = mir::term::jump(prev_builder.create_block_ref(cur_label));
                self.function
                    .add_block(prev_builder.bb.terminate(jump_to_cur));

                cur_builder
            } else {
                let mut cur_builder =
                    self.create_builder_with_label(cur_label, &switch_builder_and_expr_reg.0);
                cur_builder.break_label = Some(end_label);
                cur_builder
            };

            let body = match &case.data {
                ir::SwitchStmtCase::Case { label, body } => {
                    let (mut switch_builder, switch_expr_reg) = switch_builder_and_expr_reg;

                    let (next_switch_label, mut next_switch_builder) =
                        self.create_new_builder(&switch_builder);
                    let label_reg = self.load_int_constant(&mut switch_builder, *label);

                    let eq_ref = switch_builder.create_block_ref(cur_label);
                    let mut neq_ref = switch_builder.create_block_ref(next_switch_label);
                    neq_ref.arguments.push(switch_expr_reg.into());

                    let term = mir::term::branch_if(
                        mir::BCond::Eq,
                        switch_expr_reg,
                        label_reg,
                        eq_ref,
                        neq_ref,
                    );
                    self.function.add_block(switch_builder.bb.terminate(term));

                    let next_switch_expr_reg = self.new_register();
                    next_switch_builder
                        .bb
                        .add_argument(next_switch_expr_reg.into());

                    switch_builder_and_expr_reg = (next_switch_builder, next_switch_expr_reg);

                    body
                }
                ir::SwitchStmtCase::Default { body } => {
                    default_label = Some(cur_label);
                    body
                }
            };
            let cur_builder = self.add_ir_block_node(cur_builder, body);

            cases_builder = Some(cur_builder);
        }

        if let Some(case_builder) = cases_builder {
            let jump_to_end = mir::term::jump(case_builder.create_block_ref(end_label));
            self.function
                .add_block(case_builder.bb.terminate(jump_to_end));
        }

        let last_jump = default_label.unwrap_or(end_label);

        let switch_builder = switch_builder_and_expr_reg.0;
        let jump_to_last = mir::term::jump(switch_builder.create_block_ref(last_jump));
        self.function
            .add_block(switch_builder.bb.terminate(jump_to_last));

        end_builder
    }

    pub fn add_ir_loop(&mut self, builder: Builder, node: &ir::LoopStmtNode) -> Builder {
        let (start_label, start_builder) = self.create_new_builder(&builder);

        let to_start = mir::term::jump(builder.create_block_ref(start_label));
        self.function.add_block(builder.bb.terminate(to_start));

        let (end_label, end_builder) = self.create_new_builder(&start_builder);

        let continuation_label = match &node.continuation {
            Some(_) => self.function.create_block_label(),
            None => start_label,
        };

        if let Some(continuation_node) = &node.continuation {
            let continuation_builder =
                self.create_builder_with_label(continuation_label, &start_builder);
            let continuation_builder = self
                .add_ir_expr_node(continuation_builder, continuation_node)
                .0;

            let continuation_terminator =
                mir::term::jump(continuation_builder.create_block_ref(start_label));
            self.function
                .add_block(continuation_builder.bb.terminate(continuation_terminator));
        }

        let mut body_builder = if let Some(condition_node) = &node.condition {
            let (mut start_builder, cond_value) =
                self.add_ir_expr_node(start_builder, condition_node);

            let (body_label, body_builder) = self.create_new_builder(&start_builder);

            let branching_terminator = self.make_condition(
                &mut start_builder,
                cond_value,
                &condition_node.ty,
                body_label,
                end_label,
            );
            self.function
                .add_block(start_builder.bb.terminate(branching_terminator));

            body_builder
        } else {
            start_builder
        };

        body_builder.continue_label = Some(continuation_label);
        body_builder.break_label = Some(end_label);

        let body_builder = self.add_ir_block_node(body_builder, &node.body);

        let terminator = mir::term::jump(body_builder.create_block_ref(continuation_label));
        self.function
            .add_block(body_builder.bb.terminate(terminator));

        end_builder
    }

    pub fn add_ir_break(&mut self, builder: Builder) -> Builder {
        let new_builder = self.create_new_builder(&builder).1;

        let term_ref = builder
            .break_label
            .expect("ICE: break while not in loop/switch");
        let terminator = mir::term::jump(builder.create_block_ref(term_ref));
        self.function.add_block(builder.bb.terminate(terminator));

        new_builder
    }

    pub fn add_ir_continue(&mut self, builder: Builder) -> Builder {
        let new_builder = self.create_new_builder(&builder).1;

        let term_ref = builder
            .continue_label
            .expect("ICE: continue while not in loop");
        let terminator = mir::term::jump(builder.create_block_ref(term_ref));
        self.function.add_block(builder.bb.terminate(terminator));

        new_builder
    }

    fn add_ir_return(&mut self, builder: Builder, node: Option<&ir::ExprNode>) -> Builder {
        let (builder, reg) = match node {
            Some(node) => {
                let (builder, value) = self.add_ir_expr_node(builder, node);
                let (mut builder, value) = self.cond_to_value(builder, value);
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

    // Adds the instructions to check if a floating point value equals zere, the result will be stored in teh
    pub fn add_float_zero_eq_check(&mut self, builder: &mut Builder, reg: mir::FReg, ty: &CType) {
        let fmt =
            util::ctype_floating_fmt(ty).expect("foat equality check needs to have floating type");
        let zero_reg = match fmt {
            mir::FFmt::S => {
                let zero_reg = self.new_float_register();
                builder
                    .bb
                    .add_instruction(mir::instr::move_to_fpu(mir::Reg::ZERO, zero_reg));
                zero_reg
            }
            mir::FFmt::D => {
                //IDEA: Add virtual instruction to load zero into double register
                let zero_const = self.root_generator.add_double_constant(0.0).clone();
                self.load_double(builder, zero_const)
            }
        };
        builder
            .bb
            .add_instruction(mir::instr::cmp(mir::FCmp::Eq(fmt), zero_reg, reg))
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

    pub fn value_into_reg(
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

    pub fn cond_to_value(
        &mut self,
        builder: Builder,
        value: MipsCondOrValue,
    ) -> (Builder, MipsValue) {
        match value {
            MipsCondOrValue::FloatCond { inverse } => {
                let (end_label, mut end_builder) = self.create_new_builder(&builder);
                let out_reg = self.new_register();
                end_builder.bb.add_argument(out_reg.into());

                let mut value_setter = |value: u16| {
                    let (label, mut builder) = self.create_new_builder(&builder);
                    let out_reg = self.new_register();
                    builder
                        .bb
                        .add_instruction(mir::instr::or_imm(out_reg, mir::Reg::ZERO, value));
                    let mut to_end = builder.create_block_ref(end_label);
                    to_end.arguments.push(out_reg.into());
                    self.function
                        .add_block(builder.bb.terminate(mir::term::jump(to_end)));
                    label
                };

                let set_one_label = value_setter(1);
                let set_zero_label = value_setter(0);

                let set_one_ref = builder.create_block_ref(set_one_label);
                let set_zero_ref = builder.create_block_ref(set_zero_label);
                let branch = mir::term::branch_if_f_cond(!inverse, set_one_ref, set_zero_ref);
                self.function.add_block(builder.bb.terminate(branch));
                (end_builder, out_reg.into())
            }
            MipsCondOrValue::Value(value) => (builder, value),
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
