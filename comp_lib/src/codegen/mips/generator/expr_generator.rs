use crate::ir::{self, ctype};

use super::{
    function_generator::{Builder, FunctionGenerator},
    mips_value::MipsValue,
    util::{self, OpType},
};

use mips_ir as mir;

pub struct BinaryExprGenerator<'e, 'fg, 'g, 'i, 's> {
    pub function_generator: &'fg mut FunctionGenerator<'g, 'i, 's>,
    pub left: &'e ir::ExprNode,
    pub right: &'e ir::ExprNode,
    pub to_type: &'e ir::ctype::CType,
}

impl BinaryExprGenerator<'_, '_, '_, '_, '_> {
    fn arith<IG, FG, DG>(
        self,
        builder: Builder,
        integer: IG,
        float: FG,
        double: DG,
    ) -> (Builder, MipsValue)
    where
        IG: FnOnce(mir::Reg, mir::Reg, mir::Reg, &mut Builder),
        FG: FnOnce(mir::FReg, mir::FReg, mir::FReg, &mut Builder),
        DG: FnOnce(mir::FReg, mir::FReg, mir::FReg, &mut Builder),
    {
        let (builder, left) = self.function_generator.add_ir_expr_node(builder, self.left);
        let (mut builder, right) = self
            .function_generator
            .add_ir_expr_node(builder, self.right);

        let op_type = util::ctype_op_type(self.to_type);

        let (left_reg, right_reg) = match (left, right) {
            (MipsValue::Imm(left), MipsValue::Imm(right)) => (
                self.function_generator
                    .imm_to_reg(&mut builder, left, &self.left.ty),
                self.function_generator
                    .imm_to_reg(&mut builder, right, &self.right.ty),
            ),
            (MipsValue::Imm(imm), MipsValue::Reg(reg)) => {
                let imm_reg = self
                    .function_generator
                    .imm_to_reg(&mut builder, imm, &self.left.ty);
                (imm_reg, reg)
            }
            (MipsValue::Reg(reg), MipsValue::Imm(imm)) => {
                let imm_reg = self
                    .function_generator
                    .imm_to_reg(&mut builder, imm, &self.right.ty);
                (reg, imm_reg)
            }
            (MipsValue::Reg(left), MipsValue::Reg(right)) => (left, right),
            (MipsValue::FReg(left), MipsValue::FReg(right)) => {
                let reg = match op_type {
                    OpType::Float => {
                        let out_reg = self.function_generator.new_float_register();
                        float(out_reg, left, right, &mut builder);
                        out_reg
                    }
                    OpType::Double => {
                        let out_reg = self.function_generator.new_double_register();
                        double(out_reg, left, right, &mut builder);
                        out_reg
                    }
                    _ => unreachable!(),
                };
                return (builder, MipsValue::FReg(reg));
            }
            _ => unreachable!(),
        };
        let out_reg = self.function_generator.new_register();
        integer(out_reg, left_reg, right_reg, &mut builder);
        (builder, MipsValue::Reg(out_reg))
    }

    fn integer<IG>(self, builder: Builder, integer: IG) -> (Builder, MipsValue)
    where
        IG: FnOnce(mir::Reg, mir::Reg, mir::Reg, &mut Builder),
    {
        self.arith(
            builder,
            integer,
            |_, _, _, _| panic!("ICE: Floaing values in integer expr"),
            |_, _, _, _| panic!("ICE: Double values in integer expr"),
        )
    }

    pub fn add_ir_mul(self, builder: Builder) -> (Builder, MipsValue) {
        let op_type = util::ctype_op_type(self.to_type);
        self.arith(
            builder,
            |out, left, right, builder| {
                let mul_inst = match op_type {
                    OpType::Unsigend => mir::instr::mult_u(left, right),
                    OpType::Signed => mir::instr::mult_s(left, right),
                    _ => unreachable!(),
                };
                builder.bb.add_instruction(mul_inst);
                builder.bb.add_instruction(mir::instr::move_from_lo(out));
            },
            |out, left, right, builder| {
                builder
                    .bb
                    .add_instruction(mir::instr::mul(mir::FFmt::S, out, left, right));
            },
            |out, left, right, builder| {
                builder
                    .bb
                    .add_instruction(mir::instr::mul(mir::FFmt::D, out, left, right));
            },
        )
    }

    pub fn add_ir_div(self, builder: Builder) -> (Builder, MipsValue) {
        let op_type = util::ctype_op_type(self.to_type);
        self.arith(
            builder,
            |out, left, right, builder| {
                let div_inst = match op_type {
                    OpType::Unsigend => mir::instr::div_u(left, right),
                    OpType::Signed => mir::instr::div_s(left, right),
                    _ => unreachable!(),
                };
                builder.bb.add_instruction(div_inst);
                builder.bb.add_instruction(mir::instr::move_from_lo(out));
            },
            |out, left, right, builder| {
                builder
                    .bb
                    .add_instruction(mir::instr::mul(mir::FFmt::S, out, left, right));
            },
            |out, left, right, builder| {
                builder
                    .bb
                    .add_instruction(mir::instr::mul(mir::FFmt::D, out, left, right));
            },
        )
    }

    pub fn add_ir_rem(self, builder: Builder) -> (Builder, MipsValue) {
        let op_type = util::ctype_op_type(self.to_type);
        self.integer(builder, |out, left, right, builder| {
            let div_inst = match op_type {
                OpType::Unsigend => mir::instr::div_u(left, right),
                OpType::Signed => mir::instr::div_s(left, right),
                _ => unreachable!(),
            };
            builder.bb.add_instruction(div_inst);
            builder.bb.add_instruction(mir::instr::move_from_hi(out));
        })
    }

    pub fn integer_can_imm<RG, IG>(
        self,
        builder: Builder,
        preserve_order: bool,
        both_reg: RG,
        right_imm: IG,
    ) -> (Builder, MipsValue)
    where
        RG: FnOnce(mir::Reg, mir::Reg, mir::Reg) -> mir::Instruction,
        IG: FnOnce(mir::Reg, mir::Reg, u16) -> mir::Instruction,
    {
        let (builder, left) = self.function_generator.add_ir_expr_node(builder, self.left);
        let (mut builder, right) = self
            .function_generator
            .add_ir_expr_node(builder, self.right);

        let _op_type = util::ctype_op_type(self.to_type);

        let out_reg = self.function_generator.new_register();

        match (left, right) {
            (MipsValue::Imm(left), MipsValue::Imm(right)) => {
                let left_reg =
                    self.function_generator
                        .imm_to_reg(&mut builder, left, &self.left.ty);
                builder
                    .bb
                    .add_instruction(right_imm(out_reg, left_reg, right));
            }
            (MipsValue::Imm(left), MipsValue::Reg(right_reg)) => {
                if preserve_order {
                    let left_reg =
                        self.function_generator
                            .imm_to_reg(&mut builder, left, &self.left.ty);
                    builder
                        .bb
                        .add_instruction(both_reg(out_reg, left_reg, right_reg));
                } else {
                    builder
                        .bb
                        .add_instruction(right_imm(out_reg, right_reg, left));
                }
            }
            (MipsValue::Reg(left_reg), MipsValue::Imm(right)) => {
                builder
                    .bb
                    .add_instruction(right_imm(out_reg, left_reg, right));
            }
            (MipsValue::Reg(left_reg), MipsValue::Reg(right_reg)) => {
                builder
                    .bb
                    .add_instruction(both_reg(out_reg, left_reg, right_reg));
            }
            _ => unreachable!("ICE: floaing value in integer expr"),
        }
        (builder, MipsValue::Reg(out_reg))
    }

    pub fn add_ir_shift_left(self, builder: Builder) -> (Builder, MipsValue) {
        self.integer_can_imm(
            builder,
            true,
            mir::instr::shift_left_logical,
            mir::instr::shift_left_logical_imm,
        )
    }

    pub fn add_ir_shift_right(self, builder: Builder) -> (Builder, MipsValue) {
        self.integer_can_imm(
            builder,
            true,
            mir::instr::shift_right_arithmetic,
            mir::instr::shift_right_arithmetic_imm,
        )
    }

    pub fn add_ir_bitwise(self, builder: Builder, op: &ir::BitwiseOp) -> (Builder, MipsValue) {
        self.integer_can_imm(
            builder,
            false,
            |out, left, right| match op {
                ir::BitwiseOp::And => mir::instr::and(out, left, right),
                ir::BitwiseOp::Or => mir::instr::or(out, left, right),
                ir::BitwiseOp::Xor => mir::instr::xor(out, left, right),
            },
            |out, left, right| match op {
                ir::BitwiseOp::And => mir::instr::and_imm(out, left, right),
                ir::BitwiseOp::Or => mir::instr::or_imm(out, left, right),
                ir::BitwiseOp::Xor => mir::instr::xor_imm(out, left, right),
            },
        )
    }

    pub fn add_ir_add(self, builder: Builder) -> (Builder, MipsValue) {
        let left_is_ptr = matches!(
            &self.left.ty,
            ctype::CType::Scalar(ctype::Scalar::Pointer(..))
        );
        let (builder, left) = self.function_generator.add_ir_expr_node(builder, self.left);
        let (mut builder, right) = self
            .function_generator
            .add_ir_expr_node(builder, self.right);

        let op_type = util::ctype_op_type(self.to_type);

        fn pointer_add(
            function_generator: &mut FunctionGenerator,
            builder: &mut Builder,
            out_reg: mir::Reg,
            int_reg: mir::Reg,
            ptr_reg: mir::Reg,
            size: u128,
        ) {
            let size_reg = function_generator.new_register();
            if (size >> 32) > 0 {
                // TODO give nice Diagnostic error somehow
                todo!("ICE: to big pointer type");
            } else if (size >> 16) > 0 {
                builder
                    .bb
                    .add_instruction(mir::instr::load_upper(size_reg, (size >> 16) as u16));
            }
            builder
                .bb
                .add_instruction(mir::instr::or_imm(size_reg, mir::Reg::ZERO, size as u16));

            builder
                .bb
                .add_instruction(mir::instr::mult_u(int_reg, size_reg));
            let offset_reg = function_generator.new_register();
            builder
                .bb
                .add_instruction(mir::instr::move_from_lo(offset_reg));

            builder
                .bb
                .add_instruction(mir::instr::add_u(out_reg, ptr_reg, offset_reg));
        }

        let out_value = match (left, right) {
            (MipsValue::Imm(imm), MipsValue::Reg(reg))
            | (MipsValue::Reg(reg), MipsValue::Imm(imm)) => {
                let out_reg = self.function_generator.new_register();
                match op_type {
                    OpType::Unsigend | OpType::Signed => {
                        // `addui` still does signed addition but without rasing exceptions.
                        // (just like C)
                        builder
                            .bb
                            .add_instruction(mir::instr::add_u_imm(out_reg, reg, imm));
                    }
                    OpType::Pointer(size) => {
                        if let Some(offset) = i16::try_from(size)
                            .ok()
                            .and_then(|size| (imm as i16).checked_mul(size))
                        {
                            // optimization to do the size multiplication at compile time, if the
                            // offset fits in a immidiat mode operation.
                            builder.bb.add_instruction(mir::instr::add_u_imm(
                                out_reg,
                                reg,
                                offset as u16,
                            ));
                        } else {
                            let non_ptr_ty = match left_is_ptr {
                                true => &self.right.ty,
                                false => &self.left.ty,
                            };
                            let int_reg =
                                self.function_generator
                                    .imm_to_reg(&mut builder, imm, non_ptr_ty);
                            pointer_add(
                                self.function_generator,
                                &mut builder,
                                out_reg,
                                int_reg,
                                reg,
                                size,
                            );
                        }
                    }
                    _ => unreachable!(),
                }
                MipsValue::Reg(out_reg)
            }
            (MipsValue::Imm(left), MipsValue::Imm(right)) => {
                let left_reg =
                    self.function_generator
                        .imm_to_reg(&mut builder, left, &self.left.ty);
                let out_reg = self.function_generator.new_register();
                builder
                    .bb
                    .add_instruction(mir::instr::add_u_imm(out_reg, left_reg, right));
                MipsValue::Reg(out_reg)
            }
            (MipsValue::Reg(left), MipsValue::Reg(right)) => {
                let out_reg = self.function_generator.new_register();
                match op_type {
                    OpType::Unsigend | OpType::Signed => {
                        builder
                            .bb
                            .add_instruction(mir::instr::add_u(out_reg, left, right));
                    }
                    OpType::Pointer(size) => {
                        if left_is_ptr {
                            pointer_add(
                                self.function_generator,
                                &mut builder,
                                out_reg,
                                right,
                                left,
                                size,
                            );
                        } else {
                            pointer_add(
                                self.function_generator,
                                &mut builder,
                                out_reg,
                                left,
                                right,
                                size,
                            );
                        }
                    }
                    _ => unreachable!(),
                }
                MipsValue::Reg(out_reg)
            }
            (MipsValue::FReg(left), MipsValue::FReg(right)) => match op_type {
                OpType::Float => {
                    let out_reg = self.function_generator.new_float_register();
                    builder
                        .bb
                        .add_instruction(mir::instr::add(mir::FFmt::S, out_reg, left, right));
                    MipsValue::FReg(out_reg)
                }
                OpType::Double => {
                    let out_reg = self.function_generator.new_double_register();
                    builder
                        .bb
                        .add_instruction(mir::instr::add(mir::FFmt::D, out_reg, left, right));
                    MipsValue::FReg(out_reg)
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
        (builder, out_value)
    }

    pub fn add_ir_sub(self, _builder: Builder) -> (Builder, MipsValue) {
        todo!("subtaction is not implemented yet")
    }
}
