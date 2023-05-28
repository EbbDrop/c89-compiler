use crate::ir::{self, ctype};

use super::{
    function_generator::{Builder, FunctionGenerator},
    mips_value::{MipsCondOrValue, MipsValue},
    util::{self, OpType},
};

use mips_ir as mir;

pub struct UnaryExprGenerator<'e, 'fg, 'g, 'i, 's> {
    pub function_generator: &'fg mut FunctionGenerator<'g, 'i, 's>,
    pub expr: &'e ir::ExprNode,
    pub to_type: &'e ir::ctype::CType,
}

impl UnaryExprGenerator<'_, '_, '_, '_, '_> {
    pub fn add_ir_neg(self, builder: Builder) -> (Builder, MipsCondOrValue) {
        let (builder, value) = self.function_generator.add_ir_expr_node(builder, self.expr);
        let (mut builder, value) = self.function_generator.cond_to_value(builder, value);

        let value = match value {
            MipsValue::Imm(imm) => MipsCondOrValue::Value(MipsValue::Imm(-(imm as i16) as u16)),
            MipsValue::Reg(reg) => {
                let out_reg = self.function_generator.new_register();
                builder
                    .bb
                    .add_instruction(mir::instr::sub_u(out_reg, mir::Reg::ZERO, reg));
                out_reg.into()
            }
            MipsValue::FReg(reg) => {
                let ffmt = util::ctype_floating_fmt(&self.expr.ty).unwrap();
                let out_reg = self.function_generator.new_ffmt_register(ffmt);
                builder
                    .bb
                    .add_instruction(mir::instr::neg(ffmt, out_reg, reg));
                out_reg.into()
            }
        };
        (builder, value)
    }

    pub fn add_ir_bitnot(self, builder: Builder) -> (Builder, MipsCondOrValue) {
        let (builder, value) = self.function_generator.add_ir_expr_node(builder, self.expr);
        let (mut builder, value) = self.function_generator.cond_to_value(builder, value);

        let value = match value {
            MipsValue::Imm(imm) => MipsCondOrValue::Value(MipsValue::Imm(!imm)),
            MipsValue::Reg(reg) => {
                let out_reg = self.function_generator.new_register();
                builder
                    .bb
                    .add_instruction(mir::instr::nor(out_reg, mir::Reg::ZERO, reg));
                out_reg.into()
            }
            MipsValue::FReg(_) => unreachable!(),
        };

        (builder, value)
    }

    pub fn add_ir_not(self, builder: Builder) -> (Builder, MipsCondOrValue) {
        let (mut builder, value) = self.function_generator.add_ir_expr_node(builder, self.expr);

        let value = match value {
            MipsCondOrValue::Value(MipsValue::Imm(imm)) => match imm {
                0 => MipsCondOrValue::Value(MipsValue::Imm(1)),
                _ => MipsCondOrValue::Value(MipsValue::Imm(0)),
            },
            MipsCondOrValue::Value(MipsValue::Reg(reg)) => {
                let out_reg = self.function_generator.new_register();
                builder
                    .bb
                    .add_instruction(mir::instr::set_lt_u_imm(out_reg, reg, 1));
                out_reg.into()
            }
            MipsCondOrValue::Value(MipsValue::FReg(reg)) => {
                self.function_generator
                    .add_float_zero_eq_check(&mut builder, reg, &self.expr.ty);
                MipsCondOrValue::FloatCond { inverse: true }
            }
            MipsCondOrValue::FloatCond { inverse } => {
                MipsCondOrValue::FloatCond { inverse: !inverse }
            }
        };

        (builder, value)
    }
}

pub struct BinaryExprGenerator<'e, 'fg, 'g, 'i, 's> {
    pub function_generator: &'fg mut FunctionGenerator<'g, 'i, 's>,
    pub left: &'e ir::ExprNode,
    pub right: &'e ir::ExprNode,
    pub to_type: &'e ir::ctype::CType,
}

enum OrderImportance {
    PreserveOrder,
    AllowReorder,
}

enum ShouldAddOne {
    AddOne,
    DontAddOne,
}

impl BinaryExprGenerator<'_, '_, '_, '_, '_> {
    fn eval_inner(&mut self, builder: Builder) -> (Builder, MipsValue, MipsValue) {
        let (builder, left) = self.function_generator.add_ir_expr_node(builder, self.left);
        let (mut builder, left) = self.function_generator.cond_to_value(builder, left);

        let left = builder.push_to_expr_stack(left, &self.left.ty);

        let (builder, right) = self
            .function_generator
            .add_ir_expr_node(builder, self.right);
        let (mut builder, right) = self.function_generator.cond_to_value(builder, right);

        let left = builder.pop_off_expr_stack(left);

        (builder, left, right)
    }

    fn arith<IG, FG, DG>(
        mut self,
        builder: Builder,
        integer: IG,
        float: FG,
        double: DG,
    ) -> (Builder, MipsCondOrValue)
    where
        IG: FnOnce(mir::Reg, mir::Reg, mir::Reg, &mut Builder),
        FG: FnOnce(mir::FReg, mir::FReg, mir::FReg, &mut Builder),
        DG: FnOnce(mir::FReg, mir::FReg, mir::FReg, &mut Builder),
    {
        let (mut builder, left, right) = self.eval_inner(builder);

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
                return (builder, reg.into());
            }
            _ => unreachable!(),
        };
        let out_reg = self.function_generator.new_register();
        integer(out_reg, left_reg, right_reg, &mut builder);
        (builder, out_reg.into())
    }

    fn integer<IG>(self, builder: Builder, integer: IG) -> (Builder, MipsCondOrValue)
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

    pub fn add_ir_mul(self, builder: Builder) -> (Builder, MipsCondOrValue) {
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

    pub fn add_ir_div(self, builder: Builder) -> (Builder, MipsCondOrValue) {
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

    pub fn add_ir_rem(self, builder: Builder) -> (Builder, MipsCondOrValue) {
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

    fn integer_can_imm<RG, IG>(
        mut self,
        builder: Builder,
        order_importance: OrderImportance,
        both_reg: RG,
        right_imm: IG,
    ) -> (Builder, MipsCondOrValue)
    where
        RG: FnOnce(mir::Reg, mir::Reg, mir::Reg) -> mir::Instruction,
        IG: FnOnce(mir::Reg, mir::Reg, u16) -> mir::Instruction,
    {
        let (mut builder, left, right) = self.eval_inner(builder);

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
            (MipsValue::Imm(left), MipsValue::Reg(right_reg)) => match order_importance {
                OrderImportance::PreserveOrder => {
                    let left_reg =
                        self.function_generator
                            .imm_to_reg(&mut builder, left, &self.left.ty);
                    builder
                        .bb
                        .add_instruction(both_reg(out_reg, left_reg, right_reg));
                }
                OrderImportance::AllowReorder => {
                    builder
                        .bb
                        .add_instruction(right_imm(out_reg, right_reg, left));
                }
            },
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
        (builder, out_reg.into())
    }

    pub fn add_ir_shift_left(self, builder: Builder) -> (Builder, MipsCondOrValue) {
        self.integer_can_imm(
            builder,
            OrderImportance::PreserveOrder,
            mir::instr::shift_left_logical,
            mir::instr::shift_left_logical_imm,
        )
    }

    pub fn add_ir_shift_right(self, builder: Builder) -> (Builder, MipsCondOrValue) {
        self.integer_can_imm(
            builder,
            OrderImportance::PreserveOrder,
            mir::instr::shift_right_arithmetic,
            mir::instr::shift_right_arithmetic_imm,
        )
    }

    pub fn add_ir_bitwise(
        self,
        builder: Builder,
        op: &ir::BitwiseOp,
    ) -> (Builder, MipsCondOrValue) {
        self.integer_can_imm(
            builder,
            OrderImportance::AllowReorder,
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

    pub fn add_ir_add(mut self, builder: Builder) -> (Builder, MipsCondOrValue) {
        let (mut builder, left, right) = self.eval_inner(builder);

        let left_is_ptr = matches!(
            &self.left.ty,
            ctype::CType::Scalar(ctype::Scalar::Pointer(..))
        );

        let op_type = util::ctype_op_type(self.to_type);

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
                            pointer_arith(
                                self.function_generator,
                                &mut builder,
                                out_reg,
                                int_reg,
                                reg,
                                size,
                                PointerArithOp::Add,
                            );
                        }
                    }
                    _ => unreachable!(),
                }
                out_reg.into()
            }
            (MipsValue::Imm(left), MipsValue::Imm(right)) => {
                let left_reg =
                    self.function_generator
                        .imm_to_reg(&mut builder, left, &self.left.ty);
                let out_reg = self.function_generator.new_register();
                builder
                    .bb
                    .add_instruction(mir::instr::add_u_imm(out_reg, left_reg, right));
                out_reg.into()
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
                            pointer_arith(
                                self.function_generator,
                                &mut builder,
                                out_reg,
                                right,
                                left,
                                size,
                                PointerArithOp::Add,
                            );
                        } else {
                            pointer_arith(
                                self.function_generator,
                                &mut builder,
                                out_reg,
                                left,
                                right,
                                size,
                                PointerArithOp::Add,
                            );
                        }
                    }
                    _ => unreachable!(),
                }
                out_reg.into()
            }
            (MipsValue::FReg(left), MipsValue::FReg(right)) => match op_type {
                OpType::Float => {
                    let out_reg = self.function_generator.new_float_register();
                    builder
                        .bb
                        .add_instruction(mir::instr::add(mir::FFmt::S, out_reg, left, right));
                    out_reg.into()
                }
                OpType::Double => {
                    let out_reg = self.function_generator.new_double_register();
                    builder
                        .bb
                        .add_instruction(mir::instr::add(mir::FFmt::D, out_reg, left, right));
                    out_reg.into()
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
        (builder, out_value)
    }

    pub fn add_ir_sub(mut self, builder: Builder) -> (Builder, MipsCondOrValue) {
        let (mut builder, left, right) = self.eval_inner(builder);
        let left = self
            .function_generator
            .value_into_reg(&mut builder, left, &self.left.ty);

        let value = match (left, right) {
            (mir::AnyReg::R(left), MipsValue::Imm(value)) => {
                let out_reg = self.function_generator.new_register();
                match (&self.left.ty, &self.right.ty) {
                    (
                        ctype::CType::Scalar(ctype::Scalar::Pointer(ctype::Pointer {
                            inner, ..
                        })),
                        _,
                    ) => {
                        let size = util::ctype_props(inner).size;
                        if let Some(offset) = i16::try_from(size)
                            .ok()
                            .and_then(|size| (value as i16).checked_mul(-size))
                        {
                            // optimization to do the size multiplication at compile time, if the
                            // offset fits in a immidiat mode operation.
                            builder.bb.add_instruction(mir::instr::add_u_imm(
                                out_reg,
                                left,
                                offset as u16,
                            ));
                        } else {
                            let int_reg = self.function_generator.imm_to_reg(
                                &mut builder,
                                value,
                                &self.right.ty,
                            );
                            pointer_arith(
                                self.function_generator,
                                &mut builder,
                                out_reg,
                                int_reg,
                                left,
                                size,
                                PointerArithOp::Sub,
                            );
                        }
                    }
                    _ => {
                        builder.bb.add_instruction(mir::instr::add_u_imm(
                            out_reg,
                            left,
                            -(value as i16) as u16,
                        ));
                    }
                }
                out_reg.into()
            }
            (mir::AnyReg::R(left), MipsValue::Reg(right)) => {
                let out_reg = self.function_generator.new_register();
                match (&self.left.ty, &self.right.ty) {
                    (
                        ctype::CType::Scalar(ctype::Scalar::Pointer(ctype::Pointer {
                            inner, ..
                        })),
                        ctype::CType::Scalar(ctype::Scalar::Pointer { .. }),
                    ) => {
                        // pointer - pointer

                        let diff_reg = self.function_generator.new_register();
                        builder
                            .bb
                            .add_instruction(mir::instr::sub_u(diff_reg, left, right));

                        let size = util::ctype_props(inner).size;
                        let size_reg = self
                            .function_generator
                            .load_int_constant(&mut builder, size as i128);

                        builder
                            .bb
                            .add_instruction(mir::instr::div_s(diff_reg, size_reg));
                        builder
                            .bb
                            .add_instruction(mir::instr::move_from_lo(out_reg));
                    }
                    (
                        ctype::CType::Scalar(ctype::Scalar::Pointer(ctype::Pointer {
                            inner, ..
                        })),
                        _,
                    ) => {
                        // pointer - int
                        let size = util::ctype_props(inner).size;
                        pointer_arith(
                            self.function_generator,
                            &mut builder,
                            out_reg,
                            right,
                            left,
                            size,
                            PointerArithOp::Sub,
                        );
                    }
                    _ => {
                        // int - int
                        builder
                            .bb
                            .add_instruction(mir::instr::sub_u(out_reg, left, right));
                    }
                }
                out_reg.into()
            }
            (mir::AnyReg::F(left), MipsValue::FReg(right)) => {
                let ffmt = util::ctype_floating_fmt(self.to_type).unwrap();
                let out_reg = self.function_generator.new_ffmt_register(ffmt);
                builder
                    .bb
                    .add_instruction(mir::instr::sub(ffmt, out_reg, left, right));
                out_reg.into()
            }
            _ => unreachable!(),
        };
        (builder, value)
    }

    fn relation<RG, IG, FCmp>(
        mut self,
        builder: Builder,
        add_one_to_imm: ShouldAddOne,
        both_reg: RG,
        right_imm: IG,
        (float_cmp, float_inverse): (FCmp, bool),
    ) -> (Builder, MipsCondOrValue)
    where
        RG: FnOnce(
            mir::Reg,
            mir::Reg,
            bool,
            &mut Builder,
            &mut dyn FnMut() -> mir::Reg,
        ) -> MipsCondOrValue,
        IG: FnOnce(
            mir::Reg,
            u16,
            bool,
            &mut Builder,
            &mut dyn FnMut() -> mir::Reg,
        ) -> MipsCondOrValue,
        FCmp: FnOnce(mir::FFmt) -> mir::FCmp,
    {
        let (mut builder, left, right) = self.eval_inner(builder);
        let left = self
            .function_generator
            .value_into_reg(&mut builder, left, &self.left.ty);

        let signed = match util::ctype_op_type(&self.left.ty) {
            OpType::Unsigend => false,
            OpType::Signed => true,
            OpType::Float => true,
            OpType::Double => true,
            OpType::Pointer(_) => false,
        };

        let mut new_reg = || self.function_generator.new_register();

        let value = match (left, right) {
            (mir::AnyReg::R(left_reg), MipsValue::Imm(right)) => {
                match add_one_to_imm {
                    ShouldAddOne::AddOne => {
                        if let Some(one_more_imm) = (right as i16).checked_add(1) {
                            right_imm(
                                left_reg,
                                one_more_imm as u16,
                                signed,
                                &mut builder,
                                &mut new_reg,
                            )
                        } else {
                            // IDEA: Load the one more value directly and use a potentialy
                            // shortermethod.
                            let right_reg = self.function_generator.imm_to_reg(
                                &mut builder,
                                right,
                                &self.right.ty,
                            );

                            // Have to create a new closure here to delay the capturing of
                            // `self.function_generator`
                            both_reg(left_reg, right_reg, signed, &mut builder, &mut || {
                                self.function_generator.new_register()
                            })
                        }
                    }
                    ShouldAddOne::DontAddOne => {
                        right_imm(left_reg, right, signed, &mut builder, &mut new_reg)
                    }
                }
            }
            (mir::AnyReg::R(left_reg), MipsValue::Reg(right_reg)) => {
                both_reg(left_reg, right_reg, signed, &mut builder, &mut new_reg)
            }
            (mir::AnyReg::F(left), MipsValue::FReg(right)) => {
                let ffmt = util::ctype_floating_fmt(&self.left.ty).unwrap();
                builder
                    .bb
                    .add_instruction(mir::instr::cmp(float_cmp(ffmt), left, right));
                MipsCondOrValue::FloatCond {
                    inverse: float_inverse,
                }
            }
            _ => unreachable!("ICE: floaing value in integer expr"),
        };
        (builder, value)
    }

    pub fn add_ir_rel_eq(self, builder: Builder) -> (Builder, MipsCondOrValue) {
        self.relation(
            builder,
            ShouldAddOne::DontAddOne,
            |left, right, _, builder, new_reg| {
                let xor_reg = new_reg();
                builder
                    .bb
                    .add_instruction(mir::instr::xor(xor_reg, left, right));
                let out_reg = new_reg();
                builder
                    .bb
                    .add_instruction(mir::instr::set_lt_u_imm(out_reg, xor_reg, 1));
                out_reg.into()
            },
            |left, imm, _, builder, new_reg| {
                let xor_reg = new_reg();
                builder
                    .bb
                    .add_instruction(mir::instr::xor_imm(xor_reg, left, imm));
                let out_reg = new_reg();
                builder
                    .bb
                    .add_instruction(mir::instr::set_lt_u_imm(out_reg, xor_reg, 1));
                out_reg.into()
            },
            (mir::FCmp::Eq, false),
        )
    }

    pub fn add_ir_rel_ne(self, builder: Builder) -> (Builder, MipsCondOrValue) {
        self.relation(
            builder,
            ShouldAddOne::DontAddOne,
            |left, right, _, builder, new_reg| {
                let xor_reg = new_reg();
                builder
                    .bb
                    .add_instruction(mir::instr::xor(xor_reg, left, right));
                let out_reg = new_reg();
                builder
                    .bb
                    .add_instruction(mir::instr::set_lt_u(out_reg, mir::Reg::ZERO, xor_reg));
                out_reg.into()
            },
            |left, imm, _, builder, new_reg| {
                let xor_reg = new_reg();
                builder
                    .bb
                    .add_instruction(mir::instr::xor_imm(xor_reg, left, imm));
                let out_reg = new_reg();
                builder
                    .bb
                    .add_instruction(mir::instr::set_lt_u(out_reg, mir::Reg::ZERO, xor_reg));
                out_reg.into()
            },
            (mir::FCmp::Eq, true),
        )
    }

    pub fn add_ir_rel_lt(self, builder: Builder) -> (Builder, MipsCondOrValue) {
        self.relation(
            builder,
            ShouldAddOne::DontAddOne,
            |left, right, signed, builder, new_reg| {
                let out_reg = new_reg();
                if signed {
                    builder
                        .bb
                        .add_instruction(mir::instr::set_lt_s(out_reg, left, right));
                } else {
                    builder
                        .bb
                        .add_instruction(mir::instr::set_lt_u(out_reg, left, right));
                }
                out_reg.into()
            },
            |left, imm, signed, builder, new_reg| {
                let out_reg = new_reg();
                if signed {
                    builder
                        .bb
                        .add_instruction(mir::instr::set_lt_s_imm(out_reg, left, imm));
                } else {
                    builder
                        .bb
                        .add_instruction(mir::instr::set_lt_u_imm(out_reg, left, imm));
                }
                out_reg.into()
            },
            (mir::FCmp::Lt, false),
        )
    }

    pub fn add_ir_rel_gt(self, builder: Builder) -> (Builder, MipsCondOrValue) {
        self.relation(
            builder,
            ShouldAddOne::AddOne,
            |left, right, signed, builder, new_reg| {
                let out_reg = new_reg();
                if signed {
                    builder
                        .bb
                        .add_instruction(mir::instr::set_lt_s(out_reg, right, left));
                } else {
                    builder
                        .bb
                        .add_instruction(mir::instr::set_lt_u(out_reg, right, left));
                }
                out_reg.into()
            },
            |left, one_more_imm, signed, builder, new_reg| {
                let lte_reg = new_reg();
                if signed {
                    builder.bb.add_instruction(mir::instr::set_lt_s_imm(
                        lte_reg,
                        left,
                        one_more_imm,
                    ));
                } else {
                    builder.bb.add_instruction(mir::instr::set_lt_u_imm(
                        lte_reg,
                        left,
                        one_more_imm,
                    ));
                }
                let out_reg = new_reg();
                builder
                    .bb
                    .add_instruction(mir::instr::xor_imm(out_reg, lte_reg, 1));
                out_reg.into()
            },
            (mir::FCmp::Le, true),
        )
    }

    pub fn add_ir_rel_ge(self, builder: Builder) -> (Builder, MipsCondOrValue) {
        self.relation(
            builder,
            ShouldAddOne::DontAddOne,
            |left, right, signed, builder, new_reg| {
                let lt_reg = new_reg();
                let out_reg = new_reg();
                if signed {
                    builder
                        .bb
                        .add_instruction(mir::instr::set_lt_s(out_reg, left, right));
                } else {
                    builder
                        .bb
                        .add_instruction(mir::instr::set_lt_u(out_reg, left, right));
                }
                builder
                    .bb
                    .add_instruction(mir::instr::xor_imm(out_reg, lt_reg, 1));
                out_reg.into()
            },
            |left, imm, signed, builder, new_reg| {
                let lt_reg = new_reg();
                if signed {
                    builder
                        .bb
                        .add_instruction(mir::instr::set_lt_s_imm(lt_reg, left, imm));
                } else {
                    builder
                        .bb
                        .add_instruction(mir::instr::set_lt_u_imm(lt_reg, left, imm));
                }
                let out_reg = new_reg();
                builder
                    .bb
                    .add_instruction(mir::instr::xor_imm(out_reg, lt_reg, 1));
                out_reg.into()
            },
            (mir::FCmp::Lt, true),
        )
    }

    pub fn add_ir_rel_le(self, builder: Builder) -> (Builder, MipsCondOrValue) {
        self.relation(
            builder,
            ShouldAddOne::AddOne,
            |left, right, signed, builder, new_reg| {
                let gt_reg = new_reg();
                if signed {
                    builder
                        .bb
                        .add_instruction(mir::instr::set_lt_s(gt_reg, right, left));
                } else {
                    builder
                        .bb
                        .add_instruction(mir::instr::set_lt_u(gt_reg, right, left));
                }
                let out_reg = new_reg();
                builder
                    .bb
                    .add_instruction(mir::instr::xor_imm(out_reg, gt_reg, 1));
                out_reg.into()
            },
            |left, one_more_imm, signed, builder, new_reg| {
                let out_reg = new_reg();
                if signed {
                    builder.bb.add_instruction(mir::instr::set_lt_s_imm(
                        out_reg,
                        left,
                        one_more_imm,
                    ));
                } else {
                    builder.bb.add_instruction(mir::instr::set_lt_u_imm(
                        out_reg,
                        left,
                        one_more_imm,
                    ));
                }
                out_reg.into()
            },
            (mir::FCmp::Le, false),
        )
    }
}

enum PointerArithOp {
    Add,
    Sub,
}

fn pointer_arith(
    function_generator: &mut FunctionGenerator,
    builder: &mut Builder,
    out_reg: mir::Reg,
    int_reg: mir::Reg,
    ptr_reg: mir::Reg,
    size: u128,
    op_type: PointerArithOp,
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

    match op_type {
        PointerArithOp::Add => {
            builder
                .bb
                .add_instruction(mir::instr::add_u(out_reg, ptr_reg, offset_reg));
        }
        PointerArithOp::Sub => {
            builder
                .bb
                .add_instruction(mir::instr::sub_u(out_reg, ptr_reg, offset_reg));
        }
    }
}
