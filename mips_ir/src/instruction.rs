use crate::{BlockRef, FReg, Label, Reg};

pub mod instr {
    use super::*;

    pub fn nop() -> Instruction {
        Instruction::Nop
    }

    /// Add the signed values in the second and third register with overflow, and store the result
    /// in the first register.
    pub fn add_s(rd: Reg, rs: Reg, rt: Reg) -> Instruction {
        Instruction::Reg3(RegOp3::AddS, rd, rs, rt)
    }

    /// Add the unsigned values in the second and third register without overflow, and store the
    /// result in the first register.
    pub fn add_u(rd: Reg, rs: Reg, rt: Reg) -> Instruction {
        Instruction::Reg3(RegOp3::AddU, rd, rs, rt)
    }

    /// Subtract the signed values in the second and third register with overflow, and store the
    /// result in the first register.
    pub fn sub_s(rd: Reg, rs: Reg, rt: Reg) -> Instruction {
        Instruction::Reg3(RegOp3::SubS, rd, rs, rt)
    }

    /// Subtract the unsigned values in the second and third register without overflow, and store
    /// the result in the first register.
    pub fn sub_u(rd: Reg, rs: Reg, rt: Reg) -> Instruction {
        Instruction::Reg3(RegOp3::SubU, rd, rs, rt)
    }

    pub fn and(rd: Reg, rs: Reg, rt: Reg) -> Instruction {
        Instruction::Reg3(RegOp3::And, rd, rs, rt)
    }

    pub fn or(rd: Reg, rs: Reg, rt: Reg) -> Instruction {
        Instruction::Reg3(RegOp3::Or, rd, rs, rt)
    }

    pub fn nor(rd: Reg, rs: Reg, rt: Reg) -> Instruction {
        Instruction::Reg3(RegOp3::Nor, rd, rs, rt)
    }

    pub fn xor(rd: Reg, rs: Reg, rt: Reg) -> Instruction {
        Instruction::Reg3(RegOp3::Xor, rd, rs, rt)
    }

    pub fn shift_left_logical(rd: Reg, rs: Reg, rt: Reg) -> Instruction {
        Instruction::Reg3(RegOp3::ShiftLeftLogical, rd, rs, rt)
    }

    pub fn shift_right_logical(rd: Reg, rs: Reg, rt: Reg) -> Instruction {
        Instruction::Reg3(RegOp3::ShiftRightLogical, rd, rs, rt)
    }

    pub fn shift_right_arithmetic(rd: Reg, rs: Reg, rt: Reg) -> Instruction {
        Instruction::Reg3(RegOp3::ShiftRightArithmetic, rd, rs, rt)
    }

    /// Set the first register to 1 if the signed value in the second register is less than the
    /// signed value in the third register, otherwise set to 0.
    pub fn set_lt_s(rd: Reg, rs: Reg, rt: Reg) -> Instruction {
        Instruction::Reg3(RegOp3::SetLtS, rd, rs, rt)
    }

    /// Set the first register to 1 if the unsigned value in the second register is less than the
    /// unsigned value in the third register, otherwise set to 0.
    pub fn set_lt_u(rd: Reg, rs: Reg, rt: Reg) -> Instruction {
        Instruction::Reg3(RegOp3::SetLtU, rd, rs, rt)
    }

    /// Signed division with overflow. Divides the value in the first register by the value in the
    /// second register, then sets the LO register to the quotient and the HI register to the
    /// remainder.
    pub fn div_s(rd: Reg, rs: Reg) -> Instruction {
        Instruction::Reg2(RegOp2::DivS, rd, rs)
    }

    /// Unsigned division without overflow. Divides the value in the first register by the value in
    /// the second register, then sets the LO register to the quotient and the HI register to the
    /// remainder.
    pub fn div_u(rd: Reg, rs: Reg) -> Instruction {
        Instruction::Reg2(RegOp2::DivU, rd, rs)
    }

    /// Multiply the signed values in the first and second register, then set the HI register to the
    /// high-order 32 bits of the product and the LO register to the low-order 32 bits of the
    /// product.
    pub fn mult_s(rd: Reg, rs: Reg) -> Instruction {
        Instruction::Reg2(RegOp2::MultS, rd, rs)
    }

    /// Multiply the unsigned values in the first and second register, then set the HI register to
    /// the high-order 32 bits of the product and the LO register to the low-order 32 bits of the
    /// product.
    pub fn mult_u(rd: Reg, rs: Reg) -> Instruction {
        Instruction::Reg2(RegOp2::MultU, rd, rs)
    }

    /// Trap if the comparing the values in the first and second register by the specified condition
    /// yields true.
    pub fn trap_if(cond: TrapCond, rd: Reg, rs: Reg) -> Instruction {
        Instruction::Reg2(RegOp2::TrapIf(cond), rd, rs)
    }

    /// Copy the word from the HI register to the specified register.
    pub fn move_from_hi(rd: Reg) -> Instruction {
        Instruction::Reg1(RegOp1::MoveFromHi, rd)
    }

    /// Copy the word from the LO register to the specified register.
    pub fn move_from_lo(rd: Reg) -> Instruction {
        Instruction::Reg1(RegOp1::MoveFromLo, rd)
    }

    /// Copy the word from the specified register to the HI register.
    pub fn move_to_hi(rd: Reg) -> Instruction {
        Instruction::Reg1(RegOp1::MoveToHi, rd)
    }

    /// Copy the word from the specified register to the LO register.
    pub fn move_to_lo(rd: Reg) -> Instruction {
        Instruction::Reg1(RegOp1::MoveToLo, rd)
    }

    pub fn add_s_imm(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::AddS, rt, rs, imm)
    }

    pub fn add_u_imm(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::AddU, rt, rs, imm)
    }

    pub fn and_imm(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::And, rt, rs, imm)
    }

    pub fn or_imm(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::Or, rt, rs, imm)
    }

    pub fn xor_imm(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::Xor, rt, rs, imm)
    }

    pub fn shift_left_logical_imm(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::ShiftLeftLogical, rt, rs, imm)
    }

    pub fn shift_right_logical_imm(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::ShiftRightLogical, rt, rs, imm)
    }

    pub fn shift_right_arithmetic_imm(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::ShiftRightArithmetic, rt, rs, imm)
    }

    /// Set the first register to 1 if the signed value in the second register is less than the
    /// signed immediate value, otherwise set to 0.
    pub fn set_lt_s_imm(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::SetLtS, rt, rs, imm)
    }

    /// Set the first register to 1 if the unsigned value in the second register is less than the
    /// unsigned immediate value, otherwise set to 0.
    pub fn set_lt_u_imm(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::SetLtU, rt, rs, imm)
    }

    /// Trap if the comparing the value in the first register and the immediate value by the
    /// specified condition yields true.
    pub fn trap_if_imm(cond: TrapCondImm, rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::TrapIf(cond), rt, rs, imm)
    }

    //----------------------------------------------------------------------------------------------
    // Memory loads & stores
    //----------------------------------------------------------------------------------------------
    /// Load byte from memory and store sign-extended in the first register.
    /// Memory address computed by adding the value in the second register to the immediate value.
    pub fn load_byte_s(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::LoadByteS, rt, rs, imm)
    }

    /// Load byte from memory and store zero-extended in the first register.
    /// Memory address computed by adding the value in the second register to the immediate value.
    pub fn load_byte_u(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::LoadByteU, rt, rs, imm)
    }

    /// Load half from memory and store sign-extended in the first register.
    /// Memory address computed by adding the value in the second register to the immediate value.
    pub fn load_half_s(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::LoadHalfS, rt, rs, imm)
    }

    /// Load half from memory and store zero-extended in the first register.
    /// Memory address computed by adding the value in the second register to the immediate value.
    pub fn load_half_u(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::LoadHalfU, rt, rs, imm)
    }

    /// Load word from memory and store in the first register.
    /// Memory address computed by adding the value in the second register to the immediate value.
    pub fn load_word(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::LoadWord, rt, rs, imm)
    }

    pub fn load_word_left(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::LoadWordLeft, rt, rs, imm)
    }

    pub fn load_word_right(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::LoadWordRight, rt, rs, imm)
    }

    /// Store the low-order 8 bits of the first register in memory.
    /// Memory address computed by adding the value in the second register to the immediate value.
    pub fn store_byte(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::StoreByte, rt, rs, imm)
    }

    /// Store the low-order 16 bits of the first register in memory.
    /// Memory address computed by adding the value in the second register to the immediate value.
    pub fn store_half(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::StoreHalf, rt, rs, imm)
    }

    /// Store the word of the first register in memory.
    /// Memory address computed by adding the value in the second register to the immediate value.
    pub fn store_word(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::StoreWord, rt, rs, imm)
    }

    pub fn store_word_left(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::StoreWordLeft, rt, rs, imm)
    }

    pub fn store_word_right(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::StoreWordRight, rt, rs, imm)
    }

    /// Load a word from memory into the first registor for an atomic read-modify-write.
    ///
    /// Available from MIPS II.
    pub fn load_linked_word(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::LoadLinkedWord, rt, rs, imm)
    }

    /// Store a word from the first register to memory to complete an atomic read-modify-write.
    ///
    /// Available from MIPS II.
    pub fn store_conditional_word(rt: Reg, rs: Reg, imm: u16) -> Instruction {
        Instruction::Imm2(ImmOp2::StoreConditionalWord, rt, rs, imm)
    }

    /// Set high-order 16 bits of the first register to the 16-bit immediate and the low-order
    /// 16 bits to 0.
    pub fn load_upper(rt: Reg, imm: u16) -> Instruction {
        Instruction::Imm1(ImmOp1::LoadUpper, rt, imm)
    }

    /// Add the floating-point values in the second and third register and store the result in the
    /// first register.
    pub fn add(fmt: FFmt, fd: FReg, fs: FReg, ft: FReg) -> Instruction {
        Instruction::FReg3(FRegOp3::Add(fmt), fd, fs, ft)
    }

    /// Subtract the floating-point value in the third register from the floating-point value in the
    /// second register and store the result in the first register.
    pub fn sub(fmt: FFmt, fd: FReg, fs: FReg, ft: FReg) -> Instruction {
        Instruction::FReg3(FRegOp3::Sub(fmt), fd, fs, ft)
    }

    /// Divide the floating-point value in the second register by the floating-point value in the
    /// third register, and store the result in the first register.
    pub fn div(fmt: FFmt, fd: FReg, fs: FReg, ft: FReg) -> Instruction {
        Instruction::FReg3(FRegOp3::Div(fmt), fd, fs, ft)
    }

    /// Multiply the floating-point value in the second register by the floating-point value in the
    /// third register, and store the result in the first register.
    pub fn mul(fmt: FFmt, fd: FReg, fs: FReg, ft: FReg) -> Instruction {
        Instruction::FReg3(FRegOp3::Mul(fmt), fd, fs, ft)
    }

    /// Store the absolute value of the second register in the first register.
    pub fn abs(fmt: FFmt, fd: FReg, fs: FReg) -> Instruction {
        Instruction::FReg2(FRegOp2::Abs(fmt), fd, fs)
    }

    /// Store the negated value of the second register in the first register.
    pub fn neg(fmt: FFmt, fd: FReg, fs: FReg) -> Instruction {
        Instruction::FReg2(FRegOp2::Neg(fmt), fd, fs)
    }

    /// Store the square root of the value in the second register in the first register.
    pub fn sqrt(fmt: FFmt, fd: FReg, fs: FReg) -> Instruction {
        Instruction::FReg2(FRegOp2::Sqrt(fmt), fd, fs)
    }

    /// Compare the floating-point values in both registers and set the FPU CC 0 to the result.
    pub fn cmp(fmt: FCmp, fd: FReg, fs: FReg) -> Instruction {
        Instruction::FReg2(FRegOp2::Cmp(fmt), fd, fs)
    }

    /// Convert the floating-point value in the second register from the second fmt to the first fmt
    /// and store the result in the first register. The first and second fmt must be different!
    pub fn convert(to_fmt: FFmt, from_fmt: FFmt, fd: FReg, fs: FReg) -> Instruction {
        Instruction::FReg2(FRegOp2::Convert(to_fmt, from_fmt), fd, fs)
    }

    /// Convert the floating-point value in the second register from the specified fmt to an integer
    /// word and store the result in the first (FPU) register.
    pub fn convert_to_word(fmt: FFmt, fd: FReg, fs: FReg) -> Instruction {
        Instruction::FReg2(FRegOp2::ConvertToWord(fmt), fd, fs)
    }

    /// Convert the integer word value in the second (FPU) register to a floating-point
    /// value of the specified fmt and store the result in the first register.
    pub fn convert_from_word(fmt: FFmt, fd: FReg, fs: FReg) -> Instruction {
        Instruction::FReg2(FRegOp2::ConvertFromWord(fmt), fd, fs)
    }

    /// Copy the floating-point value from the second register to the first register.
    pub fn move_(fmt: FFmt, fd: FReg, fs: FReg) -> Instruction {
        Instruction::FReg2(FRegOp2::Move(fmt), fd, fs)
    }

    /// Load a word from memory and store it in the first (FPU) register.
    /// Memory address is computed by adding the value in the second (integer) register to the
    /// immediate value.
    pub fn load_word_to_fpu(ft: FReg, base: Reg, offset: u16) -> Instruction {
        Instruction::FImm(FImmOp::LoadWordToFpu, ft, base, offset)
    }

    /// Store the word from the first (FPU) register to memory.
    /// Memory address is computed by adding the value in the second (integer) register to the
    /// immediate value.
    pub fn store_word_from_fpu(ft: FReg, base: Reg, offset: u16) -> Instruction {
        Instruction::FImm(FImmOp::StoreWordFromFpu, ft, base, offset)
    }

    /// Load a doubleword from memory and store it in the first (even floating-point) register.
    /// Memory address is computed by adding the value in the second (integer) register to the
    /// immediate value.
    /// Loading a doubleword to an odd FPU register is not allowed.
    pub fn load_doubleword_to_fpu(ft: FReg, base: Reg, offset: u16) -> Instruction {
        Instruction::FImm(FImmOp::LoadDoublewordToFpu, ft, base, offset)
    }

    /// Store the word from the first (even FPU) register to memory.
    /// Memory address is computed by adding the value in the second (integer) register to the
    /// immediate value.
    /// Storing a doubleword from an odd FPU register is not allowed.
    pub fn store_doubleword_from_fpu(ft: FReg, base: Reg, offset: u16) -> Instruction {
        Instruction::FImm(FImmOp::StoreDoublewordFromFpu, ft, base, offset)
    }

    /// Copy the bits from the second (FPU) register to the first register.
    pub fn move_from_fpu(rt: Reg, fs: FReg) -> Instruction {
        Instruction::MoveFromFpu(rt, fs)
    }

    /// Copy the word from the first register to the second (FPU) register.
    pub fn move_to_fpu(rt: Reg, fs: FReg) -> Instruction {
        Instruction::MoveToFpu(rt, fs)
    }

    /// Execute the system call specified by the value in $v0
    pub fn syscall() -> Instruction {
        Instruction::Syscall
    }

    pub fn break_() -> Instruction {
        Instruction::Break
    }

    // Pseudo instructions supported by the assembler but not directly available in the MIPS ISA.
    pub mod pseudo {
        use super::*;

        pub fn load_address(rt: Reg, label: Label) -> Instruction {
            Instruction::Pseudo(PseudoInstruction::LoadAddress(rt, label))
        }
    }
}

pub mod term {
    use super::*;

    pub fn branch_if(
        cond: BCond,
        rs: Reg,
        rt: Reg,
        true_target: BlockRef,
        false_target: BlockRef,
    ) -> Terminator {
        Terminator::BranchIf(cond, rs, rt, true_target, false_target)
    }

    pub fn branch_if_z(
        cond: BZCond,
        rs: Reg,
        true_target: BlockRef,
        false_target: BlockRef,
    ) -> Terminator {
        Terminator::BranchIfZ(cond, rs, true_target, false_target)
    }

    pub fn branch_if_z_and_link(
        cond: BZalCond,
        rs: Reg,
        true_target: Label,
        false_target: BlockRef,
    ) -> Terminator {
        Terminator::BranchIfZAndLink(cond, rs, true_target, false_target)
    }

    pub fn branch_if_f_cond(b: bool, true_target: BlockRef, false_target: BlockRef) -> Terminator {
        Terminator::BranchIfFCond(b, true_target, false_target)
    }

    /// Jump to `target`.
    pub fn jump(target: BlockRef) -> Terminator {
        Terminator::Jump(target)
    }

    /// Store the address of the next instruction in `$ra` and jump to `target`. The address of
    /// `next_block` will be linked, so it should be the next sequential block.
    pub fn jump_and_link(target: Label, next_block: BlockRef) -> Terminator {
        Terminator::JumpAndLink(target, next_block)
    }

    /// Jump to the return address in `$ra`.
    pub fn return_to_ra() -> Terminator {
        Terminator::ReturnToRa
    }

    /// Store the address of the next instruction in `$ra` and jump to the address in register `rs`.
    /// The address of `next_block` will be linked, so it should be the next sequential block.
    pub fn jump_and_link_ra(rs: Reg, next_block: BlockRef) -> Terminator {
        Terminator::JumpAndLinkRa(rs, next_block)
    }

    /// Store the address of the next instruction in register `rd` and jump to the address in
    /// register `rs`. The address of `next_block` will be linked, so it should be the next
    /// sequential block.
    pub fn jump_and_link_reg(rd: Reg, rs: Reg, next_block: BlockRef) -> Terminator {
        Terminator::JumpAndLinkReg(rd, rs, next_block)
    }
}

/// A regular instructions that can appear inside basic blocks (i.e. doesn't branch).
#[derive(Debug, Clone)]
pub enum Instruction {
    Nop,
    Reg3(RegOp3, Reg, Reg, Reg),
    Reg2(RegOp2, Reg, Reg),
    Reg1(RegOp1, Reg),
    Imm2(ImmOp2, Reg, Reg, u16),
    Imm1(ImmOp1, Reg, u16),
    FReg3(FRegOp3, FReg, FReg, FReg),
    FReg2(FRegOp2, FReg, FReg),
    FImm(FImmOp, FReg, Reg, u16),
    /// Copy the bits from the second (FPU) register to the first register.
    MoveFromFpu(Reg, FReg),
    /// Copy the word from the second register to the first (FPU) register.
    MoveToFpu(Reg, FReg),
    /// Execute the system call specified by the value in $v0
    Syscall,
    Break,
    // Pseudo instructions supported by the assembler but not directly available in the MIPS ISA.
    Pseudo(PseudoInstruction),
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nop => f.write_str("nop"),
            Self::Reg3(op, rd, rs, rt) => write!(f, "{op}\t{rd}, {rs}, {rt}"),
            Self::Reg2(op, rd, rs) => write!(f, "{op}\t{rd}, {rs}"),
            Self::Reg1(op, rd) => write!(f, "{op}\t{rd}"),
            Self::Imm2(op, rt, rs, imm) => write!(f, "{op}\t{rt}, {rs}, {imm}"),
            Self::Imm1(op, rt, imm) => write!(f, "{op}\t{rt}, {imm}"),
            Self::FReg3(op, fd, fs, ft) => write!(f, "{op}\t{fd}, {fs}, {ft}"),
            Self::FReg2(op, fd, fs) => write!(f, "{op}\t{fd}, {fs}",),
            Self::FImm(op, ft, base, offset) => write!(f, "{op}\t{ft}, {offset}({base})"),
            Self::MoveFromFpu(rt, fs) => write!(f, "mfc1\t{rt}, {fs}"),
            Self::MoveToFpu(rt, fs) => write!(f, "mtc1\t{rt}, {fs}"),
            Self::Syscall => f.write_str("syscall"),
            Self::Break => f.write_str("break"),
            Self::Pseudo(pseudo) => pseudo.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PseudoInstruction {
    LoadAddress(Reg, Label),
}

impl std::fmt::Display for PseudoInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LoadAddress(rt, label) => write!(f, "la\t{rt}, {label}"),
        }
    }
}

/// An instruction that branches and is used to terminate basic blocks.
#[derive(Debug, Clone)]
pub enum Terminator {
    BranchIf(BCond, Reg, Reg, BlockRef, BlockRef),
    BranchIfZ(BZCond, Reg, BlockRef, BlockRef),
    BranchIfZAndLink(BZalCond, Reg, Label, BlockRef),
    BranchIfFCond(bool, BlockRef, BlockRef),
    Jump(BlockRef),
    JumpAndLink(Label, BlockRef),
    /// Unconditional jump to `$ra`. Replaces the `jr` instruction for now.
    ReturnToRa,
    JumpAndLinkRa(Reg, BlockRef),
    JumpAndLinkReg(Reg, Reg, BlockRef),
}

impl std::fmt::Display for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BranchIf(cond, rs, rt, target, _) => write!(f, "b{cond}\t{rs}, {rt}, {target}"),
            Self::BranchIfZ(cond, rs, target, _) => write!(f, "b{cond}\t{rs}, {target}"),
            Self::BranchIfZAndLink(cond, rs, target, _) => write!(f, "b{cond}al\t{rs}, {target}"),
            Self::BranchIfFCond(b, target, _) => {
                write!(f, "bc1{}\t{target}", if *b { 't' } else { 'f' })
            }
            Self::Jump(target) => write!(f, "j\t{target}"),
            Self::JumpAndLink(target, _) => write!(f, "jal\t{target}"),
            Self::ReturnToRa => write!(f, "jr\t{}", Reg::RA),
            Self::JumpAndLinkRa(rs, _) => write!(f, "jalr\t{rs}"),
            Self::JumpAndLinkReg(rd, rs, _) => write!(f, "jalr\t{rd}, {rs}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegOp3 {
    /// Add the signed values in the second and third register with overflow, and store the result
    /// in the first register.
    AddS,
    /// Add the unsigned values in the second and third register without overflow, and store the
    /// result in the first register.
    AddU,
    /// Subtract the signed values in the second and third register with overflow, and store the
    /// result in the first register.
    SubS,
    /// Subtract the unsigned values in the second and third register without overflow, and store
    /// the result in the first register.
    SubU,
    And,
    Or,
    Nor,
    Xor,
    ShiftLeftLogical,
    ShiftRightLogical,
    ShiftRightArithmetic,
    //
    // Not yet supported in MIPS I.
    //----------------------------------------------------------------------------------------------
    // /// Copy the word from the second register to the first register if the third register is zero.
    // MoveIfZero,
    // /// Copy the word from the second register to the first regsiter if the third register is not
    // /// zero.
    // MoveIfNotZero,
    //----------------------------------------------------------------------------------------------
    //
    /// Set the first register to 1 if the signed value in the second register is less than the
    /// signed value in the third register, otherwise set to 0.
    SetLtS,
    /// Set the first register to 1 if the unsigned value in the second register is less than the
    /// unsigned value in the third register, otherwise set to 0.
    SetLtU,
}

impl std::fmt::Display for RegOp3 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::AddS => "add",
            Self::AddU => "addu",
            Self::SubS => "sub",
            Self::SubU => "subu",
            Self::And => "and",
            Self::Or => "or",
            Self::Nor => "nor",
            Self::Xor => "xor",
            Self::ShiftLeftLogical => "sllv",
            Self::ShiftRightLogical => "srlv",
            Self::ShiftRightArithmetic => "srav",
            Self::SetLtS => "slt",
            Self::SetLtU => "sltu",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegOp2 {
    // Not yet supported in MIPS I.
    // /// Count the number of leading ones in the second register and store the count in the first
    // /// register.
    // CountLeadingOnes,
    // /// Count the number of leading zeros in the second register and store the count in the first
    // /// register.
    // CountLeadingZeros,
    /// Signed division with overflow. Divides the value in the first register by the value in the
    /// second register, then sets the LO register to the quotient and the HI register to the
    /// remainder.
    DivS,
    /// Unsigned division without overflow. Divides the value in the first register by the value in
    /// the second register, then sets the LO register to the quotient and the HI register to the
    /// remainder.
    DivU,
    /// Multiply the signed values in the first and second register, then set the HI register to the
    /// high-order 32 bits of the product and the LO register to the low-order 32 bits of the
    /// product.
    MultS,
    /// Multiply the unsigned values in the first and second register, then set the HI register to
    /// the high-order 32 bits of the product and the LO register to the low-order 32 bits of the
    /// product.
    MultU,
    // Not yet supported in MIPS I.
    // /// Multiply the signed values in the first and second register, then increment the value in the
    // /// HI register by the high-order 32 bits of the product and the value in the LO register by the
    // /// low-order 32 bits of the product.
    // MultAddS,
    // /// Multiply the unsigned values in the first and second register, then increment the value in
    // /// the HI register by the high-order 32 bits of the product and the value in the LO register by
    // /// the low-order 32 bits of the product.
    // MultAddU,
    // /// Multiply the signed values in the first and second register, then decrement the value in the
    // /// HI register by the high-order 32 bits of the product and the value in the LO register by the
    // /// low-order 32 bits of the product.
    // MultSubS,
    // /// Multiply the unsigned values in the first and second register, then decrement the value in
    // /// the HI register by the high-order 32 bits of the product and the value in the LO register by
    // /// the low-order 32 bits of the product.
    // MultSubU,
    /// Trap if the comparing the values in the first and second register by the specified condition
    /// yields true.
    TrapIf(TrapCond),
}

impl std::fmt::Display for RegOp2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DivS => f.write_str("div"),
            Self::DivU => f.write_str("divu"),
            Self::MultS => f.write_str("mult"),
            Self::MultU => f.write_str("multu"),
            Self::TrapIf(cond) => write!(f, "t{cond}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegOp1 {
    /// Copy the word from the HI register to the specified register.
    MoveFromHi,
    /// Copy the word from the LO register to the specified register.
    MoveFromLo,
    /// Copy the word from the specified register to the HI register.
    MoveToHi,
    /// Copy the word from the specified register to the LO register.
    MoveToLo,
}

impl std::fmt::Display for RegOp1 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::MoveFromHi => "mfhi",
            Self::MoveFromLo => "mflo",
            Self::MoveToHi => "mthi",
            Self::MoveToLo => "mtlo",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImmOp2 {
    AddS,
    AddU,
    And,
    Or,
    Xor,
    ShiftLeftLogical,
    ShiftRightLogical,
    ShiftRightArithmetic,
    /// Set the first register to 1 if the signed value in the second register is less than the
    /// signed immediate value, otherwise set to 0.
    SetLtS,
    /// Set the first register to 1 if the unsigned value in the second register is less than the
    /// unsigned immediate value, otherwise set to 0.
    SetLtU,
    /// Trap if the comparing the value in the first register and the immediate value by the
    /// specified condition yields true.
    TrapIf(TrapCondImm),
    //----------------------------------------------------------------------------------------------
    // Memory loads & stores
    //----------------------------------------------------------------------------------------------
    /// Load byte from memory and store sign-extended in the first register.
    /// Memory address computed by adding the value in the second register to the immediate value.
    LoadByteS,
    /// Load byte from memory and store zero-extended in the first register.
    /// Memory address computed by adding the value in the second register to the immediate value.
    LoadByteU,
    /// Load half from memory and store sign-extended in the first register.
    /// Memory address computed by adding the value in the second register to the immediate value.
    LoadHalfS,
    /// Load half from memory and store zero-extended in the first register.
    /// Memory address computed by adding the value in the second register to the immediate value.
    LoadHalfU,
    /// Load word from memory and store in the first register.
    /// Memory address computed by adding the value in the second register to the immediate value.
    LoadWord,
    LoadWordLeft,
    LoadWordRight,
    /// Store the low-order 8 bits of the first register in memory.
    /// Memory address computed by adding the value in the second register to the immediate value.
    StoreByte,
    /// Store the low-order 16 bits of the first register in memory.
    /// Memory address computed by adding the value in the second register to the immediate value.
    StoreHalf,
    /// Store the word of the first register in memory.
    /// Memory address computed by adding the value in the second register to the immediate value.
    StoreWord,
    StoreWordLeft,
    StoreWordRight,
    /// Load a word from memory into the first registor for an atomic read-modify-write.
    ///
    /// Available from MIPS II.
    LoadLinkedWord,
    /// Store a word from the first register to memory to complete an atomic read-modify-write.
    ///
    /// Available from MIPS II.
    StoreConditionalWord,
}

impl std::fmt::Display for ImmOp2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AddS => f.write_str("addi"),
            Self::AddU => f.write_str("addiu"),
            Self::And => f.write_str("andi"),
            Self::Or => f.write_str("ori"),
            Self::Xor => f.write_str("xori"),
            Self::ShiftLeftLogical => f.write_str("sll"),
            Self::ShiftRightLogical => f.write_str("srl"),
            Self::ShiftRightArithmetic => f.write_str("sra"),
            Self::SetLtS => f.write_str("slti"),
            Self::SetLtU => f.write_str("sltiu"),
            Self::TrapIf(cond) => write!(f, "t{cond}"),
            // Memory loads & stores
            Self::LoadByteS => f.write_str("lb"),
            Self::LoadByteU => f.write_str("lbu"),
            Self::LoadHalfS => f.write_str("lh"),
            Self::LoadHalfU => f.write_str("lhu"),
            Self::LoadWord => f.write_str("lw"),
            Self::LoadWordLeft => f.write_str("lwl"),
            Self::LoadWordRight => f.write_str("lwr"),
            Self::StoreByte => f.write_str("sb"),
            Self::StoreHalf => f.write_str("sh"),
            Self::StoreWord => f.write_str("sw"),
            Self::StoreWordLeft => f.write_str("swl"),
            Self::StoreWordRight => f.write_str("swr"),
            Self::LoadLinkedWord => f.write_str("ll"),
            Self::StoreConditionalWord => f.write_str("sc"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImmOp1 {
    /// Set high-order 16 bits of the first register to the 16-bit immediate and the low-order
    /// 16 bits to 0.
    LoadUpper,
}

impl std::fmt::Display for ImmOp1 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::LoadUpper => "lui",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FRegOp3 {
    /// Add the floating-point values in the second and third register and store the result in the
    /// first register.
    Add(FFmt),
    /// Subtract the floating-point value in the third register from the floating-point value in the
    /// second register and store the result in the first register.
    Sub(FFmt),
    /// Divide the floating-point value in the second register by the floating-point value in the
    /// third register, and store the result in the first register.
    Div(FFmt),
    /// Multiply the floating-point value in the second register by the floating-point value in the
    /// third register, and store the result in the first register.
    Mul(FFmt),
    // Not yet supported in MIPS I.
    // /// Copy the floating-point value from the second register to the first register if the third
    // /// register is zero.
    // MoveIfZero(FFmt),
    // /// Copy the floating-point value from the second register to the first register if the third
    // /// register is not zero.
    // MoveIfNotZero(FFmt),
}

impl std::fmt::Display for FRegOp3 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add(fmt) => write!(f, "add.{fmt}"),
            Self::Sub(fmt) => write!(f, "sub.{fmt}"),
            Self::Div(fmt) => write!(f, "div.{fmt}"),
            Self::Mul(fmt) => write!(f, "mul.{fmt}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FRegOp2 {
    /// Store the absolute value of the second register in the first register.
    Abs(FFmt),
    /// Store the negated value of the second register in the first register.
    Neg(FFmt),
    /// Store the square root of the value in the second register in the first register.
    Sqrt(FFmt),
    /// Compare the floating-point values in both registers and set the FPU CC 0 to the result.
    Cmp(FCmp),
    //
    // These are not yet supported in MIPS I.
    // ---------------------------------------------------------------------------------------------
    // /// Ceil (round up) the floating-point value in the second register to a 32-bit integer and store the
    // /// result in the first register.
    // CeilToWord(FFmt),
    //
    // /// Floor (round down) the floating-point value in the second register to a 32-bit integer and store the
    // /// result in the first register.
    // FloorToWord(FFmt),
    //
    // /// Round (round to nearest/even) the floating-point value in the second register to a 32-bit integer and store the
    // /// result in the first register.
    // RoundToWord(FFmt),
    //
    // /// Truncate (round to zero) the floating-point value in the second register to a 32-bit integer and store the
    // /// result in the first register.
    // TruncToWord(FFmt),
    // ---------------------------------------------------------------------------------------------
    //
    /// Convert the floating-point value in the second register from the second fmt to the first fmt
    /// and store the result in the first register. The first and second fmt must be different!
    Convert(FFmt, FFmt),
    /// Convert the floating-point value in the second register from the specified fmt to an integer
    /// word and store the result in the first (FPU) register.
    ConvertToWord(FFmt),
    /// Convert the integer word value in the second (FPU) register to a floating-point
    /// value of the specified fmt and store the result in the first register.
    ConvertFromWord(FFmt),
    /// Copy the floating-point value from the second register to the first register.
    Move(FFmt),
    // Not yet supported in MIPS I.
    // /// Copy the floating-point value from the second register to the first register if the FPU CC 0
    // /// is equal to the specified bool.
    // MoveIf(bool, FFmt),
}

impl std::fmt::Display for FRegOp2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Abs(fmt) => write!(f, "abs.{fmt}"),
            Self::Neg(fmt) => write!(f, "neq.{fmt}"),
            Self::Sqrt(fmt) => write!(f, "sqrt.{fmt}"),
            Self::Cmp(cmp) => write!(f, "c.{cmp}"),
            Self::Convert(to, from) => write!(f, "cvt.{to}.{from}"),
            Self::ConvertToWord(fmt) => write!(f, "cvt.w.{fmt}"),
            Self::ConvertFromWord(fmt) => write!(f, "cvt.{fmt}.w"),
            Self::Move(fmt) => write!(f, "mov.{fmt}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FImmOp {
    /// Load a word from memory and store it in the first (FPU) register.
    /// Memory address is computed by adding the value in the second (integer) register to the
    /// immediate value.
    LoadWordToFpu,
    /// Store the word from the first (FPU) register to memory.
    /// Memory address is computed by adding the value in the second (integer) register to the
    /// immediate value.
    StoreWordFromFpu,
    /// Load a doubleword from memory and store it in the first (even floating-point) register.
    /// Memory address is computed by adding the value in the second (integer) register to the
    /// immediate value.
    /// Loading a doubleword to an odd FPU register is not allowed.
    LoadDoublewordToFpu,
    /// Store the word from the first (even FPU) register to memory.
    /// Memory address is computed by adding the value in the second (integer) register to the
    /// immediate value.
    /// Storing a doubleword from an odd FPU register is not allowed.
    StoreDoublewordFromFpu,
}

impl std::fmt::Display for FImmOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::LoadWordToFpu => "lwc1",
            Self::StoreWordFromFpu => "swc1",
            Self::LoadDoublewordToFpu => "ldc1",
            Self::StoreDoublewordFromFpu => "ldc1",
        })
    }
}

/// Floating-point format. Either _single_ ([`FFmt::S`]) or _double_ ([`FFmt::D`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FFmt {
    /// Single precision (32 bits)
    S,
    /// Double precision (64 bits)
    D,
}

impl std::fmt::Display for FFmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::S => "s",
            Self::D => "d",
        })
    }
}

// NOTE: MIPS I already supports more comparisons that account for NaN's: ueq, olt, ngt, etc.
// MARS only supports the ones listed here.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FCmp {
    Eq(FFmt),
    Le(FFmt),
    Lt(FFmt),
}

impl std::fmt::Display for FCmp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eq(fmt) => write!(f, "eq.{fmt}"),
            Self::Le(fmt) => write!(f, "le.{fmt}"),
            Self::Lt(fmt) => write!(f, "lt.{fmt}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrapCond {
    Eq,
    Ne,
    GeS,
    GeU,
    LtS,
    LtU,
}

impl std::fmt::Display for TrapCond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Eq => "eq",
            Self::Ne => "ne",
            Self::GeS => "ge",
            Self::GeU => "geu",
            Self::LtS => "lt",
            Self::LtU => "ltu",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TrapCondImm {
    Eq,
    Ne,
    GeS,
    GeU,
    LtS,
    LtU,
}

impl std::fmt::Display for TrapCondImm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Eq => "eqi",
            Self::Ne => "nei",
            Self::GeS => "gei",
            Self::GeU => "geiu",
            Self::LtS => "lti",
            Self::LtU => "ltiu",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BCond {
    Eq,
    Ne,
}

impl std::fmt::Display for BCond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Eq => "eq",
            Self::Ne => "ne",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BZCond {
    GeZ,
    GtZ,
    LeZ,
    LtZ,
}

impl std::fmt::Display for BZCond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::GeZ => "gez",
            Self::GtZ => "gtz",
            Self::LeZ => "lez",
            Self::LtZ => "ltz",
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BZalCond {
    GtZ,
    LtZ,
}

impl std::fmt::Display for BZalCond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::GtZ => "gtz",
            Self::LtZ => "ltz",
        })
    }
}
