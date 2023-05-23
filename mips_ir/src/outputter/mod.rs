#[cfg(test)]
mod test;

use crate::{
    AnyReg, BasicBlock, BlockRef, DataDirective, FReg, Function, FunctionCall, GlobalData, ImmOp1,
    ImmOp2, Instruction, Label, PseudoInstruction, Reg, Root, Terminator, TrapCondImm,
    VirtualInstruction, VirtualTerminator,
};
use std::fmt::Result;

#[derive(Debug, Clone, Default)]
pub struct MipsOutputConfig {
    /// If `true`, registers will be named (`$sp`, `$a0`) instead of numbered (`$29`, `$4`).
    pub use_register_names: bool,
    /// If `false`, the outputter will panic when encountering virtual registers.
    pub allow_virtuals: bool,
    /// If `true`, block arguments will be shown when blocks are referenced and at the label that
    /// starts a block.
    pub show_block_arguments: bool,
    /// If `true`, all blocks in functions will be shown, even those which have no predecessors.
    pub show_all_blocks: bool,
}

/// Can be used to format [`Root`]s to a writer.
///
/// A mutable reference to the writer---an implementor of [`std::fmt::Write`]---must be passed to
/// [`new`]. Then [`Root`]s can be formatted using [`write_root`].
///
/// Some configuration is possible by passing a [`MipsOutputConfig`] to [`with_config`].
///
/// # Validity of output
///
/// The outputter can only output valid MIPS if the following constraints are met:
///
///  - No virtual registers are present.
///  - The following configuration options are set:
///    ```skip
///    MipsOutputConfig {
///        show_block_arguments: false,
///        show_all_blocks: false,
///        // ...
///    }
///    ```
///  - Blocks without predecessors can be ignored for the following constraints:
///    - Every block has at most one default successor within its graph/function.
///    - The graph reduced to the edges formed by default successors (starting from the entry point)
///      should be acyclic (a DAG).
///    - All successors of every block reachable from the entry block should be in the
///      graph/function.
///  - The entry block of each graph/function has no default predecessor.
///  - Every graph/function must have an entry point set.
pub struct MipsOutputter<'w, W: std::fmt::Write> {
    writer: &'w mut W,
    config: MipsOutputConfig,
}

impl<'w, W: std::fmt::Write> MipsOutputter<'w, W> {
    pub fn new(writer: &'w mut W) -> Self {
        Self {
            writer,
            config: Default::default(),
        }
    }

    pub fn with_config(self, config: MipsOutputConfig) -> Self {
        Self { config, ..self }
    }

    pub fn write_root(&mut self, value: &Root) -> Result {
        for label in value.exported_labels() {
            writeln!(self.writer, "\t.globl\t{label}")?
        }

        if value.has_any_data() {
            self.write_str("\t.data\n")?;
        }
        for data in value.data() {
            self.writeln()?;
            self.write_global_data(data)?;
        }

        if value.has_any_functions() {
            self.write_str("\t.text\n")?;
        }
        for function in value.functions() {
            self.writeln()?;
            self.write_function(function)?;
        }
        Ok(())
    }

    pub fn write_global_data(&mut self, value: &GlobalData) -> Result {
        if value.align() != value.data().natural_align() {
            writeln!(self.writer, "\t.align\t{}", *value.align())?;
        }

        self.write_label(value.label())?;

        match value.data() {
            DataDirective::Space(n) => writeln!(self.writer, "\t.space\t{n}"),
            DataDirective::Ascii(string) => writeln!(
                self.writer,
                "\t.ascii\t\"{}\"",
                std::str::from_utf8(string).expect("unimplemented: escaping .ascii strings")
            ),
            DataDirective::AsciiZ(string) => writeln!(
                self.writer,
                "\t.asciiz\t\"{}\"",
                std::str::from_utf8(string).expect("unimplemented: escaping .asciiz strings")
            ),
            DataDirective::Byte(x) => writeln!(self.writer, "\t.byte\t{x}"),
            DataDirective::Half(x) => writeln!(self.writer, "\t.half\t{x}"),
            DataDirective::Word(x) => writeln!(self.writer, "\t.word\t{x}"),
            DataDirective::Float(x) => writeln!(self.writer, "\t.float\t{x}"),
            DataDirective::Double(x) => writeln!(self.writer, "\t.double\t{x}"),
            DataDirective::Bytes(xs) => {
                write!(self.writer, "\t.byte\t{}", xs.first())?;
                for x in xs.iter().skip(1) {
                    write!(self.writer, ", {x}")?
                }
                self.writeln()
            }
            DataDirective::Halfs(xs) => {
                write!(self.writer, "\t.half\t{}", xs.first())?;
                for x in xs.iter().skip(1) {
                    write!(self.writer, ", {x}")?
                }
                self.writeln()
            }
            DataDirective::Words(xs) => {
                write!(self.writer, "\t.word\t{}", xs.first())?;
                for x in xs.iter().skip(1) {
                    write!(self.writer, ", {x}")?
                }
                self.writeln()
            }
            DataDirective::Floats(xs) => {
                write!(self.writer, "\t.float\t{}", xs.first())?;
                for x in xs.iter().skip(1) {
                    write!(self.writer, ", {x}")?
                }
                self.writeln()
            }
            DataDirective::Doubles(xs) => {
                write!(self.writer, "\t.double\t{}", xs.first())?;
                for x in xs.iter().skip(1) {
                    write!(self.writer, ", {x}")?
                }
                self.writeln()
            }
        }
    }

    pub fn write_function(&mut self, value: &Function) -> Result {
        self.write_label(value.label())?;
        // TODO: FIXME: Patch the graph such that there are no two nodes with the same default
        // successor (i.e. such that every node has exactly one predecessor for which it is the
        // default successor) and such that there is no node which has the entry point as default
        // successor.
        let traverser = match self.config.show_all_blocks {
            false => value.traverse(),
            true => value.traverse_all(),
        };
        for block in traverser {
            write!(self.writer, "{}", block.label())?;
            self.write_arguments(&block.arguments)?;
            self.write_str(":\n")?;
            self.write_block(block)?;
        }
        Ok(())
    }

    fn write_block(&mut self, value: &BasicBlock) -> Result {
        for instruction in &value.instructions {
            self.write_char('\t')?;
            self.write_instruction(instruction)?;
            self.writeln()?;
        }
        self.write_char('\t')?;
        self.write_terminator(&value.terminator)?;
        self.writeln()
    }

    pub fn write_instruction(&mut self, value: &Instruction) -> Result {
        match value {
            Instruction::Nop => self.write_str("nop"),
            &Instruction::Reg3(op, rd, rs, rt) => {
                write!(self.writer, "{op}\t")?;
                self.write_reg(rd)?;
                self.write_sep()?;
                self.write_reg(rs)?;
                self.write_sep()?;
                self.write_reg(rt)
            }
            &Instruction::Reg2(op, rd, rs) => {
                write!(self.writer, "{op}\t")?;
                self.write_reg(rd)?;
                self.write_sep()?;
                self.write_reg(rs)
            }
            &Instruction::Reg1(op, rd) => {
                write!(self.writer, "{op}\t")?;
                self.write_reg(rd)
            }
            &Instruction::Imm2(op, rt, rs, imm) => {
                write!(self.writer, "{op}\t")?;
                self.write_reg(rt)?;
                self.write_sep()?;
                self.write_reg(rs)?;
                self.write_sep()?;
                if display_imm2_signed(op) {
                    self.write_imm_s(imm as i16)
                } else {
                    self.write_imm_u(imm)
                }
            }
            &Instruction::Imm1(op, rt, imm) => {
                write!(self.writer, "{op}\t")?;
                self.write_reg(rt)?;
                self.write_sep()?;
                if display_imm1_signed(op) {
                    self.write_imm_s(imm as i16)
                } else {
                    self.write_imm_u(imm)
                }
            }
            &Instruction::FReg3(op, fd, fs, ft) => {
                write!(self.writer, "{op}\t")?;
                self.write_freg(fd)?;
                self.write_sep()?;
                self.write_freg(fs)?;
                self.write_sep()?;
                self.write_freg(ft)
            }
            &Instruction::FReg2(op, fd, fs) => {
                write!(self.writer, "{op}\t")?;
                self.write_freg(fd)?;
                self.write_sep()?;
                self.write_freg(fs)
            }
            &Instruction::FImm(op, ft, base, offset) => {
                write!(self.writer, "{op}\t")?;
                self.write_freg(ft)?;
                self.write_sep()?;
                self.write_imm_u(offset)?;
                self.write_char('(')?;
                self.write_reg(base)?;
                self.write_char(')')
            }
            &Instruction::MoveFromFpu(rt, fs) => {
                self.write_str("mfc1\t")?;
                self.write_reg(rt)?;
                self.write_sep()?;
                self.write_freg(fs)
            }
            &Instruction::MoveToFpu(rt, fs) => {
                self.write_str("mtc1\t")?;
                self.write_reg(rt)?;
                self.write_sep()?;
                self.write_freg(fs)
            }
            Instruction::Syscall => self.write_str("syscall"),
            Instruction::Break => self.write_str("break"),
            Instruction::Pseudo(pseudo) => self.write_pseudo_instruction(pseudo),
            Instruction::Virtual(value) => {
                if !self.config.allow_virtuals {
                    panic!("formatting virtual instruction not allowed");
                }
                self.write_virtual_instruction(value)
            }
        }
    }

    pub fn write_pseudo_instruction(&mut self, value: &PseudoInstruction) -> Result {
        match value {
            PseudoInstruction::LoadAddress(rt, label) => {
                self.write_str("la\t")?;
                self.write_reg(*rt)?;
                self.write_sep()?;
                self.write_label_ref(label)
            }
        }
    }

    pub fn write_virtual_instruction(&mut self, value: &VirtualInstruction) -> Result {
        match value {
            VirtualInstruction::FunctionCall(FunctionCall {
                label,
                return_reg,
                arguments,
            }) => {
                self.write_str("@call\t")?;
                match return_reg.as_ref() {
                    Some(reg) => self.write_any_reg(*reg)?,
                    None => self.write_str("_")?,
                }
                self.write_sep()?;
                self.write_label_ref(label)?;
                self.write_arguments(&arguments.iter().map(|(reg, _)| *reg).collect::<Vec<_>>())
            }
        }
    }

    pub fn write_terminator(&mut self, value: &Terminator) -> Result {
        match value {
            Terminator::BranchIf(cond, rs, rt, true_target, false_target) => {
                write!(self.writer, "b{cond}\t")?;
                self.write_reg(*rs)?;
                self.write_sep()?;
                self.write_reg(*rt)?;
                self.write_sep()?;
                self.write_block_ref(true_target)?;
                if self.config.show_block_arguments {
                    self.write_sep()?;
                    self.write_block_ref(false_target)?;
                }
            }
            Terminator::BranchIfZ(cond, rs, true_target, false_target) => {
                write!(self.writer, "b{cond}\t")?;
                self.write_reg(*rs)?;
                self.write_sep()?;
                self.write_block_ref(true_target)?;
                if self.config.show_block_arguments {
                    self.write_sep()?;
                    self.write_block_ref(false_target)?;
                }
            }
            Terminator::BranchIfZAndLink(cond, rs, true_target, false_target) => {
                write!(self.writer, "b{cond}al\t")?;
                self.write_reg(*rs)?;
                self.write_sep()?;
                self.write_label_ref(true_target)?;
                if self.config.show_block_arguments {
                    self.write_sep()?;
                    self.write_block_ref(false_target)?;
                }
            }
            Terminator::BranchIfFCond(b, true_target, false_target) => {
                write!(self.writer, "bc1{}\t", if *b { 't' } else { 'f' })?;
                self.write_block_ref(true_target)?;
                if self.config.show_block_arguments {
                    self.write_sep()?;
                    self.write_block_ref(false_target)?;
                }
            }
            Terminator::Jump(target) => {
                self.write_str("j\t")?;
                self.write_block_ref(target)?;
            }
            Terminator::JumpAndLink(target, next_block) => {
                self.write_str("jal\t")?;
                self.write_label_ref(target)?;
                if self.config.show_block_arguments {
                    self.write_sep()?;
                    self.write_block_ref(next_block)?;
                }
            }
            Terminator::ReturnToRa => {
                self.write_str("jr\t")?;
                self.write_reg(Reg::RA)?;
            }
            Terminator::JumpAndLinkRa(rs, next_block) => {
                self.write_str("jalr\t")?;
                self.write_reg(*rs)?;
                if self.config.show_block_arguments {
                    self.write_sep()?;
                    self.write_block_ref(next_block)?;
                }
            }
            Terminator::JumpAndLinkReg(rd, rs, next_block) => {
                self.write_str("jalr\t")?;
                self.write_reg(*rd)?;
                self.write_sep()?;
                self.write_reg(*rs)?;
                if self.config.show_block_arguments {
                    self.write_sep()?;
                    self.write_block_ref(next_block)?;
                }
            }
            Terminator::Virtual(value) => {
                if !self.config.allow_virtuals {
                    panic!("formatting virtual instruction not allowed");
                }
                self.write_virtual_terminator(value)?;
            }
        }
        Ok(())
    }

    fn write_virtual_terminator(&mut self, value: &VirtualTerminator) -> Result {
        match value {
            VirtualTerminator::Return(reg) => {
                self.write_str("@return")?;
                if let Some(reg) = reg {
                    self.write_char('[')?;
                    self.write_any_reg(*reg)?;
                    self.write_char(']')?;
                }
                Ok(())
            }
        }
    }

    fn write_label(&mut self, value: &Label) -> Result {
        writeln!(self.writer, "{}:", value)
    }

    fn write_label_ref(&mut self, value: &Label) -> Result {
        write!(self.writer, "{value}")
    }

    fn write_block_ref(&mut self, value: &BlockRef) -> Result {
        write!(self.writer, "{}", value.label)?;
        self.write_arguments(&value.arguments)
    }

    fn write_arguments(&mut self, args: &[AnyReg]) -> Result {
        if self.config.show_block_arguments && !args.is_empty() {
            let mut arguments = args.iter();
            self.write_char('[')?;
            if let Some(arg) = arguments.next() {
                self.write_any_reg(*arg)?;
            }
            for arg in arguments {
                self.write_str(", ")?;
                self.write_any_reg(*arg)?;
            }
            self.write_char(']')?;
        }
        Ok(())
    }

    fn write_any_reg(&mut self, value: AnyReg) -> Result {
        if value.is_virtual() && !self.config.allow_virtuals {
            panic!("formatting virtual registers not allowed");
        }
        if self.config.use_register_names {
            write!(self.writer, "{:#}", value)
        } else {
            write!(self.writer, "{}", value)
        }
    }

    fn write_reg(&mut self, value: Reg) -> Result {
        if value.is_virtual() && !self.config.allow_virtuals {
            panic!("formatting virtual registers not allowed");
        }
        if self.config.use_register_names {
            write!(self.writer, "{:#}", value)
        } else {
            write!(self.writer, "{}", value)
        }
    }

    fn write_freg(&mut self, value: FReg) -> Result {
        if value.is_virtual() && !self.config.allow_virtuals {
            panic!("formatting virtual registers not allowed");
        }
        if self.config.use_register_names {
            write!(self.writer, "{:#}", value)
        } else {
            write!(self.writer, "{}", value)
        }
    }

    fn write_imm_s(&mut self, value: i16) -> Result {
        write!(self.writer, "{}", value)
    }

    fn write_imm_u(&mut self, value: u16) -> Result {
        write!(self.writer, "{}", value)
    }

    /// Writes a separator: a comma followed by a space (`, `).
    #[inline]
    fn write_sep(&mut self) -> Result {
        self.write_str(", ")
    }

    /// Writes a single newline character.
    #[inline]
    fn writeln(&mut self) -> Result {
        self.write_char('\n')
    }

    /// Call to `self.writer.write_str`.
    #[inline]
    fn write_str(&mut self, s: &str) -> Result {
        self.writer.write_str(s)
    }

    /// Call to `self.writer.write_char`.
    #[inline]
    fn write_char(&mut self, c: char) -> Result {
        self.writer.write_char(c)
    }
}

/// Returns `true` if the immediate value of this operand should be displayed as a signed number.
fn display_imm2_signed(op: ImmOp2) -> bool {
    // Return `true` if the immediate is sign-extended by the operation.
    // Return `false` if the immediate is zero-extended by the operation, or if it must always be an
    // unsigned number (s.a. for shifts).
    match op {
        ImmOp2::AddS => true,
        ImmOp2::AddU => true,
        ImmOp2::And => false,
        ImmOp2::Or => false,
        ImmOp2::Xor => false,
        ImmOp2::ShiftLeftLogical => false,
        ImmOp2::ShiftRightLogical => false,
        ImmOp2::ShiftRightArithmetic => false,
        ImmOp2::SetLtS => true,
        ImmOp2::SetLtU => true,
        ImmOp2::LoadByteS => true,
        ImmOp2::LoadByteU => true,
        ImmOp2::LoadHalfS => true,
        ImmOp2::LoadHalfU => true,
        ImmOp2::LoadWord => true,
        ImmOp2::LoadWordLeft => true,
        ImmOp2::LoadWordRight => true,
        ImmOp2::StoreByte => true,
        ImmOp2::StoreHalf => true,
        ImmOp2::StoreWord => true,
        ImmOp2::StoreWordLeft => true,
        ImmOp2::StoreWordRight => true,
        ImmOp2::LoadLinkedWord => true,
        ImmOp2::StoreConditionalWord => true,
    }
}

/// Returns `true` if the immediate value of this operation should be displayed as a signed number.
fn display_imm1_signed(op: ImmOp1) -> bool {
    match op {
        ImmOp1::LoadUpper => false,
        ImmOp1::TrapIf(cond) => match cond {
            TrapCondImm::Eq => true,
            TrapCondImm::Ne => true,
            TrapCondImm::GeS => true,
            TrapCondImm::GeU => true,
            TrapCondImm::LtS => true,
            TrapCondImm::LtU => true,
        },
    }
}
