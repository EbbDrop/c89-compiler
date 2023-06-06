use crate::{AlignBoundary, FReg, Reg};
use std::collections::BTreeMap;
use std::ops::Range;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackInfo {
    pub size: u128,
    pub alignment: AlignBoundary,
    pub signed: bool,
}

/// Identifier for stack space.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackAddress(pub u32);

/// Assumes the stack is 8-byte aligned. As such, cannot handle alignments bigger than 8. It will
/// treat them incorrectly.
#[derive(Debug, Clone)]
pub struct StackFrame {
    /// `true` when $fp has been set correctly and can be used to refer to the static part of the
    /// stack frame.
    is_fp_set: bool,
    /// The params from the previous stack frame that are passed to us. Ordered from arg1 to argN.
    /// Should always contain at least 4 words, but these might not necessarily have addresses
    /// assigned, so don't need to be in the vec.
    params: Vec<StackInfo>,
    /// Maps a stack address to its index in `params`.
    stack_address_to_param_idx: BTreeMap<StackAddress, usize>,
    /// Ordered from low address to high address. Order can change when adding or removing spilled
    /// vars (in order to reduce inner holes for padding).
    spilled: Vec<StackInfo>,
    /// Maps a stack address to its index in `spilled`.
    stack_address_to_spilled_idx: BTreeMap<StackAddress, usize>,
    /// Ordered from low ($s0) to high ($s7)
    saved_cpu_regs: Vec<Reg>,
    /// Ordered from low ($f20) to high ($f30). Only even register allowed for now. Corresponding
    /// odd registers will automatically be
    saved_fpu_regs: Vec<FReg>,
    /// Does not include outer padding; space in bytes
    max_call_arguments_space: u32,
}

impl StackFrame {
    pub fn new() -> Self {
        Self {
            is_fp_set: false,
            params: Vec::new(),
            stack_address_to_param_idx: BTreeMap::new(),
            spilled: Vec::new(),
            stack_address_to_spilled_idx: BTreeMap::new(),
            saved_cpu_regs: Vec::new(),
            saved_fpu_regs: Vec::new(),
            max_call_arguments_space: 0,
        }
    }

    pub fn mark_fp_as_set(&mut self) {
        self.is_fp_set = true;
    }

    /// Undoes the effect of `mark_fp_as_set`.
    pub fn mark_fp_as_unset(&mut self) {
        self.is_fp_set = false;
    }

    fn static_base(&self) -> Reg {
        match self.is_fp_set {
            true => Reg::FP,
            false => Reg::SP,
        }
    }

    /// Iterates over the saved cpu registers that are saved in this stack frame from the lowest
    /// (e.g. $s0) to the highest (e.g. $s7).
    pub fn saved_cpu_regs(&self) -> impl DoubleEndedIterator<Item = Reg> + '_ {
        self.saved_cpu_regs.iter().copied()
    }

    pub fn saved_fpu_regs(&self) -> impl DoubleEndedIterator<Item = FReg> + '_ {
        self.saved_fpu_regs.iter().copied()
    }

    pub fn fp_slot_addr(&self) -> (Reg, u16) {
        (self.static_base(), self.fp_slot_static_offset_range().start)
    }

    pub fn ra_slot_addr(&self) -> (Reg, u16) {
        (self.static_base(), self.ra_slot_static_offset_range().start)
    }

    pub fn saved_cpu_reg_addr(&self, reg: Reg) -> Option<(Reg, u16)> {
        let position = self.saved_cpu_regs.iter().position(|&r| r == reg)? as u16;
        let offset = self.cpu_reg_save_area_static_offset_range().start
            + crate::size::WORD as u16 * position;
        Some((self.static_base(), offset))
    }

    pub fn saved_fpu_reg_addr(&self, freg: FReg) -> Option<(Reg, u16)> {
        let position = self.saved_fpu_regs.iter().position(|&r| r == freg)? as u16;
        let offset = self.fpu_reg_save_area_static_offset_range().start
            + crate::size::DOUBLE as u16 * position;
        Some((self.static_base(), offset))
    }

    pub fn addr_of(&self, stack_address: StackAddress) -> Option<(Reg, u16)> {
        self.addr_of_spilled(stack_address)
            .or_else(|| self.addr_of_param(stack_address))
    }

    fn addr_of_param(&self, stack_address: StackAddress) -> Option<(Reg, u16)> {
        let index = *self.stack_address_to_param_idx.get(&stack_address)?;
        let mut offset = self.byte_size() as u32;
        // This is not necessary, since `offset` should already be 8-byte aligned at this point.
        offset = self.params[0].alignment.next_multiple_from(offset);
        for i in 1..=index {
            offset += self.params[i - 1].size as u32;
            offset = self.params[i].alignment.next_multiple_from(offset);
        }
        Some((self.static_base(), offset as u16))
    }

    fn addr_of_spilled(&self, stack_address: StackAddress) -> Option<(Reg, u16)> {
        let index = *self.stack_address_to_spilled_idx.get(&stack_address)?;
        let mut offset = self.spilled_area_static_offset_range().start as u32;
        for i in 1..=index {
            offset += self.spilled[i - 1].size as u32;
            offset = self.spilled[i].alignment.next_multiple_from(offset);
        }
        Some((self.static_base(), offset as u16))
    }

    pub fn stack_info(&self, stack_address: StackAddress) -> Option<&StackInfo> {
        self.stack_address_to_spilled_idx
            .get(&stack_address)
            .map(|&idx| &self.spilled[idx])
            .or_else(|| {
                self.stack_address_to_param_idx
                    .get(&stack_address)
                    .map(|&idx| &self.params[idx])
            })
    }

    pub fn call_arg_addrs<'a, I>(&self, call_args: I) -> impl Iterator<Item = (Reg, u16)> + 'a
    where
        I: Iterator<Item = &'a StackInfo> + Clone + 'a,
    {
        struct Tmp<'a, I: Iterator<Item = &'a StackInfo>> {
            offset: u16,
            call_args: I,
        }

        impl<'a, I: Iterator<Item = &'a StackInfo>> Iterator for Tmp<'a, I> {
            type Item = (Reg, u16);

            fn next(&mut self) -> Option<Self::Item> {
                let stack_info = self.call_args.next()?;
                self.offset = stack_info.alignment.next_multiple_from(self.offset as u32) as u16;
                let offset = self.offset;
                self.offset += stack_info.size as u16;
                Some((Reg::SP, offset))
            }
        }

        Tmp {
            offset: self.call_arguments_area_static_offset_range().start,
            call_args,
        }
    }

    /// Returns the total size in bytes of this stack frame, including padding
    /// (always multiple of 8).
    pub fn byte_size(&self) -> u16 {
        self.ra_slot_static_offset_range().end
    }

    fn call_arguments_area_static_offset_range(&self) -> Range<u16> {
        0..(self.max_call_arguments_space as u16)
    }

    fn fpu_reg_save_area_static_offset_range(&self) -> Range<u16> {
        let prev_end = self.call_arguments_area_static_offset_range().end;
        let size = self.saved_fpu_regs.len() as u32 * crate::size::DOUBLE;
        if size == 0 {
            return prev_end..prev_end;
        }
        let start = AlignBoundary::DOUBLE.next_multiple_from(prev_end as u32) as u16;
        start..(start + size as u16)
    }

    fn cpu_reg_save_area_static_offset_range(&self) -> Range<u16> {
        let prev_end = self.fpu_reg_save_area_static_offset_range().end;
        let size = self.saved_cpu_regs.len() as u32 * crate::size::WORD;
        if size == 0 {
            return prev_end..prev_end;
        }
        let start = AlignBoundary::DOUBLE.next_multiple_from(prev_end as u32) as u16;
        start..(start + size as u16)
    }

    fn spilled_area_static_offset_range(&self) -> Range<u16> {
        let prev_end = self.cpu_reg_save_area_static_offset_range().end;
        if self.spilled.is_empty() {
            return prev_end..prev_end;
        }
        let start = self.spilled[0]
            .alignment
            .next_multiple_from(prev_end as u32);
        let mut end = start + self.spilled[0].size as u32;
        for stack_info in &self.spilled[1..] {
            end = stack_info.alignment.next_multiple_from(end);
            end += stack_info.size as u32;
        }
        (start as u16)..(end as u16)
    }

    fn fp_slot_static_offset_range(&self) -> Range<u16> {
        let prev_end = self.spilled_area_static_offset_range().end;
        let start = AlignBoundary::DOUBLE.next_multiple_from(prev_end as u32) as u16;
        start..(start + crate::size::WORD as u16)
    }

    fn ra_slot_static_offset_range(&self) -> Range<u16> {
        let prev_end = self.fp_slot_static_offset_range().end;
        let start = AlignBoundary::WORD.next_multiple_from(prev_end as u32) as u16;
        start..(start + crate::size::WORD as u16)
    }

    pub fn add_saved_cpu_reg(&mut self, reg: Reg) {
        match reg {
            Reg::S0 | Reg::S1 | Reg::S2 | Reg::S3 | Reg::S4 | Reg::S5 | Reg::S6 | Reg::S7 => {
                let position = self
                    .saved_cpu_regs
                    .iter()
                    .position(|r| reg.phy_num() <= r.phy_num());
                match position.map(|i| (i, self.saved_cpu_regs[i])) {
                    Some((_, r)) if r == reg => (),
                    Some((i, _)) => self.saved_cpu_regs.insert(i, reg),
                    None => self.saved_cpu_regs.push(reg),
                }
            }
            _ => panic!("attempt to save virtual or non-saved cpu register"),
        }
    }

    /// This must be a double-precision fp register for now
    pub fn add_saved_fpu_reg(&mut self, freg: FReg) {
        match freg {
            FReg::F(20..=31) => {
                if !freg.is_double() {
                    panic!("can only save double-precision fpu registers for now");
                }
                let position = self
                    .saved_fpu_regs
                    .iter()
                    .position(|r| freg.phy_num() <= r.phy_num());
                match position.map(|i| (i, self.saved_fpu_regs[i])) {
                    Some((_, r)) if r == freg => (),
                    Some((i, _)) => self.saved_fpu_regs.insert(i, freg),
                    None => self.saved_fpu_regs.push(freg),
                }
            }
            _ => panic!("attempt to save virtual or non-saved fpu register"),
        }
    }

    pub fn add_spilled(
        &mut self,
        stack_info: StackInfo,
        stack_addresses: impl Iterator<Item = StackAddress>,
    ) {
        // Just append for now.
        let index = self.spilled.len();
        self.spilled.push(stack_info);
        for stack_address in stack_addresses {
            self.stack_address_to_spilled_idx
                .insert(stack_address, index);
            self.stack_address_to_param_idx.remove(&stack_address);
        }
    }

    pub fn add_param(&mut self, stack_info: StackInfo, stack_address: StackAddress) {
        let index = self.params.len();
        self.params.push(stack_info);
        self.stack_address_to_param_idx.insert(stack_address, index);
        self.stack_address_to_spilled_idx.remove(&stack_address);
    }

    pub fn set_max_call_argument_space(&mut self, space: u32) {
        self.max_call_arguments_space = space;
    }
}
