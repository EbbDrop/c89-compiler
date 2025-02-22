use crate::{
    cfg::{BlockId, BlockRef},
    function::StackAddress,
    AnyReg, FFmt, FReg, Function, FunctionCall, Instruction, Reg, Root, StackInfo, Terminator,
    VirtualInstruction, VirtualTerminator,
};

pub fn run(root: &mut Root) {
    for function in root.functions_mut() {
        devirtualize_function(function);
    }
}

fn devirtualize_function(function: &mut Function) {
    for id in function.cfg.blocks().map(|(id, _)| id).collect::<Vec<_>>() {
        devirtualize_block(function, id);
    }
}

fn devirtualize_block(function: &mut Function, block_id: BlockId) {
    let mut old_instructions = Vec::new();
    std::mem::swap(
        &mut old_instructions,
        &mut function.cfg[block_id].instructions,
    );
    let mut new_instructions = Vec::with_capacity(old_instructions.len());

    let mut terminator = function.cfg[block_id].terminator.take().unwrap();

    let mut devirtualizer = Devirtualizer {
        function,
        instructions: &mut new_instructions,
    };

    for instr in old_instructions {
        devirtualizer.devirtualize_instruction(instr);
    }

    devirtualizer.devirtualize_terminator(&mut terminator);

    let block = &mut function.cfg[block_id];
    block.instructions = new_instructions;
    block.terminator = Some(terminator);
}

struct Devirtualizer<'a, 'b> {
    function: &'a Function,
    instructions: &'b mut Vec<Instruction>,
}

impl<'a, 'b> Devirtualizer<'a, 'b> {
    fn devirtualize_instruction(&mut self, instruction: Instruction) {
        let instruction = instruction.to_unhidden();
        let Instruction::Virtual(virt_instr) = instruction else {
            self.instructions.push(instruction);
            return;
        };
        match virt_instr {
            VirtualInstruction::FunctionCall(function_call) => {
                self.devirtualize_function_call(function_call)
            }
            VirtualInstruction::Declare(reg) => self.devirtualize_declare(reg),
            VirtualInstruction::Move { src, dst } => self.devirtualize_move(dst, src),
            VirtualInstruction::LoadStackAddress { reg, stack_address } => {
                self.devirtualize_load_stack_address(reg, stack_address)
            }
            VirtualInstruction::LoadFromStack { reg, stack_address } => {
                self.devirtualize_load_from_stack(reg, stack_address)
            }
            VirtualInstruction::StoreToStack { reg, stack_address } => {
                self.devirtualize_store_to_stack(reg, stack_address)
            }
        }
    }

    fn devirtualize_terminator(&mut self, terminator: &mut Terminator) {
        let Terminator::Virtual(virt_term) = terminator else {
            return;
        };
        match virt_term {
            VirtualTerminator::Return(ret_val) => {
                match *ret_val {
                    Some(AnyReg::R(reg)) => self.devirtualize_move(Reg::V0.into(), reg.into()),
                    Some(AnyReg::F(freg)) => self.devirtualize_move(FReg::F(0).into(), freg.into()),
                    None => {}
                }
                *terminator = crate::term::jump(BlockRef::new(
                    self.function.exit_block_id.unwrap(),
                    Vec::new(),
                ));
            }
        }
    }

    fn devirtualize_function_call(&mut self, function_call: FunctionCall) {
        let FunctionCall {
            label,
            return_reg,
            arguments,
        } = function_call;

        let call_arg_addrs = self
            .function
            .stack_frame
            .call_arg_addrs(arguments.iter().map(|(_, info)| info));

        for ((arg_reg, stack_info), (base, offset)) in arguments.iter().zip(call_arg_addrs) {
            self.store_to_stack(*arg_reg, stack_info, base, offset);
        }

        self.instructions.push(crate::instr::call(label));

        match return_reg {
            Some(AnyReg::R(reg)) => self.devirtualize_move(reg.into(), Reg::V0.into()),
            Some(AnyReg::F(freg)) => self.devirtualize_move(freg.into(), FReg::F(0).into()),
            None => {}
        }
    }

    fn devirtualize_declare(&mut self, _reg: AnyReg) {
        // Nothing to do
    }

    fn devirtualize_move(&mut self, dst: AnyReg, src: AnyReg) {
        let instr = match (dst, src) {
            (AnyReg::R(rd), AnyReg::R(rs)) => crate::instr::or(rd, rs, Reg::ZERO),
            (AnyReg::R(rt), AnyReg::F(fs)) => crate::instr::move_from_fpu(rt, fs),
            (AnyReg::F(fs), AnyReg::R(rt)) => crate::instr::move_to_fpu(rt, fs),
            (AnyReg::F(fd), AnyReg::F(fs)) => {
                let ffmt = match (fd.is_double(), fs.is_double()) {
                    (true, true) => FFmt::D,
                    _ => FFmt::S,
                };
                crate::instr::move_(ffmt, fd, fs)
            }
        };
        self.instructions.push(instr);
    }

    fn devirtualize_load_stack_address(&mut self, reg: Reg, stack_address: StackAddress) {
        let (base, offset) = self.function.stack_frame.addr_of(stack_address).unwrap();
        self.instructions
            .push(crate::instr::add_u_imm(reg, base, offset));
    }

    fn devirtualize_load_from_stack(&mut self, reg: AnyReg, stack_address: StackAddress) {
        let stack_info = self.function.stack_frame.stack_info(stack_address).unwrap();
        let (base, offset) = self.function.stack_frame.addr_of(stack_address).unwrap();
        self.load_from_stack(reg, stack_info, base, offset);
    }

    fn devirtualize_store_to_stack(&mut self, reg: AnyReg, stack_address: StackAddress) {
        let stack_info = self.function.stack_frame.stack_info(stack_address).unwrap();
        let (base, offset) = self.function.stack_frame.addr_of(stack_address).unwrap();
        self.store_to_stack(reg, stack_info, base, offset);
    }

    fn load_from_stack(&mut self, reg: AnyReg, stack_info: &StackInfo, base: Reg, offset: u16) {
        use crate::size::{BYTE, DOUBLE, HALF, WORD};
        let load_instr = match reg {
            AnyReg::R(reg) => match (stack_info.size as u32, stack_info.signed) {
                (BYTE, true) => crate::instr::load_byte_s(reg, base, offset),
                (BYTE, false) => crate::instr::load_byte_u(reg, base, offset),
                (HALF, true) => crate::instr::load_half_s(reg, base, offset),
                (HALF, false) => crate::instr::load_half_u(reg, base, offset),
                (WORD, _) => crate::instr::load_word(reg, base, offset),
                _ => panic!(
                    "cannot load into 32-bit cpu register from stack space of size {}",
                    stack_info.size
                ),
            },
            AnyReg::F(freg) => match stack_info.size as u32 {
                DOUBLE => crate::instr::load_doubleword_to_fpu(freg, base, offset),
                WORD => crate::instr::load_word_to_fpu(freg, base, offset),
                _ => panic!(
                    "cannot load into fpu register from stack space of size {}",
                    stack_info.size
                ),
            },
        };
        self.instructions.push(load_instr);
    }

    fn store_to_stack(&mut self, reg: AnyReg, stack_info: &StackInfo, base: Reg, offset: u16) {
        use crate::size::{BYTE, DOUBLE, HALF, WORD};
        let store_instr = match reg {
            AnyReg::R(reg) => match stack_info.size as u32 {
                BYTE => crate::instr::store_byte(reg, base, offset),
                HALF => crate::instr::store_half(reg, base, offset),
                WORD => crate::instr::store_word(reg, base, offset),
                _ => panic!(
                    "cannot store from 32-bit cpu register to stack space of size {}",
                    stack_info.size
                ),
            },
            AnyReg::F(freg) => match stack_info.size as u32 {
                DOUBLE => crate::instr::store_doubleword_from_fpu(freg, base, offset),
                WORD => crate::instr::store_word_from_fpu(freg, base, offset),
                _ => panic!(
                    "cannot store from fpu register to stack space of size {}",
                    stack_info.size
                ),
            },
        };
        self.instructions.push(store_instr);
    }
}
