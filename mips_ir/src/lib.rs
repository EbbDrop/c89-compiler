mod function;
mod global_data;
mod instruction;
mod label;
mod outputter;
mod reg;
mod root;
mod scanner;

pub mod cfg;
pub mod dfa;
pub mod fixer;
pub mod linker;
pub mod optimizer;
pub mod passes;
// pub mod validator;

pub use cfg::{BlockId, BlockRef};
pub use function::{BBBuilder, BasicBlock, Function, ReferenceRegister, StackInfo};
pub use global_data::{size, AlignBoundary, DataDirective, GlobalData};
pub use instruction::{
    instr, term, BCond, BZCond, BZalCond, FCmp, FFmt, FImmOp, FRegOp2, FRegOp3, FunctionCall,
    ImmOp1, ImmOp2, Instruction, PseudoInstruction, RegOp1, RegOp2, RegOp3, Terminator, TrapCond,
    TrapCondImm, VirtualInstruction, VirtualTerminator,
};
pub use label::Label;
pub use outputter::*;
pub use reg::{AnyReg, FReg, Reg, VARGenerator};
pub use root::Root;
