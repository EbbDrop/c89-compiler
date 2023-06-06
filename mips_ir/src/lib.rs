mod cfg;
mod dfa;
mod fixer;
mod function;
mod global_data;
mod instruction;
mod label;
mod linker;
mod optimizer;
mod outputter;
mod passes;
mod reg;
mod root;
mod scanner;

pub use cfg::{BlockId, BlockRef};
pub use function::{BBBuilder, BasicBlock, Function, ReferenceRegister, StackInfo};
pub use global_data::{size, AlignBoundary, DataDirective, GlobalData};
pub use instruction::{
    instr, term, BCond, BZCond, BZalCond, FCmp, FFmt, FImmOp, FRegOp2, FRegOp3, FunctionCall,
    ImmOp1, ImmOp2, Instruction, PseudoInstruction, RegOp1, RegOp2, RegOp3, Terminator, TrapCond,
    TrapCondImm, VirtualInstruction, VirtualTerminator,
};
pub use label::Label;
pub use outputter::{MipsOutputConfig, MipsOutputter};
pub use reg::{AnyReg, FReg, Reg, VARGenerator};
pub use root::Root;

pub fn compile_and_link(root: &mut Root) {
    passes::patcher::patch_root(root);
    dfa::dce::purge_root(root);
    passes::register_allocation::run(root);
    passes::stack_frame_builder::run(root);
    passes::devirtualizer::run(root);
    optimizer::simplifier::simplify_root(root);
    fixer::fix_root(root);
    linker::link(root).expect("linking failed");
}
