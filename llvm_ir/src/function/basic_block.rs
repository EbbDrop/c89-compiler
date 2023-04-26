use crate::instruction::{Instruction, Terminator};
use crate::{id, value};
use std::fmt::{self, Write};

#[derive(Debug)]
pub struct BasicBlock {
    pub label: value::constant::Label,
    /// Comments for the whole block. Will be displayed above the label.
    pub comment: Option<String>,
    pub instructions: Vec<Instruction>,
    /// Vec of (index, comment) pairs, where the index denotes the location in the body where the
    /// comment should be inserted. This can be equal to `instructions.len()`, in which case the
    /// comment should be inserted after the last instruction in the body (but before the terminator
    /// instruction). The indices are guaranteed to be increasing (i.e. in ascending order).
    pub comments: Vec<(usize, String)>,
    pub terminator: Terminator,
}

impl crate::FmtAsLlvmAsmFC for BasicBlock {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        if let Some(comment) = &self.comment {
            for line in comment.lines() {
                writeln!(f, ";{line}")?;
            }
        }
        match function.id(self.label.handle).as_ref() {
            id::Id::Named(named) => {
                named.fmt_as_llvm_asm(f, opts, module, function)?;
                f.write_str(":\n")?;
            }
            id::Id::Unnamed(unnamed) => {
                if opts.display_unnamed_ids {
                    unnamed.fmt_as_llvm_asm(f, opts, module, function)?;
                    f.write_str(":\n")?;
                }
            }
        }
        let mut comments = self.comments.iter().peekable();
        for (i, instruction) in self.instructions.iter().enumerate() {
            while let Some((_, comment)) = comments.next_if(|(j, _)| i == *j) {
                for line in comment.lines() {
                    writeln!(f, "    ;{line}")?;
                }
            }
            f.write_str("    ")?;
            instruction.fmt_as_llvm_asm(f, opts, module, function)?;
            f.write_char('\n')?
        }
        f.write_str("    ")?;
        self.terminator.fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char('\n')
    }
}
