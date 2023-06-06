use crate::{dfa::uda::UdAnalyzable, AnyReg, BasicBlock, FReg, Function, Root};
use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InvalidityReason {
    /// If the params of a function and the arguments of its entry block are not equivalent (i.e.,
    /// have a different length).
    ParamsArgumentsMismatch,
    /// If the type of an argument of a [`BlockRef`] doesn't match the type of the corresponding
    /// formal argument of the block that it references.
    ArgumentTypeMismatch,
    /// If the number of arguments of a [`BlockRef`] is not equal to the number of formal arguments
    /// of the block that it references.
    NrOfArgumentsMismatch,
    /// If the type of the argument passed to a function call doesn't match the type of the param of
    /// the called function.
    ParamTypeMismatch,
    /// If the number of arguments passed to a function call doesn't match the number of params of
    /// the called function.
    NrOfParamsMismatch,
    /// If a basic block references the same block more than once.
    DuplicateSuccessor,
    /// If a basic block has the same argument more than once.
    DuplicateArgument,
    /// If a virtual registers is defined more than once (SSA violation).
    MultipleDefs,
    /// If a virtual register is used before it is defined, or not defined at all (SSA violation).
    UndefUse,
    /// If a block is referenced but not in the graph.
    MissingBlock,
    /// If a label is referenced but not in the root.
    MissingLabel,
    /// If a function doesn't have an entry block.
    MissingEntryBlock,
}

/// Validates the given [`Root`]. Returns `Ok(())` if the root is valid, or `Err(_)` with the
/// [`InvalidityReason`] of the first detected invalidity.
///
/// See [`InvalidityReason`] for all things that are invalid (or at least detected).
pub fn validate_root(root: &Root) -> Result<(), InvalidityReason> {
    for function in root.functions() {
        validate_function(function)?;
        if function
            .referenced_labels()
            .any(|label| !root.has_label(label))
        {
            return Err(InvalidityReason::MissingLabel);
        }
        for call in function.function_calls() {
            let Some(callee) = root.function(&call.label) else {
                if root.is_external(&call.label) { continue; }
                return Err(InvalidityReason::MissingLabel);
            };
            match check_arguments_match(
                call.arguments.iter().map(|(r, _)| *r),
                callee.entry_block().unwrap().arguments.iter().copied(),
            ) {
                Ok(()) => {}
                Err(ArgsMismatch::DifferentLengths) => {
                    return Err(InvalidityReason::NrOfParamsMismatch)
                }
                Err(ArgsMismatch::TypeMismatch) => return Err(InvalidityReason::ParamTypeMismatch),
            }
        }
    }
    Ok(())
}

/// Note that not everything can be validated with only the function, e.g. missing labels cannot be
/// detected. Use [`validate_root`] to validate everything.
pub fn validate_function(function: &Function) -> Result<(), InvalidityReason> {
    let Some(entry_block) = function.entry_block() else {
        return Err(InvalidityReason::MissingEntryBlock);
    };
    if function.params_info().len() != entry_block.arguments.len() {
        return Err(InvalidityReason::ParamsArgumentsMismatch);
    }
    for block in function.blocks() {
        validate_basic_block(function, block)?;
    }
    Ok(())
}

fn validate_basic_block(function: &Function, block: &BasicBlock) -> Result<(), InvalidityReason> {
    let mut defs = HashSet::new();

    for &arg in &block.arguments {
        if !defs.insert(arg) {
            return Err(InvalidityReason::DuplicateArgument);
        }
    }

    let def_reg = |defs: &mut HashSet<_>, reg: AnyReg| match !reg.is_virtual() || defs.insert(reg) {
        true => Ok(()),
        false => Err(InvalidityReason::MultipleDefs),
    };
    let use_reg = |defs: &HashSet<_>, reg: AnyReg| match !reg.is_virtual() || defs.contains(&reg) {
        true => Ok(()),
        false => Err(InvalidityReason::UndefUse),
    };

    for instr in &block.instructions {
        let usedefs = instr.ud_info();
        for reg in usedefs.uses {
            use_reg(&defs, reg)?;
        }
        for reg in usedefs.defs {
            def_reg(&mut defs, reg)?;
        }
    }

    let term_usedefs = block.terminator.ud_info();
    for reg in term_usedefs.uses {
        use_reg(&defs, reg)?;
    }
    for reg in term_usedefs.defs {
        def_reg(&mut defs, reg)?;
    }

    let mut checked_succ_ids = HashSet::new();
    for bref in block.succidxs().map(|idx| block.succ_bref(idx)) {
        if !checked_succ_ids.insert(bref.label.id()) {
            return Err(InvalidityReason::DuplicateSuccessor);
        }
        let Some(succ) = function.get_block(bref.label.id()) else {
            return Err(InvalidityReason::MissingBlock);
        };
        match check_arguments_match(
            bref.arguments.iter().copied(),
            succ.arguments.iter().copied(),
        ) {
            Ok(()) => {}
            Err(ArgsMismatch::DifferentLengths) => {
                return Err(InvalidityReason::NrOfArgumentsMismatch)
            }
            Err(ArgsMismatch::TypeMismatch) => return Err(InvalidityReason::ArgumentTypeMismatch),
        }
    }

    Ok(())
}

enum ArgsMismatch {
    DifferentLengths,
    TypeMismatch,
}

fn check_arguments_match<T, U>(args1: T, args2: U) -> Result<(), ArgsMismatch>
where
    T: ExactSizeIterator<Item = AnyReg>,
    U: ExactSizeIterator<Item = AnyReg>,
{
    if args1.len() != args2.len() {
        return Err(ArgsMismatch::DifferentLengths);
    }
    for args in args1.zip(args2) {
        match args {
            (AnyReg::R(_), AnyReg::F(_)) | (AnyReg::F(_), AnyReg::R(_)) => {
                return Err(ArgsMismatch::TypeMismatch)
            }
            (AnyReg::F(f1), AnyReg::F(f2)) => match (f1, f2) {
                (f @ FReg::F(_), FReg::VirtualSingle(_))
                | (FReg::VirtualSingle(_), f @ FReg::F(_))
                    if f.is_double() =>
                {
                    return Err(ArgsMismatch::TypeMismatch)
                }
                (FReg::F(_), f @ FReg::VirtualDouble(_))
                | (FReg::VirtualDouble(_), f @ FReg::F(_))
                    if !f.is_double() =>
                {
                    return Err(ArgsMismatch::TypeMismatch)
                }
                (FReg::VirtualSingle(_), FReg::VirtualDouble(_))
                | (FReg::VirtualDouble(_), FReg::VirtualSingle(_)) => {
                    return Err(ArgsMismatch::TypeMismatch)
                }
                _ => {}
            },
            _ => {}
        }
    }
    Ok(())
}
