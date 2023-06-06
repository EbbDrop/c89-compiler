use mips_ir as mir;

/// Patches "undef use" problems by inserting no-op "Declare" instructions.
/// Note that this cannot fix "use before def" problems, i.e. when a definition exists but comes
/// after its first use. This only works for registers used but never defined.
pub fn patch_root(root: &mut mir::Root) {
    root.functions_mut().for_each(patch_function);
}

/// See [`patch_root`].
pub fn patch_function(function: &mut mir::Function) {
    match mir::dfa::uda::DuChains::try_build_from(&function.cfg) {
        Ok(_) => (),
        Err(mir::dfa::uda::DefUseError::MultipleDefs(multiple_defs)) => {
            dbg!(function);
            panic!("cannot patch: codegen generated multiple defs: {multiple_defs:#?}")
        }
        Err(mir::dfa::uda::DefUseError::UndefUses(undef_uses)) => {
            let dominator_tree = function.cfg.dominator_tree();
            for (reg, locations) in undef_uses {
                let common_dominator_id = dominator_tree
                    .first_common_dominator(locations.iter().map(|loc| loc.block_id))
                    .unwrap();
                let common_dominator = &mut function.cfg[common_dominator_id];
                let first_use = locations
                    .iter()
                    .filter_map(|loc| {
                        (loc.block_id == common_dominator_id).then_some(loc.local.0 as usize)
                    })
                    .min()
                    .unwrap_or(common_dominator.instructions.len());
                common_dominator
                    .instructions
                    .insert(first_use, mir::instr::virt::declare(reg));
            }
        }
    }
    function.finish()
}
