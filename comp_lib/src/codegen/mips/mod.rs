mod generator;
mod patcher;

use crate::{diagnostic::AggregateResult, ir, settings::Settings};
use generator::Generator;
use mips_ir as mir;

pub fn build_from_ir(
    ir: &ir::Root,
    _settings: &Settings,
    _filename: &str,
    source: &str,
) -> AggregateResult<mips_ir::Root> {
    let mut root = Generator::new(ir, source).generate();
    patcher::patch_root(&mut root);
    mir::dfa::dce::purge_root(&mut root);
    mir::passes::pre_allocation::run(&mut root);
    mir::passes::register_allocation::run(&mut root);
    mir::passes::stack_frame_builder::run(&mut root);
    mir::passes::devirtualizer::run(&mut root);
    mir::optimizer::simplifier::simplify_root(&mut root);
    mir::fixer::fix_root(&mut root);
    mir::linker::run(&mut root).expect("linking failed");
    AggregateResult::new_ok(root)
}
