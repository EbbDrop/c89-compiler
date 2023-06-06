mod generator;

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
    mir::compile_and_link(&mut root);
    AggregateResult::new_ok(root)
}
