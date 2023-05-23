mod generator;

use crate::{diagnostic::AggregateResult, ir, settings::Settings};
use generator::Generator;

pub fn build_from_ir(
    ir: &ir::Root,
    _settings: &Settings,
    _filename: &str,
    source: &str,
) -> AggregateResult<mips_ir::Root> {
    AggregateResult::new_ok(Generator::new(ir, source).generate())
}
