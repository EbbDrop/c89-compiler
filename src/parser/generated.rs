#[rustfmt::skip]
#[allow(unused_parens)]
#[allow(unused_braces)]
pub mod mainlexer;

#[rustfmt::skip]
#[allow(unused_parens)]
#[allow(unused_braces)]
pub mod mainlistener;

#[rustfmt::skip]
#[allow(unused_parens)]
#[allow(unused_braces)]
pub mod mainvisitor;

#[rustfmt::skip]
#[allow(unused_parens)]
#[allow(unused_braces)]
pub mod mainparser;

pub use mainlexer::*;
pub use mainlistener::*;
pub use mainparser::*;
pub use mainvisitor::*;
