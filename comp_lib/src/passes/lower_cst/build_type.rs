use std::rc::Rc;

use crate::{
    ast, cst,
    diagnostic::{AggregateResult, DiagnosticBuilder, Span},
};

use super::{extract_span, tree_error};

trait TypePart {
    fn to_str(&self) -> &'static str;
}

enum Base {
    Void,
    Char,
    Int,
    Float,
    Double,
}

impl TypePart for Base {
    fn to_str(&self) -> &'static str {
        match self {
            Base::Void => "void",
            Base::Char => "char",
            Base::Int => "int",
            Base::Float => "float",
            Base::Double => "double",
        }
    }
}

enum Signedness {
    Signed,
    Unsigend,
}
impl TypePart for Signedness {
    fn to_str(&self) -> &'static str {
        match self {
            Signedness::Signed => "signed",
            Signedness::Unsigend => "unsigend",
        }
    }
}

enum Size {
    Long,
    Short,
}
impl TypePart for Size {
    fn to_str(&self) -> &'static str {
        match self {
            Size::Long => "long",
            Size::Short => "short",
        }
    }
}

pub fn build_from_specifiers(
    span: Span,
    ctx: &[Rc<cst::TypeSpecifier>],
) -> AggregateResult<ast::PrimitiveType> {
    use cst::TypeSpecifier;

    let mut base: Option<(Span, Base)> = None;
    let mut signedness: Option<(Span, Signedness)> = None;
    let mut size: Option<(Span, Size)> = None;

    fn add_part<T: TypePart>(
        new_part: T,
        old_part: &mut Option<(Span, T)>,
        span: Span,
        res: &mut AggregateResult<()>,
    ) {
        match old_part {
            Some((old_span, old_part)) => {
                res.add_err(DiagnosticBuilder::new(span).build_incompatible_specifiers(
                    new_part.to_str(),
                    old_part.to_str(),
                    *old_span,
                ))
            }
            None => *old_part = Some((span, new_part)),
        }
    }

    let mut res = AggregateResult::new_ok(());

    for specifier in ctx {
        let span = extract_span(specifier.as_ref());
        match specifier.as_ref() {
            TypeSpecifier::TypeSpecifierPrimitiveContext(ctx) => {
                let b = match ctx.tp.as_deref().unwrap() {
                    cst::PrimitiveType::PrimitiveTypeVoidContext(_) => Base::Void,
                    cst::PrimitiveType::PrimitiveTypeCharContext(_) => Base::Char,
                    cst::PrimitiveType::PrimitiveTypeIntContext(_) => Base::Int,
                    cst::PrimitiveType::PrimitiveTypeFloatContext(_) => Base::Float,
                    cst::PrimitiveType::PrimitiveTypeDoubleContext(_) => Base::Double,
                    cst::PrimitiveType::Error(ectx) => tree_error(ectx),
                };

                add_part(b, &mut base, span, &mut res);
            }
            TypeSpecifier::TypeSpecifierSignedContext(_) => {
                add_part(Signedness::Signed, &mut signedness, span, &mut res);
            }
            TypeSpecifier::TypeSpecifierUnsignedContext(_) => {
                add_part(Signedness::Unsigend, &mut signedness, span, &mut res);
            }
            TypeSpecifier::TypeSpecifierShortContext(_) => {
                add_part(Size::Short, &mut size, span, &mut res);
            }
            TypeSpecifier::TypeSpecifierLongContext(_) => {
                add_part(Size::Long, &mut size, span, &mut res);
            }
            TypeSpecifier::Error(ectx) => tree_error(ectx),
        }
    }

    let base = res.and_then(|()| match base {
        Some(base) => AggregateResult::new_ok(base),
        None => AggregateResult::new_rec(
            (span, Base::Int),
            DiagnosticBuilder::new(span).build_unspecified_type(),
        ),
    });

    base.and_then(|(base_span, base)| build_from_parts(base_span, base, signedness, size))
}

fn build_from_parts(
    base_span: Span,
    base: Base,
    signedness: Option<(Span, Signedness)>,
    size: Option<(Span, Size)>,
) -> AggregateResult<ast::PrimitiveType> {
    match base {
        Base::Void => {
            let mut res = AggregateResult::new_ok(ast::PrimitiveType::Void);
            disallow_part(signedness, base_span, &base, &mut res);
            disallow_part(size, base_span, &base, &mut res);
            res
        }
        Base::Char => {
            let ty = match signedness {
                Some((_, Signedness::Signed)) => ast::PrimitiveType::SignedChar,
                Some((_, Signedness::Unsigend)) => ast::PrimitiveType::UnsignedChar,
                None => ast::PrimitiveType::Char,
            };
            let mut res = AggregateResult::new_ok(ty);
            disallow_part(size, base_span, &base, &mut res);
            res
        }
        Base::Int => {
            let ty = match (
                signedness.map(|s| s.1).unwrap_or(Signedness::Signed),
                size.map(|s| s.1),
            ) {
                (Signedness::Signed, None) => ast::PrimitiveType::SignedInt,
                (Signedness::Signed, Some(Size::Short)) => ast::PrimitiveType::SignedShortInt,
                (Signedness::Signed, Some(Size::Long)) => ast::PrimitiveType::SignedLongInt,
                (Signedness::Unsigend, None) => ast::PrimitiveType::UnsignedInt,
                (Signedness::Unsigend, Some(Size::Short)) => ast::PrimitiveType::UnsignedShortInt,
                (Signedness::Unsigend, Some(Size::Long)) => ast::PrimitiveType::UnsignedLongInt,
            };
            AggregateResult::new_ok(ty)
        }
        Base::Float => {
            let mut res = AggregateResult::new_ok(ast::PrimitiveType::Float);
            disallow_part(signedness, base_span, &base, &mut res);
            disallow_part(size, base_span, &base, &mut res);
            res
        }
        Base::Double => {
            let mut res = match size {
                Some((size_span, Size::Short)) => {
                    return AggregateResult::new_err(
                        DiagnosticBuilder::new(base_span).build_incompatible_specifiers(
                            base.to_str(),
                            Size::Short.to_str(),
                            size_span,
                        ),
                    )
                }
                Some((_, Size::Long)) => AggregateResult::new_ok(ast::PrimitiveType::LongDouble),
                None => AggregateResult::new_ok(ast::PrimitiveType::Double),
            };
            disallow_part(size, base_span, &base, &mut res);
            res
        }
    }
}

fn disallow_part<T, P: TypePart>(
    part: Option<(Span, P)>,
    base_span: Span,
    base: &Base,
    res: &mut AggregateResult<T>,
) {
    if let Some((part_span, part)) = part {
        res.add_err(
            DiagnosticBuilder::new(base_span).build_incompatible_specifiers(
                base.to_str(),
                part.to_str(),
                part_span,
            ),
        );
    }
}
