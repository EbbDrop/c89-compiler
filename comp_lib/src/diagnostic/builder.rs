use super::{Code, Diagnostic, Span};
use crate::ir;

impl DiagnosticBuilder {
    pub fn new(span: impl Into<Span>) -> Self {
        Self {
            span: span.into(),
            additional_spans: Vec::new(),
        }
    }

    pub fn with_additional_span(mut self, span: impl Into<Span>, message: Option<String>) -> Self {
        self.add_additional_span(span, message);
        self
    }

    pub fn add_additional_span(&mut self, span: impl Into<Span>, message: Option<String>) {
        self.additional_spans.push((span.into(), message));
    }

    pub fn with_ir_expr_type(mut self, expr: &ir::expr::ExprNode) -> Self {
        self.add_ir_expr_type(expr);
        self
    }

    pub fn add_ir_expr_type(&mut self, expr: &ir::expr::ExprNode) {
        self.add_additional_span(
            expr.span,
            Some(format!("this expression has type: `{}`", expr.ty)),
        );
    }

    fn build_custom(self, code: Code, message: String) -> Diagnostic {
        Diagnostic {
            code,
            message,
            main_span: (self.span, None),
            additional_spans: self.additional_spans,
        }
    }

    pub fn build_syntax_error(self, unexpected: &str, expected: Vec<&str>) -> Diagnostic {
        let message = if expected.is_empty() {
            format!("unexpected token: {unexpected}")
        } else {
            let expected = expected.join(", ");
            format!("unexpected token: {unexpected}, expected one of: {expected}")
        };
        self.build_custom(Code::SyntaxError, message)
    }

    pub fn build_unimplemented(self, feature: &str) -> Diagnostic {
        self.build_custom(Code::Unimplemented, format!("not implemented: {feature}"))
    }

    pub fn build_duplicate_qualifier(mut self, qualifier: &str, first_seen: Span) -> Diagnostic {
        self.add_additional_span(first_seen, Some("first seen here".to_string()));
        let msg = format!("duplicate qualifier: {qualifier}");
        self.build_custom(Code::DuplicateQualifier, msg)
    }

    pub fn build_multi_byte_char(self) -> Diagnostic {
        let msg = "multi-byte chars are implementation defined".to_owned();
        self.build_custom(Code::MultiByteChar, msg)
    }

    pub fn build_unspecified_type(self) -> Diagnostic {
        let msg = "missing type specifier; type defaults to int".to_owned();
        self.build_custom(Code::UnspecifiedType, msg)
    }

    pub fn build_unknown_escape_sequence(self, escape_seq: &str) -> Diagnostic {
        let msg = format!("unknown escape sequence: {escape_seq}");
        self.build_custom(Code::UnknownEscapeSequence, msg)
    }

    pub fn build_incomplete_hex_escape_sequence(mut self) -> Diagnostic {
        let end = self.span.excl_end();
        self.add_additional_span(end..end, Some("expected hex digit".to_owned()));
        let msg = "incomplete hex escape sequence".to_owned();
        self.build_custom(Code::IncompleteEscapeSequence, msg)
    }

    pub fn build_escape_sequence_out_of_range(self, base_name: &str) -> Diagnostic {
        let msg = format!("{base_name} escape sequence out of range");
        self.build_custom(Code::EscapeSequenceOutOfRange, msg)
    }

    pub fn build_embedded_null_in_string(self) -> Diagnostic {
        let msg = "embedded 0 byte in string literal".to_owned();
        self.build_custom(Code::EmbeddedNullInString, msg)
    }

    pub fn build_cant_be_const(mut self, operator: &str, expr_span: Span) -> Diagnostic {
        self.add_additional_span(expr_span, Some("this expression is const".to_string()));
        self.build_custom(
            Code::NeedConst,
            format!("can't {operator} a const expression"),
        )
    }

    pub fn build_unexpected_type(
        mut self,
        operator: &str,
        needed_type_cat: TypeCat,
        expr: &ir::expr::ExprNode,
    ) -> Diagnostic {
        let message = format!("{operator} operator needs {}", needed_type_cat.long_name());
        self.add_ir_expr_type(expr);
        self.build_custom(Code::UnexpectedType, message)
    }

    pub fn build_unexpected_type_bin(
        mut self,
        operator: &str,
        needed_type_cat: TypeCat,
        expr: &ir::expr::ExprNode,
        other_expr: Option<&ir::expr::ExprNode>,
    ) -> Diagnostic {
        self.add_ir_expr_type(expr);
        if let Some(exprr) = other_expr {
            self.add_ir_expr_type(exprr);
        }
        let message = format!(
            "{operator} operator needs two {}",
            needed_type_cat.long_name_plural()
        );
        self.build_custom(Code::UnexpectedType, message)
    }

    pub fn build_incompatible_types(
        mut self,
        operator: &str,
        expr: &ir::expr::ExprNode,
        other_expr: &ir::expr::ExprNode,
    ) -> Diagnostic {
        self.add_ir_expr_type(expr);
        self.add_ir_expr_type(other_expr);
        self.build_custom(
            Code::UnexpectedType,
            format!("incompatible types for {operator}"),
        )
    }

    pub fn build_need_lvalue(mut self, expr_type: &str, inner_span: Span) -> Diagnostic {
        // IDEA: If the inner expr is a pointer, suggest dereferencing.
        self.add_additional_span(
            inner_span,
            Some("this expression is not a lvalue, but needs to be".to_owned()),
        );
        self.build_custom(Code::NeedLvalue, format!("{expr_type} needs a lvalue"))
    }

    pub fn build_invalid_cast(mut self, reason: InvalidCastReason) -> Diagnostic {
        let message = match reason {
            InvalidCastReason::PointerFromFloat(expr) => {
                self.add_ir_expr_type(expr);
                "floating types can't be cast to pointers"
            }
            InvalidCastReason::FloatFromPointer(expr) => {
                self.add_ir_expr_type(expr);
                "pointers can't be cast to floating types"
            }
            InvalidCastReason::FromNonScaler(expr) => {
                self.add_ir_expr_type(expr);
                "can only cast from a scalar type"
            }
            InvalidCastReason::IntoNonScalar => "can only cast to scalar types",
        };
        self.build_custom(Code::InvalidCast, message.to_owned())
    }

    pub fn build_implicit_lossy_conversion(
        mut self,
        from_expr: &ir::expr::ExprNode,
        to_expr: &ir::expr::LvalueExprNode,
        only_sign: bool,
    ) -> Diagnostic {
        let message = if only_sign {
            format!(
                "implicit conversion from `{}` to `{}` could change sign",
                from_expr.ty, to_expr.ty,
            )
        } else {
            format!(
                "implicit conversion from `{}` to `{}` could lose information",
                from_expr.ty, to_expr.ty,
            )
        };
        self.add_ir_expr_type(from_expr);
        self.add_additional_span(
            to_expr.span,
            Some(format!("while this has type: `{}`", to_expr.ty)),
        );
        self.build_custom(Code::LossyImplicitConversion, message)
    }

    pub fn build_incompatible_assign(
        mut self,
        from_expr: &ir::expr::ExprNode,
        to_expr: &ir::expr::LvalueExprNode,
    ) -> Diagnostic {
        self.add_ir_expr_type(from_expr);
        self.add_additional_span(
            to_expr.span,
            Some(format!("while this has type: `{}`", to_expr.ty)),
        );
        self.build_custom(
            Code::IncompatibleAssign,
            format!(
                "assign from `{}` to the incompatible type `{}`",
                from_expr.ty, to_expr.ty
            ),
        )
    }

    pub fn build_assign_const_loss(mut self, with_const: Span, without_const: Span) -> Diagnostic {
        self.add_additional_span(with_const, Some("this points to a const value".to_owned()));
        self.add_additional_span(without_const, Some("while this doesn't".to_owned()));
        self.build_custom(
            Code::IncompatibleAssign,
            "assign loses const qualifier".to_owned(),
        )
    }

    pub fn build_too_big_constant(self, value: i128) -> Diagnostic {
        self.build_custom(
            Code::TooBigConstant,
            format!("constant {value} cannot be correctly represented in any c type"),
        )
    }

    pub fn build_undeclared_ident(self, name: &str) -> Diagnostic {
        // TODO give possible alternative names that are close
        self.build_custom(
            Code::UndeclaredIdent,
            format!("identifier `{name}` isn't declared in this scope"),
        )
    }

    pub fn build_already_defined(mut self, name: &str, original_span: Span) -> Diagnostic {
        // TODO give span of definition
        self.add_additional_span(original_span, Some("originally defined here".to_owned()));
        self.build_custom(
            Code::AlreadyDefined,
            format!("identifier `{name}` is already defined"),
        )
    }
}

pub struct DiagnosticBuilder {
    span: Span,
    additional_spans: Vec<(Span, Option<String>)>,
}

#[derive(Debug, Copy, Clone)]
pub enum TypeCat {
    Integral,
    Arithmetic,
    Scalar,
    Pointer,
}

#[derive(Debug, Clone)]
pub enum InvalidCastReason<'a> {
    PointerFromFloat(&'a ir::expr::ExprNode),
    FloatFromPointer(&'a ir::expr::ExprNode),
    FromNonScaler(&'a ir::expr::ExprNode),
    IntoNonScalar,
}

impl TypeCat {
    fn long_name(&self) -> &'static str {
        match self {
            TypeCat::Integral => "an integral type",
            TypeCat::Arithmetic => "an arithmetic type",
            TypeCat::Scalar => "a scalar type",
            TypeCat::Pointer => "a pointer type",
        }
    }

    fn long_name_plural(&self) -> &'static str {
        match self {
            TypeCat::Integral => "integral types",
            TypeCat::Arithmetic => "arithmetic types",
            TypeCat::Scalar => "scalar types",
            TypeCat::Pointer => "pointer types",
        }
    }
}
