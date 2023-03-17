use super::{Code, Diagnostic, Span};

pub struct DiagnosticBuilder {
    span: Span,
    additional_spans: Vec<(Span, Option<String>)>,
}

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
}
