use std::{
    collections::LinkedList,
    error::Error,
    fmt::{Debug, Display},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub length: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum DiagnosticType {
    Recoverable,
    NonRecoverable,
}

#[derive(Debug, Clone)]
pub struct Aggregate {
    diagnostics: LinkedList<(DiagnosticType, Diagnostic)>,
}

impl Aggregate {
    fn with_rec_diagnostic(diagnostic: Diagnostic) -> Aggregate {
        Self {
            diagnostics: LinkedList::from([(DiagnosticType::Recoverable, diagnostic)]),
        }
    }

    fn with_non_rec_diagnostic(diagnostic: Diagnostic) -> Aggregate {
        Self {
            diagnostics: LinkedList::from([(DiagnosticType::NonRecoverable, diagnostic)]),
        }
    }

    fn add_rec_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics
            .push_front((DiagnosticType::Recoverable, diagnostic));
    }

    fn add_non_rec_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics
            .push_front((DiagnosticType::NonRecoverable, diagnostic));
    }

    fn combine_with(&mut self, mut other: Self) {
        self.diagnostics.append(&mut other.diagnostics);
    }

    pub fn diagnostics(&self) -> impl Iterator<Item = (DiagnosticType, &Diagnostic)> {
        self.diagnostics.iter().map(|(dt, d)| (*dt, d))
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }
}

impl Display for Aggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (t, diagnostic) in self.diagnostics() {
            match t {
                DiagnosticType::Recoverable => {
                    writeln!(f, "W: {}", diagnostic)?;
                }
                DiagnosticType::NonRecoverable => {
                    writeln!(f, "E: {}", diagnostic)?;
                }
            }
        }
        Ok(())
    }
}

impl Error for Aggregate {}

// WARNING: Don't change the order of these (Error codes will change)
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Code {
    Unspecified = 0,
    SyntaxError,
    Unimplemented,
    DuplicateQualifier,
    MultiByteChar,
    UnspecifiedType,
}

impl Code {
    /// Get a unique numeric code for this `Code`
    fn to_code(&self) -> u32 {
        *self as u32
    }
}

impl Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{:0>4x}", self.to_code())
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    code: Code,
    message: String,
    main_span: (Span, Option<String>),
    additional_spans: Vec<(Span, Option<String>)>,
}

impl Diagnostic {
    pub fn code(&self) -> &Code {
        &self.code
    }

    pub fn message(&self) -> &String {
        &self.message
    }

    pub fn main_span(&self) -> &Span {
        &self.main_span.0
    }

    pub fn main_span_message(&self) -> Option<&String> {
        self.main_span.1.as_ref()
    }

    pub fn additional_spans(&self) -> impl Iterator<Item = (&Span, Option<&String>)> {
        self.additional_spans.iter().map(|(s, m)| (s, m.as_ref()))
    }

    pub fn additional_spans_len(&self) -> usize {
        self.additional_spans.len()
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "`{}` at {}-{}",
            self.message,
            self.main_span().start,
            self.main_span().start + self.main_span().length
        )
    }
}

pub struct DiagnosticBuilder {
    span: Span,
    additional_spans: Vec<(Span, Option<String>)>,
}

impl DiagnosticBuilder {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            additional_spans: Vec::new(),
        }
    }

    pub fn with_additional_span(mut self, span: Span, message: Option<String>) -> Self {
        self.add_additional_span(span, message);
        self
    }

    pub fn add_additional_span(&mut self, span: Span, message: Option<String>) {
        self.additional_spans.push((span, message));
    }

    pub fn build_custom(self, code: Code, message: String) -> Diagnostic {
        Diagnostic {
            code,
            message,
            main_span: (self.span, None),
            additional_spans: self.additional_spans,
        }
    }

    pub fn build_unimplemented(self, message: String) -> Diagnostic {
        self.build_custom(Code::Unimplemented, message + " are not yet implemented")
    }

    pub fn build_syntax(self, unexpected: &str, expected: Vec<&str>) -> Diagnostic {
        let message = format!(
            "unexpected token: {}, expected one of: {}",
            unexpected,
            expected.join(", ")
        );
        self.build_custom(Code::SyntaxError, message)
    }

    pub fn build_duplicate_qualifier(
        mut self,
        qualifier: &str,
        first_qualifier: Span,
    ) -> Diagnostic {
        self.add_additional_span(first_qualifier, Some("first seen here".to_string()));
        let message = format!("duplicate qualifier: {qualifier}",);
        self.build_custom(Code::DuplicateQualifier, message)
    }

    pub fn build_unspecified_type(self) -> Diagnostic {
        self.build_custom(
            Code::UnspecifiedType,
            "missing type specifier; type defaults to int".to_owned(),
        )
    }
}

/// A Result of some data and a Aggregate.
#[derive(Debug, Clone)]
pub enum AggregateResult<T> {
    Ok(T),
    Rec(T, Aggregate),
    Err(Aggregate),
}

impl<T> AggregateResult<T> {
    /// Creates a Ok with T as data
    pub fn with_value(data: T) -> Self {
        Self::Ok(data)
    }

    /// Creates a Rec or a Err based on weather or not the diagnostic is recoverable. The data is
    /// only used if the diagnostic is recoverable.
    pub fn with_rec_diagnostic(data: T, diagnostic: Diagnostic) -> Self {
        Self::Rec(data, Aggregate::with_rec_diagnostic(diagnostic))
    }

    /// Always creates a `Err` with the diagnostic in the `Aggregate`. Try to only call this
    /// function with unrecoverable diagnostics but its not a violation of the contract to do so
    /// anyway.
    pub fn with_non_rec_diagnostic(diagnostic: Diagnostic) -> Self {
        Self::Err(Aggregate::with_non_rec_diagnostic(diagnostic))
    }

    pub fn is_ok(&self) -> bool {
        match self {
            AggregateResult::Ok(_) => true,
            _ => false,
        }
    }

    pub fn is_rec(&self) -> bool {
        match self {
            AggregateResult::Rec(_, _) => true,
            _ => false,
        }
    }

    pub fn is_err(&self) -> bool {
        match self {
            AggregateResult::Err(_) => true,
            _ => false,
        }
    }

    pub fn is_not_ok(&self) -> bool {
        match self {
            Self::Ok(_) => false,
            _ => true,
        }
    }

    /// If the types is a Ok, turn it into a Rec. If the `Self` was already a `Err` adds the diagnostic as a recoverable diagnostics.
    #[must_use]
    pub fn add_rec_diagnostic(self, diagnostic: Diagnostic) -> Self {
        match self {
            Self::Ok(t) => Self::Rec(t, Aggregate::with_rec_diagnostic(diagnostic)),
            Self::Rec(t, mut a) => {
                a.add_rec_diagnostic(diagnostic);
                Self::Rec(t, a)
            }
            Self::Err(mut a) => {
                a.add_rec_diagnostic(diagnostic);
                Self::Err(a)
            }
        }
    }

    /// Turn self into `Err` if it wasent already. Adding the diagnostics as a non recoverable one.
    #[must_use]
    pub fn add_non_rec_diagnostic(self, diagnostic: Diagnostic) -> Self {
        match self {
            Self::Ok(_) => Self::Err(Aggregate::with_non_rec_diagnostic(diagnostic)),
            Self::Rec(_, mut a) => {
                a.add_non_rec_diagnostic(diagnostic);
                Self::Err(a)
            }
            Self::Err(mut a) => {
                a.add_non_rec_diagnostic(diagnostic);
                Self::Err(a)
            }
        }
    }

    #[must_use]
    pub fn map<U, F>(self, op: F) -> AggregateResult<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Self::Ok(t) => AggregateResult::Ok(op(t)),
            Self::Rec(t, a) => AggregateResult::Rec(op(t), a),
            Self::Err(a) => AggregateResult::Err(a),
        }
    }

    /// If `self` is `Ok` or `Rec` calls `op` on the data. The returned `AggregateResult` is then
    /// combined with this `AggregateResult` in the usual whey.
    #[must_use]
    pub fn and_then<U, F>(self, op: F) -> AggregateResult<U>
    where
        F: FnOnce(T) -> AggregateResult<U>,
    {
        match self {
            Self::Ok(t) => op(t),
            Self::Rec(t, mut a) => match op(t) {
                AggregateResult::Ok(u) => AggregateResult::Rec(u, a),
                AggregateResult::Rec(u, a2) => {
                    a.combine_with(a2);
                    AggregateResult::Rec(u, a)
                }
                AggregateResult::Err(a2) => {
                    a.combine_with(a2);
                    AggregateResult::Err(a)
                }
            },
            Self::Err(a) => AggregateResult::Err(a),
        }
    }

    #[must_use]
    pub fn into_combined(self) -> AggregateResult<(T,)> {
        self.map(|t| (t,))
    }

    /// Converts from `AggregateResult<T>` to `Result<T, Aggregate>`, turning [`Rec`] into
    /// [`Result::Ok`].
    ///
    /// Converts self into a `Result<T, Aggregate>`, consuming self, and discarding the aggregate in
    /// case it's a `Rec(T, Aggregate)`, converting the value to a `Result::Ok` instead.
    #[must_use]
    pub fn rec_ok(self) -> Result<T, Aggregate> {
        match self {
            Self::Ok(t) => Result::Ok(t),
            Self::Rec(t, _) => Result::Ok(t),
            Self::Err(a) => Result::Err(a),
        }
    }

    /// Converts from `AggregateResult<T>` to `Result<T, Aggregate>`, turning [`Rec`] into
    /// [`Result::Err`].
    ///
    /// Converts self into a `Result<T, Aggregate>`, consuming self, and discarding the value in
    /// case it's a `Rec(T, Aggregate)`, converting the aggregate to a `Result::Err` instead.
    #[must_use]
    pub fn rec_err(self) -> Result<T, Aggregate> {
        match self {
            Self::Ok(t) => Result::Ok(t),
            Self::Rec(_, a) => Result::Err(a),
            Self::Err(a) => Result::Err(a),
        }
    }

    /// Gives references to the T and the Aggregate, turning both `Ok` and `Rec` into `Result::Ok`
    #[must_use]
    pub fn as_ref_rec_ok(&self) -> Result<&T, &Aggregate> {
        match self {
            Self::Ok(t) => Result::Ok(t),
            Self::Rec(t, _) => Result::Ok(t),
            Self::Err(a) => Result::Err(a),
        }
    }

    /// Gives references to the T and the Aggregate, turning only `Ok` into `Result::Ok`
    #[must_use]
    pub fn as_ref_rec_err(&self) -> Result<&T, &Aggregate> {
        match self {
            Self::Ok(t) => Result::Ok(t),
            Self::Rec(_, a) => Result::Err(a),
            Self::Err(a) => Result::Err(a),
        }
    }
}

impl<T> AggregateResult<Vec<T>> {
    /// Combines two `AggregateResult`s
    ///
    /// If both are not `Err` then the T of the other `AggregateResult` will be pushed to the
    /// `Vec`. Any possible `Aggregate`s will also be combined. The resulting variant will be as
    /// Close to Ok as possible.
    #[must_use]
    pub fn combine_with(self, other: AggregateResult<T>) -> Self {
        match self {
            Self::Ok(mut v) => match other {
                AggregateResult::Ok(t) => {
                    v.push(t);
                    Self::Ok(v)
                }
                AggregateResult::Rec(t, a) => {
                    v.push(t);
                    Self::Rec(v, a)
                }
                AggregateResult::Err(a) => Self::Err(a),
            },
            Self::Rec(mut v, mut a) => match other {
                AggregateResult::Ok(t) => {
                    v.push(t);
                    Self::Rec(v, a)
                }
                AggregateResult::Rec(t, a2) => {
                    v.push(t);
                    a.combine_with(a2);
                    Self::Rec(v, a)
                }
                AggregateResult::Err(a2) => {
                    a.combine_with(a2);
                    Self::Err(a)
                }
            },
            Self::Err(mut a) => match other {
                AggregateResult::Ok(_) => Self::Err(a),
                AggregateResult::Rec(_, a2) => {
                    a.combine_with(a2);
                    Self::Err(a)
                }
                AggregateResult::Err(a2) => {
                    a.combine_with(a2);
                    Self::Err(a)
                }
            },
        }
    }
}

macro_rules! impl_aggregate_result_combine {
    () => (

    );
    ( $new:ident $($from:ident)*) => (
        impl<$($from),*> AggregateResult<($($from,)*)> {
            #[allow(non_snake_case)]
            pub fn combine_with<$new>(self, other: AggregateResult<$new>) -> AggregateResult<($($from,)* $new,)> {
                match self {
                    Self::Ok(($($from,)*)) => match other {
                        AggregateResult::Ok($new) => AggregateResult::Ok(($($from,)* $new,)),
                        AggregateResult::Rec($new, a) => AggregateResult::Rec(($($from,)* $new,), a),
                        AggregateResult::Err(a) => AggregateResult::Err(a),
                    },
                    Self::Rec(($($from,)*), mut a) => match other {
                        AggregateResult::Ok($new) => AggregateResult::Rec(($($from,)* $new,), a),
                        AggregateResult::Rec($new, a2) => {
                            a.combine_with(a2);
                            AggregateResult::Rec(($($from,)* $new,), a)
                        }
                        AggregateResult::Err(a2) => {
                            a.combine_with(a2);
                            AggregateResult::Err(a)
                        }
                    },
                    Self::Err(mut a) => match other {
                        AggregateResult::Ok(_) => AggregateResult::Err(a),
                        AggregateResult::Rec(_, a2) => {
                            a.combine_with(a2);
                            AggregateResult::Err(a)
                        }
                        AggregateResult::Err(a2) => {
                            a.combine_with(a2);
                            AggregateResult::Err(a)
                        }
                    },
                }
            }
        }

        impl_aggregate_result_combine!{$($from)*}
    );
}

impl_aggregate_result_combine! { A B C D E F }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn combine_oks() {
        let a = AggregateResult::<u8>::Ok(0);
        let b = AggregateResult::<i8>::Ok(-1);
        let res = a.into_combined().combine_with(b);

        assert_eq!(res.as_ref_rec_err().unwrap(), &(0, -1));

        let d = AggregateResult::<char>::Ok('a');
        let res = res.combine_with(d);

        assert_eq!(res.as_ref_rec_err().unwrap(), &(0, -1, 'a'));
    }

    #[test]
    fn combine_one_err() {
        let d1 = DiagnosticBuilder::new(Span {
            start: 0,
            length: 1,
        })
        .build_custom(Code::SyntaxError, "test".to_string());
        let d2 = DiagnosticBuilder::new(Span {
            start: 0,
            length: 1,
        })
        .build_custom(Code::SyntaxError, "test2".to_string());

        let a = AggregateResult::<u8>::Ok(0);
        let b = AggregateResult::<i8>::with_non_rec_diagnostic(d1);
        let c = a.into_combined().combine_with(b);

        assert!(c.is_err());

        let a = AggregateResult::<i8>::with_non_rec_diagnostic(d2);
        let b = AggregateResult::<u8>::Ok(0);
        let c = a.into_combined().combine_with(b);

        assert!(c.is_err());
    }

    #[test]
    fn combine_two_err() {
        let d1 = DiagnosticBuilder::new(Span {
            start: 0,
            length: 1,
        })
        .build_custom(Code::SyntaxError, "test".to_string());
        let d2 = DiagnosticBuilder::new(Span {
            start: 0,
            length: 1,
        })
        .build_custom(Code::SyntaxError, "test2".to_string());
        let a = AggregateResult::<u8>::with_non_rec_diagnostic(d1);
        let b = AggregateResult::<i8>::with_non_rec_diagnostic(d2);
        let c = a.into_combined().combine_with(b);

        assert_eq!(c.as_ref_rec_ok().unwrap_err().diagnostics().count(), 2);
    }

    #[test]
    fn combine_vec() {
        let d1 = DiagnosticBuilder::new(Span {
            start: 0,
            length: 1,
        })
        .build_custom(Code::SyntaxError, "test".to_string());

        let a = AggregateResult::Ok(vec![0]);
        let b = AggregateResult::Ok(1);

        let a = a.combine_with(b);
        assert_eq!(a.as_ref_rec_err().unwrap(), &[0, 1]);

        let a = a.combine_with(AggregateResult::with_rec_diagnostic(3, d1));

        assert!(a.is_rec());
        assert_eq!(a.as_ref_rec_ok().unwrap(), &[0, 1, 3]);
        assert_eq!(a.as_ref_rec_err().unwrap_err().diagnostics().count(), 1);

        let d1 = DiagnosticBuilder::new(Span {
            start: 0,
            length: 1,
        })
        .build_custom(Code::SyntaxError, "test".to_string());
        let d2 = DiagnosticBuilder::new(Span {
            start: 0,
            length: 1,
        })
        .build_custom(Code::SyntaxError, "test2".to_string());
        let a = a.combine_with(AggregateResult::with_non_rec_diagnostic(d1));
        let a = a.combine_with(AggregateResult::with_non_rec_diagnostic(d2));

        assert!(a.is_err());
        assert_eq!(a.as_ref_rec_ok().unwrap_err().diagnostics().count(), 3);
    }
}
