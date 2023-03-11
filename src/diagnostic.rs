use std::{collections::LinkedList, error::Error, fmt::Display};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub length: usize,
}

#[derive(Debug, Clone, Default)]
pub struct Aggregate {
    diagnostics: LinkedList<Diagnostic>,
}

impl Aggregate {
    pub fn new() -> Self {
        Self::default()
    }

    fn with_diagnostic(diagnostic: Diagnostic) -> Aggregate {
        Self {
            diagnostics: LinkedList::from([diagnostic]),
        }
    }

    pub fn add_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push_front(diagnostic);
    }

    pub fn combine_with(&mut self, mut other: Self) {
        self.diagnostics.append(&mut other.diagnostics);
    }

    pub fn diagnostics(&self) -> impl Iterator<Item = &Diagnostic> {
        self.diagnostics.iter()
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }
}

impl Display for Aggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for diagnostic in self.diagnostics() {
            writeln!(f, "{}", diagnostic)?;
        }
        Ok(())
    }
}

impl Error for Aggregate {}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Code {
    UnspecifiedError = 0,
    SyntaxError,

    // Everyting afther this is a warning
    DuplicateQualifier = 1 << 31,
    MultiByteChar,
}

impl Code {
    fn is_recoverable(&self) -> bool {
        *self as u32 >> 31 & 0b1 != 0
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    code: Code,
    span: Span,
    message: String,
}

impl Diagnostic {
    pub fn code(&self) -> &Code {
        &self.code
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn message(&self) -> &String {
        &self.message
    }

    fn is_recoverable(&self) -> bool {
        self.code.is_recoverable()
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Diagnostic: `{}` at {}-{}",
            self.message,
            self.span.start,
            self.span.start + self.span.length
        )
    }
}

pub struct DiagnosticBuilder {
    span: Span,
}

impl DiagnosticBuilder {
    pub fn new(span: Span) -> Self {
        Self { span }
    }

    pub fn build_custom(self, code: Code, message: String) -> Diagnostic {
        Diagnostic {
            code,
            span: self.span,
            message,
        }
    }

    pub fn build_syntax(self, unexpected: &str, expected: Vec<&str>) -> Diagnostic {
        let message = format!(
            "Unexpected token: {}, expected one of: {}",
            unexpected,
            expected.join(", ")
        );
        self.build_custom(Code::SyntaxError, message)
    }

    pub fn build_duplicate_qualifier(self, qualifier: &str) -> Diagnostic {
        let message = format!("Duplicate qualifier: {qualifier}",);
        self.build_custom(Code::DuplicateQualifier, message)
    }
}

/// A Result of some data and a Aggregate. The `AggregateResult` will only ever be a `Rec` if the
/// `Aggregate` only contains `is_recoverable` diagnostic's. A Err could contain a `Aggregate`
/// with only recoverable diagnostic but this is avoided as much as possible.
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
    pub fn with_diagnostic(data: T, diagnostic: Diagnostic) -> Self {
        if diagnostic.is_recoverable() {
            Self::Rec(data, Aggregate::with_diagnostic(diagnostic))
        } else {
            Self::Err(Aggregate::with_diagnostic(diagnostic))
        }
    }

    /// Always creates a `Err` with the diagnostic in the `Aggregate`. Try to only call this
    /// function with unrecoverable diagnostics but its not a violation of the contract to do so
    /// anyway.
    pub fn with_diagnostic_err(diagnostic: Diagnostic) -> Self {
        Self::Err(Aggregate::with_diagnostic(diagnostic))
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

    /// If the types is a Ok, turn it into a Rec or Err based on whether it is recoverable or not.
    /// If it is Rec make it Err if the diagnostic is not recoverable. If the `Self` was
    /// already a `Err` just add the diagnostic.
    #[must_use]
    pub fn add_diagnostic(self, diagnostic: Diagnostic) -> Self {
        match self {
            Self::Ok(t) => {
                if diagnostic.is_recoverable() {
                    Self::Rec(t, Aggregate::with_diagnostic(diagnostic))
                } else {
                    Self::Err(Aggregate::with_diagnostic(diagnostic))
                }
            }
            Self::Rec(t, mut a) => {
                if diagnostic.is_recoverable() {
                    a.add_diagnostic(diagnostic);
                    Self::Rec(t, a)
                } else {
                    Self::Err(Aggregate::with_diagnostic(diagnostic))
                }
            }
            Self::Err(mut a) => {
                a.add_diagnostic(diagnostic);
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
        let a = AggregateResult::<u8>::Ok(0);
        let b = AggregateResult::<i8>::Err(Aggregate::new());
        let c = a.into_combined().combine_with(b);

        assert!(c.is_err());

        let a = AggregateResult::<i8>::Err(Aggregate::new());
        let b = AggregateResult::<u8>::Ok(0);
        let c = a.into_combined().combine_with(b);

        assert!(c.is_err());
    }

    #[test]
    fn combine_two_err() {
        let mut ag_1 = Aggregate::new();
        ag_1.add_diagnostic(
            DiagnosticBuilder::new(Span {
                start: 0,
                length: 1,
            })
            .build_custom(Code::SyntaxError, "test".to_string()),
        );
        let mut ag_2 = Aggregate::new();
        ag_2.add_diagnostic(
            DiagnosticBuilder::new(Span {
                start: 0,
                length: 1,
            })
            .build_custom(Code::SyntaxError, "test2".to_string()),
        );
        let a = AggregateResult::<u8>::Err(ag_1);
        let b = AggregateResult::<i8>::Err(ag_2);
        let c = a.into_combined().combine_with(b);

        assert_eq!(c.as_ref_rec_ok().unwrap_err().diagnostics().count(), 2);
    }

    #[test]
    fn combine_vec() {
        let a = AggregateResult::Ok(vec![0]);
        let b = AggregateResult::Ok(1);

        let a = a.combine_with(b);
        assert_eq!(a.as_ref_rec_err().unwrap(), &[0, 1]);

        let a = a.combine_with(AggregateResult::Rec(3, Aggregate::new()));

        assert!(a.is_rec());
        assert_eq!(a.as_ref_rec_ok().unwrap(), &[0, 1, 3]);

        let mut ag_1 = Aggregate::new();
        ag_1.add_diagnostic(
            DiagnosticBuilder::new(Span {
                start: 0,
                length: 1,
            })
            .build_custom(Code::SyntaxError, "test".to_string()),
        );
        let mut ag_2 = Aggregate::new();
        ag_2.add_diagnostic(
            DiagnosticBuilder::new(Span {
                start: 0,
                length: 1,
            })
            .build_custom(Code::SyntaxError, "test2".to_string()),
        );
        let a = a.combine_with(AggregateResult::Err(ag_1));
        let a = a.combine_with(AggregateResult::Err(ag_2));

        assert!(a.is_err());
        assert_eq!(a.as_ref_rec_ok().unwrap_err().diagnostics().count(), 2);
    }
}
