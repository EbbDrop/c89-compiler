pub mod builder;

use std::{
    collections::LinkedList,
    fmt::{Debug, Display},
};

pub use builder::DiagnosticBuilder;

#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub struct Span {
    start: usize,
    length: usize,
}

impl From<std::ops::Range<usize>> for Span {
    fn from(value: std::ops::Range<usize>) -> Self {
        Self {
            start: value.start,
            length: value.len(),
        }
    }
}

impl From<std::ops::RangeInclusive<usize>> for Span {
    fn from(value: std::ops::RangeInclusive<usize>) -> Self {
        let (start, end) = value.into_inner();
        Self {
            start,
            length: end.saturating_sub(start),
        }
    }
}

impl From<Span> for std::ops::Range<usize> {
    fn from(val: Span) -> Self {
        val.start..val.excl_end()
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.excl_end())
    }
}

impl Span {
    pub fn start(&self) -> usize {
        self.start
    }

    pub fn excl_end(&self) -> usize {
        self.start + self.length
    }

    pub fn len(&self) -> usize {
        self.length
    }

    pub fn is_empty(&self) -> bool {
        self.length == 0
    }
}

// WARNING: Don't change the order of these (Error codes will change)
#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Code {
    /// This is an internal code that should never be used for actual diagnostics.
    Unspecified = 0,
    SyntaxError,
    Unimplemented,
    DuplicateQualifier,
    MultiByteChar,
    UnspecifiedType,
    UnknownEscapeSequence,
    IncompleteEscapeSequence,
    EscapeSequenceOutOfRange,
    EmbeddedNullInString,
    NeedConst,
    UnexpectedType,
    NeedLvalue,
    InvalidCast,
    LossyImplicitAssign,
    IncompatibleAssign,
    TooBigConstant,
    UndeclaredIdent,
    AlreadyDefined,
    UsingUninit,
    DuplicateDefault,
    InvalidJumpStmt,
    SwitchCaseNotFolded,
    SwitchCaseNotInt,
    Unreachable,
    InvalidArraySize,
    NonConstGlobalInitializer,
    AssignConstLoss,
    LossyImplicitReturn,
    IncompatibleReturn,
    ReturnConstLoss,
    UndeclaredFunction,
    WrongAmountOfArgs,
    LossyImplicitArg,
    IncompatibleArg,
    ArgConstLoss,
    IncompatibleSpecifiers,
    QualifiedVoid,
    VoidUsed,
    VoidVariable,
    ValueReturnInVoid,
    NoReturnValue,
    NotAlwaysReturn,
}

impl Code {
    /// Get a unique numeric code for this `Code`
    fn as_code(&self) -> u32 {
        *self as u32
    }
}

impl Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{:0>4x}", self.as_code())
    }
}

#[derive(Debug, Clone, PartialEq)]
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

/// Specifies the possibles types of diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticKind {
    /// For recoverable diagnostics. (cfr. warnings)
    Rec,
    /// For non-recoverable diagnostics. (cfr. errors)
    Err,
}

/// A result combining a value with aggregated diagnostics.
///
/// Can be in one of three states:
/// - _ok_: The result contains a value and has no diagnostics. Corresponds to `Result::Ok`.
/// - _rec_: recoverable: The result contains a (recovered) value and has only diagnostics of the
///   kind [`DiagnosticKind::Rec`].
/// - _err_: non-recoverable: The result does not contain a value and has at least one diagnostic of
///   the kind [`DiagnosticKind::Err`].
///
/// It is guaranteed that the result will never be completely empty (i.e. no value nor diagnostics).
#[derive(Debug, Clone, PartialEq)]
pub struct AggregateResult<T> {
    value: Option<T>,
    diagnostics: LinkedList<(DiagnosticKind, Diagnostic)>,
}

impl<T: Default> Default for AggregateResult<T> {
    /// Creates an `AggregateResult<T>` in an _ok_ state containing the default value for `T`.
    fn default() -> Self {
        Self {
            value: Some(T::default()),
            diagnostics: LinkedList::default(),
        }
    }
}

impl<T> AggregateResult<T> {
    /// Creates an `AggregateResult` in an _ok_ state containing the specified value.
    ///
    /// # Examples
    ///
    /// ```
    /// # use comp_lib::diagnostic::*;
    /// let res = AggregateResult::new_ok(2);
    ///
    /// assert!(res.is_ok());
    /// assert_eq!(res.value(), Some(&2));
    /// assert!(res.diagnostics().next().is_none());
    /// ```
    pub fn new_ok(value: T) -> Self {
        Self {
            value: Some(value),
            diagnostics: LinkedList::new(),
        }
    }

    /// Creates an `AggregateResult` in a _rec_ state containing the specified value and diagnostic.
    ///
    /// The diagnostic will be given the kind [`DiagnosticKind::Rec`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use comp_lib::diagnostic::*;
    /// # let diagnostic = DiagnosticBuilder::new(Span::default()).build_unimplemented("");
    /// let res = AggregateResult::new_rec("hi", diagnostic.clone());
    ///
    /// assert!(res.is_rec());
    /// assert_eq!(res.value(), Some(&"hi"));
    /// let mut diags = res.diagnostics();
    /// assert_eq!(diags.next(), Some((DiagnosticKind::Rec, &diagnostic)));
    /// assert!(diags.next().is_none());
    /// ```
    pub fn new_rec(value: T, diagnostic: Diagnostic) -> Self {
        Self {
            value: Some(value),
            diagnostics: LinkedList::from([(DiagnosticKind::Rec, diagnostic)]),
        }
    }

    /// Creates an `AggregateResult` in an _err_ state containing the specified diagnostic.
    ///
    /// The diagnostic will be given the kind [`DiagnosticKind::Err`].
    ///
    /// # Examples
    ///
    /// ```
    /// # use comp_lib::diagnostic::*;
    /// # let diagnostic = DiagnosticBuilder::new(Span::default()).build_unimplemented("");
    /// let res = AggregateResult::<()>::new_err(diagnostic.clone());
    ///
    /// assert!(res.is_err());
    /// assert!(res.value().is_none());
    /// let mut diags = res.diagnostics();
    /// assert_eq!(diags.next(), Some((DiagnosticKind::Err, &diagnostic)));
    /// assert!(diags.next().is_none());
    /// ```
    pub fn new_err(diagnostic: Diagnostic) -> Self {
        Self {
            value: None,
            diagnostics: LinkedList::from([(DiagnosticKind::Err, diagnostic)]),
        }
    }

    /// Returns `true` if the result is in an _ok_ state.
    ///
    /// A result in an _ok_ state is guaranteed to contain a value and have no diagnostics.
    pub fn is_ok(&self) -> bool {
        self.value.is_some() && self.diagnostics.is_empty()
    }

    /// Returns `true` if the result is in a _rec_ state.
    ///
    /// A result in a _rec_ state is guaranteed to contain a (recovered) value and only diagnostics
    /// of the kind [`DiagnosticKind::Rec`]. It will contain at least one diagnostic.
    pub fn is_rec(&self) -> bool {
        self.value.is_some() && !self.diagnostics.is_empty()
    }

    /// Returns `true` if the result is in an _err_ state.
    ///
    /// A result in an _err_ state is guaranteed to contain no value and only diagnostics of the
    /// kind [`DiagnosticKind::Err`]. It will contain at least one diagnostic.
    pub fn is_err(&self) -> bool {
        self.value.is_none()
    }

    /// Returns the contained value for _ok_ and _rec_ results.
    ///
    /// # Examples
    ///
    /// ```
    /// # use comp_lib::diagnostic::*;
    /// # let diagnostic1 = DiagnosticBuilder::new(Span::default()).build_unimplemented("");
    /// # let diagnostic2 = diagnostic1.clone();
    ///
    /// let ok = AggregateResult::new_ok(1);
    /// let rec = AggregateResult::new_rec(2, diagnostic1);
    /// let err = AggregateResult::<()>::new_err(diagnostic2);
    ///
    /// assert_eq!(ok.value(), Some(&1));
    /// assert_eq!(rec.value(), Some(&2));
    /// assert!(err.value().is_none());
    /// ```
    pub fn value(&self) -> Option<&T> {
        self.value.as_ref()
    }

    /// Converts from `&mut AggregateResult<T>` to `Option<&mut T>`, returning `Some(&mut T)` for
    /// _ok_ and _rec_ results.
    ///
    /// # Examples
    ///
    /// ```
    /// # use comp_lib::diagnostic::*;
    /// # let diagnostic1 = DiagnosticBuilder::new(Span::default()).build_unimplemented("");
    /// # let diagnostic2 = diagnostic1.clone();
    ///
    /// let mut ok = AggregateResult::new_ok(1);
    /// let mut rec = AggregateResult::new_rec(2, diagnostic1);
    /// let mut err = AggregateResult::<()>::new_err(diagnostic2);
    ///
    /// assert_eq!(ok.value_mut(), Some(&mut 1));
    /// assert_eq!(rec.value_mut(), Some(&mut 2));
    /// assert!(err.value_mut().is_none());
    /// ```
    pub fn value_mut(&mut self) -> Option<&mut T> {
        self.value.as_mut()
    }

    /// Converts from `AggregateResult<T>` to `Option<T>`, returning `Some(T)` for _ok_ and _rec_
    /// results, and consuming `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use comp_lib::diagnostic::*;
    /// # let diagnostic1 = DiagnosticBuilder::new(Span::default()).build_unimplemented("");
    /// # let diagnostic2 = diagnostic1.clone();
    ///
    /// let mut ok = AggregateResult::new_ok(1);
    /// let mut rec = AggregateResult::new_rec(2, diagnostic1);
    /// let mut err = AggregateResult::<()>::new_err(diagnostic2);
    ///
    /// assert_eq!(ok.into_value(), Some(1));
    /// assert_eq!(rec.into_value(), Some(2));
    /// assert!(err.into_value().is_none());
    /// ```
    pub fn into_value(self) -> Option<T> {
        self.value
    }

    /// Returns an iterator over the diagnostics for _rec_ and _err_ results.
    ///
    /// If this is a _rec_ result, all diagnostics are guaranteed to be of the kind
    /// [`DiagnosticKind::Rec`].
    pub fn diagnostics(&self) -> impl Iterator<Item = (DiagnosticKind, &Diagnostic)> {
        self.diagnostics.iter().map(|(dt, d)| (*dt, d))
    }

    /// Returns a consuming iterator over the diagnostics for _rec_ and _err_ results.
    ///
    /// If this is a _rec_ result, all diagnostics are guaranteed to be of the kind
    /// [`DiagnosticKind::Rec`].
    pub fn into_diagnostics(self) -> impl Iterator<Item = (DiagnosticKind, Diagnostic)> {
        self.diagnostics.into_iter()
    }

    /// Adds a recoverable diagnostic to the result.
    ///
    /// An _ok_ result will become a _rec_ result.
    pub fn add_rec_diagnostic(&mut self, diagnostic: Diagnostic) {
        self.diagnostics
            .push_back((DiagnosticKind::Rec, diagnostic));
    }

    /// Adds a non-recoverable diagnostic to the result.
    ///
    /// The result will become an _err_ result, dropping a contained value.
    pub fn add_err(&mut self, diagnostic: Diagnostic) {
        self.value = None;
        self.diagnostics
            .push_back((DiagnosticKind::Err, diagnostic));
    }

    /// Runs the predicate for all recoverable diagnostics, turning the diagnostics where the
    /// predicate returns `true` into an error. This will also make the `AggregateResult` itself
    /// an _err_.
    pub fn upgrade_diagnostics<F>(&mut self, mut predicate: F)
    where
        F: FnMut(&Diagnostic) -> bool,
    {
        for (kind, diagnostic) in &mut self.diagnostics {
            if *kind == DiagnosticKind::Err {
                continue;
            }
            if predicate(diagnostic) {
                *kind = DiagnosticKind::Err;
                self.value = None;
            }
        }
    }

    /// Maps an `AggregateResult<T, E>` to `AggregateResult<U, E>` by applying a function to a
    /// contained value, leaving diagnostics untouched.
    #[must_use]
    pub fn map<U, F>(self, op: F) -> AggregateResult<U>
    where
        F: FnOnce(T) -> U,
    {
        AggregateResult {
            value: self.value.map(op),
            diagnostics: self.diagnostics,
        }
    }

    /// Always returns false if this `AggregateResult` is _err_. Otherwise calls `f` on the inner
    /// value and returns its result.
    pub fn has_value_and<F>(&mut self, f: F) -> bool
    where
        F: FnOnce(&T) -> bool,
    {
        match &self.value {
            Some(v) => f(v),
            None => false,
        }
    }

    /// Combines the values of `self` and `other` using `f`, aggregating their diagnostics.
    ///
    /// If either `self` or `other` is in an _err_ state, the returned result will be in an _err_
    /// state as well. Otherwise, the result will contain the value returned by `f`.
    #[must_use]
    pub fn combine<U, F, R>(mut self, mut other: AggregateResult<U>, f: F) -> AggregateResult<R>
    where
        F: FnOnce(T, U) -> R,
    {
        AggregateResult {
            value: self.value.and_then(|t| other.value.map(|u| f(t, u))),
            diagnostics: {
                self.diagnostics.append(&mut other.diagnostics);
                self.diagnostics
            },
        }
    }

    /// Aggregates the diagnostics of `other` with `self`, discarding the value of self.
    #[must_use]
    pub fn aggregate<U>(mut self, mut other: AggregateResult<U>) -> AggregateResult<U> {
        self.diagnostics.append(&mut other.diagnostics);
        other.diagnostics = self.diagnostics;
        other
    }

    /// Returns `other` if the result has a value, aggregating their diagnostics. Otherwise returns
    /// an _err_ `AggregateResult<U>` with the diagnostics of `self`.
    ///
    /// The value of `self` will always be discarded.
    #[must_use]
    pub fn and<U>(mut self, mut other: AggregateResult<U>) -> AggregateResult<U> {
        match self.value {
            Some(_) => {
                self.diagnostics.append(&mut other.diagnostics);
                other.diagnostics = self.diagnostics;
                other
            }
            None => AggregateResult {
                value: None,
                diagnostics: self.diagnostics,
            },
        }
    }

    /// Calls `op` if the result has a value, aggregating the diagnostics of `self` with the result
    /// returned by `op`.
    ///
    /// The value of `self` will always be discarded.
    #[must_use]
    pub fn and_then<U, F>(mut self, op: F) -> AggregateResult<U>
    where
        F: FnOnce(T) -> AggregateResult<U>,
    {
        match self.value {
            Some(t) => {
                let mut other = op(t);
                self.diagnostics.append(&mut other.diagnostics);
                other.diagnostics = self.diagnostics;
                other
            }
            None => AggregateResult {
                value: None,
                diagnostics: self.diagnostics,
            },
        }
    }

    /// Zips the values of `self` and `other`, aggregating their diagnostics.
    ///
    /// If either `self` or `other` is in an _err_ state, the returned result will be in an _err_
    /// state as well. Otherwise, the returned result will contain a tuple with the values of `self`
    /// and `other`.
    ///
    /// This method is roughly equivalent to
    ///
    /// ```
    /// # use comp_lib::diagnostic::*;
    /// # let self_ = AggregateResult::new_ok(1);
    /// # let other = AggregateResult::new_ok(2);
    /// # let _ =
    /// self_.combine(other, |t, u| (t, u))
    /// # ;
    /// ```
    ///
    /// # Examples
    ///
    /// ```
    /// # use comp_lib::diagnostic::*;
    /// # let rec_diag = DiagnosticBuilder::new(Span::default()).build_unimplemented("");
    /// # let err_diag = DiagnosticBuilder::new(Span::default()).build_unimplemented("");
    /// let ok = AggregateResult::new_ok(1);
    /// let rec = AggregateResult::new_rec("hi", rec_diag);
    /// let err = AggregateResult::<()>::new_err(err_diag);
    ///
    /// assert_eq!(err.clone().zip(rec.clone()).value(), None);
    /// assert_eq!(ok.clone().zip(err).value(), None);
    /// assert_eq!(ok.clone().zip(rec).value(), Some(&(1, "hi")));
    /// assert_eq!(ok.clone().zip(ok).value(), Some(&(1, 1)));
    /// ```
    pub fn zip<U>(mut self, mut other: AggregateResult<U>) -> AggregateResult<(T, U)> {
        AggregateResult {
            value: self.value.zip(other.value),
            diagnostics: {
                self.diagnostics.append(&mut other.diagnostics);
                self.diagnostics
            },
        }
    }

    /// Add `self` to `other`, combining their values using `f`, and aggregating their diagnostics.
    ///
    /// This method has similar semantics to
    ///
    /// ```
    /// # use comp_lib::diagnostic::*;
    /// # let self_ = AggregateResult::new_ok(1);
    /// # let mut other = AggregateResult::new_ok(2);
    /// # fn f(_: &mut u8, _: u8) {}
    /// {
    ///     other = other.combine(self_, |mut u, t| {
    ///         f(&mut u, t);
    ///         u
    ///     });
    /// }
    /// ```
    ///
    /// except that `other` is modified in-place.
    pub fn add_to<U, F>(mut self, other: &mut AggregateResult<U>, f: F)
    where
        F: FnOnce(&mut U, T),
    {
        if let Some((u, t)) = other.value.as_mut().zip(self.value) {
            f(u, t);
        } else {
            other.value = None;
        }
        other.diagnostics.append(&mut self.diagnostics);
    }

    pub fn transpose_from(value: Option<Self>) -> AggregateResult<Option<T>> {
        match value {
            Some(res) => res.map(Some),
            None => AggregateResult::new_ok(None),
        }
    }
}

impl<T> AggregateResult<Option<T>> {
    pub fn transpose(self) -> Option<AggregateResult<T>> {
        match self.value {
            Some(value @ Some(_)) => Some(AggregateResult {
                value,
                diagnostics: self.diagnostics,
            }),
            Some(None) => None,
            None => Some(AggregateResult {
                value: None,
                diagnostics: self.diagnostics,
            }),
        }
    }
}
