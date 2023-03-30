use std::fmt;
use std::rc::Rc;

////////////////////////////////////////////////////////////////////////////////
// Name
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// A `Name` is a public thin wrapper around [`String`] that ensures it contains a valid LLVM
/// identifier.
///
/// A LLVM identifier is valid if it matches the regex `[%@][-a-zA-Z$._][-a-zA-Z$._0-9]*`. Otherwise
/// it must be surrounded in quotes.
///
/// The `From<String>` implementation will auto-wrap the identifier in quotes if it doesn't
/// match the regex.
pub struct Name(String);

impl From<String> for Name {
    fn from(value: String) -> Self {
        // TODO: check if value forms a valid llvm identifier, and surround with quotes if not
        Self(value)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Utility method to more easily create static name's of which you can guarantee it is a valid
/// LLVM IR identifier at compile time.
pub fn name(s: &'static str) -> Name {
    s.to_owned().try_into().unwrap()
}

////////////////////////////////////////////////////////////////////////////////
// Public (opaque) identifiers and related conversions
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
/// Opaque identifier for global names (i.e. module scoped).
pub struct GlobalName(pub(crate) GlobalNameId);

#[derive(Debug, Clone)]
/// Opaque identifier for referencing labels in instructions.
pub struct LabelRef(pub(crate) LabelIdAny);

#[derive(Debug, Clone)]
/// Opaque identifier for labels that can be used to name (yet-to-create) blocks.
pub struct Label(pub(crate) LabelIdUnresolved);

#[derive(Debug, Clone)]
/// Opaque placeholder for labels. Useful for labels for blocks that cannot be created yet.
pub struct LabelPlaceholder(pub(crate) usize);

impl From<Label> for LabelRef {
    fn from(value: Label) -> Self {
        Self(value.0.into())
    }
}

impl From<GlobalName> for Label {
    fn from(value: GlobalName) -> Self {
        Self(value.0.into())
    }
}

impl From<LabelPlaceholder> for Label {
    fn from(value: LabelPlaceholder) -> Self {
        Self(value.into())
    }
}

impl From<GlobalName> for LabelRef {
    fn from(value: GlobalName) -> Self {
        Self(value.0.into())
    }
}

impl From<LabelPlaceholder> for LabelRef {
    fn from(value: LabelPlaceholder) -> Self {
        Self(value.into())
    }
}

////////////////////////////////////////////////////////////////////////////////
// Private implementation details
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Id
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) enum Id {
    GlobalName(GlobalNameId),
    LocalName(LocalNameId),
    Unnamed(UnnamedId),
}

impl From<GlobalNameId> for Id {
    fn from(value: GlobalNameId) -> Self {
        Self::GlobalName(value)
    }
}

impl From<LocalNameId> for Id {
    fn from(value: LocalNameId) -> Self {
        Self::LocalName(value)
    }
}

impl From<UnnamedId> for Id {
    fn from(value: UnnamedId) -> Self {
        Self::Unnamed(value)
    }
}

impl From<LocalId> for Id {
    fn from(value: LocalId) -> Self {
        match value {
            LocalId::LocalName(id) => Self::LocalName(id),
            LocalId::Unnamed(id) => Self::Unnamed(id),
        }
    }
}

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::GlobalName(name) => write!(f, "@{name}"),
            Self::LocalName(name) => write!(f, "%{name}"),
            Self::Unnamed(n) => write!(f, "%{n}"),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// LocalId
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) enum LocalId {
    LocalName(LocalNameId),
    Unnamed(UnnamedId),
}

impl From<LocalNameId> for LocalId {
    fn from(value: LocalNameId) -> Self {
        Self::LocalName(value)
    }
}

impl From<UnnamedId> for LocalId {
    fn from(value: UnnamedId) -> Self {
        Self::Unnamed(value)
    }
}

impl fmt::Display for LocalId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::LocalName(name) => write!(f, "%{name}"),
            Self::Unnamed(n) => write!(f, "%{n}"),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// LabelId
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) enum LabelIdAny {
    GlobalName(GlobalNameId),
    Unnamed(UnnamedId),
    Placeholder(LabelPlaceholder),
}

impl From<GlobalNameId> for LabelIdAny {
    fn from(value: GlobalNameId) -> Self {
        Self::GlobalName(value)
    }
}

impl From<UnnamedId> for LabelIdAny {
    fn from(value: UnnamedId) -> Self {
        Self::Unnamed(value)
    }
}

impl From<LabelPlaceholder> for LabelIdAny {
    fn from(value: LabelPlaceholder) -> Self {
        Self::Placeholder(value)
    }
}

impl From<LabelIdUnresolved> for LabelIdAny {
    fn from(value: LabelIdUnresolved) -> Self {
        match value {
            LabelIdUnresolved::GlobalName(id) => Self::GlobalName(id),
            LabelIdUnresolved::Placeholder(p) => Self::Placeholder(p),
        }
    }
}

impl From<LabelIdResolved> for LabelIdAny {
    fn from(value: LabelIdResolved) -> Self {
        match value {
            LabelIdResolved::GlobalName(id) => Self::GlobalName(id),
            LabelIdResolved::Unnamed(id) => Self::Unnamed(id),
        }
    }
}

impl TryFrom<LabelIdAny> for Id {
    type Error = ();

    fn try_from(value: LabelIdAny) -> Result<Self, Self::Error> {
        match value {
            LabelIdAny::GlobalName(id) => Ok(Self::GlobalName(id)),
            LabelIdAny::Unnamed(id) => Ok(Self::Unnamed(id)),
            LabelIdAny::Placeholder(_) => Err(()),
        }
    }
}

impl fmt::Display for LabelIdAny {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::GlobalName(name) => write!(f, "label @{name}"),
            Self::Unnamed(n) => write!(f, "label %{n}"),
            Self::Placeholder(_) => Err(fmt::Error),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum LabelIdUnresolved {
    GlobalName(GlobalNameId),
    Placeholder(LabelPlaceholder),
}

impl From<GlobalNameId> for LabelIdUnresolved {
    fn from(value: GlobalNameId) -> Self {
        Self::GlobalName(value)
    }
}

impl From<LabelPlaceholder> for LabelIdUnresolved {
    fn from(value: LabelPlaceholder) -> Self {
        Self::Placeholder(value)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum LabelIdResolved {
    GlobalName(GlobalNameId),
    Unnamed(UnnamedId),
}

impl From<GlobalNameId> for LabelIdResolved {
    fn from(value: GlobalNameId) -> Self {
        Self::GlobalName(value)
    }
}

impl From<UnnamedId> for LabelIdResolved {
    fn from(value: UnnamedId) -> Self {
        Self::Unnamed(value)
    }
}

////////////////////////////////////////////////////////////////////////////////
// GlobalNameId, LocalNameId, and UnnamedId
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct GlobalNameId(Rc<Name>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct LocalNameId(Rc<Name>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct UnnamedId(usize);

impl GlobalNameId {
    pub(crate) fn new(name: Name) -> Self {
        Self(Rc::new(name))
    }
}

impl LocalNameId {
    pub(crate) fn new(name: Name) -> Self {
        Self(Rc::new(name))
    }
}

impl UnnamedId {
    pub(crate) fn new(value: usize) -> Self {
        Self(value)
    }
}

impl fmt::Display for GlobalNameId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for LocalNameId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for UnnamedId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
