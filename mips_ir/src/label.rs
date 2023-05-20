use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Label(Rc<str>);

impl AsRef<str> for Label {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

impl From<&str> for Label {
    fn from(value: &str) -> Self {
        Self(Rc::from(value))
    }
}

impl From<String> for Label {
    fn from(value: String) -> Self {
        Self(Rc::from(value))
    }
}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
