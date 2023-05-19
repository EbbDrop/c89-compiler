mod function;
mod global_data;
mod instruction;
mod reg;

pub use function::*;
pub use global_data::*;
pub use instruction::*;
pub use reg::*;

use std::collections::{HashMap, HashSet};
use std::ops::Deref;
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

#[derive(Debug, Default)]
pub struct Root {
    /// All labels that should be made available for linking.
    globals: HashSet<Label>,
    /// All global data with associated labels. Order is important.
    data: Vec<GlobalData>,
    /// Map of label to function. Order of functions is not important.
    functions: HashMap<Label, Function>,
    external_labels: HashSet<Label>,
}

impl Root {
    pub fn new() -> Self {
        Self::default()
    }

    /// Makes the specified label global. This will make the label visible to other files when
    /// assembling. Panics if the label doesn't exist (yet) or if the label is an external label.
    pub fn make_global(&mut self, label: Label) {
        if !self.has_label(&label) {
            panic!("declaring nonexistent label as global: {label}");
        }
        if self.external_labels.contains(&label) {
            panic!("declaring external label as global: {label}");
        }
        self.globals.insert(label);
    }

    /// Adds global data. Panics if the function's label is already used.
    pub fn add_data(&mut self, data: GlobalData) {
        let label = data.label();
        if self.has_label(label) {
            panic!("label already exists: {label}");
        }
        self.data.push(data);
    }

    /// Adds a function. Panics if the function's label is already used.
    pub fn add_function(&mut self, function: Function) {
        let label = function.label();
        if self.has_label(label) {
            panic!("label already exists: {label}");
        }
        self.functions.insert(label.deref().clone(), function);
    }

    /// Creates the label as external (i.e. will be defined in another file). Panics if the label is
    /// already used.
    pub fn create_external_label(&mut self, name: &str) -> Label {
        let label = Label::from(name);
        if self.has_label(&label) {
            panic!("label already exists: {label}");
        }
        self.external_labels.insert(label.clone());
        label
    }

    fn has_label(&self, label: &Label) -> bool {
        self.external_labels.contains(label)
            || self.functions.contains_key(label)
            || self.data.iter().any(|d| d.label() == label)
    }
}

impl std::fmt::Display for Root {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for global in &self.globals {
            writeln!(f, "\t.globl {global}")?;
        }

        if !self.data.is_empty() {
            f.write_str("\t.data\n")?;
        }
        for data in &self.data {
            write!(f, "\n{data}")?;
        }

        if !self.functions.is_empty() {
            f.write_str("\t.text\n")?;
        }
        for function in self.functions.values() {
            write!(f, "\n{function}")?;
        }

        Ok(())
    }
}
