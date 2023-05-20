use crate::{Function, GlobalData, Label};
use std::collections::{HashMap, HashSet};
use std::ops::Deref;

#[derive(Debug, Default)]
pub struct Root {
    /// All labels that should be made available for linking.
    exported_labels: HashSet<Label>,
    /// All global data with associated labels. Order is important.
    data: Vec<GlobalData>,
    /// Map of label to function. Order of functions is not important.
    functions: HashMap<Label, Function>,
    /// All referenced labels that are not defined in this file.
    external_labels: HashSet<Label>,
}

impl Root {
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns an iterator over all labels that are marked as global.
    pub fn exported_labels(&self) -> impl Iterator<Item = &Label> {
        self.exported_labels.iter()
    }

    /// Returns `true` if the root exports the provided label.
    pub fn exports_label(&self, label: &Label) -> bool {
        self.exported_labels.contains(label)
    }

    /// Returns a reference to all global data in order.
    pub fn data(&self) -> &[GlobalData] {
        &self.data
    }

    /// Returns `true` if at least one global data item was added to this root.
    pub fn has_any_data(&self) -> bool {
        !self.data.is_empty()
    }

    /// Returns an iterator over all functions in an undefined order.
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.functions.values()
    }

    /// Returns `true` if at least one function was added to this root.
    pub fn has_any_functions(&self) -> bool {
        !self.functions.is_empty()
    }

    /// Marks the specified label as `global`. This will make the label visible to other files when
    /// assembling. Panics if the label doesn't exist (yet) or if the label is
    /// an external label.
    pub fn export_label(&mut self, label: Label) {
        if !self.has_label(&label) {
            panic!("attempt to export nonexistent label: {label}");
        }
        if self.external_labels.contains(&label) {
            panic!("attempt to export external label: {label}");
        }
        self.exported_labels.insert(label);
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
