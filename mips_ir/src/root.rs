use crate::{Function, GlobalData, Label};
use std::collections::{HashMap, HashSet};

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
    raw_text: Vec<String>,
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

    /// Returns `true` if the label was created using [`create_external_label`].
    pub fn is_external(&self, label: &Label) -> bool {
        self.external_labels.contains(label)
    }

    /// Returns `true` if the label refers to global data, a function, or is declared as external.
    pub fn has_label(&self, label: &Label) -> bool {
        self.external_labels.contains(label)
            || self.functions.contains_key(label)
            || self.data.iter().any(|d| d.label() == label)
    }

    /// Returns a reference to all global data in order.
    pub fn data(&self) -> &[GlobalData] {
        &self.data
    }

    /// Returns a mutable reference to all global data in order.
    // This is safe, since the label of global data items cannot be changed.
    pub fn data_mut(&mut self) -> &mut [GlobalData] {
        &mut self.data
    }

    /// Returns `true` if at least one global data item was added to this root.
    pub fn has_any_data(&self) -> bool {
        !self.data.is_empty()
    }

    /// Returns an iterator over all labels that define functions in an undefined order. Note that
    /// this order isn't necessary equal to the order of [`functions`] or [`functions_mut`].
    ///
    /// Semantically equivalent to `self.functions().map(|f| f.label())`.
    pub fn function_labels(&self) -> impl Iterator<Item = &Label> {
        self.functions.keys()
    }

    /// Returns an iterator over all functions in an undefined order.
    pub fn functions(&self) -> impl Iterator<Item = &Function> {
        self.functions.values()
    }

    /// Returns an iterator over all functions in an undefined order.
    // This is safe, since functions do not allow changing their id.
    pub fn functions_mut(&mut self) -> impl Iterator<Item = &mut Function> {
        self.functions.values_mut()
    }

    /// Returns `true` if at least one function was added to this root.
    pub fn has_any_functions(&self) -> bool {
        !self.functions.is_empty()
    }

    /// Returns the function associated with the specified label, if such function exists.
    pub fn function(&self, label: &Label) -> Option<&Function> {
        self.functions.get(label)
    }

    /// Returns the function associated with the specified label, if such function exists.
    // This is safe, since functions do not allow changing their id.
    pub fn function_mut(&mut self, label: &Label) -> Option<&mut Function> {
        self.functions.get_mut(label)
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

    /// Removes global data. Returns the removed global data if the label actually referred to
    /// existing data.
    pub fn remove_data(&mut self, label: &Label) -> Option<GlobalData> {
        if !self.has_label(label) {
            panic!("label doesn't exist: {label}");
        }
        self.data
            .iter()
            .position(|d| d.label() == label)
            .map(|i| self.data.remove(i))
    }

    /// Adds a function. Panics if the function's label is already used.
    pub fn add_function(&mut self, function: Function) {
        let label = function.label();
        if self.has_label(label) {
            panic!("label already exists: {label}");
        }
        self.functions.insert(label.clone(), function);
    }

    /// Removes a function. Returns the removed function if the label actually referred to an
    /// existing function.
    pub fn remove_function(&mut self, label: &Label) -> Option<Function> {
        if !self.has_label(label) {
            panic!("label doesn't exist: {label}");
        }
        self.functions.remove(label)
    }

    pub fn raw_text(&self) -> &[String] {
        self.raw_text.as_slice()
    }

    pub fn add_raw_text(&mut self, raw_text: String) {
        self.raw_text.push(raw_text);
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

    pub fn remove_external_label(&mut self, label: &Label) {
        if !self.external_labels.remove(label) {
            panic!("label didn't exist: {label}");
        }
    }
}
