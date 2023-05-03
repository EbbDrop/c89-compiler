use crate::{id, Name};
use std::{cell::Cell, collections::HashSet, rc::Rc};

/// NOTE: performing operations on invalidated id's can lead to panics.
#[derive(Debug)]
pub struct IdStore<I>
where
    I: From<id::Id> + AsRef<id::Id> + AsMut<id::Id>,
{
    names: HashSet<Rc<Name>>,
    // The first element of each pair is the handle, and should always contain the index of itself.
    ids: Vec<(Rc<Cell<usize>>, I)>,
    unnamed_id_counter: usize,
}

impl<I> Default for IdStore<I>
where
    I: From<id::Id> + AsRef<id::Id> + AsMut<id::Id>,
{
    fn default() -> Self {
        Self {
            names: HashSet::new(),
            ids: Vec::new(),
            unnamed_id_counter: 0,
        }
    }
}

impl<I> Clone for IdStore<I>
where
    // NOTE: On top of the constraints specified by `IdStore`, for `Clone` to work, the ids must be
    // `Clone` as well.
    I: From<id::Id> + AsRef<id::Id> + AsMut<id::Id> + Clone,
{
    fn clone(&self) -> Self {
        Self {
            // It's fine to just clone these since `Name`s don't have interior mutability.
            names: self.names.clone(),
            // These must be deep-cloned, i.e. disconnected from the previous IdStore.
            ids: self
                .ids
                .iter()
                .map(|(handle, id)| (Rc::new(Cell::new(handle.get())), id.clone()))
                .collect(),
            unnamed_id_counter: self.unnamed_id_counter,
        }
    }
}

/// Id handles have interior mutability. This means they should not be used as keys in maps!
///
/// To prevent being used as keys, `PartialOrd` and `Hash` are not implemented.
#[derive(Debug, Clone)]
pub struct IdHandle(Rc<Cell<usize>>);

impl IdHandle {
    /// Checks whether the handle is still valid, i.e. whether it still points to an existing id.
    #[allow(unused)] // TODO: remove once in use
    pub fn is_valid(&self) -> bool {
        self.0.get() != usize::MAX
    }
}

impl PartialEq for IdHandle {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0) || (self.0.get() != usize::MAX && self.0 == other.0)
    }
}

impl Eq for IdHandle {}

impl<I> IdStore<I>
where
    I: From<id::Id> + AsRef<id::Id> + AsMut<id::Id>,
{
    pub fn new() -> Self {
        Default::default()
    }

    pub fn id(&self, handle: &IdHandle) -> &I {
        &self.ids[handle.0.get()].1
    }

    fn id_mut(&mut self, handle: &IdHandle) -> &mut I {
        &mut self.ids[handle.0.get()].1
    }

    pub fn create_unnamed_id_handle(&mut self) -> IdHandle {
        let id: id::Id = self.next_unnamed_id().into();
        self.create_handle(id.into())
    }

    pub fn create_named_id_handle(&mut self, name: Name) -> Result<IdHandle, Name> {
        self.add_named_id(name)
            .map(|name| self.create_handle(id::Id::from(name).into()))
    }

    /// returns whether the id was updated
    /// NOTE: the id may not be updated in two cases: the handle points to an named id, or the id
    /// was already the latest
    pub fn update_unnamed_id(&mut self, handle: &IdHandle) -> bool {
        let unnamed_id_counter = self.unnamed_id_counter;
        match self.id_mut(handle).as_mut() {
            id::Id::Named(_) => false,
            id::Id::Unnamed(unnamed_id) if unnamed_id.0 + 1 == unnamed_id_counter => false,
            id::Id::Unnamed(unnamed_id) => {
                let pivot = unnamed_id.0;
                unnamed_id.0 = unnamed_id_counter;
                for (_, id) in &mut self.ids {
                    match id.as_mut() {
                        id::Id::Unnamed(id::Unnamed(n)) if *n > pivot => *n -= 1,
                        _ => {}
                    }
                }
                true
            }
        }
    }

    #[allow(unused)] // TODO: remove once in use
    pub fn remove_id(&mut self, handle: &mut IdHandle) -> I {
        // First make sure the unnamed id numbering doesn't break when removing this id.
        self.update_unnamed_id(handle);
        // Now we can remove the id and invalidate its handle (for now, because it is perfectly
        // valid to later reuse the handle to point to another id, see `merge_ids` for an example).
        let old_index = handle.0.replace(usize::MAX);
        let (_, id) = self.ids.swap_remove(old_index);
        self.ids[old_index].0.set(old_index);
        id
    }

    /// Merges the ids identified by the specified handles. Prefers preserving a named id over an
    /// unnamed id. If both id's are named, the id belonging to `handle1` will be preserved.
    /// Returns the id that was discarded, but `None` if both handles pointed to the same id.
    #[allow(unused)] // TODO: remove once in use
    pub fn merge_ids(&mut self, handle1: &mut IdHandle, handle2: &mut IdHandle) -> Option<I> {
        if !handle1.is_valid() || !handle2.is_valid() || handle1.0.get() == handle2.0.get() {
            return None;
        }
        if !self.id(handle1).as_ref().is_named() && self.id(handle2).as_ref().is_named() {
            Some(self.merge_distinct_ids_preserve_first(handle2, handle1))
        } else {
            Some(self.merge_distinct_ids_preserve_first(handle1, handle2))
        }
    }

    // Assumes the handles are valid and point to distinc ids. Always preserves the id of `h1`.
    fn merge_distinct_ids_preserve_first(&mut self, h1: &mut IdHandle, h2: &mut IdHandle) -> I {
        let discarded_id = self.remove_id(h2);
        h2.0.set(h1.0.get());
        discarded_id
    }

    // /// If `handle` points to a named id, the id will be replaced by a newly created unnamed id.
    // /// Otherwise, the unnamed id is updated, and whether the id was actually changed is returned.
    // pub fn replace_named_by_unnamed_id(&mut self, handle: IdHandle) -> bool {
    //     match self.ids.get(handle.0).unwrap().as_ref() {
    //         id::Id::Named(named_id) => {
    //             self.names.remove(&named_id.0);
    //             *self.id_mut(handle).as_mut() = self.next_unnamed_id().into();
    //             true
    //         }
    //         id::Id::Unnamed(_) => self.update_unnamed_id(handle),
    //     }
    // }

    // /// The id pointed to by `handle` is replace by a new named id.
    // pub fn replace_by_named_id(&mut self, handle: IdHandle, name: Name) -> Result<(), ()> {
    //     if let id::Id::Named(named_id) = self.ids.get(handle.0).unwrap().as_ref() {
    //         self.names.remove(&named_id.0);
    //     }
    //     *self.id_mut(handle).as_mut() = self.add_named_id(name).ok_or(())?.into();
    //     Ok(())
    // }

    fn create_handle(&mut self, id: I) -> IdHandle {
        let index = self.ids.len();
        let handle = Rc::new(Cell::new(index));
        self.ids.push((Rc::clone(&handle), id));
        IdHandle(handle)
    }

    fn next_unnamed_id(&mut self) -> id::Unnamed {
        let unnamed_id = id::Unnamed(self.unnamed_id_counter);
        self.unnamed_id_counter += 1;
        unnamed_id
    }

    fn add_named_id(&mut self, name: Name) -> Result<id::Named, Name> {
        let name = Rc::new(name);
        let inserted = self.names.insert(Rc::clone(&name));
        if inserted {
            Ok(id::Named(name))
        } else {
            Err(Rc::try_unwrap(name).unwrap())
        }
    }
}
