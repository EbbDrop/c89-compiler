use crate::{id, Name};
use std::{collections::HashSet, rc::Rc};

#[derive(Debug)]
pub struct IdStore<I>
where
    I: From<id::Id> + AsRef<id::Id> + AsMut<id::Id>,
{
    names: HashSet<Rc<Name>>,
    ids: Vec<I>,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IdHandle(usize);

impl<I> IdStore<I>
where
    I: From<id::Id> + AsRef<id::Id> + AsMut<id::Id>,
{
    pub fn new() -> Self {
        Default::default()
    }

    pub fn id(&self, handle: IdHandle) -> &I {
        self.ids.get(handle.0).unwrap()
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
    pub fn update_unnamed_id(&mut self, handle: IdHandle) -> bool {
        match self.ids.get_mut(handle.0).unwrap().as_mut() {
            id::Id::Named(_) => false,
            id::Id::Unnamed(unnamed_id) if unnamed_id.0 + 1 == self.unnamed_id_counter => false,
            id::Id::Unnamed(unnamed_id) => {
                let pivot = unnamed_id.0;
                unnamed_id.0 = self.unnamed_id_counter;
                for id in &mut self.ids {
                    match id.as_mut() {
                        id::Id::Unnamed(id::Unnamed(n)) if *n > pivot => *n -= 1,
                        _ => {}
                    }
                }
                true
            }
        }
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
        self.ids.push(id);
        IdHandle(index)
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
