use crate::ir::table::{Item, ItemId, Table};

/// A table of items with a associated names in a scope.
///
/// One table should be made per function. Use [`into_table`] after your done to forget the names
/// and keep a table that links id's to the Items. Use [`get_scoped_handle`] to create a object you
/// can use to add items to this table.
///
/// [`into_table`]: ScopedTable::into_table
/// [`get_scoped_handle`]: ScopedTable::get_scoped_handle
#[derive(Debug)]
pub struct ScopedTable<I = Item> {
    table: Table<I>,
    vars: Vec<(String, ItemId)>,
}

impl<I> Default for ScopedTable<I> {
    fn default() -> Self {
        Self {
            table: Table::default(),
            vars: Vec::default(),
        }
    }
}

impl<I> ScopedTable<I> {
    /// Turns into a table forgetting naming and scope details and only having the items linked to
    /// there [`ItemId`]s.
    pub fn into_table(self) -> Table<I> {
        self.table
    }

    /// Crate a [`ScopedHandle`] for this table.
    ///
    /// It will not be possible to use this [`ScopedTable`] as long as the given [`ScopedHandle`]
    /// lives. This is statically checked by the compiler. So if you get lifetime errors trying to
    /// use other methods, you probably forgot to drop the [`ScopedHandle`] first.
    pub fn get_scoped_handle<'a, 'b>(&'a mut self) -> ScopedHandle<'b, I>
    where
        'a: 'b,
    {
        let start = self.vars.len();
        ScopedHandle {
            root_table: self,
            start,
        }
    }
}

/// Every instances of this object signifies a single scope.
///
/// Adding items to this scope will only be accessible from this scope and any scopes created from
/// this scope. Reading items will look first in this scope and then go further out to bigger
/// scopes to find the item.
#[derive(Debug)]
pub struct ScopedHandle<'a, I = Item> {
    root_table: &'a mut ScopedTable<I>,
    start: usize,
}

impl<'a, I> ScopedHandle<'a, I> {
    /// Get a reference to the root table associated with this scope.
    pub fn root_table(&self) -> &Table<I> {
        &self.root_table.table
    }

    /// Create a new scope inside this scope.
    ///
    /// You will not be able to use this scope as long as the inner scope lives.
    pub fn new_scope(&mut self) -> ScopedHandle<'_, I> {
        let start = self.root_table.vars.len();
        ScopedHandle {
            root_table: self.root_table,
            start,
        }
    }

    /// Adds a new item to this scope, returning its root table id in a [`Ok`]. If there is already a item with
    /// this name in this scope, [`Err`] will be returned with its [`ItemId`].
    pub fn declare(&mut self, name: String, item: I) -> Result<ItemId, ItemId> {
        if let Some(id) = self.root_table.vars[self.start..]
            .iter()
            .find(|(n, _)| n == &name)
        {
            return Err(id.1);
        }
        let id = self.root_table.table.add_item(item);
        self.root_table.vars.push((name, id));
        Ok(id)
    }

    /// Searches for a name in this scope or any outer scopes. A [`Some`] will be returned with the
    /// [`ItemId`] in the root scope of the item, for convenience a reference to the item is also
    /// directly returned. If the name is not found in any scope a [`None`] is returned.
    pub fn _reference(&self, name: &str) -> Option<(ItemId, &I)> {
        self.root_table
            .vars
            .iter()
            .rev()
            .find(|(n, _)| n == name)
            .map(|(_, id)| {
                let item = self.root_table.table.get(*id);
                (*id, item)
            })
    }

    /// Same rules as [`reference`] but gived a `&mut I`
    pub fn reference_mut(&mut self, name: &str) -> Option<(ItemId, &mut I)> {
        self.root_table
            .vars
            .iter()
            .rev()
            .find(|(n, _)| n == name)
            .map(|(_, id)| {
                let item = self.root_table.table.get_mut(*id);
                (*id, item)
            })
    }
}

impl<'a, I> Drop for ScopedHandle<'a, I> {
    fn drop(&mut self) {
        self.root_table.vars.truncate(self.start);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_scope() {
        let mut table: ScopedTable<char> = ScopedTable::default();
        assert_eq!(table.table.len(), 0);

        let mut root_scope = table.get_scoped_handle();

        let id_a = root_scope.declare("A".to_owned(), 'a').unwrap();

        let id_b = root_scope.declare("B".to_owned(), 'b').unwrap();

        std::mem::drop(root_scope);

        assert_eq!(table.table.len(), 2);
        assert_eq!(table.table.get(id_a), &'a');
        assert_eq!(table.table.get(id_b), &'b');
    }

    #[test]
    fn two_scopes() {
        let mut table: ScopedTable<char> = ScopedTable::default();
        assert_eq!(table.table.len(), 0);

        let id_a;
        let id_b;

        {
            let mut root_scope = table.get_scoped_handle();

            id_a = root_scope.declare("A".to_owned(), 'a').unwrap();

            {
                let mut inner_scope = root_scope.new_scope();

                id_b = inner_scope.declare("B".to_owned(), 'b').unwrap();
                assert_eq!(inner_scope._reference("B"), Some((id_b, &'b')));
                assert_eq!(inner_scope._reference("A"), Some((id_a, &'a')));
            }

            assert!(root_scope._reference("B").is_none());
            assert_eq!(root_scope._reference("A"), Some((id_a, &'a')));
        }

        assert_eq!(table.table.len(), 2);
        assert_eq!(table.table.get(id_a), &'a');
        assert_eq!(table.table.get(id_b), &'b');
    }

    #[test]
    fn same_name_in_diff_scopes() {
        let mut table: ScopedTable<char> = ScopedTable::default();
        assert_eq!(table.table.len(), 0);

        let id_a;
        let id_b1;
        let id_b2;
        {
            let mut root_scope = table.get_scoped_handle();

            id_a = root_scope.declare("A".to_owned(), 'a').unwrap();

            id_b1 = {
                let mut inner_scope = root_scope.new_scope();

                inner_scope.declare("B".to_owned(), 'b').unwrap()
            };
            id_b2 = {
                let mut inner_scope = root_scope.new_scope();

                inner_scope.declare("B".to_owned(), 'b').unwrap()
            };
            assert_ne!(id_b1, id_b2);

            assert!(root_scope._reference("B").is_none());
        }

        assert_eq!(table.table.len(), 3);
        assert_eq!(table.table.get(id_a), &'a');
        assert_eq!(table.table.get(id_b1), &'b');
        assert_eq!(table.table.get(id_b2), &'b');
    }

    #[test]
    fn shadow_in_new_scope() {
        let mut table: ScopedTable<char> = ScopedTable::default();
        assert_eq!(table.table.len(), 0);

        let id_outer;
        let id_inner;

        {
            let mut root_scope = table.get_scoped_handle();

            id_outer = root_scope.declare("A".to_owned(), 'o').unwrap();

            {
                let mut inner_scope = root_scope.new_scope();

                id_inner = inner_scope.declare("A".to_owned(), 'i').unwrap();
                assert_eq!(inner_scope._reference("A"), Some((id_inner, &'i')));
            }

            assert_eq!(root_scope._reference("A"), Some((id_outer, &'o')));
        }

        assert_eq!(table.table.len(), 2);
        assert_eq!(table.table.get(id_outer), &'o');
        assert_eq!(table.table.get(id_inner), &'i');
    }
}
