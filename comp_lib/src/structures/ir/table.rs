use std::ops::Deref;

use crate::diagnostic::Span;

use super::ctype::CType;

/// A single item in a symbol table
#[derive(Debug, Clone)]
pub struct Item {
    /// The type
    pub ty: CType,
    /// Defined as const
    pub is_const: bool,
    /// The span where this item was defined
    pub original_span: Span,
}

/// A reference into a [`Table`]
///
/// `ItemId`s will always be valid as long as you use a `ItemId` only in the table it was made
/// from.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ItemId(usize);

/// A table from id to item, without names or scopes
#[derive(Debug, Clone)]
pub struct Table<I = Item>(Vec<I>);

impl<I> Deref for Table<I> {
    type Target = [I];

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

impl<I> Default for Table<I> {
    fn default() -> Self {
        Table(Vec::default())
    }
}

impl<I> Table<I> {
    pub fn add_item(&mut self, item: I) -> ItemId {
        let id = ItemId(self.0.len());
        self.0.push(item);
        id
    }

    /// Panics if the given [`ItemId`] is invalid. It should not be possible to create a invalid
    /// `ItemId` so this probably means you used a id from another scope.
    ///
    /// Note: It is not  guaranteed that this function will panic when using a id from a different
    /// scope, you could just be getting wrong data.
    #[track_caller]
    #[inline]
    pub fn get(&self, id: ItemId) -> &I {
        self.0.get(id.0).expect("Invalid id")
    }
}
