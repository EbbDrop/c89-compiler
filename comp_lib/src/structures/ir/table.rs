use crate::diagnostic::Span;
use crate::ir::ctype::CType;
use std::ops::Deref;

/// A single item in a symbol table
#[derive(Debug, Clone)]
pub struct VariableItem {
    /// The span where this item was defined
    pub original_span: Span,
    /// The type
    pub ty: CType,
    /// Defined as const
    pub is_const: bool,
    pub initialized: bool,
}

#[derive(Debug, Clone)]
pub struct FunctionItem {
    /// Original span of the first declaration or definition of this function.
    pub original_span: Span,
    pub return_type: CType,
    pub param_types: Vec<CType>,
    pub is_vararg: bool,
    /// `true` if this function is defined somewhere in the translation unit.
    pub is_defined: bool,
}

/// A reference into a [`Table`]
///
/// `ItemId`s will always be valid as long as you use a `ItemId` only in the table it was made
/// from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ItemId(usize);

/// A table from id to item, without names or scopes
#[derive(Debug, Clone)]
pub struct Table<I>(Vec<I>);

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

    /// Same rules as [`get`] but gives a `&mut I`
    #[track_caller]
    #[inline]
    pub fn get_mut(&mut self, id: ItemId) -> &mut I {
        self.0.get_mut(id.0).expect("Invalid id")
    }

    /// An iterator visiting elements of type `(ItemId, &I)` where the second element is the item
    /// with the first element as id. The order is unspecified.
    pub fn iter(&self) -> impl Iterator<Item = (ItemId, &I)> {
        self.0.iter().enumerate().map(|(i, item)| (ItemId(i), item))
    }

    /// An iterator visiting all item id's in an unspecified order.
    pub fn item_ids(&self) -> impl Iterator<Item = ItemId> {
        (0..self.0.len()).map(ItemId)
    }

    /// An iterator visiting all item's in an unspecified order.
    ///
    /// Equivalent to `self.deref().iter()`.
    pub fn items(&self) -> impl Iterator<Item = &I> {
        self.0.iter()
    }
}

// /// A table from id to item, without names or scopes
// #[derive(Debug, Clone)]
// pub struct GlobalTable<I>(HashMap<String, I>);

// impl<I> Default for GlobalTable<I> {
//     fn default() -> Self {
//         Self(HashMap::default())
//     }
// }

// impl<I> GlobalTable<I> {
//     /// Adds `item` to this table with `ident` as its identifier. If the table already contains an
//     /// item with this identifier, the existing item will be returned in an `Err(_)`, and the passed
//     /// item will be dropped. Otherwise, a reference to the ident is return in an `Ok(_)`.
//     pub fn add_item(&mut self, ident: String, item: I) -> Result<&str, &I> {
//         match self.0.get(&ident) {
//             Some(existing) => Err(existing),
//             None => {
//                 self.0.insert(ident, item);
//                 Ok(&ident)
//             }
//         }
//     }

//     /// Same as [`add_item`], except that a mutable reference is returned to the existing item.
//     pub fn add_item_mut(&mut self, ident: String, item: I) -> Result<&str, &mut I> {
//         match self.0.get_mut(&ident) {
//             Some(existing) => Err(existing),
//             None => {
//                 self.0.insert(ident, item);
//                 Ok(&ident)
//             }
//         }
//     }

//     /// Returns `true` if the specified ident is part of this table.
//     pub fn has(&self, ident: &str) -> bool {
//         self.0.contains_key(ident)
//     }

//     /// Returns a reference to the item correspondig to the ident, if such an item exists in this
//     /// table.
//     pub fn get(&self, ident: &str) -> Option<&I> {
//         self.0.get(ident)
//     }

//     /// Returns a mutable reference to the item correspondig to the ident, if such an item exists in
//     /// this table.
//     pub fn get_mut(&mut self, ident: &str) -> Option<&mut I> {
//         self.0.get_mut(ident)
//     }

//     /// An iterator visiting elements of type `(&str, &I)` where the second element is the item
//     /// with the first element as ident. The order is unspecified.
//     pub fn iter(&self) -> impl Iterator<Item = (&str, &I)> {
//         self.0.iter().map(|(k, v)| (k.as_str(), v))
//     }

//     /// An iterator visiting all item identifier names in an unspecified order.
//     pub fn idents(&self) -> impl Iterator<Item = &str> {
//         self.0.keys().map(AsRef::as_ref)
//     }

//     /// An iterator visiting all item's in an unspecified order.
//     pub fn items(&self) -> impl Iterator<Item = &I> {
//         self.0.values()
//     }
// }
