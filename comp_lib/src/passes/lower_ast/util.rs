use crate::ir::{
    ctype::{self, CType},
    expr::{Expr, ExprNode},
    table::Item,
};

use super::symbol_table::ScopedHandle;

#[derive(Debug)]
pub struct Scope<'a> {
    pub vars: ScopedHandle<'a, Item>,
    pub func_return_type: &'a CType,
    pub in_switch: bool,
    pub in_loop: bool,
}

impl<'s> Scope<'s> {
    pub fn new_scope(&mut self) -> Scope<'_> {
        Scope {
            vars: self.vars.new_scope(),
            func_return_type: self.func_return_type,
            in_switch: self.in_switch,
            in_loop: self.in_loop,
        }
    }

    pub fn in_switch(self) -> Self {
        Self {
            in_switch: true,
            ..self
        }
    }

    pub fn in_loop(self) -> Self {
        Self {
            in_loop: true,
            ..self
        }
    }
}

/// Only inserts a cast if `to_ty != inner.ty`. This function does *not* check if the cast is
/// allowed.
pub fn maybe_cast(inner: ExprNode, to_ty: CType) -> ExprNode {
    if to_ty == inner.ty {
        return inner;
    }

    ExprNode {
        span: inner.span,
        ty: to_ty,
        expr: Expr::Cast(Box::new(inner)),
    }
}

/// Returns the first [`ctype::Arithmetic`] that is able to hold the value without any lossyness
pub fn find_first_fit(value: i128, opts: &[ctype::Arithmetic]) -> Option<ctype::Arithmetic> {
    let is_neg = value < 0;
    let needed_bits = if is_neg {
        128 - value.leading_ones()
    } else {
        128 - value.leading_zeros()
    };
    for opt in opts {
        let bits = opt.size_in_bits() - if opt.is_signed() || is_neg { 1 } else { 0 };
        if bits >= needed_bits {
            return Some(*opt);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn best_fit() {
        use ctype::Arithmetic::*;
        let order = [SignedInt, SignedLongInt, UnsignedLongInt];

        assert_eq!(find_first_fit(0, &order), Some(SignedInt));
        assert_eq!(
            find_first_fit(9223372036854775807, &order),
            Some(SignedLongInt)
        );
        assert_eq!(
            find_first_fit(9223372036854775808, &order),
            Some(UnsignedLongInt)
        );
        assert_eq!(
            find_first_fit(-9223372036854775808, &order),
            Some(SignedLongInt)
        );
        assert_eq!(find_first_fit(-9223372036854775809, &order), None);
    }
}
