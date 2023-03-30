use crate::id::GlobalName;

use crate::id::Id;
use crate::ty::{self, Type};
use std::fmt;

////////////////////////////////////////////////////////////////////////////////
// TypedValue
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub struct TypedValue {
    pub(crate) ty: Type,
    pub(crate) value: Value,
}

impl TypedValue {
    pub fn ty(&self) -> Type {
        self.ty
    }
}

impl fmt::Display for TypedValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.ty, self.value)
    }
}

////////////////////////////////////////////////////////////////////////////////
// IntoTyped & TryIntoTyped
////////////////////////////////////////////////////////////////////////////////

pub trait IntoTyped {
    fn into_typed(self, ty: Type) -> TypedValue;
}

pub trait TryIntoTyped {
    type Error;

    fn try_into_typed(self, ty: Type) -> Result<TypedValue, Self::Error>;
}

////////////////////////////////////////////////////////////////////////////////
// Value
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Constant(Constant),
    Identifier(Id),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Constant(constant) => constant.fmt(f),
            Value::Identifier(identifier) => identifier.fmt(f),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// Constant
////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone)]
pub enum Constant {
    Boolean(bool),
    Integer(i128),
    FloatingPoint(f64),
    NullPointer,
    Undefined,
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Constant::Boolean(b) => f.write_str(if b { "true" } else { "false" }),
            Constant::Integer(i) => i.fmt(f),
            Constant::FloatingPoint(fp) => fp.fmt(f),
            Constant::NullPointer => f.write_str("null"),
            Constant::Undefined => f.write_str("undef"),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// Implementations of IntoTyped & TryIntoTyped
////////////////////////////////////////////////////////////////////////////////

impl<T: Into<Id>> IntoTyped for T {
    fn into_typed(self, ty: Type) -> TypedValue {
        TypedValue {
            ty,
            value: Value::Identifier(self.into()),
        }
    }
}

// TEMP: in the future the user should only be aware of TypedValue's and this can be removed
impl IntoTyped for GlobalName {
    fn into_typed(self, ty: Type) -> TypedValue {
        TypedValue {
            ty,
            value: Value::Identifier(self.0.into()),
        }
    }
}

impl TryIntoTyped for Constant {
    type Error = crate::Error;

    fn try_into_typed(self, ty: Type) -> crate::Result<TypedValue> {
        // TODO: check if constants fit in `ty`
        let can_convert = match self {
            Constant::Boolean(_) => ty == ty::I1.into(),
            Constant::Integer(_) => ty.to_type_cat() == ty::TypeCat::Integer,
            Constant::FloatingPoint(_) => ty.to_type_cat() == ty::TypeCat::FloatingPoint,
            Constant::NullPointer => ty == ty::Ptr.into(),
            Constant::Undefined => ty != ty::Void.into(),
        };
        match can_convert {
            true => Ok(TypedValue {
                ty,
                value: Value::Constant(self),
            }),
            false => Err(crate::Error),
        }
    }
}
