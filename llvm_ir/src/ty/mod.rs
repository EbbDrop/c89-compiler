#[macro_use]
mod macros;

mod any;
mod identified;

pub use any::*;
pub use identified::*;

#[derive(Debug)]
pub struct TypeConversionError(String);

impl TypeConversionError {
    fn new<From: Type, To: Type>(ty: From) -> Self {
        Self(format!(
            "type conversion of {} into {} failed: error converting type: {ty:?}",
            std::any::type_name::<From>(),
            std::any::type_name::<To>()
        ))
    }
}

impl std::fmt::Display for TypeConversionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::error::Error for TypeConversionError {}

impl From<TypeConversionError> for String {
    fn from(value: TypeConversionError) -> Self {
        value.to_string()
    }
}

pub trait Type: std::fmt::Debug + Clone + crate::FmtAsLlvmAsmMC {
    fn equiv_to(&self, other: &Self) -> bool;

    fn has_opaque_struct(&self) -> bool;

    fn has_scalable_vec(&self) -> bool;

    fn is_identified(&self) -> bool;
}

pub trait BitSize {
    fn bit_size(&self) -> u32;
}
