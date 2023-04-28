use crate::{function, ty, value};

use super::ValueConversionError;

#[derive(Debug, Clone)]
pub struct Register<T: ty::FirstClassType> {
    pub(crate) ty: T,
    pub(crate) handle: function::LocalIdHandle,
}

impl<T: ty::FirstClassType> Register<T> {
    pub(crate) fn is_named(&self, function: &crate::FunctionDeclaration) -> bool {
        function.id(&self.handle).as_ref().is_named()
    }
}

impl<T: ty::FirstClassType> value::Value for Register<T> {
    type Type = T;

    fn ty(&self) -> Self::Type {
        self.ty.clone()
    }
}

impl<T: ty::FirstClassType> crate::FmtAsLlvmAsmFC for Register<T> {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut std::fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> std::fmt::Result {
        function
            .id(&self.handle)
            .fmt_as_llvm_asm(f, opts, module, function)
    }
}

impl<U, T> crate::convert::From_<Register<T>> for Register<U>
where
    U: ty::FirstClassType,
    T: ty::FirstClassType + Into<U>,
{
    fn from_(value: Register<T>) -> Self {
        Self {
            ty: value.ty.into(),
            handle: value.handle,
        }
    }
}

impl<U, T> crate::convert::TryFrom_<Register<T>> for Register<U>
where
    U: ty::FirstClassType,
    T: ty::FirstClassType + TryInto<U, Error = ty::TypeConversionError>,
{
    type Error = ValueConversionError;

    fn try_from_(value: Register<T>) -> Result<Self, Self::Error> {
        value
            .ty
            .clone()
            .try_into()
            .map(|ty| Register {
                ty,
                handle: value.handle.clone(),
            })
            .map_err(|err| ValueConversionError::wrap::<Register<T>, Self>(value, err))
    }
}

// impl<U, T> crate::convert::TryFrom_<Register<T>> for U
// where
//     U: value::Value + From<Register<U::Type>>,
//     T: ty::FirstClassType + TryInto<U::Type>,
// {
//     type Error = <T as TryInto<U::Type>>::Error;

//     fn try_from_(value: Register<T>) -> Result<Self, Self::Error> {
//         value.ty.try_into().map(|ty| {
//             Register {
//                 ty,
//                 handle: value.handle,
//             }
//             .into()
//         })
//     }
// }
