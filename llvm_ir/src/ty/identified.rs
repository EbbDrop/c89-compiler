use crate::module;
use crate::ty::{BitSize, Type};

#[derive(Debug, Clone)]
pub struct Identified<T: Type> {
    pub(crate) ty: T,
    pub(crate) handle: module::LocalIdHandle,
}

impl<T: Type> Type for Identified<T> {
    fn equiv_to(&self, other: &Self) -> bool {
        self.ty.equiv_to(&other.ty)
    }

    fn has_opaque_struct(&self) -> bool {
        self.ty.has_opaque_struct()
    }

    fn has_scalable_vec(&self) -> bool {
        self.ty.has_scalable_vec()
    }

    fn is_identified(&self) -> bool {
        true
    }
}

impl<T: Type> crate::FmtAsLlvmAsmMC for Identified<T> {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut std::fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
    ) -> std::fmt::Result {
        module
            .local_id(self.handle)
            .fmt_as_llvm_asm(f, opts, module)
    }
}

impl<U, T> crate::convert::From_<Identified<T>> for Identified<U>
where
    U: Type,
    T: Type + Into<U>,
{
    fn from_(value: Identified<T>) -> Self {
        Self {
            ty: value.ty.into(),
            handle: value.handle,
        }
    }
}

impl<U, T> crate::convert::TryFrom_<Identified<T>> for U
where
    U: Type + From<Identified<U>>,
    T: Type + TryInto<U>,
{
    type Error = T::Error;

    fn try_from_(value: Identified<T>) -> Result<Self, Self::Error> {
        value.ty.try_into().map(|ty| {
            Identified {
                ty,
                handle: value.handle,
            }
            .into()
        })
    }
}

impl<T: Type + BitSize> BitSize for Identified<T> {
    fn bit_size(&self) -> u32 {
        self.ty.bit_size()
    }
}
