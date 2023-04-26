use crate::constant::{Element, FirstClass, Primitive, PrimitiveConstant, Single};
use crate::value::PointerValue;
use crate::{module::GlobalIdHandle, ty, AddressSpace};

declare_base_constant! {
    into_pointer_constant;

    (Pointer(ConstantPointer): ty::Pointer, (PointerConstant, PointerValue): ty::PointerType)
        => (Primitive, PrimitiveConstant) => Single => Element => FirstClass
    {
        Null,
        GlobalAddress(GlobalIdHandle)
    }

    fn fmt_as_llvm_asm(self, f, opts, module) {
        ConstantPointer::Null => f.write_str("null"),
        ConstantPointer::GlobalAddress(handle) => {
            module.global_id(*handle).fmt_as_llvm_asm(f, opts, module)
        }
    }
}

impl Pointer {
    pub const NULL: Self = Self {
        ty: ty::Pointer::new_literal()
            .with_address_space(AddressSpace(0))
            .build(),
        value: ConstantPointer::Null,
    };

    pub(crate) fn from_global_address(ty: ty::Pointer, handle: GlobalIdHandle) -> Self {
        Self {
            ty,
            value: ConstantPointer::GlobalAddress(handle),
        }
    }
}
