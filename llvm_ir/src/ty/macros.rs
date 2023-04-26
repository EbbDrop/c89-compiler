macro_rules! declare_type_cat {
    (
        $into_ty:ident;
        $try_from_ty:ident;
        ($Type:ident, $TypeCat:ident $(: $ExtraTrait:ident $(+ $ExtraTraits:ident)* )?)
        $( => ($ParentType:ty, $ParentTypeCat:path) $(=> $GrandParentType:ty)* )?
        { $( $ChildType:ident ),+ $(,)? }
    ) => {
        pub trait $TypeCat: crate::ty::Type + Into<$Type> + std::convert::TryFrom<$Type> $(+ $ParentTypeCat)? $(+ $ExtraTrait $(+ $ExtraTraits)*)? {
            fn $into_ty(self) -> $Type {
                self.into()
            }

            fn $try_from_ty(value: $Type) -> Result<Self, <Self as std::convert::TryFrom<$Type>>::Error> {
                value.try_into()
            }
        }
        impl<T: crate::ty::Type + Into<$Type> + std::convert::TryFrom<$Type> $(+ $ParentTypeCat)? $(+ $ExtraTrait $(+ $ExtraTraits)*)?> $TypeCat for T {}

        declare_type_union! {
            #[derive(Debug, Clone)]
            pub enum $Type {
                $($ChildType,)+
            }
        }

        $(
            impl TryFrom<$Type> for $ChildType {
                type Error = crate::ty::TypeConversionError;

                fn try_from(value: $Type) -> Result<Self, Self::Error> {
                    match value {
                        $Type::$ChildType(c) => Ok(c),
                        _ => Err(crate::ty::TypeConversionError::new::<$Type, Self>(value)),
                    }
                }
            }

            impl TryFrom<$Type> for crate::ty::Identified<$ChildType> {
                type Error = crate::ty::TypeConversionError;

                fn try_from(value: $Type) -> Result<Self, Self::Error> {
                    match value {
                        $Type::$ChildType(ty) => ty.try_into(),
                        _ => Err(crate::ty::TypeConversionError::new::<$Type, Self>(value)),
                    }
                }
            }
        )+


        $(
            impl TryFrom<$Type> for crate::ty::Identified<$ParentType> {
                type Error = crate::ty::TypeConversionError;

                fn try_from(value: $Type) -> Result<Self, Self::Error> {
                    crate::ty::Identified::<$Type>::try_from(value).map(crate::convert::Into_::into_)
                }
            }

            $(
                impl From<$Type> for $GrandParentType {
                    fn from(value: $Type) -> Self {
                        <$ParentType>::from(value).into()
                    }
                }

                impl TryFrom<$Type> for crate::ty::Identified<$GrandParentType> {
                    type Error = crate::ty::TypeConversionError;

                    fn try_from(value: $Type) -> Result<Self, Self::Error> {
                        crate::ty::Identified::<$Type>::try_from(value).map(crate::convert::Into_::into_)
                    }
                }

                impl std::convert::TryFrom<$GrandParentType> for $Type {
                    type Error = crate::ty::TypeConversionError;

                    fn try_from(value: $GrandParentType) -> Result<Self, Self::Error> {
                        <$ParentType>::try_from(value)?.try_into()
                    }
                }
            )*
        )?

        impl crate::ty::Type for $Type {
            fn equiv_to(&self, other: &Self) -> bool {
                match (self, other) {
                    $((Self::$ChildType(t1), Self::$ChildType(t2)) => t1.equiv_to(t2),)+
                    _ => false,
                }
            }

            fn has_opaque_struct(&self) -> bool {
                match self { $(Self::$ChildType(t) => t.has_opaque_struct(),)+ }
            }

            fn has_scalable_vec(&self) -> bool {
                match self { $(Self::$ChildType(t) => t.has_opaque_struct(),)+ }
            }

            fn is_identified(&self) -> bool {
                match self { $(Self::$ChildType(t) => t.is_identified(),)+ }
            }
        }

        impl crate::FmtAsLlvmAsmMC for $Type {
            fn fmt_as_llvm_asm(
                &self,
                f: &mut std::fmt::Formatter,
                opts: &crate::FmtOpts,
                module: &crate::Module,
            ) -> std::fmt::Result {
                match self { $(Self::$ChildType(t) => t.fmt_as_llvm_asm(f, opts, module),)+ }
            }
        }

        impl<T: crate::ty::Type + Into<$Type>> From<crate::ty::Identified<T>> for $Type {
            fn from(value: crate::ty::Identified<T>) -> Self {
                let handle = value.handle;
                match value.ty.into() {
                    $(Self::$ChildType(ty) => {
                        Self::$ChildType(crate::ty::Identified { ty, handle }.into())
                    })+
                }
            }
        }

        impl TryFrom<$Type> for crate::ty::Identified<$Type> {
            type Error = crate::ty::TypeConversionError;

            fn try_from(value: $Type) -> Result<Self, Self::Error> {
                match value { $($Type::$ChildType(t) => t.try_into(),)+ }
            }
        }
    };
}

macro_rules! declare_base_type {
    (
        @common
        $(<{$($gen_decl:tt)*}>)?
        $into_ty:ident;
        ($Type:ident$(($LiteralType:ty))?, $TypeCat:ident $(: $ExtraTrait:ident $(+ $ExtraTraits:ident)* )? )$(<{$($gen:tt)*}>)?
        => ($ParentType:ty, $ParentTypeCat:path)
    ) => {
        pub trait $TypeCat$(<$($gen_decl)*>)?: crate::ty::Type + Into<$Type$(<$($gen)*>)?> + $ParentTypeCat $(+ $ExtraTrait $(+ $ExtraTraits)*)? {
            fn $into_ty(self) -> $Type$(<$($gen)*>)? {
                self.into()
            }
        }
        impl<$($($gen_decl)*,)? T: crate::ty::Type + Into<$Type$(<$($gen)*>)?> + $ParentTypeCat $(+ $ExtraTrait $(+ $ExtraTraits)*)?> $TypeCat$(<$($gen)*>)? for T {}

        #[derive(Debug, Clone)]
        pub enum $Type$(<$($gen_decl)*>)? {
            Literal$(($LiteralType))?,
            Identified(Box<crate::ty::Identified<Self>>),
        }

        impl<$($($gen_decl)*,)? T: $TypeCat$(<$($gen)*>)?> From<crate::ty::Identified<T>> for $Type$(<$($gen)*>)? {
            fn from(value: crate::ty::Identified<T>) -> Self {
                Self::Identified(Box::new(crate::ty::Identified {
                    ty: value.ty.into(),
                    handle: value.handle,
                }))
            }
        }

        impl<$($($gen_decl)*,)?> TryFrom<$Type$(<$($gen)*>)?> for crate::ty::Identified<$Type$(<$($gen)*>)?> {
            type Error = crate::ty::TypeConversionError;

            fn try_from(value: $Type$(<$($gen)*>)?) -> Result<Self, Self::Error> {
                match value {
                    $Type::Identified(identified) => Ok(*identified),
                    _ => Err(crate::ty::TypeConversionError::new::<$Type$(<$($gen)*>)?, Self>(value)),
                }
            }
        }

        impl<$($($gen_decl)*,)?> TryFrom<$Type$(<$($gen)*>)?> for crate::ty::Identified<$ParentType> {
            type Error = crate::ty::TypeConversionError;

            fn try_from(value: $Type$(<$($gen)*>)?) -> Result<Self, Self::Error> {
                crate::ty::Identified::<$Type$(::<$($gen)*>)?>::try_from(value).map(crate::convert::Into_::into_)
            }
        }
    };
    (
        <$gen_decl:tt>
        $into_ty:ident;
        ($Type:ident$(($LiteralType:ty))?, $TypeCat:ident $(: $ExtraTrait:ident $(+ $ExtraTraits:ident)* )? )$(<{$($gen:tt)*}>)?
        => ($ParentType:ty, $ParentTypeCat:path) $(=> $GrandParentType:ty)*
    ) => {
        declare_base_type!{
            @common <$gen_decl> $into_ty;
            ($Type$(($LiteralType))?, $TypeCat $(: $ExtraTrait $(+ $ExtraTraits)* )?)$(<{$($gen)*}>)? => ($ParentType, $ParentTypeCat)
        }
        impl_from! {
            @many <$gen_decl> |value: $Type$(<$($gen)*>)?| -> $($GrandParentType),+ {
                <$ParentType>::from(value).into()
            }
        }
        impl_try_from! {
            @many <$gen_decl> |value: $($GrandParentType),+| -> <$Type$(<$($gen)*>)?, crate::ty::TypeConversionError> {
                <$ParentType>::try_from(value)?.try_into()
            }
        }
        impl_try_from! {
            @into_many <$gen_decl> |value: $Type$(<$($gen)*>)?| -> $(<crate::ty::Identified<$GrandParentType>, crate::ty::TypeConversionError>),+ {
                crate::ty::Identified::<$Type$(::<$($gen)*>)?>::try_from(value).map(crate::convert::Into_::into_)
            }
        }
        impl_try_from! {
            @many <$gen_decl> |value: $($GrandParentType),+| -> <crate::ty::Identified<$Type$(<$($gen)*>)?>, crate::ty::TypeConversionError> {
                $Type::try_from(value).and_then(TryInto::try_into)
            }
        }
    };
    (
        $into_ty:ident;
        ($Type:ident$(($LiteralType:ty))?, $TypeCat:ident $(: $ExtraTrait:ident $(+ $ExtraTraits:ident)* )? )
        => ($ParentType:ty, $ParentTypeCat:path) $(=> $GrandParentType:ty)*
    ) => {
        declare_base_type!{
            @common $into_ty;
            ($Type$(($LiteralType))?, $TypeCat $(: $ExtraTrait $(+ $ExtraTraits)* )?) => ($ParentType, $ParentTypeCat)
        }
        $(
            impl_from! {
                |value: $Type| -> $GrandParentType {
                    <$ParentType>::from(value).into()
                }
            }
            impl_try_from! {
                |value: $GrandParentType| -> <$Type, crate::ty::TypeConversionError> {
                    <$ParentType>::try_from(value)?.try_into()
                }
            }
            impl_try_from! {
                |value: $Type| -> <crate::ty::Identified<$GrandParentType>, crate::ty::TypeConversionError> {
                    crate::ty::Identified::<$Type>::try_from(value).map(crate::convert::Into_::into_)
                }
            }
            impl_try_from! {
                |value: $GrandParentType| -> <crate::ty::Identified<$Type>, crate::ty::TypeConversionError> {
                    $Type::try_from(value).and_then(TryInto::try_into)
                }
            }
        )*
    }
}

macro_rules! impl_base_type_is_identified {
    () => {
        fn is_identified(&self) -> bool {
            match self {
                Self::Identified(_) => true,
                _ => false,
            }
        }
    };
}

macro_rules! impl_base_type_fmt {
    (|$f:ident, $opts:ident, $module:ident| $fmt_expr:expr) => {
        fn fmt_as_llvm_asm(
            &self,
            $f: &mut std::fmt::Formatter,
            $opts: &crate::FmtOpts,
            $module: &crate::Module,
        ) -> std::fmt::Result {
            match self {
                Self::Literal => $fmt_expr,
                Self::Identified(identified) => identified.fmt_as_llvm_asm($f, $opts, $module),
            }
        }
    };
    (|$literal:ident, $f:ident, $opts:ident, $module:ident| $fmt_expr:expr) => {
        fn fmt_as_llvm_asm(
            &self,
            $f: &mut std::fmt::Formatter,
            $opts: &crate::FmtOpts,
            $module: &crate::Module,
        ) -> std::fmt::Result {
            match self {
                Self::Literal($literal) => $fmt_expr,
                Self::Identified(identified) => identified.fmt_as_llvm_asm($f, $opts, $module),
            }
        }
    };
}

macro_rules! impl_base_type_getter {
    ($field:ident: $field_ty:ty = |$literal:ident| $get_expr:expr) => {
        pub fn $field(&self) -> $field_ty {
            match self {
                Self::Literal($literal) => $get_expr,
                Self::Identified(identified) => identified.ty.$field(),
            }
        }
    };
}
