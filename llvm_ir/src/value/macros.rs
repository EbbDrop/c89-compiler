macro_rules! declare_value_common {
    (
        $( @FmtAsLlvmAsmFC $FmtAsLlvmAsmFC:tt; )?
        $Value:ident: $TypeCat:path
        $( => $ParentValue:ty $(=> $GrandParentValue:ident)* )?
        { $( $ChildValue:ident ),+ $(,)? }
    ) => {
        declare_type_union! {
            #[derive(Debug, Clone)]
            pub enum $Value {
                $($ChildValue,)+
            }
        }

        $($(
            impl From<$Value> for $GrandParentValue {
                fn from(value: $Value) -> Self {
                    <$ParentValue>::from(value).into()
                }
            }

            impl TryFrom<$GrandParentValue> for $Value {
                type Error = crate::value::ValueConversionError;

                fn try_from(value: $GrandParentValue) -> Result<Self, Self::Error> {
                    <$ParentValue>::try_from(value)?.try_into()
                }
            }
        )*)?

        $(
            impl TryFrom<$Value> for $ChildValue {
                type Error = crate::value::ValueConversionError;

                fn try_from(value: $Value) -> Result<Self, Self::Error> {
                    match value {
                        $Value::$ChildValue(v) => Ok(v),
                        _ => Err(crate::value::ValueConversionError::new::<$Value, Self>(value)),
                    }
                }
            }
        )+

        impl crate::value::Value for $Value {
            type Type = crate::ty::$Value;

            fn ty(&self) -> Self::Type {
                match self {
                    $(Self::$ChildValue(child) => child.ty().into(),)+
                }
            }
        }

        when! { ($($FmtAsLlvmAsmFC)?) {
            impl crate::FmtAsLlvmAsmFC for $Value {
                fn fmt_as_llvm_asm(
                    &self,
                    f: &mut std::fmt::Formatter,
                    opts: &crate::FmtOpts,
                    module: &crate::Module,
                    function: &crate::FunctionDeclaration,
                ) -> std::fmt::Result {
                    match self {
                        $(Self::$ChildValue(child) => {
                            child.fmt_as_llvm_asm(f, opts, module, function)
                        })+
                    }
                }
            }
        } else {
            impl crate::FmtAsLlvmAsmMC for $Value {
                fn fmt_as_llvm_asm(
                    &self,
                    f: &mut std::fmt::Formatter,
                    opts: &crate::FmtOpts,
                    module: &crate::Module,
                ) -> std::fmt::Result {
                    match self {
                        $(Self::$ChildValue(child) => { child.fmt_as_llvm_asm(f, opts, module) })+
                    }
                }
            }
        }}
    };
}

macro_rules! declare_value_cat {
    (
        $(@no_register $no_register:tt;)?
        $into_value:ident;
        ($Value:ident: $Type:ty, $ValueCat:ident: $TypeCat:path)
        $( => ($ParentValue:ident, $ParentValueCat:path) $(=> $GrandParentValue:ident)* )?
        { $( $ChildValue:ident ),+ $(,)? }
    ) => {
        pub trait $ValueCat: crate::value::Value + Into<$Value> $(+ $ParentValueCat)? {
            fn $into_value(self) -> $Value {
                self.into()
            }
        }
        impl<V: crate::value::Value + Into<$Value> $(+ $ParentValueCat)?> $ValueCat for V {}

        declare_value_common! {
            $(@no_regiser $no_register)?
            @FmtAsLlvmAsmFC _;
            $Value: $TypeCat $( => $ParentValue $(=> $GrandParentValue)* )? { $( $ChildValue, )+ }
        }

        impl From<crate::constant::$Value> for $Value {
            fn from(value: crate::constant::$Value) -> Self {
                match value {
                    $(crate::constant::$Value::$ChildValue(c) => Self::$ChildValue(c.into()),)+
                }
            }
        }

        impl TryFrom<$Value> for crate::constant::$Value {
            type Error = crate::value::ValueConversionError;

            fn try_from(value: $Value) -> Result<Self, Self::Error> {
                match value {
                    $($Value::$ChildValue(c) => Ok(Self::$ChildValue(c.try_into()?)),)+
                }
            }
        }

        impl<T: $TypeCat> From<crate::value::Register<T>> for $Value {
            fn from(value: crate::value::Register<T>) -> Self {
                let handle = value.handle;
                let ty = value.ty.into();
                match ty {
                    $(crate::ty::$Value::$ChildValue(ty)
                        => Self::$ChildValue(crate::value::Register { ty, handle }.into()),)+
                }
            }
        }


        $(
            impl TryFrom<$Value> for crate::value::Register<crate::ty::$ParentValue> {
                type Error = crate::value::ValueConversionError;

                fn try_from(value: $Value) -> Result<Self, Self::Error> {
                    crate::value::Register::<crate::ty::$Value>::try_from(value).map(crate::convert::Into_::into_)
                }
            }

            $(
                impl TryFrom<$Value> for crate::value::Register<crate::ty::$GrandParentValue> {
                    type Error = crate::value::ValueConversionError;

                    fn try_from(value: $Value) -> Result<Self, Self::Error> {
                        crate::value::Register::<crate::ty::$Value>::try_from(value).map(crate::convert::Into_::into_)
                    }
                }
            )*
        )?

        impl TryFrom<$Value> for crate::value::Register<crate::ty::$Value> {
            type Error = crate::value::ValueConversionError;

            fn try_from(value: $Value) -> Result<Self, Self::Error> {
                match value { $($Value::$ChildValue(c) => c.try_into(),)+ }
            }
        }

        $(
            impl TryFrom<$Value> for crate::value::Register<crate::ty::$ChildValue> {
                type Error = crate::value::ValueConversionError;

                fn try_from(value: $Value) -> Result<Self, Self::Error> {
                    match value {
                        $Value::$ChildValue(v) => v.try_into(),
                        _ => Err(crate::value::ValueConversionError::new::<$Value, Self>(value)),
                    }
                }
            }

            impl TryFrom<crate::value::Register<$Type>> for $ChildValue {
                type Error = crate::value::ValueConversionError;

                fn try_from(value: crate::value::Register<$Type>) -> Result<Self, Self::Error> {
                    use crate::convert::TryFrom_;
                    crate::value::Register::<crate::ty::$ChildValue>::try_from_(value).map(Into::into)
                }
            }
        )+
    };
}

macro_rules! declare_constant_cat {
    (
        $into_constant:ident;
        $( @no_special $no_special:tt; )?
        $( @FmtAsLlvmAsmFC $FmtAsLlvmAsmFC:tt; )?
        ($Constant:ident, ($ConstantCat:ident, $ValueCat:path): $TypeCat:path)
        $( => ($ParentConstant:ident, $ParentConstantCat:path) $(=> $GrandParentConstant:ident)* )?
        { $( $ChildConstant:ident ),+ $(,)? }
    ) => {
        pub trait $ConstantCat: $ValueCat + Into<$Constant> $(+ $ParentConstantCat)? {
            fn $into_constant(self) -> $Constant {
                self.into()
            }
        }
        impl<V: $ValueCat + Into<$Constant> $(+ $ParentConstantCat)?> $ConstantCat for V {}

        declare_value_common! {
            $( @FmtAsLlvmAsmFC $FmtAsLlvmAsmFC; )?
            $Constant: $TypeCat $( => $ParentConstant $(=> $GrandParentConstant)* )? { $( $ChildConstant, )+ }
        }

        // impl From<$Constant> for crate::value::$Constant {
        //     fn from(value: V) -> Self {
        //         match value.into() {
        //             $($Constant::$ChildConstant(child) => Self::$ChildConstant(child.into()),)+
        //         }
        //     }
        // }

        $(
            impl From<$Constant> for crate::value::$ParentConstant {
                fn from(value: $Constant) -> Self {
                    crate::value::$Constant::from(value).into()
                }
            }

            impl TryFrom<crate::value::$ParentConstant> for $Constant {
                type Error = crate::value::ValueConversionError;

                fn try_from(value: crate::value::$ParentConstant) -> Result<Self, Self::Error> {
                    crate::value::$Constant::try_from(value)?.try_into()
                }
            }

            $(
            impl From<$Constant> for crate::value::$GrandParentConstant {
                fn from(value: $Constant) -> Self {
                    crate::value::$Constant::from(value).into()
                }
            }

            impl TryFrom<crate::value::$GrandParentConstant> for $Constant {
                type Error = crate::value::ValueConversionError;

                fn try_from(value: crate::value::$GrandParentConstant) -> Result<Self, Self::Error> {
                    crate::value::$Constant::try_from(value)?.try_into()
                }
            }
        )*)?

        when! { not ($($no_special)?) {
            impl From<crate::constant::Undef<crate::ty::$Constant>> for $Constant {
                fn from(value: Undef<crate::ty::$Constant>) -> Self {
                    match value.0 {
                        $(crate::ty::$Constant::$ChildConstant(child) => {
                            Self::$ChildConstant(Undef(child).into())
                        })+
                    }
                }
            }

            impl From<crate::constant::Poison<crate::ty::$Constant>> for $Constant {
                fn from(value: Poison<crate::ty::$Constant>) -> Self {
                    match value.0 {
                        $(crate::ty::$Constant::$ChildConstant(child) => {
                            Self::$ChildConstant(Poison(child).into())
                        })+
                    }
                }
            }

            impl From<crate::constant::ZeroInitializer<crate::ty::$Constant>> for $Constant {
                fn from(value: ZeroInitializer<crate::ty::$Constant>) -> Self {
                    match value.0 {
                        $(crate::ty::$Constant::$ChildConstant(child) => {
                            Self::$ChildConstant(ZeroInitializer(child).into())
                        })+
                    }
                }
            }

            impl From<Undef<crate::ty::$Constant>> for crate::value::$Constant {
                fn from(value: Undef<crate::ty::$Constant>) -> Self {
                    $Constant::from(value).into()
                }
            }

            impl From<Poison<crate::ty::$Constant>> for crate::value::$Constant {
                fn from(value: Poison<crate::ty::$Constant>) -> Self {
                    $Constant::from(value).into()
                }
            }

            impl From<ZeroInitializer<crate::ty::$Constant>> for crate::value::$Constant {
                fn from(value: ZeroInitializer<crate::ty::$Constant>) -> Self {
                    $Constant::from(value).into()
                }
            }
        }}
    };
}

macro_rules! declare_base_value {
    (
        $into_value:ident;
        $( @FmtAsLlvmAsmFC $FmtAsLlvmAsmFC:tt; )?
        ($Value:ident: $Type:ty, $ValueCat:ident: $TypeCat:path)
        => ($ParentValue:ident, $ParentValueCat:path) $(=> $GrandParentValue:ident)*
    ) => {
        pub trait $ValueCat: crate::value::Value + Into<$Value> + $ParentValueCat {
            fn $into_value(self) -> $Value {
                self.into()
            }
        }
        impl<V: crate::value::Value + Into<$Value> + $ParentValueCat> $ValueCat for V {}

        #[derive(Debug, Clone)]
        pub enum $Value {
            Constant(crate::constant::$Value),
            Register(crate::value::Register<$Type>),
        }

        impl From<crate::constant::$Value> for $Value {
            fn from(value: crate::constant::$Value) -> Self {
                Self::Constant(value)
            }
        }

        impl<T: $TypeCat> From<crate::value::Register<T>> for $Value {
            fn from(value: crate::value::Register<T>) -> Self {
                Self::Register(Register {
                    ty: value.ty.into(),
                    handle: value.handle,
                })
            }
        }

        impl TryFrom<$Value> for crate::value::Register<$Type> {
            type Error = crate::value::ValueConversionError;

            fn try_from(value: $Value) -> Result<Self, Self::Error> {
                match value {
                    $Value::Register(register) => Ok(register),
                    _ => Err(crate::value::ValueConversionError::new::<$Value, Self>(value))
                }
            }
        }

        impl TryFrom<$Value> for crate::value::Register<crate::ty::$ParentValue> {
            type Error = crate::value::ValueConversionError;

            fn try_from(value: $Value) -> Result<Self, Self::Error> {
                crate::value::Register::<$Type>::try_from(value).map(crate::convert::Into_::into_)
            }
        }

        impl TryFrom<$Value> for crate::constant::$Value {
            type Error = crate::value::ValueConversionError;

            fn try_from(value: $Value) -> Result<Self, Self::Error> {
                match value {
                    $Value::Constant(c) => Ok(c),
                    $Value::Register(_) => Err(crate::value::ValueConversionError::new::<$Value, Self>(value)),
                }
            }
        }

        $(
            impl_from! {
                |value: $Value| -> $GrandParentValue {
                    <$ParentValue>::from(value).into()
                }
            }
            impl_try_from! {
                |value: $GrandParentValue| -> <$Value, crate::value::ValueConversionError> {
                    <$ParentValue>::try_from(value)?.try_into()
                }
            }
            impl_try_from! {
                |value: $Value| -> <crate::value::Register<crate::ty::$GrandParentValue>, crate::value::ValueConversionError> {
                    crate::value::Register::<$Type>::try_from(value).map(crate::convert::Into_::into_)
                }
            }
            impl_try_from! {
                |value: crate::value::Register<crate::ty::$GrandParentValue>| -> <$Value, crate::value::ValueConversionError> {
                    use crate::convert::TryFrom_;
                    crate::value::Register::<$Type>::try_from_(value).map(Into::into)
                }
            }
            // impl_try_from! {
            //     |value: crate::value::Register<crate::ty::$ParentValue>| -> $Value {
            //         crate::value::Register::<$Type>::try_from(value).map(crate::convert::TryInto_::try_into_)
            //     }
            // }
            impl_try_from! {
                |value: $GrandParentValue| -> <crate::value::Register<$Type>, crate::value::ValueConversionError> {
                    $Value::try_from(value).and_then(TryInto::try_into)
                }
            }
        )*

        impl crate::value::Value for $Value {
            type Type = $Type;

            fn ty(&self) -> Self::Type {
                match self {
                    Self::Constant(constant) => constant.ty(),
                    Self::Register(register) => register.ty(),
                }
            }
        }

        impl crate::FmtAsLlvmAsmFC for $Value {
            fn fmt_as_llvm_asm(
                &self,
                f: &mut std::fmt::Formatter,
                opts: &crate::FmtOpts,
                module: &crate::Module,
                function: &crate::FunctionDeclaration,
            ) -> std::fmt::Result {
                match self {
                    Self::Constant(constant) => {
                        constant.fmt_as_llvm_asm(f, opts, module, function)
                    }
                    Self::Register(register) => {
                        register.fmt_as_llvm_asm(f, opts, module, function)
                    }
                }
            }
        }
    };
}

macro_rules! declare_base_constant {
    (
        @no_special
        $into_constant:ident;
        ($Constant:ident: $ty_vis:vis $Type:ty, ($ConstantCat:ident, $ValueCat:path))
            => ($ParentConstant:ident, $ParentConstantCat:path) $(=> $GrandParentConstant:ident)*
        $({ $($fields:tt)* })?
    ) => {
        pub trait $ConstantCat: $ValueCat + Into<$Constant> + $ParentConstantCat {
            fn $into_constant(self) -> $Constant {
                self.into()
            }
        }
        impl<V: $ValueCat + Into<$Constant> + $ParentConstantCat> $ConstantCat for V {}

        #[derive(Debug, Clone)]
        pub struct $Constant {
            $ty_vis ty: $Type,
            $($($fields)*)?
        }

        $(
            impl_from! {
                |value: $Constant| -> $GrandParentConstant {
                    $ParentConstant::from(value).into()
                }
            }
            impl_try_from! {
                |value: $GrandParentConstant| -> <$Constant, crate::value::ValueConversionError> {
                    $ParentConstant::try_from(value)?.try_into()
                }
            }
        )*

        impl_from! {
            @many |value: $Constant| -> crate::value::$ParentConstant $(, crate::value::$GrandParentConstant)* {
                crate::value::$Constant::from(value).into()
            }
        }

        impl_try_from! {
            @many |value: crate::value::$ParentConstant $(, crate::value::$GrandParentConstant)*| -> <$Constant, crate::value::ValueConversionError> {
                crate::value::$Constant::try_from(value)?.try_into()
            }
        }

        impl crate::value::Value for $Constant {
            type Type = $Type;

            fn ty(&self) -> Self::Type {
                self.ty.clone()
            }
        }
    };
    (
        $into_constant:ident;
        (
            $Constant:ident($ConstantValue:ident): $Type:ty,
            ($ConstantCat:ident, $ValueCat:path): $TypeCat:path
        )
            => ($ParentConstant:ident, $ParentConstantCat:path) $(=> $GrandParentConstant:ident)*
        $({ $($rest:tt)* })?
        fn fmt_as_llvm_asm($self:ident, $f:ident, $opts:ident $(, $module:ident)?) {
            $($fmt_rest:tt)*
        }
    ) => {
        declare_base_constant! {
            @no_special
            $into_constant;
            ($Constant: $Type, ($ConstantCat, $ValueCat))
                => ($ParentConstant, $ParentConstantCat) $(=> $GrandParentConstant)*
            { value: $ConstantValue }
        }

        #[derive(Debug, Clone)]
        enum $ConstantValue {
            Undef,
            Poison,
            ZeroInitializer,
            $($($rest)*)?
        }

        when! { ($($module)?) {
            impl crate::FmtAsLlvmAsmMC for $Constant {
                    fn fmt_as_llvm_asm(
                    &$self,
                    $f: &mut std::fmt::Formatter,
                    $opts: &crate::FmtOpts,
                    $($module: &crate::Module,)?
                ) -> std::fmt::Result {
                    match &$self.value {
                        $ConstantValue::Undef => $f.write_str("undef"),
                        $ConstantValue::Poison => $f.write_str("poison"),
                        $ConstantValue::ZeroInitializer => $f.write_str("zeroinitializer"),
                        $($fmt_rest)*
                    }
                }
            }
        } else {
            impl crate::FmtAsLlvmAsm for $Constant {
                fn fmt_as_llvm_asm(
                    &$self,
                    $f: &mut std::fmt::Formatter,
                    $opts: &crate::FmtOpts,
                ) -> std::fmt::Result {
                    match &$self.value {
                        $ConstantValue::Undef => $f.write_str("undef"),
                        $ConstantValue::Poison => $f.write_str("poison"),
                        $ConstantValue::ZeroInitializer => $f.write_str("zeroinitializer"),
                        $($fmt_rest)*
                    }
                }
            }
        }}

        impl<T: $TypeCat> From<crate::constant::Undef<T>> for $Constant {
            fn from(value: crate::constant::Undef<T>) -> Self {
                Self {
                    ty: value.0.into(),
                    value: $ConstantValue::Undef
                }
            }
        }

        impl<T: $TypeCat> From<crate::constant::Poison<T>> for $Constant {
            fn from(value: crate::constant::Poison<T>) -> Self {
                Self {
                    ty: value.0.into(),
                    value: $ConstantValue::Poison
                }
            }
        }

        impl<T: $TypeCat> From<crate::constant::ZeroInitializer<T>> for $Constant {
            fn from(value: crate::constant::ZeroInitializer<T>) -> Self {
                Self {
                    ty: value.0.into(),
                    value: $ConstantValue::ZeroInitializer
                }
            }
        }

        impl<T: $TypeCat> From<crate::constant::Undef<T>> for crate::value::$Constant {
            fn from(value: crate::constant::Undef<T>) -> Self {
                $Constant::from(value).into()
            }
        }

        impl<T: $TypeCat> From<crate::constant::Poison<T>> for crate::value::$Constant {
            fn from(value: crate::constant::Poison<T>) -> Self {
                $Constant::from(value).into()
            }
        }

        impl<T: $TypeCat> From<crate::constant::ZeroInitializer<T>> for crate::value::$Constant {
            fn from(value: crate::constant::ZeroInitializer<T>) -> Self {
                $Constant::from(value).into()
            }
        }
    };
}
