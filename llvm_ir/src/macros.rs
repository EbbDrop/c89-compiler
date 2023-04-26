macro_rules! when {
    (not ($($cond:tt)+) { $($true:tt)* } $( else { $($false:tt)* } )? ) => {
        $( $($false)* )?
    };
    (not () { $($true:tt)* } $( else { $($false:tt)* } )? ) => {
        $($true)*
    };
    (($($cond:tt)+) { $($true:tt)* } $( else { $($false:tt)* } )? ) => {
        $($true)*
    };
    (() { $($_true:tt)* } $( else { $($false:tt)* } )? ) => {
        $( $($false)* )?
    };
}

macro_rules! declare_type_union {
    (
        $(#[$meta:meta])*
        $vis:vis enum $enum_name:ident { $($variant:ident),* $(,)? }
    ) => {
        $(#[$meta])*
        $vis enum $enum_name { $($variant($variant)),* }

        $(
            impl From<$variant> for $enum_name {
                fn from(value: $variant) -> Self { Self::$variant(value) }
            }
        )*
    };
}

macro_rules! impl_from {
    (@many <$gen:tt> |$value:ident: $FromType:ty| -> $($ToType:ty),+ $body:block) => {
        $( impl_from! { <$gen> |$value: $FromType| -> $ToType $body } )+
    };
    (@many |$value:ident: $FromType:ty| -> $($ToType:ty),+ $body:block) => {
        $( impl_from! { |$value: $FromType| -> $ToType $body } )+
    };
    ($(<{$($gen:tt)*}>)? |$value:ident: $FromType:ty| -> $ToType:ty $body:block) => {
        impl$(<$($gen)*>)? From<$FromType> for $ToType {
            fn from($value: $FromType) -> Self $body
        }
    };
}

macro_rules! impl_try_from {
    (@into_many <$gen:tt> |$value:ident: $FromType:ty| -> $(<$ToType:ty, $ErrType:ty>),+ $body:block) => {
        $( impl_try_from! { <$gen> |$value: $FromType| -> <$ToType, $ErrType> $body } )+
    };
    (@into_many |$value:ident: $FromType:ty| -> $(<$ToType:ty, $ErrType:ty>),+ $body:block) => {
        $( impl_try_from! { |$value: $FromType| -> <$ToType, $ErrType> $body } )+
    };
    (@many <$gen:tt> |$value:ident: $($FromType:ty),+| -> <$ToType:ty, $ErrType:ty> $body:block) => {
        $( impl_try_from! { <$gen> |$value: $FromType| -> <$ToType, $ErrType> $body } )+
    };
    (@many |$value:ident: $($FromType:ty),+| -> <$ToType:ty, $ErrType:ty> $body:block) => {
        $( impl_try_from! { |$value: $FromType| -> <$ToType, $ErrType> $body } )+
    };
    ($(<{$($gen:tt)*}>)? |$value:ident: $FromType:ty| -> <$ToType:ty, $ErrType:ty> $body:block) => {
        impl$(<$($gen)*>)? std::convert::TryFrom<$FromType> for $ToType {
            type Error = $ErrType;

            fn try_from($value: $FromType) -> Result<Self, Self::Error> $body
        }
    };
}
