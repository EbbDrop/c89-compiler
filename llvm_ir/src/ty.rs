macro_rules! define_types {
    ($vis:vis enum $enum_name:ident |$self:ident, $f:ident| {$($ty:ident $(($($def_vis:vis $def_ty:ty),*))? => $fmt:stmt),+}) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        $vis enum $enum_name {
            $($ty($ty)),+
        }

        impl std::fmt::Display for $enum_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $($enum_name::$ty(inner) => write!(f, "{inner}")),+
                }
            }
        }

        $(
            #[derive(Debug, Clone, Copy, PartialEq, Eq)]
            $vis struct $ty $(($($def_vis $def_ty),*))?;

            impl std::fmt::Display for $ty {
                fn fmt(&$self, $f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    $fmt
                }
            }

            impl From<$ty> for $enum_name {
                fn from(value: $ty) -> Self {
                    Self::$ty(value)
                }
            }
        )+
    };
}

define_types!(
    pub enum Type |self, f| {
        Void => f.write_str("void"),
        Int(pub u32) => write!(f, "i{}", self.0),
        Float => f.write_str("float"),
        Double => f.write_str("double"),
        Ptr => f.write_str("ptr")
    }
);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeCat {
    Void,
    Integer,
    FloatingPoint,
    Ptr,
}

impl Type {
    pub fn to_type_cat(self) -> TypeCat {
        match self {
            Type::Void(_) => TypeCat::Void,
            Type::Int(_) => TypeCat::Integer,
            Type::Float(_) | Type::Double(_) => TypeCat::FloatingPoint,
            Type::Ptr(_) => TypeCat::Ptr,
        }
    }

    pub fn bit_size(self) -> BitSize {
        match self {
            Type::Void(_) => BitSize::Exact(0),
            Type::Int(Int(n)) => BitSize::Exact(n),
            Type::Float(_) => BitSize::Exact(32),
            Type::Double(_) => BitSize::Exact(64),
            Type::Ptr(_) => todo!(),
        }
    }
}

impl Int {
    pub fn bits(&self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BitSize {
    Exact(u32),
    Ptr,
}

impl BitSize {
    pub fn exact(self) -> Option<u32> {
        match self {
            BitSize::Exact(n) => Some(n),
            BitSize::Ptr => None,
        }
    }

    pub fn to_bytes_exact(self) -> Option<u32> {
        match self {
            BitSize::Exact(n) => n.checked_rem(8).and_then(|r| (r == 0).then_some(n >> 3)),
            BitSize::Ptr => None,
        }
    }
}

// pub trait AType: Into<Type> + std::fmt::Display + std::fmt::Debug + Clone {}

// pub trait Integer: AType {}
// pub trait FloatingPoint: AType {}

// impl Integer for Int {}
// impl FloatingPoint for Float {}
// impl FloatingPoint for Double {}

pub const I1: Int = Int(1);
pub const I8: Int = Int(8);
pub const I16: Int = Int(16);
pub const I32: Int = Int(32);
pub const I64: Int = Int(64);
