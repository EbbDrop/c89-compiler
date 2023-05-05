use std::fmt::Display;

use crate::ast;

/// Called a unqualified object in the standard
///
/// There is no is_const here since it is only relevant to lvalues and so it is stored there
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CType {
    // Struct(Struct),
    // Union(Union),
    Scalar(Scalar),
    Aggregate(Aggregate),
    Void,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncompatibilityReason {
    /// Used for differences like `int*` and `float*` or `int**` and `int*`
    DifferentType,
    /// use for a difference like `const int*` and `int*`
    DifferentConstness,
    DifferentLength,
}

impl CType {
    pub fn from_ast_type(type_name: &ast::UnqualifiedType) -> Self {
        use ast::UnqualifiedType as UT;
        use Arithmetic as A;
        use Scalar as S;
        match type_name {
            UT::PointerType(inner_ty) => Self::Scalar(Scalar::Pointer(Pointer {
                inner: Box::new(Self::from_ast_type(&inner_ty.unqualified.data)),
                inner_const: inner_ty.is_const.is_some(),
            })),
            UT::Void => Self::Void,
            UT::Float => Self::Scalar(S::Arithmetic(A::Float)),
            UT::Double => Self::Scalar(S::Arithmetic(A::Double)),
            UT::LongDouble => Self::Scalar(S::Arithmetic(A::LongDouble)),
            UT::Char => Self::Scalar(S::Arithmetic(A::Char)),
            UT::SignedChar => Self::Scalar(S::Arithmetic(A::SignedChar)),
            UT::SignedShortInt => Self::Scalar(S::Arithmetic(A::SignedShortInt)),
            UT::SignedInt => Self::Scalar(S::Arithmetic(A::SignedInt)),
            UT::SignedLongInt => Self::Scalar(S::Arithmetic(A::SignedLongInt)),
            UT::UnsignedChar => Self::Scalar(S::Arithmetic(A::UnsignedChar)),
            UT::UnsignedShortInt => Self::Scalar(S::Arithmetic(A::UnsignedShortInt)),
            UT::UnsignedInt => Self::Scalar(S::Arithmetic(A::UnsignedInt)),
            UT::UnsignedLongInt => Self::Scalar(S::Arithmetic(A::UnsignedLongInt)),
        }
    }

    /// 3.1.2.6
    ///
    /// If there is a different constness and different type, [`IncompatibilityReason::DifferentType`] will always be
    /// emitted
    pub fn compatible_with(&self, ty: &CType) -> Result<(), IncompatibilityReason> {
        match (self, ty) {
            (CType::Scalar(s1), CType::Scalar(s2)) => s1.compatible_with(s2),
            (CType::Aggregate(a1), CType::Aggregate(a2)) => a1.compatible_with(a2),
            _ => Err(IncompatibilityReason::DifferentType),
        }
    }
}

impl Display for CType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            CType::Scalar(s) => match s {
                Scalar::Arithmetic(ref a) => write!(f, "{a}"),
                Scalar::Pointer(Pointer {
                    inner,
                    inner_const: is_const,
                }) => {
                    let inner_is_pointer = matches!(**inner, CType::Scalar(Scalar::Pointer(_)));

                    if *is_const {
                        if inner_is_pointer {
                            write!(f, "{inner}const *")
                        } else {
                            write!(f, "const {inner} *")
                        }
                    } else {
                        write!(f, "{inner} *")
                    }
                }
            },
            CType::Aggregate(ref a) => write!(f, "{a}"),
            CType::Void => write!(f, "void"),
        }
    }
}

/// The bool indicates whether or not the type being pointed to is const
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pointer {
    pub inner: Box<CType>,
    pub inner_const: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Scalar {
    Arithmetic(Arithmetic),
    Pointer(Pointer),
}

impl Scalar {
    pub fn compatible_with(&self, other: &Scalar) -> Result<(), IncompatibilityReason> {
        match (self, other) {
            (Scalar::Arithmetic(a1), Scalar::Arithmetic(a2)) => a1.compatible_with(a2),
            (
                Scalar::Pointer(Pointer {
                    inner: inner1,
                    inner_const: is_const1,
                }),
                Scalar::Pointer(Pointer {
                    inner: inner2,
                    inner_const: is_const2,
                }),
            ) => {
                inner1.compatible_with(inner2)?;
                if is_const1 != is_const2 {
                    Err(IncompatibilityReason::DifferentType)
                } else {
                    Ok(())
                }
            }
            _ => Err(IncompatibilityReason::DifferentType),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arithmetic {
    // Floating types
    Float,
    Double,
    LongDouble,
    // Integer types
    Char,
    SignedChar,
    SignedShortInt,
    SignedInt,
    SignedLongInt,
    UnsignedChar,
    UnsignedShortInt,
    UnsignedInt,
    UnsignedLongInt,
}

#[derive(Debug, Clone, Copy)]
pub enum ConversionLossyness {
    Lossless,
    SignChange,
    Lossy,
}

impl Arithmetic {
    pub fn is_integral(&self) -> bool {
        use Arithmetic::*;
        match self {
            Float | Double | LongDouble => false,
            Char | SignedChar | SignedShortInt | SignedInt | SignedLongInt | UnsignedChar
            | UnsignedShortInt | UnsignedInt | UnsignedLongInt => true,
        }
    }

    pub fn is_floating(&self) -> bool {
        !self.is_integral()
    }

    /// Returns true if the type can represnt negative numbers.
    /// Always returns true for floating types
    pub fn is_signed(&self) -> bool {
        // TODO this should be platform specific becous of Char
        use Arithmetic::*;
        match self {
            Float | Double | LongDouble => true,
            Char => false,
            SignedChar | SignedShortInt | SignedInt | SignedLongInt => true,
            UnsignedChar | UnsignedShortInt | UnsignedInt | UnsignedLongInt => false,
        }
    }

    /// Returns the promoted type and sets the bool to true if the type had to change. Since
    /// promotions are only defined for integers types, this function will always returns false
    /// as second value for floating types.
    ///
    /// 3.2.1.1
    pub fn promote(&self) -> (Self, bool) {
        //TODO this function will prob have to change for MIPS
        use Arithmetic::*;
        match self {
            Char => (SignedInt, true),
            SignedChar => (SignedInt, true),
            SignedShortInt => (SignedInt, true),
            UnsignedChar => (SignedInt, true),
            UnsignedShortInt => (SignedInt, true),
            t => (*t, false),
        }
    }

    /// Implements the algorithm from 3.2.1.5
    pub fn usual_arithmetic_conversions(left: &Self, right: &Self) -> Self {
        use Arithmetic::*;
        for posible in &[LongDouble, Double, Float] {
            if left == posible || right == posible {
                return *posible;
            }
        }
        let left = left.promote().0;
        let right = right.promote().0;

        let either_eq = |test| left == test || right == test;

        if either_eq(UnsignedLongInt) {
            UnsignedLongInt
        } else if either_eq(SignedLongInt) && either_eq(UnsignedLongInt) {
            if SignedLongInt.size_in_bits() > UnsignedLongInt.size_in_bits() {
                SignedLongInt
            } else {
                UnsignedLongInt
            }
        } else if either_eq(SignedLongInt) {
            SignedLongInt
        } else if either_eq(UnsignedInt) {
            UnsignedInt
        } else if either_eq(SignedInt) {
            SignedInt
        } else {
            panic!("ICE: promotion made type smaller than a int")
        }
    }

    pub fn compatible_with(&self, a2: &Arithmetic) -> Result<(), IncompatibilityReason> {
        if self == a2 {
            Ok(())
        } else {
            Err(IncompatibilityReason::DifferentType)
        }
    }

    pub fn size_in_bits(&self) -> u32 {
        // TODO this should be platform specific
        match self {
            Arithmetic::Float => 32,
            Arithmetic::Double => 64,
            Arithmetic::LongDouble => 64,
            Arithmetic::Char => 8,
            Arithmetic::SignedChar => 8,
            Arithmetic::SignedShortInt => 16,
            Arithmetic::SignedInt => 32,
            Arithmetic::SignedLongInt => 64,
            Arithmetic::UnsignedChar => 8,
            Arithmetic::UnsignedShortInt => 16,
            Arithmetic::UnsignedInt => 32,
            Arithmetic::UnsignedLongInt => 64,
        }
    }

    pub fn conversion_lossynes_into(&self, to_ty: &Arithmetic) -> ConversionLossyness {
        // TODO this should be platform specific
        use {std::cmp::Ordering::*, ConversionLossyness::*};
        let from_ty = self;
        if from_ty == to_ty {
            return Lossless;
        }
        if from_ty.is_integral() && to_ty.is_floating() {
            Lossless
        } else if from_ty.is_floating() && to_ty.is_integral() {
            Lossy
        } else if from_ty.is_floating() && to_ty.is_floating() {
            match from_ty.size_in_bits().cmp(&to_ty.size_in_bits()) {
                Less => Lossy,
                Equal => Lossless,
                Greater => Lossless,
            }
        } else {
            // both are integral
            match from_ty.size_in_bits().cmp(&to_ty.size_in_bits()) {
                Less => Lossless,
                Equal => {
                    if from_ty.is_signed() == to_ty.is_signed() {
                        Lossless
                    } else {
                        SignChange
                    }
                }
                Greater => Lossy,
            }
        }
    }
}

impl Display for Arithmetic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            Arithmetic::Float => "float",
            Arithmetic::Double => "double",
            Arithmetic::LongDouble => "long double",
            Arithmetic::Char => "char",
            Arithmetic::SignedChar => "signed char",
            Arithmetic::SignedShortInt => "short int",
            Arithmetic::SignedInt => "int",
            Arithmetic::SignedLongInt => "long int",
            Arithmetic::UnsignedChar => "unsigned char",
            Arithmetic::UnsignedShortInt => "unsigned short int",
            Arithmetic::UnsignedInt => "unsigned int",
            Arithmetic::UnsignedLongInt => "unsigned long int",
        };
        write!(f, "{name}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Aggregate {
    Array(Array),
}

impl std::fmt::Display for Aggregate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Aggregate::Array(a) => write!(f, "{a}"),
        }
    }
}

impl Aggregate {
    fn compatible_with(&self, other: &Aggregate) -> Result<(), IncompatibilityReason> {
        match (self, other) {
            (Aggregate::Array(a1), Aggregate::Array(a2)) => {
                match a1.inner.compatible_with(&a2.inner) {
                    Ok(_) => match a1.length == a2.length {
                        true => Ok(()),
                        false => Err(IncompatibilityReason::DifferentLength),
                    },
                    Err(err) => Err(err),
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Array {
    pub inner: Box<CType>,
    pub length: u128,
}

impl std::fmt::Display for Array {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (most_inner, array_parts) = self.to_string_parts();
        write!(f, "{most_inner}{array_parts}")
    }
}

impl Array {
    fn to_string_parts(&self) -> (String, String) {
        let array_part = format!("[{}]", self.length);
        match &*self.inner {
            CType::Aggregate(Aggregate::Array(a)) => {
                let (inner, inner_array_part) = a.to_string_parts();
                (inner, array_part + &inner_array_part)
            }
            other => (other.to_string(), array_part),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn to_string() {
        let test = [
            (
                CType::Scalar(Scalar::Arithmetic(Arithmetic::SignedInt)),
                "int",
            ),
            (
                CType::Scalar(Scalar::Arithmetic(Arithmetic::UnsignedInt)),
                "unsigned int",
            ),
            (
                CType::Scalar(Scalar::Arithmetic(Arithmetic::SignedChar)),
                "signed char",
            ),
            (
                CType::Scalar(Scalar::Arithmetic(Arithmetic::SignedChar)),
                "signed char",
            ),
        ];

        for (ct, s) in &test {
            assert_eq!(&ct.to_string(), s);
        }

        for (ct, s) in &test {
            let ty = CType::Scalar(Scalar::Pointer(Pointer {
                inner: Box::new(ct.clone()),
                inner_const: false,
            }));
            assert_eq!(ty.to_string(), format!("{s} *"));
        }

        for (ct, s) in &test {
            let ty = CType::Scalar(Scalar::Pointer(Pointer {
                inner: Box::new(ct.clone()),
                inner_const: true,
            }));
            assert_eq!(ty.to_string(), format!("const {s} *"));
        }

        for (ct, s) in &test {
            let ty = CType::Scalar(Scalar::Pointer(Pointer {
                inner: Box::new(CType::Scalar(Scalar::Pointer(Pointer {
                    inner: Box::new(ct.clone()),
                    inner_const: false,
                }))),
                inner_const: true,
            }));
            assert_eq!(ty.to_string(), format!("{s} *const *"));
        }

        for (ct, s) in &test {
            let ty = CType::Aggregate(Aggregate::Array(Array {
                inner: Box::new(CType::Aggregate(Aggregate::Array(Array {
                    inner: Box::new(ct.clone()),
                    length: 2,
                }))),
                length: 4,
            }));
            assert_eq!(ty.to_string(), format!("{s}[4][2]"));
        }
    }

    #[test]
    fn test_usual_arithmetic_conversions() {
        use Arithmetic::*;

        let test = [
            ((Double, Float), Double),
            ((Double, LongDouble), LongDouble),
            ((Char, Char), SignedInt),
            ((SignedInt, SignedLongInt), SignedLongInt),
            ((UnsignedLongInt, UnsignedShortInt), UnsignedLongInt),
            ((UnsignedLongInt, UnsignedLongInt), UnsignedLongInt),
        ];

        for ((l, r), a) in test {
            assert_eq!(Arithmetic::usual_arithmetic_conversions(&l, &r), a);
        }
    }
}
