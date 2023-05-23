use crate::ir::ctype::{self, CType};
use mips_ir as mir;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CTypeProps {
    /// Exponent `n` indicating the ctype requires to be aligned at a `2.pow(n)`-byte boundary.
    pub align: mir::AlignBoundary,
    /// Number of bytes required to store a value of the ctype.
    pub size: u128,
    pub signed: bool,
}

impl From<CTypeProps> for mir::StackInfo {
    fn from(value: CTypeProps) -> Self {
        mir::StackInfo {
            size: value.size,
            aligement: value.align,
        }
    }
}

pub fn ctype_props(ctype: &CType) -> CTypeProps {
    match ctype {
        CType::Scalar(scalar) => match scalar {
            ctype::Scalar::Arithmetic(arithmetic) => {
                let signed = arithmetic.is_signed();
                match arithmetic {
                    ctype::Arithmetic::Char
                    | ctype::Arithmetic::SignedChar
                    | ctype::Arithmetic::UnsignedChar => CTypeProps {
                        align: mir::AlignBoundary::BYTE,
                        size: mir::size::BYTE as u128,
                        signed,
                    },
                    ctype::Arithmetic::SignedShortInt | ctype::Arithmetic::UnsignedShortInt => {
                        CTypeProps {
                            align: mir::AlignBoundary::HALF,
                            size: mir::size::HALF as u128,
                            signed,
                        }
                    }
                    ctype::Arithmetic::SignedInt
                    | ctype::Arithmetic::UnsignedInt
                    | ctype::Arithmetic::SignedLongInt
                    | ctype::Arithmetic::UnsignedLongInt
                    | ctype::Arithmetic::Float => CTypeProps {
                        align: mir::AlignBoundary::WORD,
                        size: mir::size::WORD as u128,
                        signed,
                    },
                    ctype::Arithmetic::Double | ctype::Arithmetic::LongDouble => CTypeProps {
                        align: mir::AlignBoundary::DOUBLE,
                        size: mir::size::DOUBLE as u128,
                        signed,
                    },
                }
            }
            ctype::Scalar::Pointer(_) => CTypeProps {
                align: mir::AlignBoundary::WORD,
                size: mir::size::WORD as u128,
                signed: false,
            },
        },
        CType::Aggregate(aggregate) => match aggregate {
            &ctype::Aggregate::Array(ctype::Array { ref inner, length }) => {
                let CTypeProps { align, size, .. } = ctype_props(inner);
                assert!(
                    (size % align.0 as u128) == 0,
                    "Arrays where the size of the items is not a multiple of the allignment are not supported",
                );
                CTypeProps {
                    align,
                    size: size * length,
                    signed: false,
                }
            }
        },
        CType::Void => CTypeProps {
            align: mir::AlignBoundary::BYTE,
            size: 0,
            signed: false,
        },
    }
}

pub enum OpType {
    Unsigend,
    Signed,
    Float,
    Double,
    // pointer operatin where the pointer to type has the given size
    Pointer(u128),
}

/// Gives the signednes of a arithmetic type, and [`OpType::Floaing`] for the floating types,
/// pointer are also [`OpType::Pointer`]. Panics on arrays and void.
pub fn ctype_op_type(ctype: &CType) -> OpType {
    match ctype {
        CType::Scalar(scalar) => match scalar {
            ctype::Scalar::Arithmetic(arithmetic) => match arithmetic {
                ctype::Arithmetic::Float => OpType::Float,
                ctype::Arithmetic::Double | ctype::Arithmetic::LongDouble => OpType::Double,
                _ => {
                    if arithmetic.is_signed() {
                        OpType::Signed
                    } else {
                        OpType::Unsigend
                    }
                }
            },
            ctype::Scalar::Pointer(ctype::Pointer { inner, .. }) => {
                OpType::Pointer(ctype_props(inner).size)
            }
        },
        CType::Aggregate(_) => panic!("ICE: should not be operating on aggregate types"),
        CType::Void => panic!("ICE: should not be operating on void types"),
    }
}

/// Gives the [`mir::FFmt`] for the ctype if it is a floating type, [`None`] otherwise.
pub fn ctype_floating_fmt(ctype: &CType) -> Option<mir::FFmt> {
    match ctype {
        CType::Scalar(ctype::Scalar::Arithmetic(ctype::Arithmetic::Float)) => Some(mir::FFmt::S),
        CType::Scalar(ctype::Scalar::Arithmetic(
            ctype::Arithmetic::Double | ctype::Arithmetic::LongDouble,
        )) => Some(mir::FFmt::D),
        _ => None,
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RegType {
    Int,
    Single,
    Double,
}

pub fn ctype_reg_type(ctype: &CType) -> RegType {
    match ctype_floating_fmt(ctype) {
        Some(mir::FFmt::S) => RegType::Single,
        Some(mir::FFmt::D) => RegType::Double,
        None => RegType::Int,
    }
}
