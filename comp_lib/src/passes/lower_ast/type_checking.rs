use crate::{
    diagnostic::builder::TypeCat,
    ir::ctype::{Arithmetic, CType, Scalar},
};

/// The result of a binary type rule check.
#[derive(Debug, Clone)]
pub struct CheckBinOk {
    /// The type the left expresions should be converted to before doing the operation.
    /// [`None`] means to leave it unchaned.
    pub left_ty: Option<CType>,
    /// The type the right expresions should be converted to before doing the operation.
    /// [`None`] means to leave it unchaned.
    pub right_ty: Option<CType>,
    /// The type the result of the operation should have.
    pub out_ty: CType,
}

#[derive(Debug, Clone, Copy)]
pub enum CheckBinErr {
    Left(TypeCat),
    Right(TypeCat),
    Both(TypeCat),
    Unknown,
}

pub trait TypeRuleBin
where
    Self: Sized,
{
    fn check(self, left: &CType, right: &CType) -> Result<CheckBinOk, CheckBinErr>;

    /// Creates a new `TypeRuleBin` that tries this type rule first, if that one matches its result
    /// is returned. Otherwise `then` is used. If neither match a [`CheckBinErr::Unknow`] is allways
    /// returned.
    fn or<TR: TypeRuleBin>(self, then: TR) -> OrBin<Self, TR> {
        OrBin(self, then)
    }

    /// Creates a new `TypeRuleBin` that maps the `out_ty` of a successful check to the
    /// given type `ty`.
    fn map_out_ty_bin(self, ty: CType) -> MapOut<Self> {
        MapOut { inner: self, ty }
    }
}

/// See [`TypeRuleBin::or`]
pub struct OrBin<F, S>(F, S)
where
    F: TypeRuleBin,
    S: TypeRuleBin;
impl<F, S> TypeRuleBin for OrBin<F, S>
where
    F: TypeRuleBin,
    S: TypeRuleBin,
{
    fn check(self, left: &CType, right: &CType) -> Result<CheckBinOk, CheckBinErr> {
        match self.0.check(left, right) {
            Ok(res) => Ok(res),
            Err(_) => self.1.check(left, right).map_err(|_| CheckBinErr::Unknown),
        }
    }
}

/// See [`TypeRuleBin::map_out_ty_un`] and [`TypeRuleBin::map_out_ty_bin`].
pub struct MapOut<T> {
    inner: T,
    ty: CType,
}

impl<T: TypeRuleBin> TypeRuleBin for MapOut<T> {
    fn check(self, left: &CType, right: &CType) -> Result<CheckBinOk, CheckBinErr> {
        self.inner.check(left, right).map(|mut res| {
            res.out_ty = self.ty;
            res
        })
    }
}

impl<T: TypeRuleUn> TypeRuleUn for MapOut<T> {
    fn check(self, inner: &CType) -> Result<CheckUnOk, CheckUnErr> {
        self.inner.check(inner).map(|mut res| {
            res.out_ty = self.ty;
            res
        })
    }
}

/// The result of a unary type rule check.
#[derive(Debug, Clone)]
pub struct CheckUnOk {
    /// The type the inner expresions should be converted to before doing the operation.
    /// [`None`] means to leave it unchaned.
    pub inner_ty: Option<CType>,
    /// The type the result of the operation should have.
    pub out_ty: CType,
}

#[derive(Debug, Clone, Copy)]
pub enum CheckUnErr {
    Expected(TypeCat),
}

pub trait TypeRuleUn
where
    Self: Sized,
{
    fn check(self, inner: &CType) -> Result<CheckUnOk, CheckUnErr>;

    /// Creates a new `TypeRuleUn` that maps the `out_ty` of a successful check to the given `ty`.
    fn map_out_ty_un(self, ty: CType) -> MapOut<Self> {
        MapOut { inner: self, ty }
    }
}

/// Checks if both types are arithmetic, if so the usual usual arithmetic conversions (3.2.1.5)
/// are performed. If in only integer mode then both types are checked weather or not they are
/// integers beforehand.
pub struct UsualArithConversions {
    only_integer: bool,
}

impl UsualArithConversions {
    pub fn new() -> Self {
        UsualArithConversions {
            only_integer: false,
        }
    }

    pub fn only_int() -> Self {
        UsualArithConversions { only_integer: true }
    }
}

impl TypeRuleBin for UsualArithConversions {
    fn check(self, left: &CType, right: &CType) -> Result<CheckBinOk, CheckBinErr> {
        let type_cat = match self.only_integer {
            true => TypeCat::Integral,
            false => TypeCat::Arithmetic,
        };
        let (left, right) = match (left, right) {
            (CType::Scalar(Scalar::Arithmetic(left)), CType::Scalar(Scalar::Arithmetic(right))) => {
                if self.only_integer {
                    match (left.is_floating(), right.is_floating()) {
                        (true, true) => return Err(CheckBinErr::Both(type_cat)),
                        (true, false) => return Err(CheckBinErr::Left(type_cat)),
                        (false, true) => return Err(CheckBinErr::Right(type_cat)),
                        (false, false) => {}
                    }
                }
                (left, right)
            }
            (_, CType::Scalar(Scalar::Arithmetic(_))) => return Err(CheckBinErr::Left(type_cat)),
            (CType::Scalar(Scalar::Arithmetic(_)), _) => return Err(CheckBinErr::Right(type_cat)),
            _ => return Err(CheckBinErr::Both(type_cat)),
        };

        let out_arith = Arithmetic::usual_arithmetic_conversions(left, right);
        let out_type = CType::Scalar(Scalar::Arithmetic(out_arith));
        Ok(CheckBinOk {
            left_ty: Some(out_type.clone()),
            right_ty: Some(out_type.clone()),
            out_ty: out_type,
        })
    }
}

/// Checks for two pointers with compatible inner type ignoring the constness of this level but not the inner levels.
pub struct CompatPointer;

impl TypeRuleBin for CompatPointer {
    fn check(self, left: &CType, right: &CType) -> Result<CheckBinOk, CheckBinErr> {
        let (left, right) = match (left, right) {
            (CType::Scalar(Scalar::Pointer(left, _)), CType::Scalar(Scalar::Pointer(right, _))) => {
                (left, right)
            }
            (CType::Scalar(Scalar::Pointer(_, _)), _) => {
                return Err(CheckBinErr::Right(TypeCat::Pointer))
            }
            (_, CType::Scalar(Scalar::Pointer(_, _))) => {
                return Err(CheckBinErr::Left(TypeCat::Pointer))
            }
            _ => return Err(CheckBinErr::Both(TypeCat::Pointer)),
        };
        match left.compatible_with(right) {
            Ok(_) => Ok(CheckBinOk {
                left_ty: None,
                right_ty: None,
                out_ty: *left.clone(),
            }),
            Err(_) => Err(CheckBinErr::Unknown),
        }
    }
}

/// Checks for one pointer and another integer, the pointer is left unchanged and the integer is
/// cast to a integer of the same size as a pointer. The out type will be the same as the pointer.
pub struct PointerInteger {
    only_ptr_first: bool,
}

impl PointerInteger {
    pub fn new() -> Self {
        Self {
            only_ptr_first: false,
        }
    }

    pub fn only_ptr_first() -> Self {
        Self {
            only_ptr_first: true,
        }
    }
}

impl TypeRuleBin for PointerInteger {
    fn check(self, left: &CType, right: &CType) -> Result<CheckBinOk, CheckBinErr> {
        // TODO this dependent on the environment
        let pointer_size = CType::Scalar(Scalar::Arithmetic(Arithmetic::UnsignedLongInt));

        match (left, right) {
            (CType::Scalar(Scalar::Pointer(_, _)), CType::Scalar(Scalar::Arithmetic(other))) => {
                if other.is_integral() {
                    Ok(CheckBinOk {
                        left_ty: None,
                        right_ty: Some(pointer_size),
                        out_ty: left.clone(),
                    })
                } else {
                    Err(CheckBinErr::Right(TypeCat::Integral))
                }
            }
            (CType::Scalar(Scalar::Arithmetic(other)), CType::Scalar(Scalar::Pointer(_, _))) => {
                if self.only_ptr_first {
                    return Err(CheckBinErr::Unknown);
                }
                if other.is_integral() {
                    Ok(CheckBinOk {
                        left_ty: Some(pointer_size),
                        right_ty: None,
                        out_ty: right.clone(),
                    })
                } else {
                    Err(CheckBinErr::Right(TypeCat::Integral))
                }
            }
            _ => Err(CheckBinErr::Unknown),
        }
    }
}

/// Checks for two scalar types, sugesting no changes and giving the left/only type as out_ty.
/// You can use [`TypeRuleUn::map_out_ty_un`]/[`TypeRuleBin::map_out_ty_bin`] to change that.
pub struct AnyScaler;

impl TypeRuleBin for AnyScaler {
    fn check(self, left: &CType, right: &CType) -> Result<CheckBinOk, CheckBinErr> {
        match (left, right) {
            (CType::Scalar(_), CType::Scalar(_)) => Ok(CheckBinOk {
                left_ty: None,
                right_ty: None,
                out_ty: left.clone(),
            }),
        }
    }
}

impl TypeRuleUn for AnyScaler {
    fn check(self, inner: &CType) -> Result<CheckUnOk, CheckUnErr> {
        match inner {
            CType::Scalar(_) => Ok(CheckUnOk {
                inner_ty: None,
                out_ty: inner.clone(),
            }),
        }
    }
}

/// Checks if the type is arithmetic (and integer if in only interger mode). If it is its,
/// promotions are aplied to the input type and the same type is set as `out_type`.
pub struct PromoteArith {
    only_integer: bool,
}

impl PromoteArith {
    pub fn new() -> Self {
        PromoteArith {
            only_integer: false,
        }
    }

    pub fn only_int() -> Self {
        PromoteArith { only_integer: true }
    }
}

impl TypeRuleUn for PromoteArith {
    fn check(self, inner: &CType) -> Result<CheckUnOk, CheckUnErr> {
        match inner {
            CType::Scalar(Scalar::Arithmetic(a)) => {
                if self.only_integer && !a.is_integral() {
                    return Err(CheckUnErr::Expected(TypeCat::Integral));
                }
                let ty = CType::Scalar(Scalar::Arithmetic(a.promote().0));
                Ok(CheckUnOk {
                    inner_ty: Some(ty.clone()),
                    out_ty: ty,
                })
            }
            _ => Err(CheckUnErr::Expected(match self.only_integer {
                true => TypeCat::Integral,
                false => TypeCat::Arithmetic,
            })),
        }
    }
}
