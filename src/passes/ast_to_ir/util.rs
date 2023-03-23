use crate::{
    ctype::{self, CType},
    ir::expr::{Expr, ExprNode},
};

/// Only inserts a cast if `to_ty != inner.ty`. This function does *not* check if the cast is
/// allowed.
pub fn maybe_cast(inner: ExprNode, to_ty: CType) -> ExprNode {
    if to_ty == inner.ty {
        return inner;
    }

    ExprNode {
        span: inner.span,
        ty: to_ty,
        expr: Expr::Cast(Box::new(inner)),
    }
}

/// promotes the inner expression by possibly inserting a cast. Promotions is based on 3.2.1.1; See
/// [`ctype::Arithmetic::promote`] for details. If the inner expression does not have a Arithmetic
/// type [`Err`] is returned with the original expression as `E`.
pub fn cast_to_promoted(inner: ExprNode) -> Result<ExprNode, ExprNode> {
    let CType::Scalar(ctype::Scalar::Arithmetic(ty)) = inner.ty else {
        return Err(inner);
    };

    match ty.promote() {
        (promoted, true) => Ok(ExprNode {
            span: inner.span,
            ty: CType::Scalar(ctype::Scalar::Arithmetic(promoted)),
            expr: Expr::Cast(Box::new(inner)),
        }),
        (_, false) => Ok(inner),
    }
}

type ArithmeticConversionsOk = (ExprNode, ExprNode, CType);
#[derive(Debug)]
pub enum ArithmeticConversionsErr {
    BothWrong(ExprNode, ExprNode),
    OneWrong(ExprNode),
}

/// Tries to get a common type between the types of the two expressions. If any of the two
/// expressions is not arithmetic, [`Err`] will be returned where the wrong expressions are given
/// back with [`Some`]. It is guaranteed [`Err`] will have at least one [`Some`].
///
/// If both types are arithmetic, [`Ok`] is returned with both expressions with possible casts
/// inserted where needed. The used type is also returned.
///
/// 3.2.1.5
pub fn try_usual_arithmetic_conversions(
    left: ExprNode,
    right: ExprNode,
    need_integral: bool,
) -> Result<ArithmeticConversionsOk, ArithmeticConversionsErr> {
    use ctype::{CType::Scalar, Scalar::Arithmetic};
    use ArithmeticConversionsErr::*;
    let out_type = match (&left.ty, &right.ty) {
        (Scalar(Arithmetic(left_ty)), Scalar(Arithmetic(right_ty))) => {
            if need_integral {
                match (left_ty.is_integral(), right_ty.is_integral()) {
                    (true, true) => {}
                    (true, false) => return Err(OneWrong(right)),
                    (false, true) => return Err(OneWrong(left)),
                    (false, false) => return Err(BothWrong(left, right)),
                };
            }
            ctype::Arithmetic::usual_arithmetic_conversions(left_ty, right_ty)
        }
        (Scalar(Arithmetic(_)), _) => return Err(OneWrong(right)),
        (_, Scalar(Arithmetic(_))) => return Err(OneWrong(left)),
        _ => return Err(BothWrong(left, right)),
    };
    let out_type = Scalar(Arithmetic(out_type));
    Ok((
        maybe_cast(left, out_type.clone()),
        maybe_cast(right, out_type.clone()),
        out_type,
    ))
}

/// Possibly inserts a cast to the size of a pointer
///
/// Returns [`Err`] with the original inner, if inner does not have a integral type.
pub fn cast_to_pointer_size(inner: ExprNode) -> Result<ExprNode, ExprNode> {
    if !inner.ty.is_integral() {
        return Err(inner);
    }
    // TODO this dependent on the environment
    let out_type = CType::Scalar(ctype::Scalar::Arithmetic(
        ctype::Arithmetic::UnsignedLongInt,
    ));

    Ok(maybe_cast(inner, out_type))
}
