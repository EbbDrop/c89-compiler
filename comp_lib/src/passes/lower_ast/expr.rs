use crate::{
    ast,
    diagnostic::{
        builder::{InvalidCastReason, TypeCat},
        AggregateResult, DiagnosticBuilder, Span,
    },
    ir::{
        self,
        ctype::{self, CType},
        expr::{
            BinaryOp, BitwiseOp, Expr, ExprNode, LvalueExpr, LvalueExprNode, RelationOp, UnaryOp,
        },
    },
};

use super::{
    symbol_table::ScopedHandle,
    util::{
        self, cast_to_pointer_size, cast_to_promoted, find_first_fit, maybe_cast,
        try_usual_arithmetic_conversions,
    },
};

pub fn build_ir_expr(
    e: &ast::ExpressionNode,
    scope: &mut ScopedHandle,
) -> AggregateResult<ExprNode> {
    let span = e.span;
    match &e.data {
        ast::Expression::Assignment(lhs, op, rhs) => {
            // Building rhs first to make shure the lvalue is not assignent yet
            build_ir_expr(rhs, scope)
                .zip(build_ir_lvalue(lhs, "assignment", true, op.span, scope))
                .and_then(|(rhs, lhs)| assign(lhs, rhs, span, op.span))
        }
        ast::Expression::Binary(left, op, right) => {
            build_binary_op_ir_expr(op, left, right, span, scope)
        }
        ast::Expression::Unary(op, inner) => build_unary_op_ir_expr(op, inner, span, scope),
        ast::Expression::Cast(type_name, inner) => build_ir_expr(inner, scope).and_then(|inner| {
            cast(
                inner,
                CType::from_ast_type(&type_name.data.inner.data),
                span,
                type_name.span,
            )
        }),
        ast::Expression::Literal(lit) => literal(lit),
        ast::Expression::Ident(idt) => ident(idt, false, scope).and_then(lvalue_dereference),
    }
}

fn build_binary_op_ir_expr(
    op: &ast::BinaryOperatorNode,
    left: &ast::ExpressionNode,
    right: &ast::ExpressionNode,
    span: Span,
    scope: &mut ScopedHandle,
) -> AggregateResult<ExprNode> {
    use ast::BinaryOperator::*;
    let res = build_ir_expr(left, scope);
    let res = res.zip(build_ir_expr(right, scope));
    res.and_then(|(left, right)| {
        let builder = BinaryBuilder {
            full_span: span,
            op_span: op.span,
            left,
            right,
        };
        match &op.data {
            Plus => builder.add(),
            Minus => builder.sub(),
            Star => builder.mul(),
            Slash => builder.div(),
            Percent => builder.rem(),
            Pipe => builder.generic_bitwise_op(BitwiseOp::Or),
            Caret => builder.generic_bitwise_op(BitwiseOp::Xor),
            Ampersand => builder.generic_bitwise_op(BitwiseOp::And),
            AngleLeft => builder.relation(RelationOp::Lt),
            AngleRight => builder.relation(RelationOp::Gt),
            DoubleEquals => builder.relation(RelationOp::Eq),
            BangEquals => builder.relation(RelationOp::Ne),
            AngleLeftEquals => builder.relation(RelationOp::Le),
            AngleRightEquals => builder.relation(RelationOp::Ge),
            DoubleAmpersand => builder.logical_and(),
            DoublePipe => builder.logical_or(),
            DoubleAngleLeft => builder.shift_left(),
            DoubleAngleRight => builder.shift_right(),
        }
    })
}

fn build_unary_op_ir_expr(
    op: &ast::UnaryOperatorNode,
    inner: &ast::ExpressionNode,
    span: Span,
    scope: &mut ScopedHandle,
) -> AggregateResult<ExprNode> {
    use ast::UnaryOperator::*;
    let builder = UnaryBuilder {
        full_span: span,
        op_span: op.span,
        inner,
        scope,
    };
    match &op.data {
        Bang => builder.not(),
        Plus => builder.plus(),
        Tilde => builder.bit_not(),
        Minus => builder.neg(),
        DoublePlusPrefix => builder.prefix_inc(),
        DoubleMinusPrefix => builder.prefix_dec(),
        DoublePlusPostfix => builder.postfix_inc(),
        DoubleMinusPostfix => builder.postfix_dec(),
        Ampersand => builder.reference(),
        Star => builder.dereference().and_then(lvalue_dereference),
    }
}

/// Tries to build a lvalue, a err [`AggregateResult`] is retuned if the `e` can't be made into a
/// lvalue.
pub fn build_ir_lvalue(
    e: &ast::ExpressionNode,
    needed_for: &str,
    will_init: bool,
    op_span: Span,
    scope: &mut ScopedHandle,
) -> AggregateResult<LvalueExprNode> {
    let lvalue = match &e.data {
        ast::Expression::Unary(op, inner) => {
            let builder = UnaryBuilder {
                full_span: e.span,
                op_span: op.span,
                inner,
                scope,
            };
            match op.data {
                ast::UnaryOperator::Star => Some(builder.dereference()),
                _ => None,
            }
        }
        ast::Expression::Ident(idt) => Some(ident(idt, will_init, scope)),
        _ => None,
    };
    lvalue.unwrap_or_else(|| {
        AggregateResult::new_err(
            DiagnosticBuilder::new(op_span).build_need_lvalue(needed_for, e.span),
        )
    })
}

fn lvalue_dereference(inner: LvalueExprNode) -> AggregateResult<ExprNode> {
    AggregateResult::new_ok(ExprNode {
        span: inner.span,
        ty: inner.ty.clone(),
        expr: Expr::LvalueDeref(Box::new(inner)),
    })
}

fn ident(
    idt: &ast::IdentNode,
    will_init: bool,
    scope: &mut ScopedHandle,
) -> AggregateResult<LvalueExprNode> {
    let Some((id, ty)) = scope.reference_mut(&idt.data) else {
        return AggregateResult::new_err(DiagnosticBuilder::new(idt.span).build_undeclared_ident(&idt.data));
    };

    let mut res = AggregateResult::new_ok(());
    if !will_init && !ty.initialized {
        res.add_rec_diagnostic(DiagnosticBuilder::new(idt.span).build_usign_uninit(&idt.data));
    } else if will_init {
        ty.initialized = true;
    }
    res.map(|()| LvalueExprNode {
        span: idt.span,
        is_const: ty.is_const,
        ty: ty.ty.clone(),
        expr: LvalueExpr::Ident(id),
    })
}

fn literal(lit: &ast::LiteralNode) -> AggregateResult<ExprNode> {
    use ctype::Arithmetic::*;
    // TODO these need to change if we ever support sufixes
    let (value, pos_types) = match &lit.data {
        ast::Literal::Dec(i) => (*i, &[SignedInt, SignedLongInt, UnsignedLongInt]),
        ast::Literal::Hex(i) | ast::Literal::Octal(i) => {
            (*i, &[SignedInt, UnsignedInt, SignedLongInt])
        }
        ast::Literal::Char(i) => {
            return AggregateResult::new_ok(ExprNode {
                span: lit.span,
                ty: CType::Scalar(ctype::Scalar::Arithmetic(Char)),
                expr: Expr::Constant(ir::expr::Constant::Integer(*i)),
            })
        }
        ast::Literal::Float(value) => {
            return AggregateResult::new_ok(ExprNode {
                span: lit.span,
                ty: CType::Scalar(ctype::Scalar::Arithmetic(Double)),
                expr: Expr::Constant(ir::expr::Constant::Float(*value)),
            })
        }
    };

    match find_first_fit(value, pos_types) {
        Some(ty) => AggregateResult::new_ok(ExprNode {
            span: lit.span,
            ty: CType::Scalar(ctype::Scalar::Arithmetic(ty)),
            expr: Expr::Constant(ir::expr::Constant::Integer(value)),
        }),
        None => {
            AggregateResult::new_err(DiagnosticBuilder::new(lit.span).build_too_big_constant(value))
        }
    }
}

/// This function should only be used for explicit casts that where is the ast
///
/// The inner type, and the to type both needs to scalar. You can also not cast a floating type to
/// a pointer.
fn cast(inner: ExprNode, to_ty: CType, span: Span, op_span: Span) -> AggregateResult<ExprNode> {
    let mut res = AggregateResult::new_ok(());
    if !inner.ty.is_scalar() {
        res.add_err(
            DiagnosticBuilder::new(span)
                .with_ir_expr_type(&inner)
                .build_invalid_cast(InvalidCastReason::FromNonScaler(&inner)),
        );
    }
    // TODO: allows casting to void
    if !to_ty.is_scalar() {
        res.add_err(
            DiagnosticBuilder::new(op_span).build_invalid_cast(InvalidCastReason::IntoNonScalar),
        );
    }
    if inner.ty.is_floating() && to_ty.is_pointer() {
        res.add_err(
            DiagnosticBuilder::new(op_span)
                .build_invalid_cast(InvalidCastReason::PointerFromFloat(&inner)),
        );
    }
    if inner.ty.is_pointer() && to_ty.is_floating() {
        res.add_err(
            DiagnosticBuilder::new(op_span)
                .build_invalid_cast(InvalidCastReason::FloatFromPointer(&inner)),
        );
    }

    res.map(|()| ExprNode {
        span,
        ty: to_ty,
        expr: Expr::Cast(Box::new(inner)),
    })
}

pub fn assign(
    to: LvalueExprNode,
    from: ExprNode,
    span: Span,
    op_span: Span,
) -> AggregateResult<ExprNode> {
    let mut res = AggregateResult::new_ok(());
    if to.is_const {
        res.add_err(DiagnosticBuilder::new(op_span).build_cant_be_const("assign to", to.span));
    }
    use ctype::Scalar;
    match (&to.ty, &from.ty) {
        (CType::Scalar(Scalar::Arithmetic(to_ty)), CType::Scalar(Scalar::Arithmetic(from_ty))) => {
            match from_ty.conversion_lossynes_into(to_ty) {
                ctype::ConversionLossyness::Lossless => {}
                ctype::ConversionLossyness::SignChange => res.add_rec_diagnostic(
                    DiagnosticBuilder::new(op_span)
                        .build_implicit_lossy_conversion(&from, &to, true),
                ),
                ctype::ConversionLossyness::Lossy => res.add_rec_diagnostic(
                    DiagnosticBuilder::new(op_span)
                        .build_implicit_lossy_conversion(&from, &to, false),
                ),
            }
        }
        (
            CType::Scalar(Scalar::Pointer(to_ty, to_is_const)),
            CType::Scalar(Scalar::Pointer(from_ty, from_is_const)),
        ) => {
            if to_ty.compatible_with(from_ty).is_err() {
                res.add_rec_diagnostic(
                    DiagnosticBuilder::new(op_span).build_incompatible_assign(&from, &to),
                )
            }
            // IDEA: this could brake standard and use a smarter algo
            if *from_is_const && !to_is_const {
                res.add_rec_diagnostic(
                    DiagnosticBuilder::new(op_span).build_assign_const_loss(from.span, to.span),
                )
            }
        }
        (CType::Scalar(Scalar::Arithmetic(a)), CType::Scalar(Scalar::Pointer(_, _)))
        | (CType::Scalar(Scalar::Pointer(_, _)), CType::Scalar(Scalar::Arithmetic(a))) => {
            let diagnostic = DiagnosticBuilder::new(op_span).build_incompatible_assign(&from, &to);
            if a.is_floating() {
                res.add_err(diagnostic);
            } else {
                res.add_rec_diagnostic(diagnostic);
            }
        }
    };

    let to_type = to.ty.clone();
    res.map(|()| ExprNode {
        span,
        ty: to_type.clone(),
        expr: Expr::Assign(Box::new(to), Box::new(maybe_cast(from, to_type))),
    })
}

struct UnaryBuilder<'a, 'b> {
    full_span: Span,
    op_span: Span,
    inner: &'a ast::ExpressionNode,
    scope: &'a mut ScopedHandle<'b>,
}

impl<'a, 'b> UnaryBuilder<'a, 'b> {
    fn build_inc_dec<F>(self, name: &str, wrap_expr: F) -> AggregateResult<ExprNode>
    where
        F: FnOnce(Box<LvalueExprNode>) -> Expr,
    {
        let mut res = build_ir_lvalue(self.inner, name, false, self.op_span, self.scope);
        if res.has_value_and(|inner| inner.is_const) {
            res.add_err(
                DiagnosticBuilder::new(self.op_span).build_cant_be_const(name, self.inner.span),
            )
        }
        res.map(|inner| ExprNode {
            span: self.full_span,
            ty: inner.ty.clone(),
            expr: wrap_expr(Box::new(inner)),
        })
    }

    /// Inner can't be const
    /// See [`Expr::PostfixInc`]
    fn postfix_inc(self) -> AggregateResult<ExprNode> {
        self.build_inc_dec("increment", Expr::PostfixInc)
    }

    /// Inner can't be const
    /// See [`Expr::PostfixDec`]
    fn postfix_dec(self) -> AggregateResult<ExprNode> {
        self.build_inc_dec("decrement", Expr::PostfixDec)
    }

    /// Inner can't be const
    /// See [`Expr::PrefixInc`]
    fn prefix_inc(self) -> AggregateResult<ExprNode> {
        self.build_inc_dec("increment", Expr::PrefixInc)
    }

    /// Inner can't be const
    /// See [`Expr::PrefixDec`]
    fn prefix_dec(self) -> AggregateResult<ExprNode> {
        self.build_inc_dec("decrement", Expr::PrefixDec)
    }

    /// See [`Expr::Reference`]
    fn reference(self) -> AggregateResult<ExprNode> {
        let res = build_ir_lvalue(self.inner, "refrence", false, self.op_span, self.scope);
        res.map(|inner| {
            let ty = CType::Scalar(ctype::Scalar::Pointer(
                Box::new(inner.ty.clone()),
                inner.is_const,
            ));

            ExprNode {
                span: self.full_span,
                ty,
                expr: Expr::Reference(Box::new(inner)),
            }
        })
    }

    /// See [`LvalueExpr::Dereference`]
    fn dereference(self) -> AggregateResult<LvalueExprNode> {
        let res = build_ir_expr(self.inner, self.scope);
        let res = res.and_then(|inner| match &inner.ty {
            CType::Scalar(ref scalar) => match scalar {
                ctype::Scalar::Pointer(ref pointed_to_ty, is_const) => {
                    let ty = pointed_to_ty.as_ref().clone();
                    AggregateResult::new_ok((ty, *is_const, inner))
                }
                ctype::Scalar::Arithmetic(_) => AggregateResult::new_err(
                    DiagnosticBuilder::new(self.op_span).build_unexpected_type(
                        "dereference",
                        TypeCat::Pointer,
                        &inner,
                    ),
                ),
            },
        });

        res.map(|(ty, is_const, inner)| LvalueExprNode {
            span: self.full_span,
            is_const,
            ty,
            expr: LvalueExpr::Dereference(Box::new(inner)),
        })
    }

    /// Promotions for the inner type are automatically inserted
    /// (This node will only do possible promotions, no actual plus node is inserted)
    fn plus(self) -> AggregateResult<ExprNode> {
        let res = build_ir_expr(self.inner, self.scope);
        res.and_then(|inner| {
            let mut expr = match cast_to_promoted(inner) {
                Ok(expr) => expr,
                Err(expr) => {
                    return AggregateResult::new_err(
                        DiagnosticBuilder::new(self.op_span).build_unexpected_type(
                            "`+`",
                            TypeCat::Arithmetic,
                            &expr,
                        ),
                    );
                }
            };
            expr.span = self.full_span;

            AggregateResult::new_ok(expr)
        })
    }

    /// See [`UnaryOp::Neg`]
    fn neg(self) -> AggregateResult<ExprNode> {
        let res = build_ir_expr(self.inner, self.scope);
        let res = res.and_then(|inner| match cast_to_promoted(inner) {
            Ok(expr) => AggregateResult::new_ok(expr),
            Err(expr) => AggregateResult::new_err(
                DiagnosticBuilder::new(self.op_span).build_unexpected_type(
                    "negation",
                    TypeCat::Arithmetic,
                    &expr,
                ),
            ),
        });

        res.map(|inner| ExprNode {
            span: self.full_span,
            ty: inner.ty.clone(),
            expr: Expr::UnaryArith(UnaryOp::Neg, Box::new(inner)),
        })
    }

    /// See [`UnaryOp::BitNot`]
    fn bit_not(self) -> AggregateResult<ExprNode> {
        let res = build_ir_expr(self.inner, self.scope);
        let res = res.and_then(|inner| {
            if inner.ty.is_integral() {
                AggregateResult::new_ok(
                    cast_to_promoted(inner)
                        .expect("ICE: integral types should always be able to be promoted"),
                )
            } else {
                AggregateResult::new_err(
                    DiagnosticBuilder::new(self.op_span).build_unexpected_type(
                        "bitwise not",
                        TypeCat::Integral,
                        &inner,
                    ),
                )
            }
        });
        res.map(|inner| ExprNode {
            span: self.full_span,
            ty: inner.ty.clone(),
            expr: Expr::UnaryArith(UnaryOp::Not, Box::new(inner)),
        })
    }

    /// See [`UnaryOp::Not`]
    fn not(self) -> AggregateResult<ExprNode> {
        let res = build_ir_expr(self.inner, self.scope);
        let res = res.and_then(|inner| match inner.ty.is_scalar() {
            true => AggregateResult::new_ok(inner),
            false => AggregateResult::new_err(
                DiagnosticBuilder::new(self.op_span).build_unexpected_type(
                    "not",
                    TypeCat::Scalar,
                    &inner,
                ),
            ),
        });

        res.map(|inner| ExprNode {
            span: self.full_span,
            ty: CType::Scalar(ctype::Scalar::Arithmetic(ctype::Arithmetic::SignedInt)),
            expr: Expr::UnaryArith(UnaryOp::Not, Box::new(inner)),
        })
    }
}

struct BinaryBuilder {
    full_span: Span,
    op_span: Span,
    left: ExprNode,
    right: ExprNode,
}

impl BinaryBuilder {
    fn usual_arithmetic_conversions(
        self,
        need_integral: bool,
        op_name: &str,
    ) -> AggregateResult<(ExprNode, ExprNode, CType)> {
        use util::ArithmeticConversionsErr;

        let needed_type_cat = if need_integral {
            TypeCat::Integral
        } else {
            TypeCat::Arithmetic
        };
        match try_usual_arithmetic_conversions(self.left, self.right, need_integral) {
            Ok(o) => AggregateResult::new_ok(o),
            Err(e) => match e {
                ArithmeticConversionsErr::BothWrong(e1, e2) => AggregateResult::new_err(
                    DiagnosticBuilder::new(self.op_span).build_unexpected_type_bin(
                        op_name,
                        needed_type_cat,
                        &e1,
                        Some(&e2),
                    ),
                ),
                ArithmeticConversionsErr::OneWrong(e) => AggregateResult::new_err(
                    DiagnosticBuilder::new(self.op_span).build_unexpected_type_bin(
                        op_name,
                        needed_type_cat,
                        &e,
                        None,
                    ),
                ),
            },
        }
    }

    fn two_scalar(self, op_name: &str) -> AggregateResult<(ExprNode, ExprNode)> {
        match (self.left.ty.is_scalar(), self.right.ty.is_scalar()) {
            (true, true) => AggregateResult::new_ok((self.left, self.right)),
            (true, false) => AggregateResult::new_err(
                DiagnosticBuilder::new(self.op_span).build_unexpected_type_bin(
                    op_name,
                    TypeCat::Scalar,
                    &self.right,
                    None,
                ),
            ),
            (false, true) => AggregateResult::new_err(
                DiagnosticBuilder::new(self.op_span).build_unexpected_type_bin(
                    op_name,
                    TypeCat::Scalar,
                    &self.left,
                    None,
                ),
            ),
            (false, false) => AggregateResult::new_err(
                DiagnosticBuilder::new(self.op_span).build_unexpected_type_bin(
                    op_name,
                    TypeCat::Scalar,
                    &self.right,
                    Some(&self.left),
                ),
            ),
        }
    }

    /// See [`BinaryOp::Mul`]
    fn mul(self) -> AggregateResult<ExprNode> {
        let span = self.full_span;
        self.usual_arithmetic_conversions(false, "multiplication")
            .map(|(left, right, ty)| ExprNode {
                span,
                ty,
                expr: Expr::Binary(Box::new(left), BinaryOp::Mul, Box::new(right)),
            })
    }

    /// See [`BinaryOp::Div`]
    fn div(self) -> AggregateResult<ExprNode> {
        let span = self.full_span;
        self.usual_arithmetic_conversions(false, "division")
            .map(|(left, right, ty)| ExprNode {
                span,
                ty,
                expr: Expr::Binary(Box::new(left), BinaryOp::Div, Box::new(right)),
            })
    }

    /// See [`BinaryOp::Rem`]
    fn rem(self) -> AggregateResult<ExprNode> {
        let span = self.full_span;
        self.usual_arithmetic_conversions(true, "remainder")
            .map(|(left, right, ty)| ExprNode {
                span,
                ty,
                expr: Expr::Binary(Box::new(left), BinaryOp::Rem, Box::new(right)),
            })
    }

    /// See [`BinaryOp::Add`]
    fn add(self) -> AggregateResult<ExprNode> {
        use ctype::Scalar;
        let Self {
            full_span,
            op_span,
            left,
            right,
        } = self;
        let result = match (&left.ty, &right.ty) {
            (CType::Scalar(Scalar::Arithmetic(_)), CType::Scalar(Scalar::Arithmetic(_))) => {
                let (left, right, ty) = try_usual_arithmetic_conversions(left, right, false)
                    .expect(
                    "ICE: try_usual_arithmetic_conversions should always succeed with arithmetic types",
                );
                Ok((left, right, ty))
            }
            (CType::Scalar(Scalar::Pointer(_, _)), CType::Scalar(Scalar::Arithmetic(_))) => {
                let ty = left.ty.clone();
                match cast_to_pointer_size(right) {
                    Ok(right) => Ok((left, right, ty)),
                    Err(right) => Err((left, right)),
                }
            }
            (CType::Scalar(Scalar::Arithmetic(_)), CType::Scalar(Scalar::Pointer(_, _))) => {
                let ty = right.ty.clone();
                match cast_to_pointer_size(left) {
                    Ok(left) => Ok((left, right, ty)),
                    Err(left) => Err((left, right)),
                }
            }
            _ => Err((left, right)),
        };
        match result {
            Ok((left, right, ty)) => AggregateResult::new_ok(ExprNode {
                span: full_span,
                ty,
                expr: Expr::Binary(Box::new(left), BinaryOp::Add, Box::new(right)),
            }),
            Err((left, right)) => {
                // TODO Add notes about what types are possible
                AggregateResult::new_err(
                    DiagnosticBuilder::new(op_span)
                        .build_incompatible_types("addition", &left, &right),
                )
            }
        }
    }

    /// See [`BinaryOp::Sub`]
    fn sub(self) -> AggregateResult<ExprNode> {
        use ctype::Scalar;
        let Self {
            full_span,
            op_span,
            left,
            right,
        } = self;
        let result = match (&left.ty, &right.ty) {
            (CType::Scalar(Scalar::Arithmetic(_)), CType::Scalar(Scalar::Arithmetic(_))) => {
                let (left, right, ty) = try_usual_arithmetic_conversions(left, right, false)
                    .expect(
                    "ICE: try_usual_arithmetic_conversions should always succeed with arithmetic types",
                );
                Ok((left, right, ty))
            }
            (CType::Scalar(Scalar::Pointer(_, _)), CType::Scalar(Scalar::Arithmetic(_))) => {
                let ty = left.ty.clone();
                match cast_to_pointer_size(right) {
                    Ok(right) => Ok((left, right, ty)),
                    Err(right) => Err((left, right)),
                }
            }
            (CType::Scalar(Scalar::Arithmetic(_)), CType::Scalar(Scalar::Pointer(_, _))) => {
                let ty = right.ty.clone();
                match cast_to_pointer_size(left) {
                    Ok(left) => Ok((left, right, ty)),
                    Err(left) => Err((left, right)),
                }
            }
            (
                CType::Scalar(Scalar::Pointer(left_ty, _)),
                CType::Scalar(Scalar::Pointer(right_ty, _)),
            ) => {
                // The standard explicitly says to ignore the is_const's above

                if left_ty.compatible_with(right_ty).is_ok() {
                    // TODO SignedLongInt chosen since that is what g++ uses but should probably be
                    // platform dependent, this represents the maximum length between two pointers
                    let ty = CType::Scalar(Scalar::Arithmetic(ctype::Arithmetic::SignedLongInt));

                    // Can't cast left and right yet since further code will not know what type of Sub
                    // it is
                    Ok((left, right, ty))
                } else {
                    Err((left, right))
                }
            }
        };
        match result {
            Ok((left, right, ty)) => AggregateResult::new_ok(ExprNode {
                span: full_span,
                ty,
                expr: Expr::Binary(Box::new(left), BinaryOp::Sub, Box::new(right)),
            }),
            Err((left, right)) => {
                // TODO Add notes about what types are possible
                AggregateResult::new_err(DiagnosticBuilder::new(op_span).build_incompatible_types(
                    "subtraction",
                    &left,
                    &right,
                ))
            }
        }
    }

    /// See [`Expr::Relation`]
    fn relation(self, op: RelationOp) -> AggregateResult<ExprNode> {
        let Self {
            full_span,
            op_span,
            left,
            right,
        } = self;
        use ctype::Scalar;
        let result = match (&left.ty, &right.ty) {
            (CType::Scalar(Scalar::Arithmetic(_)), CType::Scalar(Scalar::Arithmetic(_))) => {
                let (left, right, _) = try_usual_arithmetic_conversions(left, right, false).expect(
                    "ICE: try_usual_arithmetic_conversions should always succeed with arithmetic types",
                );
                Ok((left, right))
            }
            (
                CType::Scalar(Scalar::Pointer(left_ty, _)),
                CType::Scalar(Scalar::Pointer(right_ty, _)),
            ) => {
                // The standard explicitly says to ignore the is_const's above
                if left_ty.compatible_with(right_ty).is_ok() {
                    Ok((left, right))
                } else {
                    // TODO Eq and NotEq need a exception for void* (see 3.3.9)
                    Err((left, right))
                }
            }
            _ => Err((left, right)),
        };
        match result {
            Ok((left, right)) => AggregateResult::new_ok(ExprNode {
                span: full_span,
                ty: CType::Scalar(Scalar::Arithmetic(ctype::Arithmetic::SignedInt)),
                expr: Expr::Relation(Box::new(left), op, Box::new(right)),
            }),
            Err((left, right)) => {
                // TODO Add notes about what types are possible
                AggregateResult::new_err(DiagnosticBuilder::new(op_span).build_incompatible_types(
                    op.long_name(),
                    &left,
                    &right,
                ))
            }
        }
    }

    fn generic_integral_promoted_bin(
        self,
        op: BinaryOp,
        op_name: &str,
    ) -> AggregateResult<ExprNode> {
        let span = self.full_span;
        self.usual_arithmetic_conversions(true, op_name)
            .map(|(left, right, ty)| ExprNode {
                span,
                ty,
                expr: Expr::Binary(Box::new(left), op, Box::new(right)),
            })
    }

    fn shift_left(self) -> AggregateResult<ExprNode> {
        self.generic_integral_promoted_bin(BinaryOp::ShiftLeft, "shift left")
    }

    fn shift_right(self) -> AggregateResult<ExprNode> {
        self.generic_integral_promoted_bin(BinaryOp::ShiftLeft, "shift right")
    }

    fn generic_bitwise_op(self, op: BitwiseOp) -> AggregateResult<ExprNode> {
        let name = op.long_name();
        self.generic_integral_promoted_bin(BinaryOp::Bitwise(op), name)
    }

    fn logical_and(self) -> AggregateResult<ExprNode> {
        let span = self.full_span;
        self.two_scalar("logical and")
            .map(|(left, right)| ExprNode {
                span,
                ty: CType::Scalar(ctype::Scalar::Arithmetic(ctype::Arithmetic::SignedInt)),
                expr: Expr::LogicalAnd(Box::new(left), Box::new(right)),
            })
    }

    fn logical_or(self) -> AggregateResult<ExprNode> {
        let span = self.full_span;
        self.two_scalar("logical or").map(|(left, right)| ExprNode {
            span,
            ty: CType::Scalar(ctype::Scalar::Arithmetic(ctype::Arithmetic::SignedInt)),
            expr: Expr::LogicalOr(Box::new(left), Box::new(right)),
        })
    }
}
