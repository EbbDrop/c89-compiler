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
    passes::lower_ast::type_checking::{CompatPointer, PointerIntger, UsualArithConversions},
};

use super::{
    symbol_table::ScopedHandle,
    type_checking::{
        AnyScaler, CheckBinErr, CheckBinOk, CheckUnErr, CheckUnOk, PromoteArith, TypeRuleBin,
        TypeRuleUn,
    },
    util::{find_first_fit, maybe_cast},
};

const SIGNED_INT: CType = CType::Scalar(ctype::Scalar::Arithmetic(ctype::Arithmetic::SignedInt));

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
        ast::Expression::ArraySubscript(_lhs, _rhs) => AggregateResult::new_err(
            DiagnosticBuilder::new(span).build_unimplemented("array subscription"),
        ),
        ast::Expression::Unary(op, inner) => build_unary_op_ir_expr(op, inner, span, scope),
        ast::Expression::Cast(type_name, inner) => build_ir_expr(inner, scope).and_then(|inner| {
            cast(
                inner,
                CType::from_ast_type(&type_name.data.inner.data),
                span,
                type_name.span,
            )
        }),
        ast::Expression::FunctionCall(_) => AggregateResult::new_err(
            DiagnosticBuilder::new(span).build_unimplemented("function call"),
        ),
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
            Star => builder.bin_op(UsualArithConversions::new(), BinaryOp::Mul),
            Slash => builder.bin_op(UsualArithConversions::new(), BinaryOp::Div),
            Percent => builder.bin_op(UsualArithConversions::only_int(), BinaryOp::Rem),
            Pipe => builder.bitwise_op(BitwiseOp::Or),
            Caret => builder.bitwise_op(BitwiseOp::Xor),
            Ampersand => builder.bitwise_op(BitwiseOp::And),
            AngleLeft => builder.relation(RelationOp::Lt),
            AngleRight => builder.relation(RelationOp::Gt),
            DoubleEquals => builder.relation(RelationOp::Eq),
            BangEquals => builder.relation(RelationOp::Ne),
            AngleLeftEquals => builder.relation(RelationOp::Le),
            AngleRightEquals => builder.relation(RelationOp::Ge),
            DoubleAmpersand => builder.logical_and(),
            DoublePipe => builder.logical_or(),
            DoubleAngleLeft => {
                builder.bin_op(UsualArithConversions::only_int(), BinaryOp::ShiftLeft)
            }
            DoubleAngleRight => {
                builder.bin_op(UsualArithConversions::only_int(), BinaryOp::ShiftRight)
            }
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
        Bang => builder.value().and_then(|v| v.not()),
        Plus => builder.value().and_then(|v| v.plus()),
        Tilde => builder.value().and_then(|v| v.bit_not()),
        Minus => builder.value().and_then(|v| v.neg()),
        DoublePlusPrefix => builder.prefix_inc(),
        DoubleMinusPrefix => builder.prefix_dec(),
        DoublePlusPostfix => builder.postfix_inc(),
        DoubleMinusPostfix => builder.postfix_dec(),
        Ampersand => builder.reference(),
        Star => builder
            .value()
            .and_then(|v| v.dereference())
            .and_then(lvalue_dereference),
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
                ast::UnaryOperator::Star => Some(builder.value().and_then(|v| v.dereference())),
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
    use ctype::Scalar;
    let reason = match (&to_ty, &inner.ty) {
        (CType::Scalar(Scalar::Pointer(_, _)), CType::Scalar(Scalar::Arithmetic(a)))
            if a.is_floating() =>
        {
            InvalidCastReason::PointerFromFloat(&inner)
        }
        (CType::Scalar(Scalar::Arithmetic(a)), CType::Scalar(Scalar::Pointer(_, _)))
            if a.is_floating() =>
        {
            InvalidCastReason::FloatFromPointer(&inner)
        }
        (CType::Scalar(_), CType::Scalar(_)) => {
            return AggregateResult::new_ok(ExprNode {
                span,
                ty: to_ty,
                expr: Expr::Cast(Box::new(inner)),
            })
        }
        // TODO: This will become reachable when non scalar types exists
        #[allow(unreachable_patterns)]
        (CType::Scalar(_), _) => InvalidCastReason::FromNonScaler(&inner),
        #[allow(unreachable_patterns)]
        (_, CType::Scalar(_)) => InvalidCastReason::IntoNonScalar,
    };
    AggregateResult::new_err(DiagnosticBuilder::new(op_span).build_invalid_cast(reason))
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
            // IDEA: this could brake the standard and use a smarter algo
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

struct ValueUnaryBuilder {
    full_span: Span,
    op_span: Span,
    inner: ExprNode,
}

enum LvalueBuildErr {
    NoConst,
    // Would be used if non scalar types are suported.
    _WrongType(TypeCat),
}

impl<'a, 'b> UnaryBuilder<'a, 'b> {
    fn lvalue_build<R, F>(self, rule: R, name: &str, build: F) -> AggregateResult<ExprNode>
    where
        R: FnOnce(&CType, bool) -> Result<CType, LvalueBuildErr>,
        F: FnOnce(Box<LvalueExprNode>) -> Expr,
    {
        let res = build_ir_lvalue(self.inner, name, false, self.op_span, self.scope);

        res.and_then(|inner| match rule(&inner.ty, inner.is_const) {
            Ok(out_ty) => AggregateResult::new_ok(ExprNode {
                span: self.full_span,
                ty: out_ty,
                expr: build(Box::new(inner)),
            }),
            Err(err) => {
                let builder = DiagnosticBuilder::new(self.op_span);
                let diag = match err {
                    LvalueBuildErr::NoConst => builder.build_cant_be_const(name, inner.span),
                    LvalueBuildErr::_WrongType(type_cat) => {
                        builder.build_unexpected_type_lvalue(name, type_cat, &inner)
                    }
                };
                AggregateResult::new_err(diag)
            }
        })
    }

    fn build_inc_dec<F>(self, name: &str, wrap_expr: F) -> AggregateResult<ExprNode>
    where
        F: FnOnce(Box<LvalueExprNode>) -> Expr,
    {
        self.lvalue_build(
            |ty, is_const| {
                if is_const {
                    return Err(LvalueBuildErr::NoConst);
                }
                match ty {
                    CType::Scalar(_) => Ok(ty.clone()),
                }
            },
            name,
            wrap_expr,
        )
    }

    /// See [`Expr::PostfixInc`]
    fn postfix_inc(self) -> AggregateResult<ExprNode> {
        self.build_inc_dec("increment", Expr::PostfixInc)
    }

    /// See [`Expr::PostfixDec`]
    fn postfix_dec(self) -> AggregateResult<ExprNode> {
        self.build_inc_dec("decrement", Expr::PostfixDec)
    }

    /// See [`Expr::PrefixInc`]
    fn prefix_inc(self) -> AggregateResult<ExprNode> {
        self.build_inc_dec("increment", Expr::PrefixInc)
    }

    /// See [`Expr::PrefixDec`]
    fn prefix_dec(self) -> AggregateResult<ExprNode> {
        self.build_inc_dec("decrement", Expr::PrefixDec)
    }

    /// See [`Expr::Reference`]
    fn reference(self) -> AggregateResult<ExprNode> {
        self.lvalue_build(
            |ty, is_const| {
                Ok(CType::Scalar(ctype::Scalar::Pointer(
                    Box::new(ty.clone()),
                    is_const,
                )))
            },
            "refrence",
            Expr::Reference,
        )
    }

    fn value(self) -> AggregateResult<ValueUnaryBuilder> {
        let res = build_ir_expr(self.inner, self.scope);
        res.map(|inner| ValueUnaryBuilder {
            full_span: self.full_span,
            op_span: self.op_span,
            inner,
        })
    }
}

impl ValueUnaryBuilder {
    fn build<R, F>(self, rule: R, name: &str, build: F) -> AggregateResult<ExprNode>
    where
        R: TypeRuleUn,
        F: FnOnce(Box<ExprNode>) -> Expr,
    {
        match rule.check(&self.inner.ty) {
            Ok(CheckUnOk { inner_ty, out_ty }) => {
                let inner = match inner_ty {
                    Some(inner_ty) => maybe_cast(self.inner, inner_ty),
                    None => self.inner,
                };
                AggregateResult::new_ok(ExprNode {
                    span: self.full_span,
                    ty: out_ty,
                    expr: build(Box::new(inner)),
                })
            }
            Err(err) => {
                let builder = DiagnosticBuilder::new(self.op_span);
                let diag = match err {
                    CheckUnErr::Expected(type_cat) => {
                        builder.build_unexpected_type(name, type_cat, &self.inner)
                    }
                };
                AggregateResult::new_err(diag)
            }
        }
    }

    /// Promotions for the inner type are automatically inserted
    /// (This node will only do possible promotions, no actual plus node is inserted)
    fn plus(self) -> AggregateResult<ExprNode> {
        self.build(PromoteArith::new(), "`+`", |expr| expr.expr)
    }

    /// See [`UnaryOp::Neg`]
    fn neg(self) -> AggregateResult<ExprNode> {
        self.build(PromoteArith::new(), "negation", |expr| {
            Expr::UnaryArith(UnaryOp::Neg, expr)
        })
    }

    /// See [`UnaryOp::BitNot`]
    fn bit_not(self) -> AggregateResult<ExprNode> {
        self.build(PromoteArith::only_int(), "bitwise not", |expr| {
            Expr::UnaryArith(UnaryOp::BitNot, expr)
        })
    }

    /// See [`UnaryOp::Not`]
    fn not(self) -> AggregateResult<ExprNode> {
        self.build(AnyScaler.map_out_ty_un(SIGNED_INT), "not", |expr| {
            Expr::UnaryArith(UnaryOp::Not, expr)
        })
    }

    /// See [`LvalueExpr::Dereference`]
    fn dereference(self) -> AggregateResult<LvalueExprNode> {
        match &self.inner.ty {
            CType::Scalar(ctype::Scalar::Pointer(ref pointed_to_ty, is_const)) => {
                let ty = pointed_to_ty.as_ref().clone();
                let res = LvalueExprNode {
                    span: self.full_span,
                    is_const: *is_const,
                    ty,
                    expr: LvalueExpr::Dereference(Box::new(self.inner)),
                };

                AggregateResult::new_ok(res)
            }
            _ => AggregateResult::new_err(
                DiagnosticBuilder::new(self.op_span).build_unexpected_type(
                    "dereference",
                    TypeCat::Pointer,
                    &self.inner,
                ),
            ),
        }
    }
}

struct BinaryBuilder {
    full_span: Span,
    op_span: Span,
    left: ExprNode,
    right: ExprNode,
}

impl BinaryBuilder {
    fn build<R, F>(self, rule: R, name: &str, build: F) -> AggregateResult<ExprNode>
    where
        R: TypeRuleBin,
        F: FnOnce(Box<ExprNode>, Box<ExprNode>) -> Expr,
    {
        match rule.check(&self.left.ty, &self.right.ty) {
            Ok(CheckBinOk {
                left_ty,
                right_ty,
                out_ty,
            }) => {
                let left = match left_ty {
                    Some(left_ty) => maybe_cast(self.left, left_ty),
                    None => self.left,
                };
                let right = match right_ty {
                    Some(right_ty) => maybe_cast(self.right, right_ty),
                    None => self.right,
                };
                AggregateResult::new_ok(ExprNode {
                    span: self.full_span,
                    ty: out_ty,
                    expr: build(Box::new(left), Box::new(right)),
                })
            }
            Err(err) => {
                let builder = DiagnosticBuilder::new(self.op_span);
                let diag = match err {
                    CheckBinErr::Left(type_cat) => {
                        builder.build_unexpected_type_bin(name, type_cat, &self.left, None)
                    }
                    CheckBinErr::Right(type_cat) => {
                        builder.build_unexpected_type_bin(name, type_cat, &self.right, None)
                    }
                    CheckBinErr::Both(type_cat) => builder.build_unexpected_type_bin(
                        name,
                        type_cat,
                        &self.left,
                        Some(&self.right),
                    ),
                    CheckBinErr::Unknow => {
                        builder.build_incompatible_types(name, &self.left, &self.right)
                    }
                };
                AggregateResult::new_err(diag)
            }
        }
    }

    fn bin_op<R>(self, rule: R, op: BinaryOp) -> AggregateResult<ExprNode>
    where
        R: TypeRuleBin,
    {
        self.build(rule, op.long_name(), |l, r| Expr::Binary(l, op, r))
    }

    /// See [`BinaryOp::Add`]
    fn add(self) -> AggregateResult<ExprNode> {
        let rule = UsualArithConversions::new().or(PointerIntger);
        self.bin_op(rule, BinaryOp::Add)
    }

    /// See [`BinaryOp::Sub`]
    fn sub(self) -> AggregateResult<ExprNode> {
        // TODO SignedLongInt chosen since that is what g++ uses but should probably be
        // platform dependent, this represents the maximum length between two pointers
        let pointer_pointer_distance_ty =
            CType::Scalar(ctype::Scalar::Arithmetic(ctype::Arithmetic::SignedLongInt));

        let rule = UsualArithConversions::new()
            .or(PointerIntger)
            .or(CompatPointer.map_out_ty_bin(pointer_pointer_distance_ty));
        self.bin_op(rule, BinaryOp::Sub)
    }

    /// See [`Expr::Relation`]
    fn relation(self, op: RelationOp) -> AggregateResult<ExprNode> {
        let rule = UsualArithConversions::new()
            .or(CompatPointer)
            .map_out_ty_bin(SIGNED_INT);
        return self.build(rule, op.long_name(), |l, r| Expr::Relation(l, op, r));
    }

    fn bitwise_op(self, op: BitwiseOp) -> AggregateResult<ExprNode> {
        self.bin_op(UsualArithConversions::only_int(), BinaryOp::Bitwise(op))
    }

    fn logical_and(self) -> AggregateResult<ExprNode> {
        let rule = AnyScaler.map_out_ty_bin(SIGNED_INT);
        self.build(rule, "logical and", Expr::LogicalAnd)
    }

    fn logical_or(self) -> AggregateResult<ExprNode> {
        let rule = AnyScaler.map_out_ty_bin(SIGNED_INT);
        self.build(rule, "logical or", Expr::LogicalOr)
    }
}
