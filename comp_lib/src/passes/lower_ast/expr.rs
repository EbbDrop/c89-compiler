use std::iter::repeat;

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
    passes::lower_ast::{
        type_checking::{
            check_assign, AnyScaler, CheckBinErr, CheckBinOk, CheckUnErr, CheckUnOk, CompatPointer,
            PointerInteger, PromoteArith, TypeRuleBin, TypeRuleUn, UsualArithConversions,
        },
        util::{find_first_fit, maybe_cast, FunctionScope},
    },
    settings::Settings,
};

const SIGNED_INT: CType = CType::Scalar(ctype::Scalar::Arithmetic(ctype::Arithmetic::SignedInt));

pub fn build_ir_expr(
    e: &ast::ExpressionNode,
    settings: &Settings,
    scope: &mut FunctionScope,
) -> AggregateResult<ExprNode> {
    let span = e.span;
    match &e.data {
        ast::Expression::Assignment(lhs, op, rhs) => {
            // Building rhs first to make shure the lvalue is not assignent yet
            build_ir_expr(rhs, settings, scope)
                .zip(build_ir_lvalue(
                    lhs,
                    "assignment",
                    true,
                    op.span,
                    settings,
                    scope,
                ))
                .and_then(|(rhs, lhs)| assign(lhs, rhs, span, op.span, settings))
        }
        ast::Expression::Binary(left, op, right) => {
            build_binary_op_ir_expr(op, left, right, span, settings, scope)
        }
        ast::Expression::ArraySubscript(left, right) => {
            arrays_subscript(left, right, span, settings, scope).map(lvalue_dereference)
        }
        ast::Expression::Unary(op, inner) => {
            build_unary_op_ir_expr(op, inner, span, settings, scope)
        }
        ast::Expression::Cast(type_name, inner) => {
            build_ir_expr(inner, settings, scope).and_then(|inner| {
                cast(
                    inner,
                    CType::from_ast_type(&type_name.unqualified.data),
                    span,
                    type_name.span,
                )
            })
        }
        ast::Expression::FunctionCall(fcall) => function_call(fcall, span, settings, scope),
        ast::Expression::Literal(lit) => literal(lit, settings),
        ast::Expression::Ident(idt) => variable_ident(idt, false, scope).map(lvalue_dereference),
    }
}

fn build_binary_op_ir_expr(
    op: &ast::BinaryOperatorNode,
    left: &ast::ExpressionNode,
    right: &ast::ExpressionNode,
    span: Span,
    settings: &Settings,
    scope: &mut FunctionScope,
) -> AggregateResult<ExprNode> {
    use ast::BinaryOperator::*;
    let res = build_ir_expr(left, settings, scope);
    let res = res.zip(build_ir_expr(right, settings, scope));
    res.and_then(|(left, right)| {
        let builder = BinaryBuilder {
            full_span: span,
            op_span: op.span,
            left,
            right,
            settings,
        };
        match &op.data {
            Plus => builder.add(false),
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
    settings: &Settings,
    scope: &mut FunctionScope,
) -> AggregateResult<ExprNode> {
    use ast::UnaryOperator::*;
    let builder = UnaryBuilder {
        full_span: span,
        op_span: op.span,
        inner,
        settings,
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
            .and_then(|v| v.dereference(false))
            .map(lvalue_dereference),
    }
}

/// Tries to build a lvalue, a err [`AggregateResult`] is retuned if the `e` can't be made into a
/// lvalue.
pub fn build_ir_lvalue(
    e: &ast::ExpressionNode,
    needed_for: &str,
    will_init: bool,
    op_span: Span,
    settings: &Settings,
    scope: &mut FunctionScope,
) -> AggregateResult<LvalueExprNode> {
    let lvalue = match &e.data {
        ast::Expression::Unary(op, inner) => {
            let builder = UnaryBuilder {
                full_span: e.span,
                op_span: op.span,
                inner,
                settings,
                scope,
            };
            match op.data {
                ast::UnaryOperator::Star => {
                    Some(builder.value().and_then(|v| v.dereference(false)))
                }
                _ => None,
            }
        }
        ast::Expression::ArraySubscript(left, right) => {
            Some(arrays_subscript(left, right, e.span, settings, scope))
        }
        ast::Expression::Ident(idt) => Some(variable_ident(idt, will_init, scope)),
        _ => None,
    };
    lvalue.unwrap_or_else(|| {
        AggregateResult::new_err(
            DiagnosticBuilder::new(op_span).build_need_lvalue(needed_for, e.span),
        )
    })
}

fn arrays_subscript(
    left: &ast::ExpressionNode,
    right: &ast::ExpressionNode,
    span: Span,
    settings: &Settings,
    scope: &mut FunctionScope,
) -> AggregateResult<LvalueExprNode> {
    build_ir_expr(left, settings, scope)
        .zip(build_ir_expr(right, settings, scope))
        .and_then(|(left, right)| {
            let builder = BinaryBuilder {
                full_span: span,
                op_span: span,
                left,
                right,
                settings,
            };
            builder.add(true)
        })
        .and_then(|add| {
            ValueUnaryBuilder {
                full_span: span,
                op_span: span,
                inner: add,
                settings,
            }
            .dereference(true)
        })
}

fn function_call(
    fcall: &ast::FunctionCall,
    span: Span,
    settings: &Settings,
    scope: &mut FunctionScope,
) -> AggregateResult<ExprNode> {
    let Some(func) = scope.global.functions.get(&fcall.ident.data) else {
        return  AggregateResult::new_err(DiagnosticBuilder::new(fcall.ident.span).build_undeclared_function(&fcall.ident.data));
    };

    match fcall.args.len().cmp(&func.params.len()) {
        std::cmp::Ordering::Greater => {
            //check vararg
            if !func.is_vararg {
                return AggregateResult::new_err(
                    DiagnosticBuilder::new(span).build_wrong_amount_of_args(
                        fcall.args.len(),
                        func.params.len(),
                        func.original_span,
                        true,
                    ),
                );
            }
        }
        std::cmp::Ordering::Equal => {}
        std::cmp::Ordering::Less => {
            return AggregateResult::new_err(
                DiagnosticBuilder::new(span).build_wrong_amount_of_args(
                    fcall.args.len(),
                    func.params.len(),
                    func.original_span,
                    false,
                ),
            )
        }
    }

    let mut args_res = AggregateResult::new_ok(Vec::new());

    for (arg, param) in fcall
        .args
        .iter()
        .zip(func.params.iter().map(Some).chain(repeat(None)))
    {
        let arg = build_ir_expr(arg, settings, scope);

        arg.and_then(|arg| {
            let to_type = match param {
                Some(param) => {
                    // Parameters can be arrays be completely work like ptr's so do the conversion now
                    let out_ty = match &param.ty {
                        CType::Aggregate(ctype::Aggregate::Array(arr)) => {
                            CType::Scalar(ctype::Scalar::Pointer(ctype::Pointer {
                                inner: arr.inner.clone(),
                                inner_const: false,
                            }))
                        }
                        ty => ty.clone(),
                    };

                    let builder = DiagnosticBuilder::new(arg.span);
                    use super::type_checking::AssignCheckResult::*;
                    match check_assign(&out_ty, &arg.ty, settings) {
                        Ok => AggregateResult::new_ok(out_ty),
                        Lossy => AggregateResult::new_rec(
                            out_ty,
                            builder.build_implicit_lossy_arg(&arg, param, false),
                        ),
                        SignChange => AggregateResult::new_rec(
                            out_ty,
                            builder.build_implicit_lossy_arg(&arg, param, true),
                        ),
                        Incompatible | PointerAndInt => AggregateResult::new_rec(
                            out_ty,
                            builder.build_incompatible_arg(&arg, param),
                        ),
                        LossOfConst => AggregateResult::new_rec(
                            out_ty,
                            builder.build_arg_const_loss(arg.span, param.span),
                        ),
                        PointerAndFloat => {
                            AggregateResult::new_err(builder.build_incompatible_arg(&arg, param))
                        }
                        FromVoid => AggregateResult::new_err(builder.build_void_used(&arg)),
                        ToArray | FromArray => unreachable!(
                            "ICE: Array should have been converted to a pointer by now"
                        ),
                        ToVoid => unreachable!(
                            "ICE: Array should have been converted to a pointer by now"
                        ),
                    }
                }
                None => {
                    // This is a vararg
                    let out_ty = match &arg.ty {
                        CType::Scalar(ctype::Scalar::Arithmetic(a)) => {
                            if a.is_integral() {
                                let p = a.promote(settings);
                                CType::Scalar(ctype::Scalar::Arithmetic(p))
                            } else {
                                CType::Scalar(ctype::Scalar::Arithmetic(ctype::Arithmetic::Double))
                            }
                        }
                        other => other.clone(),
                    };
                    AggregateResult::new_ok(out_ty)
                }
            };
            to_type.map(|to_type| (arg, to_type))
        })
        .map(|(arg, to_type)| maybe_cast(arg, to_type))
        .add_to(&mut args_res, |res, a| res.push(a));
    }
    args_res.combine(function_ident(&fcall.ident, scope), |args, (id, func)| {
        ir::ExprNode {
            span,
            ty: func.return_type.clone(),
            expr: ir::Expr::FunctionCall(String::from(id), args),
        }
    })
}

fn lvalue_dereference(inner: LvalueExprNode) -> ExprNode {
    match &inner.ty {
        CType::Aggregate(ctype::Aggregate::Array(arr)) => ExprNode {
            span: inner.span,
            ty: CType::Scalar(ctype::Scalar::Pointer(ctype::Pointer {
                inner: arr.inner.clone(),
                inner_const: false,
            })),
            expr: Expr::LvalueDeref(Box::new(inner)),
        },
        _ => ExprNode {
            span: inner.span,
            ty: inner.ty.clone(),
            expr: Expr::LvalueDeref(Box::new(inner)),
        },
    }
}

fn variable_ident(
    idt: &ast::IdentNode,
    will_init: bool,
    scope: &mut FunctionScope,
) -> AggregateResult<LvalueExprNode> {
    let mut res = AggregateResult::new_ok(());

    let (expr, ty, is_const) = if let Some((id, ty)) = scope.vars.reference_mut(&idt.data) {
        // init checks are disabled for arrays since we can't check element by element (yet)
        if !will_init
            && !ty.initialized
            && !matches!(ty.ty, CType::Aggregate(ctype::Aggregate::Array(_)))
        {
            res.add_rec_diagnostic(DiagnosticBuilder::new(idt.span).build_usign_uninit(&idt.data));
        } else if will_init {
            ty.initialized = true;
        }
        (LvalueExpr::Ident(id), ty.ty.clone(), ty.is_const)
    } else if let Some(global_var) = scope.global.vars.get(&idt.data) {
        (
            LvalueExpr::GlobalIdent(idt.data.clone()),
            global_var.ty.clone(),
            global_var.is_const,
        )
    } else {
        return AggregateResult::new_err(
            DiagnosticBuilder::new(idt.span).build_undeclared_ident(&idt.data),
        );
    };

    res.map(|()| LvalueExprNode {
        span: idt.span,
        is_const,
        ty,
        expr,
    })
}

fn function_ident<'a, 'b, 'c, 'g: 'a>(
    ident_node: &'a ast::IdentNode,
    scope: &'b mut FunctionScope<'c, 'g>,
) -> AggregateResult<(&'a str, &'b ir::FunctionNode)> {
    scope
        .global
        .functions
        .get_key_value(&ident_node.data)
        .map(|(k, v)| AggregateResult::new_ok((k.as_str(), v)))
        .unwrap_or_else(|| {
            AggregateResult::new_err(
                DiagnosticBuilder::new(ident_node.span).build_undeclared_ident(&ident_node.data),
            )
        })
}

pub fn literal(lit: &ast::LiteralNode, settings: &Settings) -> AggregateResult<ExprNode> {
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
                expr: Expr::Constant(ir::expr::Constant::Integer(*i as i128)),
            })
        }
        ast::Literal::Float(value) => {
            return AggregateResult::new_ok(ExprNode {
                span: lit.span,
                ty: CType::Scalar(ctype::Scalar::Arithmetic(Double)),
                expr: Expr::Constant(ir::expr::Constant::Float(*value)),
            })
        }
        ast::Literal::String(s) => {
            let mut s = s.clone();
            s.push(0u8); // C strings always implicitly end with a NULL byte
            return AggregateResult::new_ok(ExprNode {
                span: lit.span,
                ty: CType::Scalar(ctype::Scalar::Pointer(ctype::Pointer {
                    inner: Box::new(CType::Scalar(ctype::Scalar::Arithmetic(Char))),
                    inner_const: true,
                })),
                expr: Expr::Constant(ir::expr::Constant::String(s)),
            });
        }
    };

    match find_first_fit(value, pos_types, settings) {
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
        (CType::Scalar(Scalar::Pointer(_)), CType::Scalar(Scalar::Arithmetic(a)))
            if a.is_floating() =>
        {
            InvalidCastReason::PointerFromFloat(&inner)
        }
        (CType::Scalar(Scalar::Arithmetic(a)), CType::Scalar(Scalar::Pointer(_)))
            if a.is_floating() =>
        {
            InvalidCastReason::FloatFromPointer(&inner)
        }
        (CType::Scalar(_) | CType::Void, CType::Scalar(_)) => {
            return AggregateResult::new_ok(ExprNode {
                span,
                ty: to_ty,
                expr: Expr::Cast(Box::new(inner)),
            })
        }
        (CType::Void, CType::Void) => {
            return AggregateResult::new_ok(inner);
        }
        (CType::Scalar(_), _) => InvalidCastReason::FromNonScaler(&inner),
        (_, CType::Scalar(_)) => InvalidCastReason::IntoNonScalar,
        (_, _) => InvalidCastReason::BothNonScalar(&inner),
    };
    AggregateResult::new_err(DiagnosticBuilder::new(op_span).build_invalid_cast(reason))
}

pub fn assign(
    to: LvalueExprNode,
    from: ExprNode,
    span: Span,
    op_span: Span,
    settings: &Settings,
) -> AggregateResult<ExprNode> {
    let mut res = AggregateResult::new_ok(());
    if to.is_const {
        res.add_err(DiagnosticBuilder::new(op_span).build_cant_be_const("assign to", to.span));
    }

    use super::type_checking::AssignCheckResult::*;
    let builder = DiagnosticBuilder::new(op_span);
    match check_assign(&to.ty, &from.ty, settings) {
        Ok => {}
        Lossy => res.add_rec_diagnostic(builder.build_implicit_lossy_assign(&from, &to, false)),
        SignChange => res.add_rec_diagnostic(builder.build_implicit_lossy_assign(&from, &to, true)),
        Incompatible => res.add_rec_diagnostic(builder.build_incompatible_assign(&from, &to)),
        LossOfConst => res.add_rec_diagnostic(builder.build_assign_const_loss(from.span, to.span)),
        PointerAndInt => res.add_rec_diagnostic(builder.build_incompatible_assign(&from, &to)),
        PointerAndFloat => res.add_err(builder.build_incompatible_assign(&from, &to)),
        ToArray => res.add_err(builder.build_assign_to_array(&to)),
        FromVoid => res.add_err(builder.build_void_used(&from)),
        FromArray => unreachable!("ICE: Array should have been converted to a pointer by now"),
        ToVoid => unreachable!("ICE: Lvalue with void type should not exist"),
    }

    let to_type = to.ty.clone();
    res.map(|()| ExprNode {
        span,
        ty: to_type.clone(),
        expr: Expr::Assign(Box::new(to), Box::new(maybe_cast(from, to_type))),
    })
}

struct UnaryBuilder<'a, 'b, 'g> {
    full_span: Span,
    op_span: Span,
    inner: &'a ast::ExpressionNode,
    settings: &'a Settings,
    scope: &'a mut FunctionScope<'b, 'g>,
}

struct ValueUnaryBuilder<'s> {
    full_span: Span,
    op_span: Span,
    inner: ExprNode,
    settings: &'s Settings,
}

enum LvalueBuildErr {
    NoConst,
    // Would be used if non scalar types are suported.
    WrongType(TypeCat),
}

impl<'a, 'b, 'g> UnaryBuilder<'a, 'b, 'g> {
    fn lvalue_build<R, F>(self, rule: R, name: &str, build: F) -> AggregateResult<ExprNode>
    where
        R: FnOnce(&CType, bool) -> Result<CType, LvalueBuildErr>,
        F: FnOnce(Box<LvalueExprNode>) -> Expr,
    {
        let res = build_ir_lvalue(
            self.inner,
            name,
            false,
            self.op_span,
            self.settings,
            self.scope,
        );

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
                    LvalueBuildErr::WrongType(type_cat) => {
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
                    CType::Aggregate(_) => Err(LvalueBuildErr::WrongType(TypeCat::Scalar)),
                    CType::Void => Err(LvalueBuildErr::WrongType(TypeCat::Scalar)),
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
                Ok(CType::Scalar(ctype::Scalar::Pointer(ctype::Pointer {
                    inner: Box::new(ty.clone()),
                    inner_const: is_const,
                })))
            },
            "reference",
            Expr::Reference,
        )
    }

    // Also turn arrays into ptrs
    fn value(self) -> AggregateResult<ValueUnaryBuilder<'a>> {
        let res = build_ir_expr(self.inner, self.settings, self.scope);
        res.map(|inner| ValueUnaryBuilder {
            full_span: self.full_span,
            op_span: self.op_span,
            inner,
            settings: self.settings,
        })
    }
}

impl<'s> ValueUnaryBuilder<'s> {
    fn build<R, F>(self, rule: R, name: &str, build: F) -> AggregateResult<ExprNode>
    where
        R: TypeRuleUn,
        F: FnOnce(Box<ExprNode>) -> Expr,
    {
        match rule.check(&self.inner.ty, self.settings) {
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
    fn dereference(self, from_array_subscription: bool) -> AggregateResult<LvalueExprNode> {
        let name = match from_array_subscription {
            true => "array subscription",
            false => "dereference",
        };
        match &self.inner.ty {
            CType::Scalar(ctype::Scalar::Pointer(ctype::Pointer {
                inner: ref pointed_to_ty,
                inner_const: is_const,
            })) => {
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
                    name,
                    TypeCat::Pointer,
                    &self.inner,
                ),
            ),
        }
    }
}

struct BinaryBuilder<'s> {
    full_span: Span,
    op_span: Span,
    left: ExprNode,
    right: ExprNode,
    settings: &'s Settings,
}

impl<'s> BinaryBuilder<'s> {
    fn build<R, F>(self, rule: R, name: &str, build: F) -> AggregateResult<ExprNode>
    where
        R: TypeRuleBin,
        F: FnOnce(Box<ExprNode>, Box<ExprNode>) -> Expr,
    {
        match rule.check(&self.left.ty, &self.right.ty, self.settings) {
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
                    CheckBinErr::Unknown => {
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
    fn add(self, from_array_subscript: bool) -> AggregateResult<ExprNode> {
        let rule = UsualArithConversions::new().or(PointerInteger::new());
        let name = match from_array_subscript {
            true => "array subscription",
            false => BinaryOp::Add.long_name(),
        };

        self.build(rule, name, |l, r| Expr::Binary(l, BinaryOp::Add, r))
    }

    /// See [`BinaryOp::Sub`]
    fn sub(self) -> AggregateResult<ExprNode> {
        // TODO SignedLongInt chosen since that is what g++ uses but should probably be
        // platform dependent, this represents the maximum length between two pointers
        let pointer_pointer_distance_ty =
            CType::Scalar(ctype::Scalar::Arithmetic(ctype::Arithmetic::SignedLongInt));

        let rule = UsualArithConversions::new()
            .or(PointerInteger::only_ptr_first())
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
