use std::fmt::Debug;

use super::{expr::build_ir_expr, expr::build_ir_lvalue, symbol_table::ScopedTable};
use crate::{
    ast::*,
    diagnostic::{AggregateResult, Code},
    ir::{
        ctype::{Arithmetic::*, CType::*, Scalar::*},
        expr::{ExprNode, LvalueExprNode},
    },
    passes::lower_ast::symbol_table::ScopedHandle,
    structures::ir::*,
};

fn create_unary_op(op: UnaryOperator, inner: Expression) -> ExpressionNode {
    ExpressionNode {
        span: (0..0).into(),
        data: Expression::Unary(
            UnaryOperatorNode {
                span: (1..1).into(),
                data: op,
            },
            Box::new(ExpressionNode {
                span: (2..2).into(),
                data: inner,
            }),
        ),
    }
}

/// Panics if the AggregateResult does *NOT* have a diagnostic with the specified code. The
/// AggregateResult can have diagnostics with other codes but has to have one with the specified
/// one to not panic
#[track_caller]
fn has_error(res: AggregateResult<expr::ExprNode>, code: Code) {
    let found = res.diagnostics().any(|d| d.1.code() == &code);
    if !found {
        let diagnostics: Vec<_> = res.into_diagnostics().collect();

        if diagnostics.is_empty() {
            eprintln!("No diagnostics given");
        } else {
            eprintln!(
                "diagnostics: {}",
                diagnostics
                    .into_iter()
                    .map(|d| format!("{:?}", d.1))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        }

        panic!("Didn't find diagnostic with code: {:?}", code);
    }
}

/// Paincs if the AggregateResult does not have a value, or if the T's don't match, also print the
/// diagnostics then
#[track_caller]
fn aggr_res_eq<T: PartialEq + Debug>(res: AggregateResult<T>, exp: T) {
    if res.value() != Some(&exp) {
        let diagnostics: Vec<_> = res.diagnostics().collect();

        if diagnostics.is_empty() {
            eprintln!("No diagnostics given");
        } else {
            eprintln!(
                "diagnostics: {}",
                diagnostics
                    .into_iter()
                    .map(|d| format!("{:?}", d.1))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
        }

        assert_eq!(res.value(), Some(&exp))
    }
}

/// For every item in the iterator, creates a `ast::ExpresionNode` with the op from `un_op` and a
/// inner ident with the type from the iterator. This ast is then given to `build_ir_expr`. If
/// the given AggregateResult is _err_ the `Option` in the iterator has to be `None`, otherwise
/// panics. Else checks whether the value in the AggregateResult has the type in the `Option` from
/// the iterator
#[track_caller]
fn check_type_un<I>(tests: I, un_op: UnaryOperator)
where
    I: IntoIterator<Item = (ctype::CType, Option<ctype::CType>)>,
{
    let mut scope = ScopedTable::default();
    for t in tests {
        let mut scope = scope.get_scoped_handle();

        scope
            .declare(
                "A".to_owned(),
                table::Item {
                    ty: t.0,
                    is_const: false,
                    original_span: (4..4).into(),
                    initialized: true,
                },
            )
            .unwrap();

        let ast = create_unary_op(
            un_op.clone(),
            Expression::Ident(IdentNode {
                span: (3..3).into(),
                data: "A".to_owned(),
            }),
        );

        let out_expr = build_ir_expr(&ast, &mut scope);

        match t.1 {
            Some(out_ty) => {
                if let Some(out_expr) = out_expr.value() {
                    if out_expr.ty != out_ty {
                        panic!("Got type: {}, excpected type: {}", out_expr.ty, out_ty);
                    }
                } else {
                    panic!(
                        "Expected expression with type: {}. But got these diagnostics:\n{}",
                        out_ty,
                        out_expr
                            .diagnostics()
                            .map(|d| format!("{:?}", d.1))
                            .collect::<Vec<_>>()
                            .join("\n")
                    )
                }
            }
            None => has_error(out_expr, Code::UnexpectedType),
        }
    }
}

/// For every item in the iterator, creates a `ast::ExpresionNode` with the op from `bin_op` and
/// two inner idents with the type from the iterator. This ast is then given to `build_ir_expr`. If
/// the given AggregateResult is _err_ the `Option` in the iterator has to be `None`, otherwise
/// panics. Else checks whether the value in the AggregateResult has the type in the `Option` from
/// the iterator
#[track_caller]
fn check_type_bin<I>(tests: I, bin_op: BinaryOperator)
where
    I: IntoIterator<Item = (ctype::CType, ctype::CType, Option<ctype::CType>)>,
{
    let mut scope = ScopedTable::default();
    for (i, t) in tests.into_iter().enumerate() {
        println!("testing index {}", i);
        let mut scope = scope.get_scoped_handle();

        scope
            .declare(
                "A".to_owned(),
                table::Item {
                    ty: t.0,
                    is_const: false,
                    original_span: (0..0).into(),
                    initialized: true,
                },
            )
            .unwrap();
        scope
            .declare(
                "B".to_owned(),
                table::Item {
                    ty: t.1,
                    is_const: false,
                    original_span: (0..0).into(),
                    initialized: true,
                },
            )
            .unwrap();

        let ast = {
            let inner_a = Expression::Ident(IdentNode {
                span: (3..3).into(),
                data: "A".to_owned(),
            });
            let inner_b = Expression::Ident(IdentNode {
                span: (3..3).into(),
                data: "B".to_owned(),
            });
            ExpressionNode {
                span: (0..0).into(),
                data: Expression::Binary(
                    Box::new(ExpressionNode {
                        span: (2..2).into(),
                        data: inner_a,
                    }),
                    BinaryOperatorNode {
                        span: (1..1).into(),
                        data: bin_op.clone(),
                    },
                    Box::new(ExpressionNode {
                        span: (2..2).into(),
                        data: inner_b,
                    }),
                ),
            }
        };

        let out_expr = build_ir_expr(&ast, &mut scope);

        match t.2 {
            Some(out_ty) => {
                if let Some(out_expr) = out_expr.value() {
                    if out_expr.ty != out_ty {
                        panic!("Got type: {}, excpected type: {}", out_expr.ty, out_ty);
                    }
                } else {
                    panic!(
                        "Expected expression with type: {}. But got these diagnostics:\n{}",
                        out_ty,
                        out_expr
                            .diagnostics()
                            .map(|d| format!("{:?}", d.1))
                            .collect::<Vec<_>>()
                            .join("\n")
                    )
                }
            }
            None => has_error(out_expr, Code::UnexpectedType),
        }
    }
}

#[test]
fn cast() {
    fn build_ir(
        to_ty: QualifiedType,
        from_ty: ctype::CType,
        scope: &mut ScopedHandle,
    ) -> (AggregateResult<ExprNode>, LvalueExprNode) {
        let mut scope = scope._new_scope();
        let from_id = scope
            .declare(
                "FROM".to_owned(),
                table::Item {
                    ty: from_ty.clone(),
                    is_const: false,
                    original_span: (1..1).into(),
                    initialized: true,
                },
            )
            .unwrap();

        let ast_lit = {
            ExpressionNode {
                span: (0..0).into(),
                data: Expression::Cast(
                    QualifiedTypeNode {
                        span: (1..1).into(),
                        data: to_ty,
                    },
                    Box::new(ExpressionNode {
                        span: (3..3).into(),
                        data: Expression::Ident(IdentNode {
                            span: (3..3).into(),
                            data: "FROM".to_owned(),
                        }),
                    }),
                ),
            }
        };
        let ir = build_ir_expr(&ast_lit, &mut scope);
        let expect_from_expr = LvalueExprNode {
            span: (3..3).into(),
            is_const: false,
            ty: from_ty,
            expr: expr::LvalueExpr::Ident(from_id),
        };
        (ir, expect_from_expr)
    }

    let mut scope = ScopedTable::default();
    let mut scope = scope.get_scoped_handle();

    let ast_int = QualifiedTypeNode {
        span: (1..1).into(),
        data: QualifiedType {
            is_const: None,
            inner: UnqualifiedTypeNode {
                span: (1..1).into(),
                data: UnqualifiedType::PlainType(PlainType::Primitive(PrimitiveType::Int)),
            },
        },
    };

    let ast_ptr_int = QualifiedType {
        is_const: None,
        inner: UnqualifiedTypeNode {
            span: (1..1).into(),
            data: UnqualifiedType::PointerType(Box::new(ast_int)),
        },
    };

    // Can't cast floating to pointer
    let ir = build_ir(ast_ptr_int.clone(), Scalar(Arithmetic(Float)), &mut scope).0;
    assert_eq!(ir.value(), None);
    has_error(ir, Code::InvalidCast);

    let (ir, expect_from_expr) = build_ir(ast_ptr_int, Scalar(Arithmetic(SignedInt)), &mut scope);
    aggr_res_eq(
        ir,
        ExprNode {
            span: (0..0).into(),
            ty: Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            expr: expr::Expr::Cast(Box::new(ExprNode {
                span: (3..3).into(),
                ty: Scalar(Arithmetic(SignedInt)),
                expr: expr::Expr::LvalueDeref(Box::new(expect_from_expr)),
            })),
        },
    );
}

#[track_caller]
fn post_pre_fix_inc_dec<F>(ast: UnaryOperator, make_ir_expr: F)
where
    F: FnOnce(Box<expr::LvalueExprNode>) -> expr::Expr,
{
    let mut scope = ScopedTable::default();
    let mut scope = scope.get_scoped_handle();

    let ast_lit = create_unary_op(
        ast.clone(),
        Expression::Literal(LiteralNode {
            span: (3..3).into(),
            data: Literal::Dec(10),
        }),
    );
    let ir = build_ir_expr(&ast_lit, &mut scope);
    assert_eq!(ir.value(), None);
    has_error(ir, Code::NeedLvalue);

    let ty = Scalar(Arithmetic(Char));

    let id_a = scope
        .declare(
            "A".to_owned(),
            table::Item {
                ty: ty.clone(),
                is_const: false,
                original_span: (4..4).into(),
                initialized: true,
            },
        )
        .unwrap();
    let ast_ident = create_unary_op(
        ast.clone(),
        Expression::Ident(IdentNode {
            span: (3..3).into(),
            data: "A".to_owned(),
        }),
    );
    let ir = build_ir_expr(&ast_ident, &mut scope);
    aggr_res_eq(
        ir,
        ExprNode {
            span: (0..0).into(),
            ty: ty.clone(),
            expr: make_ir_expr(Box::new(expr::LvalueExprNode {
                span: (3..3).into(),
                is_const: false,
                ty: ty.clone(),
                expr: expr::LvalueExpr::Ident(id_a),
            })),
        },
    );
    scope
        .declare(
            "B".to_owned(),
            table::Item {
                ty,
                is_const: true,
                original_span: (4..4).into(),
                initialized: true,
            },
        )
        .unwrap();
    let ast_ident = create_unary_op(
        ast,
        Expression::Ident(IdentNode {
            span: (3..3).into(),
            data: "B".to_owned(),
        }),
    );
    let ir = build_ir_expr(&ast_ident, &mut scope);
    assert_eq!(ir.value(), None);
    has_error(ir, Code::NeedConst)
}

#[test]
fn postfix_inc() {
    post_pre_fix_inc_dec(UnaryOperator::DoublePlusPrefix, expr::Expr::PrefixInc);
}

#[test]
fn prefix_inc() {
    post_pre_fix_inc_dec(UnaryOperator::DoublePlusPostfix, expr::Expr::PostfixInc)
}

#[test]
fn postfix_dec() {
    post_pre_fix_inc_dec(UnaryOperator::DoubleMinusPostfix, expr::Expr::PostfixDec);
}

#[test]
fn prefix_dec() {
    post_pre_fix_inc_dec(UnaryOperator::DoubleMinusPrefix, expr::Expr::PrefixDec);
}

#[test]
fn refrence() {
    let mut scope = ScopedTable::default();
    let mut scope = scope.get_scoped_handle();

    let ast_lit = create_unary_op(
        UnaryOperator::Ampersand,
        Expression::Literal(LiteralNode {
            span: (3..3).into(),
            data: Literal::Dec(10),
        }),
    );
    let ir = build_ir_expr(&ast_lit, &mut scope);
    assert_eq!(ir.value(), None);
    has_error(ir, Code::NeedLvalue);

    let ty = Scalar(Arithmetic(Char));

    for is_const in [false, true] {
        let mut scope = scope._new_scope();
        let id_a = scope
            .declare(
                "A".to_owned(),
                table::Item {
                    ty: ty.clone(),
                    is_const,
                    original_span: (4..4).into(),
                    initialized: true,
                },
            )
            .unwrap();
        let ast_ident = create_unary_op(
            UnaryOperator::Ampersand,
            Expression::Ident(IdentNode {
                span: (3..3).into(),
                data: "A".to_owned(),
            }),
        );
        let ir = build_ir_expr(&ast_ident, &mut scope);
        aggr_res_eq(
            ir,
            ExprNode {
                span: (0..0).into(),
                ty: Scalar(Pointer(Box::new(ty.clone()), is_const)),
                expr: expr::Expr::Reference(Box::new(expr::LvalueExprNode {
                    span: (3..3).into(),
                    is_const,
                    ty: ty.clone(),
                    expr: expr::LvalueExpr::Ident(id_a),
                })),
            },
        );
    }
}

#[test]
fn derefrence() {
    let mut scope = ScopedTable::default();
    let mut scope = scope.get_scoped_handle();

    let ast_lit = create_unary_op(
        UnaryOperator::Star,
        Expression::Literal(LiteralNode {
            span: (3..3).into(),
            data: Literal::Dec(10),
        }),
    );
    let ir = build_ir_expr(&ast_lit, &mut scope);
    assert_eq!(ir.value(), None);
    has_error(ir, Code::UnexpectedType);

    let ty = Scalar(Arithmetic(Char));

    for is_const in [true, false] {
        let mut scope = scope._new_scope();

        let ty_with_ptr = Scalar(Pointer(Box::new(ty.clone()), is_const));

        let id_a = scope
            .declare(
                "A".to_owned(),
                table::Item {
                    ty: ty_with_ptr.clone(),
                    is_const,
                    original_span: (4..4).into(),
                    initialized: true,
                },
            )
            .unwrap();
        let ast_ident = create_unary_op(
            UnaryOperator::Star,
            Expression::Ident(IdentNode {
                span: (3..3).into(),
                data: "A".to_owned(),
            }),
        );

        let lvalue_epxr = LvalueExprNode {
            span: (0..0).into(),
            is_const,
            ty: ty.clone(),
            expr: expr::LvalueExpr::Dereference(Box::new(expr::ExprNode {
                span: (3..3).into(),
                ty: ty_with_ptr.clone(),
                expr: expr::Expr::LvalueDeref(Box::new(LvalueExprNode {
                    span: (3..3).into(),
                    is_const,
                    ty: ty_with_ptr,
                    expr: expr::LvalueExpr::Ident(id_a),
                })),
            })),
        };

        let ir = build_ir_lvalue(&ast_ident, "", false, (5..5).into(), &mut scope);
        aggr_res_eq(ir, lvalue_epxr.clone());

        let ir = build_ir_expr(&ast_ident, &mut scope);
        aggr_res_eq(
            ir,
            ExprNode {
                span: (0..0).into(),
                ty: ty.clone(),
                expr: expr::Expr::LvalueDeref(Box::new(lvalue_epxr)),
            },
        );
    }
}
#[test]
fn un_plus_min() {
    let tests = [
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            None,
        ),
        (
            Scalar(Arithmetic(SignedInt)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(SignedLongInt)),
            Some(Scalar(Arithmetic(SignedLongInt))),
        ),
        (
            Scalar(Arithmetic(Char)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (Scalar(Arithmetic(Float)), Some(Scalar(Arithmetic(Float)))),
    ];
    check_type_un(tests.clone(), UnaryOperator::Plus);
    check_type_un(tests, UnaryOperator::Minus);
}

#[test]
fn un_bit_not() {
    let tests = [
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            None,
        ),
        (
            Scalar(Arithmetic(SignedInt)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(SignedLongInt)),
            Some(Scalar(Arithmetic(SignedLongInt))),
        ),
        (
            Scalar(Arithmetic(Char)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (Scalar(Arithmetic(Float)), None),
    ];
    check_type_un(tests, UnaryOperator::Tilde);
}

#[test]
fn un_not() {
    let tests = [
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(SignedInt)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(SignedLongInt)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(Char)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(Float)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
    ];
    check_type_un(tests, UnaryOperator::Bang);
}

#[test]
fn times_div() {
    let tests = [
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            None,
        ),
        (
            Scalar(Arithmetic(SignedInt)),
            Scalar(Arithmetic(Char)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(UnsignedLongInt)),
            Scalar(Arithmetic(SignedLongInt)),
            Some(Scalar(Arithmetic(UnsignedLongInt))),
        ),
        (
            Scalar(Arithmetic(Char)),
            Scalar(Arithmetic(Char)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(Double)),
            Scalar(Arithmetic(Float)),
            Some(Scalar(Arithmetic(Double))),
        ),
    ];
    check_type_bin(tests.clone(), BinaryOperator::Star);
    check_type_bin(tests, BinaryOperator::Slash);
}

#[test]
fn times_mod() {
    let tests = [
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            None,
        ),
        (
            Scalar(Arithmetic(SignedInt)),
            Scalar(Arithmetic(Char)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(UnsignedLongInt)),
            Scalar(Arithmetic(SignedLongInt)),
            Some(Scalar(Arithmetic(UnsignedLongInt))),
        ),
        (
            Scalar(Arithmetic(Char)),
            Scalar(Arithmetic(Char)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(SignedInt)),
            Scalar(Arithmetic(Float)),
            None,
        ),
        (Scalar(Arithmetic(Double)), Scalar(Arithmetic(Float)), None),
    ];
    check_type_bin(tests, BinaryOperator::Percent);
}

#[test]
fn add() {
    let tests = [
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            None,
        ),
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Scalar(Arithmetic(Char)),
            Some(Scalar(Pointer(
                Box::new(Scalar(Arithmetic(SignedInt))),
                false,
            ))),
        ),
        (
            Scalar(Arithmetic(Char)),
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Some(Scalar(Pointer(
                Box::new(Scalar(Arithmetic(SignedInt))),
                false,
            ))),
        ),
        (
            Scalar(Arithmetic(UnsignedLongInt)),
            Scalar(Arithmetic(SignedLongInt)),
            Some(Scalar(Arithmetic(UnsignedLongInt))),
        ),
        (
            Scalar(Arithmetic(Char)),
            Scalar(Arithmetic(Char)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(Double)),
            Scalar(Arithmetic(Float)),
            Some(Scalar(Arithmetic(Double))),
        ),
    ];
    check_type_bin(tests, BinaryOperator::Plus);
}

#[test]
fn sub() {
    let tests = [
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Some(Scalar(Arithmetic(SignedLongInt))),
        ),
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Scalar(Arithmetic(Char)),
            Some(Scalar(Pointer(
                Box::new(Scalar(Arithmetic(SignedInt))),
                false,
            ))),
        ),
        (
            Scalar(Arithmetic(Char)),
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            None,
        ),
        (
            Scalar(Arithmetic(UnsignedLongInt)),
            Scalar(Arithmetic(SignedLongInt)),
            Some(Scalar(Arithmetic(UnsignedLongInt))),
        ),
        (
            Scalar(Arithmetic(Char)),
            Scalar(Arithmetic(Char)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(Double)),
            Scalar(Arithmetic(Float)),
            Some(Scalar(Arithmetic(Double))),
        ),
    ];
    check_type_bin(tests, BinaryOperator::Minus);
}

#[test]
fn relations() {
    let tests = [
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Scalar(Pointer(
                Box::new(Scalar(Arithmetic(UnsignedLongInt))),
                false,
            )),
            None,
        ),
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(Float))), false)),
            Scalar(Pointer(
                Box::new(Scalar(Arithmetic(UnsignedLongInt))),
                false,
            )),
            None,
        ),
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Scalar(Arithmetic(Char)),
            None,
        ),
        (
            Scalar(Arithmetic(Char)),
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            None,
        ),
        (
            Scalar(Arithmetic(UnsignedLongInt)),
            Scalar(Arithmetic(SignedLongInt)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(Char)),
            Scalar(Arithmetic(Char)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(Double)),
            Scalar(Arithmetic(Float)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
    ];
    check_type_bin(tests.clone(), BinaryOperator::AngleLeft);
    check_type_bin(tests.clone(), BinaryOperator::AngleRight);
    check_type_bin(tests.clone(), BinaryOperator::AngleLeftEquals);
    check_type_bin(tests.clone(), BinaryOperator::AngleRightEquals);
    check_type_bin(tests.clone(), BinaryOperator::DoubleEquals);
    check_type_bin(tests, BinaryOperator::BangEquals);
}

#[test]
fn bitwise() {
    let tests = [
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            None,
        ),
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Scalar(Arithmetic(Char)),
            None,
        ),
        (
            Scalar(Arithmetic(Char)),
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            None,
        ),
        (
            Scalar(Arithmetic(SignedInt)),
            Scalar(Arithmetic(SignedInt)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(UnsignedLongInt)),
            Scalar(Arithmetic(SignedLongInt)),
            Some(Scalar(Arithmetic(UnsignedLongInt))),
        ),
        (
            Scalar(Arithmetic(Char)),
            Scalar(Arithmetic(Char)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (Scalar(Arithmetic(Double)), Scalar(Arithmetic(Float)), None),
    ];
    check_type_bin(tests.clone(), BinaryOperator::Ampersand);
    check_type_bin(tests.clone(), BinaryOperator::Pipe);
    check_type_bin(tests, BinaryOperator::Caret);
}

#[test]
fn logical() {
    let tests = [
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Scalar(Arithmetic(Char)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(Char)),
            Scalar(Pointer(Box::new(Scalar(Arithmetic(SignedInt))), false)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(SignedInt)),
            Scalar(Arithmetic(SignedInt)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(UnsignedLongInt)),
            Scalar(Arithmetic(SignedLongInt)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(Char)),
            Scalar(Arithmetic(Char)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
        (
            Scalar(Arithmetic(Double)),
            Scalar(Arithmetic(Float)),
            Some(Scalar(Arithmetic(SignedInt))),
        ),
    ];
    check_type_bin(tests.clone(), BinaryOperator::DoubleAmpersand);
    check_type_bin(tests, BinaryOperator::DoublePipe);
}

#[test]
fn assign() {
    fn build_ir(
        to: Expression,
        from: Expression,
        scope: &mut ScopedHandle,
    ) -> AggregateResult<ExprNode> {
        let ast_lit = {
            ExpressionNode {
                span: (0..0).into(),
                data: Expression::Assignment(
                    Box::new(ExpressionNode {
                        span: (2..2).into(),
                        data: to,
                    }),
                    AssignmentOperatorNode {
                        span: (4..4).into(),
                    },
                    Box::new(ExpressionNode {
                        span: (3..3).into(),
                        data: from,
                    }),
                ),
            }
        };
        build_ir_expr(&ast_lit, scope)
    }
    fn build_ir_ty(
        to_ty: ctype::CType,
        to_const: bool,
        from_ty: ctype::CType,
        scope: &mut ScopedHandle,
    ) -> (AggregateResult<ExprNode>, LvalueExprNode, LvalueExprNode) {
        let mut scope = scope._new_scope();
        let to_id = scope
            .declare(
                "TO".to_owned(),
                table::Item {
                    ty: to_ty.clone(),
                    is_const: to_const,
                    original_span: (1..1).into(),
                    initialized: true,
                },
            )
            .unwrap();
        let from_id = scope
            .declare(
                "FROM".to_owned(),
                table::Item {
                    ty: from_ty.clone(),
                    is_const: false,
                    original_span: (1..1).into(),
                    initialized: true,
                },
            )
            .unwrap();

        let ast_lit = {
            ExpressionNode {
                span: (0..0).into(),
                data: Expression::Assignment(
                    Box::new(ExpressionNode {
                        span: (2..2).into(),
                        data: Expression::Ident(IdentNode {
                            span: (2..2).into(),
                            data: "TO".to_owned(),
                        }),
                    }),
                    AssignmentOperatorNode {
                        span: (4..4).into(),
                    },
                    Box::new(ExpressionNode {
                        span: (3..3).into(),
                        data: Expression::Ident(IdentNode {
                            span: (3..3).into(),
                            data: "FROM".to_owned(),
                        }),
                    }),
                ),
            }
        };
        let ir = build_ir_expr(&ast_lit, &mut scope);
        let expect_to_expr = LvalueExprNode {
            span: (2..2).into(),
            is_const: to_const,
            ty: to_ty,
            expr: expr::LvalueExpr::Ident(to_id),
        };
        let expect_from_expr = LvalueExprNode {
            span: (3..3).into(),
            is_const: false,
            ty: from_ty,
            expr: expr::LvalueExpr::Ident(from_id),
        };
        (ir, expect_to_expr, expect_from_expr)
    }

    let mut scope = ScopedTable::default();
    let mut scope = scope.get_scoped_handle();

    let ir = build_ir(
        Expression::Literal(LiteralNode {
            span: (5..5).into(),
            data: Literal::Dec(10),
        }),
        Expression::Literal(LiteralNode {
            span: (5..5).into(),
            data: Literal::Dec(10),
        }),
        &mut scope,
    );
    assert_eq!(ir.value(), None);
    has_error(ir, Code::NeedLvalue);

    let ir = build_ir_ty(
        Scalar(Arithmetic(Char)),
        true,
        Scalar(Arithmetic(Char)),
        &mut scope,
    )
    .0;
    assert_eq!(ir.value(), None);
    has_error(ir, Code::NeedConst);

    let (ir, expect_to_expr, expect_from_expr) = build_ir_ty(
        Scalar(Arithmetic(Char)),
        false,
        Scalar(Arithmetic(Char)),
        &mut scope,
    );
    aggr_res_eq(
        ir,
        ExprNode {
            span: (0..0).into(),
            ty: Scalar(Arithmetic(Char)),
            expr: expr::Expr::Assign(
                Box::new(expect_to_expr),
                Box::new(ExprNode {
                    span: (3..3).into(),
                    ty: Scalar(Arithmetic(Char)),
                    expr: expr::Expr::LvalueDeref(Box::new(expect_from_expr)),
                }),
            ),
        },
    );

    let (ir, expect_to_expr, expect_from_expr) = build_ir_ty(
        Scalar(Arithmetic(Char)),
        false,
        Scalar(Arithmetic(Double)),
        &mut scope,
    );
    aggr_res_eq(
        ir,
        ExprNode {
            span: (0..0).into(),
            ty: Scalar(Arithmetic(Char)),
            expr: expr::Expr::Assign(
                Box::new(expect_to_expr),
                Box::new(ExprNode {
                    span: (3..3).into(),
                    ty: Scalar(Arithmetic(Char)),
                    expr: expr::Expr::Cast(Box::new(ExprNode {
                        span: (3..3).into(),
                        ty: Scalar(Arithmetic(Double)),
                        expr: expr::Expr::LvalueDeref(Box::new(expect_from_expr)),
                    })),
                }),
            ),
        },
    );
}
