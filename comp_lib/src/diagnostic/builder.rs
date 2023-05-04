use super::{Code, Diagnostic, Span};
use crate::ir;

impl DiagnosticBuilder {
    pub fn new(span: impl Into<Span>) -> Self {
        Self {
            span: span.into(),
            additional_spans: Vec::new(),
        }
    }

    pub fn with_additional_span(mut self, span: impl Into<Span>, message: Option<String>) -> Self {
        self.add_additional_span(span, message);
        self
    }

    pub fn add_additional_span(&mut self, span: impl Into<Span>, message: Option<String>) {
        self.additional_spans.push((span.into(), message));
    }

    pub fn with_ir_expr_type(mut self, expr: &ir::expr::ExprNode) -> Self {
        self.add_ir_expr_type(expr);
        self
    }

    pub fn add_ir_expr_type(&mut self, expr: &ir::expr::ExprNode) {
        self.add_additional_span(
            expr.span,
            Some(format!("this expression has type: `{}`", expr.ty)),
        );
    }

    pub fn add_ir_expr_type_lvalue(&mut self, expr: &ir::expr::LvalueExprNode) {
        self.add_additional_span(
            expr.span,
            Some(format!("this expression has type: `{}`", expr.ty)),
        );
    }

    pub fn add_ir_return_type(&mut self, span: Span, ty: &ir::ctype::CType) {
        self.add_additional_span(
            span,
            Some(format!("return type specified to be `{}` here", ty)),
        );
    }

    fn add_function_def(&mut self, span: Span) {
        self.add_additional_span(span, Some("function defined here".to_owned()));
    }

    fn add_ir_param_type(&mut self, param: &ir::FunctionParamNode) {
        self.add_additional_span(param.span, Some("parameter defined here".to_owned()));
    }

    fn build_custom(self, code: Code, message: String) -> Diagnostic {
        Diagnostic {
            code,
            message,
            main_span: (self.span, None),
            additional_spans: self.additional_spans,
        }
    }

    pub fn build_syntax_error(self, unexpected: &str, expected: Vec<String>) -> Diagnostic {
        let message = if expected.is_empty() {
            format!("unexpected token: {unexpected}")
        } else {
            let expected = expected.join(", ");
            format!("unexpected token: {unexpected}, expected one of: {expected}")
        };
        self.build_custom(Code::SyntaxError, message)
    }

    pub fn build_unimplemented(self, feature: &str) -> Diagnostic {
        self.build_custom(Code::Unimplemented, format!("not implemented: {feature}"))
    }

    pub fn build_multiple_defaults(mut self, first_seen: Span) -> Diagnostic {
        self.add_additional_span(first_seen, Some("first case here".to_string()));
        let msg = "multiple default cases in switch".to_string();
        self.build_custom(Code::DuplicateDefault, msg)
    }

    pub fn build_multiple_qualifiers(mut self, qualifier: &str, first_seen: Span) -> Diagnostic {
        self.add_additional_span(first_seen, Some("first seen here".to_string()));
        let msg = format!("`{qualifier}` qualifier used multiple times");
        self.build_custom(Code::DuplicateQualifier, msg)
    }

    pub fn build_incompatible_specifiers(
        mut self,
        spec1: &str,
        spec2: &str,
        other: Span,
    ) -> Diagnostic {
        self.add_additional_span(other, None);
        let msg = if spec1 == spec2 {
            format!("`{spec1}` specifier used multiple times")
        } else {
            format!("`{spec1}` and `{spec2}` can't be used together")
        };
        self.build_custom(Code::IncompatibleSpecifiers, msg)
    }

    pub fn build_multi_byte_char(self) -> Diagnostic {
        let msg = "multi-byte chars are implementation defined".to_owned();
        self.build_custom(Code::MultiByteChar, msg)
    }

    pub fn build_unspecified_type(self) -> Diagnostic {
        let msg = "missing type specifier; type defaults to int".to_owned();
        self.build_custom(Code::UnspecifiedType, msg)
    }

    pub fn build_unknown_escape_sequence(self, escape_seq: &str) -> Diagnostic {
        let msg = format!("unknown escape sequence: {escape_seq}");
        self.build_custom(Code::UnknownEscapeSequence, msg)
    }

    pub fn build_incomplete_hex_escape_sequence(mut self) -> Diagnostic {
        let end = self.span.excl_end();
        self.add_additional_span(end..end, Some("expected hex digit".to_owned()));
        let msg = "incomplete hex escape sequence".to_owned();
        self.build_custom(Code::IncompleteEscapeSequence, msg)
    }

    pub fn build_escape_sequence_out_of_range(self, base_name: &str) -> Diagnostic {
        let msg = format!("{base_name} escape sequence out of range");
        self.build_custom(Code::EscapeSequenceOutOfRange, msg)
    }

    pub fn build_embedded_null_in_string(self) -> Diagnostic {
        let msg = "embedded 0 byte in string literal".to_owned();
        self.build_custom(Code::EmbeddedNullInString, msg)
    }

    pub fn build_cant_be_const(mut self, operator: &str, expr_span: Span) -> Diagnostic {
        self.add_additional_span(expr_span, Some("this expression is const".to_string()));
        self.build_custom(
            Code::NeedConst,
            format!("can't {operator} a const expression"),
        )
    }

    pub fn build_unexpected_type(
        mut self,
        operator: &str,
        needed_type_cat: TypeCat,
        expr: &ir::expr::ExprNode,
    ) -> Diagnostic {
        let message = format!("{operator} operator needs {}", needed_type_cat.long_name());
        self.add_ir_expr_type(expr);
        self.build_custom(Code::UnexpectedType, message)
    }

    pub fn build_unexpected_type_lvalue(
        mut self,
        operator: &str,
        needed_type_cat: TypeCat,
        expr: &ir::expr::LvalueExprNode,
    ) -> Diagnostic {
        let message = format!("{operator} operator needs {}", needed_type_cat.long_name());
        self.add_ir_expr_type_lvalue(expr);
        self.build_custom(Code::UnexpectedType, message)
    }

    pub fn build_unexpected_type_bin(
        mut self,
        operator: &str,
        needed_type_cat: TypeCat,
        expr: &ir::expr::ExprNode,
        other_expr: Option<&ir::expr::ExprNode>,
    ) -> Diagnostic {
        self.add_ir_expr_type(expr);
        if let Some(exprr) = other_expr {
            self.add_ir_expr_type(exprr);
        }
        let message = format!(
            "{operator} operator needs two {}",
            needed_type_cat.long_name_plural()
        );
        self.build_custom(Code::UnexpectedType, message)
    }

    pub fn build_incompatible_types(
        mut self,
        operator: &str,
        expr: &ir::expr::ExprNode,
        other_expr: &ir::expr::ExprNode,
    ) -> Diagnostic {
        self.add_ir_expr_type(expr);
        self.add_ir_expr_type(other_expr);
        self.build_custom(
            Code::UnexpectedType,
            format!("incompatible types for {operator}"),
        )
    }

    pub fn build_need_lvalue(mut self, expr_type: &str, inner_span: Span) -> Diagnostic {
        // IDEA: If the inner expr is a pointer, suggest dereferencing.
        self.add_additional_span(
            inner_span,
            Some("this expression is not a lvalue, but needs to be".to_owned()),
        );
        self.build_custom(Code::NeedLvalue, format!("{expr_type} needs a lvalue"))
    }

    pub fn build_invalid_cast(mut self, reason: InvalidCastReason) -> Diagnostic {
        let message = match reason {
            InvalidCastReason::PointerFromFloat(expr) => {
                self.add_ir_expr_type(expr);
                "floating types can't be cast to pointers"
            }
            InvalidCastReason::FloatFromPointer(expr) => {
                self.add_ir_expr_type(expr);
                "pointers can't be cast to floating types"
            }
            InvalidCastReason::FromNonScaler(expr) => {
                self.add_ir_expr_type(expr);
                "can only cast from a scalar type"
            }
            InvalidCastReason::BothNonScalar(expr) => {
                self.add_ir_expr_type(expr);
                "can only cast from and to scalar types"
            }
            InvalidCastReason::IntoNonScalar => "can only cast to scalar types",
        };
        self.build_custom(Code::InvalidCast, message.to_owned())
    }

    pub fn build_implicit_lossy_assign(
        mut self,
        from_expr: &ir::expr::ExprNode,
        to_expr: &ir::expr::LvalueExprNode,
        only_sign: bool,
    ) -> Diagnostic {
        let message = if only_sign {
            format!(
                "implicit conversion from `{}` to `{}` could change sign",
                from_expr.ty, to_expr.ty,
            )
        } else {
            format!(
                "implicit conversion from `{}` to `{}` could lose information",
                from_expr.ty, to_expr.ty,
            )
        };
        self.add_ir_expr_type(from_expr);
        self.add_additional_span(
            to_expr.span,
            Some(format!("while this has type: `{}`", to_expr.ty)),
        );
        self.build_custom(Code::LossyImplicitAssign, message)
    }

    pub fn build_incompatible_assign(
        mut self,
        from_expr: &ir::expr::ExprNode,
        to_expr: &ir::expr::LvalueExprNode,
    ) -> Diagnostic {
        self.add_ir_expr_type(from_expr);
        self.add_additional_span(
            to_expr.span,
            Some(format!("while this has type: `{}`", to_expr.ty)),
        );
        self.build_custom(
            Code::IncompatibleAssign,
            format!(
                "assign from `{}` to the incompatible type `{}`",
                from_expr.ty, to_expr.ty
            ),
        )
    }

    pub fn build_assign_to_array(mut self, to: &ir::LvalueExprNode) -> Diagnostic {
        self.add_additional_span(to.span, Some(format!("this has type: `{}`", to.ty)));
        self.build_custom(
            Code::IncompatibleAssign,
            "can't assign to an array".to_owned(),
        )
    }

    pub fn build_assign_const_loss(mut self, with_const: Span, without_const: Span) -> Diagnostic {
        self.add_additional_span(with_const, Some("this points to a const value".to_owned()));
        self.add_additional_span(without_const, Some("while this doesn't".to_owned()));
        self.build_custom(
            Code::AssignConstLoss,
            "assign loses const qualifier".to_owned(),
        )
    }

    pub fn build_implicit_lossy_return(
        mut self,
        from_expr: &ir::expr::ExprNode,
        to_span: Span,
        to_type: &ir::ctype::CType,
        only_sign: bool,
    ) -> Diagnostic {
        let message = if only_sign {
            format!(
                "implicit conversion from `{}` to `{}` could change sign",
                from_expr.ty, to_type,
            )
        } else {
            format!(
                "implicit conversion from `{}` to `{}` could lose information",
                from_expr.ty, to_type,
            )
        };
        self.add_ir_expr_type(from_expr);
        self.add_ir_return_type(to_span, to_type);
        self.build_custom(Code::LossyImplicitReturn, message)
    }

    pub fn build_incompatible_return(
        mut self,
        from_expr: &ir::expr::ExprNode,
        to_span: Span,
        to_type: &ir::ctype::CType,
    ) -> Diagnostic {
        self.add_ir_expr_type(from_expr);
        self.add_ir_return_type(to_span, to_type);
        self.build_custom(
            Code::IncompatibleReturn,
            format!(
                "return of type `{}` in function with incompatible type `{}`",
                from_expr.ty, to_type
            ),
        )
    }

    pub fn build_return_const_loss(mut self, with_const: Span, return_span: Span) -> Diagnostic {
        self.add_additional_span(
            with_const,
            Some("but this points to a const value".to_owned()),
        );
        self.add_additional_span(
            return_span,
            Some("function returns a non const pointer".to_owned()),
        );
        self.build_custom(
            Code::ReturnConstLoss,
            "return loses const qualifier".to_owned(),
        )
    }

    pub fn build_implicit_lossy_arg(
        mut self,
        from_expr: &ir::expr::ExprNode,
        param: &ir::stmt::FunctionParamNode,
        only_sign: bool,
    ) -> Diagnostic {
        let message = if only_sign {
            format!(
                "implicit conversion from `{}` to parameter type `{}` could change sign",
                from_expr.ty, param.ty,
            )
        } else {
            format!(
                "implicit conversion from `{}` to parameter type `{}` could lose information",
                from_expr.ty, param.ty,
            )
        };
        self.add_ir_expr_type(from_expr);
        self.add_ir_param_type(param);
        self.build_custom(Code::LossyImplicitArg, message)
    }

    pub fn build_incompatible_arg(
        mut self,
        from_expr: &ir::expr::ExprNode,
        param: &ir::stmt::FunctionParamNode,
    ) -> Diagnostic {
        self.add_ir_expr_type(from_expr);
        self.add_ir_param_type(param);
        self.build_custom(
            Code::IncompatibleArg,
            format!(
                "argument has type `{}` with is not compatible with parameter type `{}`",
                from_expr.ty, param.ty
            ),
        )
    }

    pub fn build_arg_const_loss(mut self, with_const: Span, param_span: Span) -> Diagnostic {
        self.add_additional_span(with_const, Some("this points to a const value".to_owned()));
        self.add_additional_span(
            param_span,
            Some("parameter is a non const pointer".to_owned()),
        );
        self.build_custom(
            Code::ArgConstLoss,
            "argument loses const qualifier in function".to_owned(),
        )
    }

    pub fn build_wrong_statement_type(
        self,
        field_name: &str,
        got_type: &ir::ctype::CType,
        want_type: TypeCat,
    ) -> Diagnostic {
        self.build_custom(
            Code::UnexpectedType,
            format!(
                "{field_name} needs {}, but got a expression of type `{got_type}`",
                want_type.long_name()
            ),
        )
    }

    pub fn build_too_big_constant(self, value: i128) -> Diagnostic {
        self.build_custom(
            Code::TooBigConstant,
            format!("constant {value} cannot be correctly represented in any c type"),
        )
    }

    pub fn build_undeclared_ident(self, name: &str) -> Diagnostic {
        // IDEA: give possible alternative names that are close
        self.build_custom(
            Code::UndeclaredIdent,
            format!("identifier `{name}` isn't declared in this scope"),
        )
    }

    pub fn build_undeclared_function(self, name: &str) -> Diagnostic {
        // IDEA: give possible alternative names that are close
        self.build_custom(
            Code::UndeclaredFunction,
            format!("function with name `{name}` not found"),
        )
    }

    pub fn build_already_defined(mut self, name: &str, original_span: Span) -> Diagnostic {
        self.add_additional_span(original_span, Some("originally defined here".to_owned()));
        self.build_custom(
            Code::AlreadyDefined,
            format!("identifier `{name}` is already defined"),
        )
    }

    pub fn build_usign_uninit(self, name: &str) -> Diagnostic {
        self.build_custom(
            Code::UsingUninit,
            format!("`{name}` is possibly uninitialized"),
        )
    }

    pub fn build_invalid_break(self) -> Diagnostic {
        self.build_custom(
            Code::InvalidJumpStmt,
            "break statement not inside loop or switch".to_owned(),
        )
    }

    pub fn build_invalid_continue(self) -> Diagnostic {
        self.build_custom(
            Code::InvalidJumpStmt,
            "continue statement not inside loop".to_owned(),
        )
    }

    pub fn build_case_not_int(self) -> Diagnostic {
        self.build_custom(
            Code::SwitchCaseNotInt,
            "switch case does not have interger type".to_owned(),
        )
    }

    pub fn build_case_not_folded(self) -> Diagnostic {
        self.build_custom(
            Code::SwitchCaseNotFolded,
            "was not able to fold switch case".to_owned(),
        )
    }

    pub fn build_unreachable_code(
        mut self,
        unreachable_span: Span,
        infinite: Option<Span>,
    ) -> Diagnostic {
        self.add_additional_span(
            unreachable_span,
            Some("this will never be executed".to_owned()),
        );
        if let Some(infinite) = infinite {
            self.add_additional_span(infinite, Some("this loop will run forever".to_owned()));
        }
        self.build_custom(
            Code::Unreachable,
            "any code after this statement is not reachable".to_owned(),
        )
    }

    pub fn build_always_true_false(
        mut self,
        always_true: bool,
        not_executed: Option<Span>,
    ) -> Diagnostic {
        if let Some(not_executed) = not_executed {
            self.add_additional_span(not_executed, Some("this will never be executed".to_owned()));
        }
        self.build_custom(
            Code::Unreachable,
            format!(
                "this expression will always be {}",
                match always_true {
                    true => "truthy",
                    false => "falsy",
                }
            ),
        )
    }

    pub fn build_invalid_array_size(self, reason: InvalidArraySize) -> Diagnostic {
        let message = match reason {
            InvalidArraySize::NegativeSized => "size of array is negative",
            InvalidArraySize::ZeroSized => "size of array is zero",
            InvalidArraySize::NonInt => "size of array needs to be an interger",
        };

        self.build_custom(Code::InvalidArraySize, message.to_owned())
    }

    pub fn build_non_const_global_initializer(self) -> Diagnostic {
        self.build_custom(
            Code::NonConstGlobalInitializer,
            "global variables can only be initialized by constant expressions".to_owned(),
        )
    }

    pub fn build_wrong_amount_of_args(
        mut self,
        got: usize,
        expected: usize,
        original_span: Span,
        to_many: bool,
    ) -> Diagnostic {
        self.add_function_def(original_span);

        let problem = match to_many {
            true => "many",
            false => "little",
        };

        self.build_custom(
            Code::WrongAmountOfArgs,
            format!("to {problem} arguments. got {got} but expected {expected}"),
        )
    }

    pub fn build_qualified_void(mut self, qual_span: Span) -> Diagnostic {
        self.add_additional_span(qual_span, None);
        self.build_custom(
            Code::QualifiedVoid,
            "void type can't be qualified".to_owned(),
        )
    }

    pub fn build_void_used(mut self, arg: &ir::ExprNode) -> Diagnostic {
        self.add_ir_expr_type(arg);
        self.build_custom(Code::VoidUsed, "expression with type void used".to_owned())
    }

    pub fn build_void_vars(self) -> Diagnostic {
        self.build_custom(Code::VoidVariable, "void variable can't exist".to_owned())
    }

    pub fn build_void_param(self) -> Diagnostic {
        self.build_custom(Code::VoidVariable, "void parameter can't exist".to_owned())
    }

    pub fn build_value_return_to_void(mut self, return_span: Span) -> Diagnostic {
        self.add_ir_return_type(return_span, &ir::ctype::CType::Void);
        self.build_custom(
            Code::ValueReturnInVoid,
            "value return used in void function".to_owned(),
        )
    }

    pub fn build_no_return_value(
        mut self,
        return_span: Span,
        return_type: &ir::ctype::CType,
    ) -> Diagnostic {
        self.add_ir_return_type(return_span, return_type);
        self.build_custom(
            Code::NoReturnValue,
            format!("empty return used in function returning `{}`", return_type),
        )
    }

    pub fn build_no_return_in_value_func(self) -> Diagnostic {
        self.build_custom(
            Code::NotAlwaysReturn,
            "non-void function does not return a value in all control paths".to_owned(),
        )
    }

    pub fn build_func_def_with_name_of_var(mut self, name: &str, var_span: Span) -> Diagnostic {
        self.add_additional_span(var_span, Some("variable defined here".to_owned()));
        self.build_custom(
            Code::ClashingGlobalName,
            format!("declaration of function `{name}` uses name of global variable"),
        )
    }

    pub fn build_var_def_with_name_of_func(mut self, name: &str, func_span: Span) -> Diagnostic {
        self.add_additional_span(func_span, Some("function defined here".to_owned()));
        self.build_custom(
            Code::ClashingGlobalName,
            format!("declaration of variable `{name}` uses name of function"),
        )
    }

    pub fn build_func_redec_with_different_return(
        mut self,
        name: &str,
        new_return_type: &ir::ctype::CType,
        original_def_span: Span,
        original_return_type: &ir::ctype::CType,
    ) -> Diagnostic {
        self.add_additional_span(
            original_def_span,
            Some(format!(
                "original function declared here returning `{}`",
                original_return_type
            )),
        );
        self.build_custom(
            Code::IncompatibleFunctionRedef,
            format!(
                "redeclaration of function `{name}` with a different return type `{}`",
                new_return_type
            ),
        )
    }

    pub fn build_func_redec_with_different_parms(
        mut self,
        name: &str,
        original_def_span: Span,
    ) -> Diagnostic {
        self.add_additional_span(
            original_def_span,
            Some("original function declared here".to_owned()),
        );
        self.build_custom(
            Code::IncompatibleFunctionRedef,
            format!("redeclaration of function `{name}` has different parameters"),
        )
    }

    pub fn build_function_already_defined(mut self, name: &str, original_span: Span) -> Diagnostic {
        self.add_additional_span(
            original_span,
            Some("original function defined here".to_owned()),
        );
        self.build_custom(
            Code::MultipleFunctionDef,
            format!("function `{name}` has multiple definitions"),
        )
    }

    pub fn build_var_redec_with_different_type(
        mut self,
        name: &str,
        new_type: &ir::ctype::CType,
        original_def_span: Span,
        original_def_type: &ir::ctype::CType,
    ) -> Diagnostic {
        self.add_additional_span(
            original_def_span,
            Some(format!(
                "original variable declared here with type `{}`",
                original_def_type
            )),
        );
        self.build_custom(
            Code::IncompatibleVariableRedef,
            format!("redeclaration of variable `{name}` has different type `{new_type}`"),
        )
    }

    pub fn build_var_redec_with_different_constness(
        mut self,
        name: &str,
        original_def_span: Span,
        became_const: bool,
    ) -> Diagnostic {
        self.add_additional_span(
            original_def_span,
            Some("original variable declared here".to_owned()),
        );
        let msg = match became_const {
            true => format!("redeclaration of variable `{name}` is const while original is not"),
            false => {
                format!("redeclaration of variable `{name}` is not const while the original was")
            }
        };
        self.build_custom(Code::IncompatibleVariableRedef, msg)
    }

    pub fn build_global_var_already_defined(
        mut self,
        name: &str,
        original_span: Span,
    ) -> Diagnostic {
        self.add_additional_span(
            original_span,
            Some("original global variable defined here".to_owned()),
        );
        self.build_custom(
            Code::MultipleVariableDef,
            format!("variable `{name}` has multiple definitions"),
        )
    }
}

pub struct DiagnosticBuilder {
    span: Span,
    additional_spans: Vec<(Span, Option<String>)>,
}

#[derive(Debug, Copy, Clone)]
pub enum TypeCat {
    Integral,
    Arithmetic,
    Scalar,
    Pointer,
}

#[derive(Debug, Clone)]
pub enum InvalidCastReason<'a> {
    PointerFromFloat(&'a ir::expr::ExprNode),
    FloatFromPointer(&'a ir::expr::ExprNode),
    FromNonScaler(&'a ir::expr::ExprNode),
    BothNonScalar(&'a ir::expr::ExprNode),
    IntoNonScalar,
}

impl TypeCat {
    fn long_name(&self) -> &'static str {
        match self {
            TypeCat::Integral => "an integral type",
            TypeCat::Arithmetic => "an arithmetic type",
            TypeCat::Scalar => "a scalar type",
            TypeCat::Pointer => "a pointer type",
        }
    }

    fn long_name_plural(&self) -> &'static str {
        match self {
            TypeCat::Integral => "integral types",
            TypeCat::Arithmetic => "arithmetic types",
            TypeCat::Scalar => "scalar types",
            TypeCat::Pointer => "pointer types",
        }
    }
}

pub enum InvalidArraySize {
    NegativeSized,
    ZeroSized,
    NonInt,
}
