use crate::function;
use crate::ty::{Any, AnyType, FirstClass, FirstClassType, Type};
use std::fmt::Write;

declare_base_type! {
    into_function;
    (Function(LiteralFunction), FunctionType) => (Any, AnyType)
}

impl Function {
    pub fn new_literal(return_type: function::ReturnType) -> LiteralFunctionBuilder {
        LiteralFunctionBuilder(LiteralFunction {
            return_type,
            param_types: Vec::new(),
            is_vararg: false,
        })
    }

    impl_base_type_getter! { return_type: &function::ReturnType = |literal| &literal.return_type }
    impl_base_type_getter! { param_types: &[FirstClass] = |literal| &literal.param_types }
    impl_base_type_getter! { is_vararg: bool = |literal| literal.is_vararg }
}

impl Type for Function {
    fn equiv_to(&self, other: &Self) -> bool {
        self.is_vararg() == other.is_vararg()
            && self.return_type().equiv_to(other.return_type())
            && self
                .param_types()
                .iter()
                .zip(other.param_types())
                .all(|(a, b)| a.equiv_to(b))
    }

    fn has_opaque_struct(&self) -> bool {
        self.return_type().has_opaque_struct()
            || self.param_types().iter().any(|t| t.has_opaque_struct())
    }

    fn has_scalable_vec(&self) -> bool {
        self.return_type().has_scalable_vec()
            || self.param_types().iter().any(|t| t.has_scalable_vec())
    }

    impl_base_type_is_identified! {}
}

impl crate::FmtAsLlvmAsmMC for Function {
    impl_base_type_fmt! {
        |literal, f, opts, module| {
            literal.return_type.fmt_as_llvm_asm(f, opts, module)?;
            f.write_str(" (")?;
            if let [head, tail @ ..] = literal.param_types.as_slice() {
                head.fmt_as_llvm_asm(f, opts, module)?;
                for param in tail {
                    f.write_str(", ")?;
                    param.fmt_as_llvm_asm(f, opts, module)?;
                }
            }
            if literal.is_vararg {
                if !literal.param_types.is_empty() {
                    f.write_str(", ")?;
                }
                f.write_str("...")?;
            }
            f.write_char(')')
        }
    }
}

/// Describes a function's signature. Consists of the function's return type and the types of
/// its formal parameters.
#[derive(Debug, Clone)]
pub struct LiteralFunction {
    /// Return type of the function.
    return_type: function::ReturnType,
    /// Types of the formal parameters of the function.
    param_types: Vec<FirstClass>,
    /// Indicates whether this functions takes a variable number of arguments.
    is_vararg: bool,
}

#[derive(Debug, Clone)]
pub struct LiteralFunctionBuilder(LiteralFunction);

impl LiteralFunctionBuilder {
    pub fn with_param(mut self, param: impl FirstClassType) -> Self {
        self.0.param_types.push(param.into());
        self
    }

    pub fn with_params(mut self, params: impl IntoIterator<Item = FirstClass>) -> Self {
        self.0.param_types.extend(params);
        self
    }

    pub fn with_vararg(mut self, is_vararg: bool) -> Self {
        self.0.is_vararg = is_vararg;
        self
    }

    pub fn build(self) -> Function {
        Function::Literal(self.0)
    }
}
