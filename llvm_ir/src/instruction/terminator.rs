use super::{IntoValidated, ValidationError};
use crate::value::{self, Value};
use std::fmt::{self, Write};

impl IntoValidated<Terminator> for Terminator {
    type ValidatedType = Terminator;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        Ok(self)
    }
}

declare_type_union_validated! {
    #[derive(Debug, Clone)]
    pub enum Terminator {
        ValidatedReturnVoid(ReturnVoid),
        ValidatedReturn(Return),
        ValidatedBranch(Branch),
        ValidatedBranchConditional(BranchConditional),
        ValidatedIndirectBranch(IndirectBranch),
        ValidatedSwitch(Switch),
        // ValidatedInvoke(Invoke),
        // ValidatedCallBranch(CallBranch),
        // ValidatedResume(Resume),
        // ValidatedCatchSwitch(CatchSwitch),
        // ValidatedCatchReturn(CatchReturn),
        // ValidatedCleanupReturn(CleanupReturn),
        // ValidatedUnreachable(Unreachable),
    }
}

/// Return nothing from a function with return type `void`.
#[derive(Debug, Clone)]
pub struct ReturnVoid;

impl crate::FmtAsLlvmAsm for ReturnVoid {
    fn fmt_as_llvm_asm(&self, f: &mut fmt::Formatter, _opts: &crate::FmtOpts) -> fmt::Result {
        f.write_str("ret void")
    }
}

/// Return of value with non-void type.
#[derive(Debug, Clone)]
pub struct Return<V: value::ElementValue = value::Element>(pub V);

impl<V: value::ElementValue> IntoValidated<Terminator> for Return<V> {
    type ValidatedType = ValidatedReturn;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        Ok(ValidatedReturn(Return(self.0.into())))
    }
}

impl<V: value::ElementValue> crate::FmtAsLlvmAsmFC for Return<V> {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        f.write_str("ret ")?;
        self.0.ty().fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char(' ')?;
        self.0.fmt_as_llvm_asm(f, opts, module, function)
    }
}

/// Unconditional branch.
#[derive(Debug, Clone)]
pub struct Branch<V: value::LabelValue = value::Label> {
    pub dest: V,
}

impl<V: value::LabelValue> IntoValidated<Terminator> for Branch<V> {
    type ValidatedType = ValidatedBranch;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        Ok(ValidatedBranch(Branch {
            dest: self.dest.into(),
        }))
    }
}

impl<V: value::LabelValue> crate::FmtAsLlvmAsmFC for Branch<V> {
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        f.write_str("br ")?;
        self.dest.ty().fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char(' ')?;
        self.dest.fmt_as_llvm_asm(f, opts, module, function)
    }
}

/// Conditional branch.
#[derive(Debug, Clone)]
pub struct BranchConditional<C = value::Boolean, T = value::Label, F = value::Label>
where
    C: value::BooleanValue,
    T: value::LabelValue,
    F: value::LabelValue,
{
    pub cond: C,
    pub dest_true: T,
    pub dest_false: F,
}

impl<C, T, F> IntoValidated<Terminator> for BranchConditional<C, T, F>
where
    C: value::BooleanValue,
    T: value::LabelValue,
    F: value::LabelValue,
{
    type ValidatedType = ValidatedBranchConditional;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        Ok(ValidatedBranchConditional(BranchConditional {
            cond: self.cond.into(),
            dest_true: self.dest_true.into(),
            dest_false: self.dest_false.into(),
        }))
    }
}

impl<C, T, F> crate::FmtAsLlvmAsmFC for BranchConditional<C, T, F>
where
    C: value::BooleanValue,
    T: value::LabelValue,
    F: value::LabelValue,
{
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        f.write_str("br ")?;
        self.cond.ty().fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char(' ')?;
        self.cond.fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_str(", ")?;
        self.dest_true
            .ty()
            .fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char(' ')?;
        self.dest_true.fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_str(", ")?;
        self.dest_false
            .ty()
            .fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char(' ')?;
        self.dest_false.fmt_as_llvm_asm(f, opts, module, function)
    }
}

#[derive(Debug, Clone)]
pub struct IndirectBranch<A = value::Pointer, L = value::Label>
where
    A: value::PointerValue,
    L: value::LabelValue,
{
    /// The address must be derived from a blockadress.
    pub address: A,
    pub targets: Vec<L>,
}

impl<A, L> IntoValidated<Terminator> for IndirectBranch<A, L>
where
    A: value::PointerValue,
    L: value::LabelValue,
{
    type ValidatedType = ValidatedIndirectBranch;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        Ok(ValidatedIndirectBranch(IndirectBranch {
            address: self.address.into(),
            targets: self.targets.into_iter().map(Into::into).collect(),
        }))
    }
}

impl<A, L> crate::FmtAsLlvmAsmFC for IndirectBranch<A, L>
where
    A: value::PointerValue,
    L: value::LabelValue,
{
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        f.write_str("indirectbr ")?;
        self.address
            .ty()
            .fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char(' ')?;
        self.address.fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_str(", [ ")?;
        if let [head, tail @ ..] = self.targets.as_slice() {
            head.ty().fmt_as_llvm_asm(f, opts, module, function)?;
            f.write_char(' ')?;
            head.fmt_as_llvm_asm(f, opts, module, function)?;
            for target in tail {
                f.write_str(", ")?;
                target.ty().fmt_as_llvm_asm(f, opts, module, function)?;
                f.write_char(' ')?;
                target.fmt_as_llvm_asm(f, opts, module, function)?;
            }
        }
        f.write_str(" ]")
    }
}

#[derive(Debug, Clone)]
pub struct Switch<V: value::IntegerValue = value::Integer, L: value::LabelValue = value::Label> {
    pub value: V,
    pub default_dest: L,
    pub branches: Vec<(value::constant::Integer, L)>,
}

impl<V, L> IntoValidated<Terminator> for Switch<V, L>
where
    V: value::IntegerValue,
    L: value::LabelValue,
{
    type ValidatedType = ValidatedSwitch;

    fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
        Ok(ValidatedSwitch(Switch {
            value: self.value.into(),
            default_dest: self.default_dest.into(),
            branches: self
                .branches
                .into_iter()
                .map(|(i, label)| (i, label.into()))
                .collect(),
        }))
    }
}

impl<V, L> crate::FmtAsLlvmAsmFC for Switch<V, L>
where
    V: value::IntegerValue,
    L: value::LabelValue,
{
    fn fmt_as_llvm_asm(
        &self,
        f: &mut fmt::Formatter,
        opts: &crate::FmtOpts,
        module: &crate::Module,
        function: &crate::FunctionDeclaration,
    ) -> fmt::Result {
        f.write_str("switch ")?;
        self.value.ty().fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char(' ')?;
        self.value.fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_str(", ")?;
        self.default_dest
            .ty()
            .fmt_as_llvm_asm(f, opts, module, function)?;
        f.write_char(' ')?;
        self.default_dest
            .fmt_as_llvm_asm(f, opts, module, function)?;
        for (value, label) in &self.branches {
            f.write_str(" [ ")?;
            value.ty().fmt_as_llvm_asm(f, opts, module, function)?;
            f.write_char(' ')?;
            value.fmt_as_llvm_asm(f, opts, module, function)?;
            f.write_str(", ")?;
            label.ty().fmt_as_llvm_asm(f, opts, module, function)?;
            f.write_char(' ')?;
            label.fmt_as_llvm_asm(f, opts, module, function)?;
            f.write_str(" ]")?;
        }
        Ok(())
    }
}

// #[derive(Debug, Clone)]
// pub struct Invoke {}

// impl crate::FmtAsLlvmAsmFC for Invoke {
//     fn fmt_as_llvm_asm(
//         &self,
//         f: &mut fmt::Formatter,
//         opts: &crate::FmtOpts,
//         module: &crate::Module,
//         function: &crate::FunctionDeclaration,
//     ) -> fmt::Result {
//         todo!()
//     }
// }

// #[derive(Debug, Clone)]
// pub struct CallBranch {}

// impl crate::FmtAsLlvmAsmFC for CallBranch {
//     fn fmt_as_llvm_asm(
//         &self,
//         f: &mut fmt::Formatter,
//         opts: &crate::FmtOpts,
//         module: &crate::Module,
//         function: &crate::FunctionDeclaration,
//     ) -> fmt::Result {
//         todo!()
//     }
// }

// #[derive(Debug, Clone)]
// pub struct Resume {}

// impl crate::FmtAsLlvmAsmFC for Resume {
//     fn fmt_as_llvm_asm(
//         &self,
//         f: &mut fmt::Formatter,
//         opts: &crate::FmtOpts,
//         module: &crate::Module,
//         function: &crate::FunctionDeclaration,
//     ) -> fmt::Result {
//         todo!()
//     }
// }

// #[derive(Debug, Clone)]
// pub struct CatchSwitch {}

// impl crate::FmtAsLlvmAsmFC for CatchSwitch {
//     fn fmt_as_llvm_asm(
//         &self,
//         f: &mut fmt::Formatter,
//         opts: &crate::FmtOpts,
//         module: &crate::Module,
//         function: &crate::FunctionDeclaration,
//     ) -> fmt::Result {
//         todo!()
//     }
// }

// #[derive(Debug, Clone)]
// pub struct CatchReturn {}

// impl crate::FmtAsLlvmAsmFC for CatchReturn {
//     fn fmt_as_llvm_asm(
//         &self,
//         f: &mut fmt::Formatter,
//         opts: &crate::FmtOpts,
//         module: &crate::Module,
//         function: &crate::FunctionDeclaration,
//     ) -> fmt::Result {
//         todo!()
//     }
// }

// #[derive(Debug, Clone)]
// pub struct CleanupReturn {}

// impl crate::FmtAsLlvmAsmFC for CleanupReturn {
//     fn fmt_as_llvm_asm(
//         &self,
//         f: &mut fmt::Formatter,
//         opts: &crate::FmtOpts,
//         module: &crate::Module,
//         function: &crate::FunctionDeclaration,
//     ) -> fmt::Result {
//         todo!()
//     }
// }

// #[derive(Debug, Clone)]
// pub struct Unreachable {}

// impl crate::FmtAsLlvmAsmFC for Unreachable {
//     fn fmt_as_llvm_asm(
//         &self,
//         f: &mut fmt::Formatter,
//         opts: &crate::FmtOpts,
//         module: &crate::Module,
//         function: &crate::FunctionDeclaration,
//     ) -> fmt::Result {
//         todo!()
//     }
// }

macro_rules! impl_noop_intovalidated {
    ($($ValidatedType:ident($Type:ident)),+ $(,)?) => {
        $(
            impl IntoValidated<Terminator> for $Type {
                type ValidatedType = $ValidatedType;

                fn into_validated(self) -> Result<Self::ValidatedType, ValidationError> {
                    Ok($ValidatedType(self))
                }
            }
        )+
    };
}

impl_noop_intovalidated! {
    ValidatedReturnVoid(ReturnVoid),
    // ValidatedInvoke(Invoke),
    // ValidatedCallBranch(CallBranch),
    // ValidatedResume(Resume),
    // ValidatedCatchSwitch(CatchSwitch),
    // ValidatedCatchReturn(CatchReturn),
    // ValidatedCleanupReturn(CleanupReturn),
    // ValidatedUnreachable(Unreachable),
}
