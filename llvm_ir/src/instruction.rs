use crate::id::{Id, LabelRef};
use crate::ty::{self, Type};
use crate::TypedValue;
use std::fmt;
use std::ops::Deref;

#[derive(Debug)]
pub struct TerminatorInstruction(RawTerminatorInstruction);

#[derive(Debug)]
pub(crate) enum Instruction {
    Yielding(Id, YieldingInstruction),
    Yieldless(YieldlessInstruction),
}

#[derive(Debug)]
pub struct YieldingInstruction(RawYieldingInstruction);

#[derive(Debug)]
pub struct YieldlessInstruction(RawYieldlessInstruction);

#[derive(Debug)]
pub enum RawTerminatorInstruction {
    ReturnVoid,
    Return(TypedValue),
    BranchUnconditional(LabelRef),
    BranchConditional {
        cond: TypedValue,
        if_dest: LabelRef,
        else_dest: LabelRef,
    },
}

#[derive(Debug)]
pub enum RawYieldingInstruction {
    UnaryOp {
        op: UnaryOp,
        op1: TypedValue,
    },
    BinaryOp {
        op: BinaryOp,
        op1: TypedValue,
        op2: TypedValue,
    },
    Compare {
        op: CompareOp,
        op1: TypedValue,
        op2: TypedValue,
    },
    Cast {
        op: CastOp,
        value: TypedValue,
        to_ty: Type,
    },
    GetElementPtr {
        ty: Type,
        ptrval: TypedValue,
        index: TypedValue,
    },
    Phi {
        args: Vec<(TypedValue, LabelRef)>,
    },
    Alloca {
        ty: Type,
        amount: Option<TypedValue>,
    },
    Load {
        ty: Type,
        pointer: TypedValue,
    },
    Printf {
        args: Vec<TypedValue>,
    },
}

#[derive(Debug)]
pub enum RawYieldlessInstruction {
    Store {
        value: TypedValue,
        pointer: TypedValue,
    },
}

#[derive(Debug)]
pub enum UnaryOp {
    Fneg,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Fadd,
    Sub,
    Fsub,
    Mul,
    Fmul,
    Udiv,
    Sdiv,
    Fdiv,
    Urem,
    Srem,
    Frem,
    And,
    Or,
    Xor,
    Shl,
    Lshr,
    Ashr,
}

#[derive(Debug)]
pub enum CompareOp {
    /// The `imcp` instruction compares two integer or pointer values and returns a boolean.
    Icmp(IcmpCond),
    /// The `fcmp` instruction compares two floating-point values and returns a boolean.
    Fcmp(FcmpCond),
}

#[derive(Debug)]
pub enum IcmpCond {
    /// Equal.
    ///
    /// Yields `true` if the operands are equal, `false` otherwise.
    /// No sign interpretation is necessary or performed.
    Eq,
    /// Not equal.
    ///
    /// Yields `true` if the operands are unequal, `false` otherwise.
    /// No sign interpretation is necessary or performed.
    Ne,
    /// Unsigned greater than.
    ///
    /// Interprets the operands as unsigned values and yields `true` if `op1` is greater than `op2`.
    Ugt,
    /// Unsigned greater or equal.
    ///
    /// Interprets the operands as unsigned values and yields `true` if `op1` is greater than or
    /// equal to `op2`.
    Uge,
    /// Unsigned less than.
    ///
    /// Interprets the operands as unsigned values and yields `true` if `op1` is less than `op2`.
    Ult,
    /// Unsigned less or equal.
    ///
    /// Interprets the operands as unsigned values and yields `true` if `op1` is less than or equal
    /// to `op2`.
    Ule,
    /// Signed greater than.
    ///
    /// Interprets the operands as signed values and yields `true` if `op1` is greater than `op2`.
    Sgt,
    /// Signed greater or equal.
    ///
    /// Interprets the operands as signed values and yields `true` if `op1` is greater than or
    /// equal to `op2`.
    Sge,
    /// Signed less than.
    ///
    /// Interprets the operands as signed values and yields `true` if `op1` is less than `op2`.
    Slt,
    /// Signed less or equal.
    ///
    /// Interprets the operands as signed values and yields `true` if `op1` is less than or equal to
    /// `op2`.
    Sle,
}

#[derive(Debug)]
pub enum FcmpCond {
    /// No comparison, always returns `false`.
    False,
    /// Ordered and equal.
    Oeq,
    /// Ordered and greater than.
    Ogt,
    /// Ordered and greater than or equal.
    Oge,
    /// Ordered and less than.
    Olt,
    /// Ordered and less than or equal.
    Ole,
    /// Ordered and not equal.
    One,
    /// Ordered (no `NaN`s).
    Ord,
    /// Unordered or equal.
    Ueq,
    /// Unordered or greater than.
    Ugt,
    /// Unordered or greater than or equal.
    Uge,
    /// Unordered or less than.
    Ult,
    /// Unordered or less than or equal.
    Ule,
    /// Unordered or not equal.
    Une,
    /// Unordered (either `NaN`s).
    Uno,
    /// No comparison, always returns `true`.
    True,
}

#[derive(Debug)]
pub enum CastOp {
    /// Truncate.
    Trunc,
    /// Zero extend.
    Zext,
    /// Sign extend.
    Sext,
    /// Floating-point truncate.
    Fptrunc,
    /// Floating-point extend.
    Fpext,
    /// Floating-point to unsigned integer.
    Fptoui,
    /// Floating-point to signed integer.
    Fptosi,
    /// Unsigned integer to floating-point.
    Uitofp,
    /// Signed integer to floating-point.
    Sitofp,
    /// Convert pointer to integer.
    Ptrtoint,
    /// Convert integer to pointer.
    Inttoptr,
    /// Bitcast.
    ///
    /// Converts `value` to `to_ty` without changing any bits.
    Bitcast,
}

impl Deref for TerminatorInstruction {
    type Target = RawTerminatorInstruction;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Deref for YieldingInstruction {
    type Target = RawYieldingInstruction;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Deref for YieldlessInstruction {
    type Target = RawYieldlessInstruction;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl TryFrom<RawTerminatorInstruction> for TerminatorInstruction {
    type Error = ();

    fn try_from(value: RawTerminatorInstruction) -> Result<Self, Self::Error> {
        let is_valid = match &value {
            RawTerminatorInstruction::ReturnVoid => true,
            RawTerminatorInstruction::Return(value) => value.ty != ty::Void.into(),
            RawTerminatorInstruction::BranchUnconditional(_) => true,
            RawTerminatorInstruction::BranchConditional { cond, .. } => cond.ty == ty::I1.into(),
        };
        is_valid.then_some(Self(value)).ok_or(())
    }
}

impl TryFrom<RawYieldingInstruction> for YieldingInstruction {
    type Error = ();

    fn try_from(value: RawYieldingInstruction) -> Result<Self, Self::Error> {
        let is_valid = match &value {
            RawYieldingInstruction::UnaryOp { op, op1 } => match op {
                UnaryOp::Fneg => op1.ty.to_type_cat() == ty::TypeCat::FloatingPoint,
            },
            RawYieldingInstruction::BinaryOp { op, op1, op2 } => {
                op1.ty == op2.ty
                    && match op {
                        // integer operations
                        BinaryOp::Add
                        | BinaryOp::Sub
                        | BinaryOp::Mul
                        | BinaryOp::Udiv
                        | BinaryOp::Sdiv
                        | BinaryOp::Urem
                        | BinaryOp::Srem
                        | BinaryOp::And
                        | BinaryOp::Or
                        | BinaryOp::Xor
                        | BinaryOp::Shl
                        | BinaryOp::Lshr
                        | BinaryOp::Ashr => op1.ty.to_type_cat() == ty::TypeCat::Integer,
                        // floating-point operations
                        BinaryOp::Fadd
                        | BinaryOp::Fsub
                        | BinaryOp::Fmul
                        | BinaryOp::Fdiv
                        | BinaryOp::Frem => op1.ty.to_type_cat() == ty::TypeCat::FloatingPoint,
                    }
            }
            RawYieldingInstruction::Compare { op, op1, op2 } => {
                op1.ty == op2.ty
                    && matches!(
                        (op, op1.ty.to_type_cat()),
                        (CompareOp::Icmp(_), ty::TypeCat::Integer | ty::TypeCat::Ptr)
                            | (CompareOp::Fcmp(_), ty::TypeCat::FloatingPoint)
                    )
            }
            RawYieldingInstruction::Cast { op, value, to_ty } => match op {
                CastOp::Trunc => match (value.ty, to_ty) {
                    (Type::Int(from), Type::Int(to)) => from.bits() > to.bits(),
                    _ => false,
                },
                CastOp::Zext | CastOp::Sext => match (value.ty, to_ty) {
                    (Type::Int(from), Type::Int(to)) => from.bits() < to.bits(),
                    _ => false,
                },
                CastOp::Fptrunc => {
                    value.ty.to_type_cat() == ty::TypeCat::FloatingPoint
                        && to_ty.to_type_cat() == ty::TypeCat::FloatingPoint
                        && to_ty.bit_size().exact().unwrap() < value.ty.bit_size().exact().unwrap()
                }
                CastOp::Fpext => {
                    value.ty.to_type_cat() == ty::TypeCat::FloatingPoint
                        && to_ty.to_type_cat() == ty::TypeCat::FloatingPoint
                        && to_ty.bit_size().exact().unwrap() > value.ty.bit_size().exact().unwrap()
                }

                CastOp::Fptoui | CastOp::Fptosi => {
                    value.ty.to_type_cat() == ty::TypeCat::FloatingPoint
                        && to_ty.to_type_cat() == ty::TypeCat::Integer
                }
                CastOp::Uitofp | CastOp::Sitofp => {
                    value.ty.to_type_cat() == ty::TypeCat::Integer
                        && to_ty.to_type_cat() == ty::TypeCat::FloatingPoint
                }
                CastOp::Ptrtoint => {
                    value.ty.to_type_cat() == ty::TypeCat::Ptr
                        && to_ty.to_type_cat() == ty::TypeCat::Integer
                }
                CastOp::Inttoptr => {
                    value.ty.to_type_cat() == ty::TypeCat::Integer
                        && to_ty.to_type_cat() == ty::TypeCat::Ptr
                }
                CastOp::Bitcast => match (value.ty, to_ty) {
                    (Type::Ptr(_), Type::Ptr(_)) => true,
                    (Type::Ptr(_), _) => false,
                    (_, Type::Ptr(_)) => false,
                    _ => value.ty.bit_size() == to_ty.bit_size(),
                },
            },
            RawYieldingInstruction::GetElementPtr { ptrval, index, .. } => {
                ptrval.ty == ty::Ptr.into() && index.ty.to_type_cat() == ty::TypeCat::Integer
            }
            RawYieldingInstruction::Phi { args } => match args.as_slice() {
                [head, tail @ ..] => tail.iter().all(|(tyval, _)| tyval.ty == head.0.ty),
                [] => false,
            },
            RawYieldingInstruction::Alloca { amount, .. } => match amount {
                Some(amount) => amount.ty.to_type_cat() == ty::TypeCat::Integer,
                None => true,
            },
            RawYieldingInstruction::Load { pointer, .. } => {
                pointer.ty.to_type_cat() == ty::TypeCat::Ptr
            }
            RawYieldingInstruction::Printf { .. } => true,
        };
        is_valid.then_some(Self(value)).ok_or(())
    }
}

impl TryFrom<RawYieldlessInstruction> for YieldlessInstruction {
    type Error = ();

    fn try_from(value: RawYieldlessInstruction) -> Result<Self, Self::Error> {
        let is_valid = match &value {
            RawYieldlessInstruction::Store { pointer, .. } => pointer.ty == ty::Ptr.into(),
        };
        is_valid.then_some(Self(value)).ok_or(())
    }
}

impl TerminatorInstruction {
    // NOTE: This implementation must make sure it doesn't make the underlying
    // RawTerminatorInstruction invalid! This should never be the case, since modifying the
    // labels isn't supposed to change the validity of the instruction.
    pub(crate) fn for_each_label(&mut self, mut f: impl FnMut(&mut LabelRef)) {
        match &mut self.0 {
            RawTerminatorInstruction::BranchUnconditional(label) => f(label),
            RawTerminatorInstruction::BranchConditional {
                cond: _,
                if_dest,
                else_dest,
            } => {
                f(if_dest);
                f(else_dest);
            }
            // Nothing needs to be done for instructions that don't have labels.
            RawTerminatorInstruction::ReturnVoid | RawTerminatorInstruction::Return(_) => {}
        }
    }
}

impl Instruction {
    pub(crate) fn for_each_label(&mut self, f: impl FnMut(&mut LabelRef)) {
        match self {
            Instruction::Yielding(_, instr) => instr.for_each_label(f),
            Instruction::Yieldless(instr) => instr.for_each_label(f),
        }
    }
}

impl YieldingInstruction {
    pub(crate) fn result_ty(&self) -> ty::Type {
        match self.deref() {
            RawYieldingInstruction::UnaryOp { op1, .. } => op1.ty,
            RawYieldingInstruction::BinaryOp { op1, .. } => op1.ty,
            RawYieldingInstruction::Compare { .. } => ty::I1.into(),
            RawYieldingInstruction::Cast { to_ty, .. } => *to_ty,
            RawYieldingInstruction::GetElementPtr { .. } => ty::Ptr.into(),
            RawYieldingInstruction::Phi { args } => args.get(0).unwrap().0.ty,
            RawYieldingInstruction::Alloca { .. } => ty::Ptr.into(),
            RawYieldingInstruction::Load { ty, .. } => *ty,
            RawYieldingInstruction::Printf { .. } => ty::I32.into(),
        }
    }

    // NOTE: This implementation must make sure it doesn't make the underlying
    // RawYieldingInstruction invalid! This should never be the case, since modifying the
    // labels isn't supposed to change the validity of the instruction.
    pub(crate) fn for_each_label(&mut self, mut f: impl FnMut(&mut LabelRef)) {
        match &mut self.0 {
            RawYieldingInstruction::Phi { args } => {
                for (_, label) in args {
                    f(label);
                }
            }
            // Nothing needs to be done for instructions that don't have labels.
            RawYieldingInstruction::UnaryOp { .. }
            | RawYieldingInstruction::BinaryOp { .. }
            | RawYieldingInstruction::Compare { .. }
            | RawYieldingInstruction::Cast { .. }
            | RawYieldingInstruction::GetElementPtr { .. }
            | RawYieldingInstruction::Alloca { .. }
            | RawYieldingInstruction::Load { .. }
            | RawYieldingInstruction::Printf { .. } => {}
        }
    }
}

impl YieldlessInstruction {
    // NOTE: This implementation must make sure it doesn't make the underlying
    // RawYieldlessInstruction invalid! This should never be the case, since modifying the
    // labels isn't supposed to change the validity of the instruction.
    pub(crate) fn for_each_label(&mut self, mut _f: impl FnMut(&mut LabelRef)) {
        match &mut self.0 {
            // Nothing needs to be done for instructions that don't have labels.
            RawYieldlessInstruction::Store { .. } => {}
        }
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Yielding(result, instr) => write!(f, "{result} = {instr}"),
            Instruction::Yieldless(instr) => instr.fmt(f),
        }
    }
}

impl fmt::Display for TerminatorInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.deref() {
            RawTerminatorInstruction::ReturnVoid => write!(f, "ret {}", ty::Void),
            RawTerminatorInstruction::Return(value) => {
                write!(f, "ret {ty} {value}", ty = value.ty, value = value.value)
            }
            RawTerminatorInstruction::BranchUnconditional(dest) => {
                write!(f, "br {dest}", dest = dest.0)
            }
            RawTerminatorInstruction::BranchConditional {
                cond,
                if_dest,
                else_dest,
            } => write!(
                f,
                "br {i1} {cond}, {if_dest}, {else_dest}",
                i1 = cond.ty,
                cond = cond.value,
                if_dest = if_dest.0,
                else_dest = else_dest.0
            ),
        }
    }
}

impl fmt::Display for YieldingInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.deref() {
            RawYieldingInstruction::UnaryOp { op, op1 } => {
                write!(f, "{op} {op1}")
            }
            RawYieldingInstruction::BinaryOp { op, op1, op2 } => {
                write!(
                    f,
                    "{op} {ty} {op1}, {op2}",
                    ty = self.result_ty(),
                    op1 = op1.value,
                    op2 = op2.value
                )
            }
            RawYieldingInstruction::Compare { op, op1, op2 } => {
                write!(
                    f,
                    "{op} {ty} {op1}, {op2}",
                    ty = op1.ty, // same as op2.ty
                    op1 = op1.value,
                    op2 = op2.value
                )
            }
            RawYieldingInstruction::Cast { op, value, to_ty } => {
                write!(f, "{op} {value} to {to_ty}")
            }
            RawYieldingInstruction::GetElementPtr { ty, ptrval, index } => {
                write!(f, "getelementptr {ty}, {ptrval}, {index}",)
            }
            RawYieldingInstruction::Phi { args } => match args.as_slice() {
                [(tyval, label), tail @ ..] => {
                    write!(
                        f,
                        "phi {} [ {}, {} ]",
                        tyval.ty,
                        tyval.value,
                        Id::try_from(label.0.clone()).map_err(|_| fmt::Error)?
                    )?;
                    for (tyval, label) in tail {
                        write!(
                            f,
                            ", [ {}, {} ]",
                            tyval.value,
                            Id::try_from(label.0.clone()).map_err(|_| fmt::Error)?
                        )?;
                    }
                    Ok(())
                }
                [] => unreachable!(),
            },
            RawYieldingInstruction::Alloca { ty, amount } => {
                write!(f, "alloca {ty}")?;
                if let Some(amount) = amount {
                    write!(f, ", {amount}")?;
                }
                Ok(())
            }
            RawYieldingInstruction::Load { ty, pointer } => write!(f, "load {ty}, {pointer}"),
            RawYieldingInstruction::Printf { args } => {
                write!(f, "call i32 (ptr, ...) @printf(")?;
                if let [head, tail @ ..] = args.as_slice() {
                    head.fmt(f)?;
                    for arg in tail {
                        write!(f, ", {arg}")?;
                    }
                }
                f.write_str(")")
            }
        }
    }
}

impl fmt::Display for YieldlessInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.deref() {
            RawYieldlessInstruction::Store { value, pointer } => {
                write!(f, "store {value}, {pointer}")
            }
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Fneg => "fneg",
        })
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            BinaryOp::Add => "add",
            BinaryOp::Fadd => "fadd",
            BinaryOp::Sub => "sub",
            BinaryOp::Fsub => "fsub",
            BinaryOp::Mul => "mul",
            BinaryOp::Fmul => "fmul",
            BinaryOp::Udiv => "udiv",
            BinaryOp::Sdiv => "sdiv",
            BinaryOp::Fdiv => "fdiv",
            BinaryOp::Urem => "urem",
            BinaryOp::Srem => "srem",
            BinaryOp::Frem => "frem",
            BinaryOp::And => "and",
            BinaryOp::Or => "or",
            BinaryOp::Xor => "xor",
            BinaryOp::Shl => "shl",
            BinaryOp::Lshr => "lshr",
            BinaryOp::Ashr => "ashr",
        })
    }
}

impl fmt::Display for CompareOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompareOp::Icmp(cond) => write!(f, "icmp {cond}"),
            CompareOp::Fcmp(cond) => write!(f, "fcmp {cond}"),
        }
    }
}

impl fmt::Display for IcmpCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            IcmpCond::Eq => "eq",
            IcmpCond::Ne => "ne",
            IcmpCond::Ugt => "ugt",
            IcmpCond::Uge => "uge",
            IcmpCond::Ult => "ult",
            IcmpCond::Ule => "ule",
            IcmpCond::Sgt => "sgt",
            IcmpCond::Sge => "sge",
            IcmpCond::Slt => "slt",
            IcmpCond::Sle => "sle",
        })
    }
}

impl fmt::Display for FcmpCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            FcmpCond::False => "false",
            FcmpCond::Oeq => "oeq",
            FcmpCond::Ogt => "ogt",
            FcmpCond::Oge => "oge",
            FcmpCond::Olt => "olt",
            FcmpCond::Ole => "ole",
            FcmpCond::One => "one",
            FcmpCond::Ord => "ord",
            FcmpCond::Ueq => "ueq",
            FcmpCond::Ugt => "ugt",
            FcmpCond::Uge => "uge",
            FcmpCond::Ult => "ult",
            FcmpCond::Ule => "ule",
            FcmpCond::Une => "une",
            FcmpCond::Uno => "uno",
            FcmpCond::True => "true",
        })
    }
}

impl fmt::Display for CastOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            CastOp::Trunc => "trunc",
            CastOp::Zext => "zext",
            CastOp::Sext => "sext",
            CastOp::Fptrunc => "fptrunc",
            CastOp::Fpext => "fpext",
            CastOp::Fptoui => "fptoui",
            CastOp::Fptosi => "fptosi",
            CastOp::Uitofp => "uitofp",
            CastOp::Sitofp => "sitofp",
            CastOp::Ptrtoint => "ptrtoint",
            CastOp::Inttoptr => "inttoptr",
            CastOp::Bitcast => "bitcast",
        })
    }
}
