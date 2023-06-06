#[cfg(test)]
mod test;

mod var_generator;

pub use var_generator::*;

use crate::StackInfo;

/// Represents a (possibly virtual) MIPS register. Can be a CPU or a FPU register.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum AnyReg {
    R(Reg),
    F(FReg),
}

impl AnyReg {
    pub fn is_virtual(&self) -> bool {
        match self {
            AnyReg::R(reg) => reg.is_virtual(),
            AnyReg::F(freg) => freg.is_virtual(),
        }
    }

    pub fn stack_info(&self) -> StackInfo {
        match self {
            AnyReg::R(reg) => reg.stack_info(),
            AnyReg::F(freg) => freg.stack_info(),
        }
    }
}

impl From<Reg> for AnyReg {
    fn from(value: Reg) -> Self {
        Self::R(value)
    }
}

impl From<FReg> for AnyReg {
    fn from(value: FReg) -> Self {
        Self::F(value)
    }
}

impl TryFrom<AnyReg> for Reg {
    type Error = ();

    fn try_from(value: AnyReg) -> Result<Self, Self::Error> {
        match value {
            AnyReg::R(reg) => Ok(reg),
            AnyReg::F(_) => Err(()),
        }
    }
}

impl TryFrom<AnyReg> for FReg {
    type Error = ();

    fn try_from(value: AnyReg) -> Result<Self, Self::Error> {
        match value {
            AnyReg::R(_) => Err(()),
            AnyReg::F(freg) => Ok(freg),
        }
    }
}

impl std::fmt::Debug for AnyReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::fmt::Display for AnyReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnyReg::R(reg) => reg.fmt(f),
            AnyReg::F(freg) => freg.fmt(f),
        }
    }
}

/// Represents a (possibly virtual) MIPS CPU register.
///
/// The MIPS registers are conventinally used as follows:
///
/// | register    | name         | preserved? | usage |
/// | ----------- | ------------ | --- | ------------ |
/// |`$0`         |`$zero`       | yes | always zero |
/// |`$1`         |`$at`         | no  | assembler temporary, shouldn't be used by instructions |
/// |`$2` - `$3`  |`$v0` - `$v1` | no  | value for function results and expression evaluation |
/// |`$4` - `$7`  |`$a0` - `$a3` | no  | function arguments |
/// |`$8` - `$15` |`$t0` - `$t7` | no  | temporaries |
/// |`$16` - `$23`|`$s0` - `$s7` | yes | saved temporaries |
/// |`$24` - `$25`|`$t8` - `$t9` | no  | temporaries |
/// |`$26` - `$27`|`$k0` - `$k1` | no  | reserved for os kernel |
/// |`$28`        |`$gp`         | yes | global pointer |
/// |`$29`        |`$sp`         | yes | stack pointer |
/// |`$30`        |`$fp` or `$s8`| yes | frame pointer or another saved temporary |
/// |`$31`        |`$ra`         | yes | return address (used by e.g. `jal`) |
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg {
    /// $0 - $31
    R(u8),
    /// A virtual register that yet has to be converted to an actual MIPS register.
    Virtual(u32),
}

impl Reg {
    // $zero
    pub const ZERO: Self = Self::R(0);
    // $at
    pub const AT: Self = Self::R(1);
    // $v0 - $v1
    pub const V0: Self = Self::R(2);
    pub const V1: Self = Self::R(3);
    // $a0 - $a3
    pub const A0: Self = Self::R(4);
    pub const A1: Self = Self::R(5);
    pub const A2: Self = Self::R(6);
    pub const A3: Self = Self::R(7);
    // $t0 - $t7
    pub const T0: Self = Self::R(8);
    pub const T1: Self = Self::R(9);
    pub const T2: Self = Self::R(10);
    pub const T3: Self = Self::R(11);
    pub const T4: Self = Self::R(12);
    pub const T5: Self = Self::R(13);
    pub const T6: Self = Self::R(14);
    pub const T7: Self = Self::R(15);
    // $s0 - $s7
    pub const S0: Self = Self::R(16);
    pub const S1: Self = Self::R(17);
    pub const S2: Self = Self::R(18);
    pub const S3: Self = Self::R(19);
    pub const S4: Self = Self::R(20);
    pub const S5: Self = Self::R(21);
    pub const S6: Self = Self::R(22);
    pub const S7: Self = Self::R(23);
    // $t8 - $t9
    pub const T8: Self = Self::R(24);
    pub const T9: Self = Self::R(25);
    /// $k0 - $k1
    pub const K0: Self = Self::R(26);
    pub const K1: Self = Self::R(27);
    /// $gp, $sp, $fp or $s8, $ra
    pub const GP: Self = Self::R(28);
    pub const SP: Self = Self::R(29);
    pub const FP: Self = Self::R(30);
    pub const S8: Self = Self::R(30);
    pub const RA: Self = Self::R(31);

    /// Returns `true` if the register is by convention preserved when calling a function.
    ///
    /// Virtual registers are considered to be preserved.
    pub fn is_saved(&self) -> bool {
        match *self {
            Self::ZERO => true,
            Self::AT => false,
            Self::V0 | Self::V1 => false,
            Self::A0 | Self::A1 | Self::A2 | Self::A3 => false,
            Self::T0
            | Self::T1
            | Self::T2
            | Self::T3
            | Self::T4
            | Self::T5
            | Self::T6
            | Self::T7
            | Self::T8
            | Self::T9 => false,
            Self::S0
            | Self::S1
            | Self::S2
            | Self::S3
            | Self::S4
            | Self::S5
            | Self::S6
            | Self::S7 => true,
            Self::K0 | Self::K1 => false,
            Self::GP => true,
            Self::SP => true,
            Self::FP => true,
            Self::RA => true,
            Self::R(n) => panic!("encountered nonexisting register ${n}"),
            Self::Virtual(_) => true,
        }
    }

    /// Returns `true` if this is a virtual register.
    ///
    /// A virtual register is a register that yet has to be converted to a real register.
    /// There are an infinite number of virtual registers. They can be used in an initial pass that
    /// doesn't have to restrict itself to the limits imposed by real registers (e.g. that there's
    /// only a finite number of them, that not all of them are preserved in calls, etc.). A later
    /// pass then has to convert them to real registers.
    pub fn is_virtual(&self) -> bool {
        matches!(self, Self::Virtual(_))
    }

    /// Returns the physical register number of this register. Panics if this is a virtual register.
    pub fn phy_num(&self) -> u8 {
        match self {
            Reg::R(n) => *n,
            Reg::Virtual(_) => panic!("virtual register doesn't have physical number"),
        }
    }

    pub fn stack_info(&self) -> StackInfo {
        StackInfo {
            signed: false,
            size: crate::size::WORD as u128,
            alignment: crate::AlignBoundary::WORD,
        }
    }
}

impl std::fmt::Debug for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            f.write_str(match *self {
                Self::ZERO => "$zero",
                Self::AT => "$at",
                Self::V0 => "$v0",
                Self::V1 => "$v1",
                Self::A0 => "$a0",
                Self::A1 => "$a1",
                Self::A2 => "$a2",
                Self::A3 => "$a3",
                Self::T0 => "$t0",
                Self::T1 => "$t1",
                Self::T2 => "$t2",
                Self::T3 => "$t3",
                Self::T4 => "$t4",
                Self::T5 => "$t5",
                Self::T6 => "$t6",
                Self::T7 => "$t7",
                Self::T8 => "$t8",
                Self::T9 => "$t9",
                Self::S0 => "$s0",
                Self::S1 => "$s1",
                Self::S2 => "$s2",
                Self::S3 => "$s3",
                Self::S4 => "$s4",
                Self::S5 => "$s5",
                Self::S6 => "$s6",
                Self::S7 => "$s7",
                Self::K0 => "$k0",
                Self::K1 => "$k1",
                Self::GP => "$gp",
                Self::SP => "$sp",
                Self::FP => "$fp",
                Self::RA => "$ra",
                Self::R(n) => panic!("encountered nonexistent register ${n}"),
                Self::Virtual(n) => return write!(f, "@{n}"),
            })
        } else {
            match self {
                Self::R(n @ 0..=31) => write!(f, "${n}"),
                Self::R(n) => panic!("encountered nonexistent register: ${n}"),
                Self::Virtual(n) => write!(f, "@{n}"),
            }
        }
    }
}

/// Represents a (possibly virtual) MIPS FPU register.
///
/// Real FPU registers have the following restrictions:
///
///  - There're only 32: $f0 - $f31.
///  - Only the even registers ($f0, $f2, ..., $f30) can be used to for double-precision values.
///  - If an even register is used for a double-precision value, the next uneven register cannot be
///    used for a single-precision value without invalidating the double-precision value.
///
/// The MIPS registers are conventinally used as follows:
///
/// | registers      | preserved? | usage |
/// | -------------- | --- | ------------ |
/// |`$f0`  - `$f2`  | no  | floating-point function results |
/// |`$f4`  - `$f10` | no  | temporaries |
/// |`$f12` - `$f14` | no  | to pass the first arguments |
/// |`$f16` - `$f18` | no  | temporaries |
/// |`$f20` - `$f30` | yes | saved temporaries |

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum FReg {
    F(u8),
    /// Virtual single-precision FPU register.
    VirtualSingle(u32),
    /// Virtual double-precision FPU register.
    VirtualDouble(u32),
}

impl FReg {
    /// Returns `true` if this fpu register can be used for operations on doubles.
    ///
    /// Only even-numbered fpu registers can be used for storing doubles.
    ///
    /// # Examples
    ///
    /// ```
    /// use mips_ir::FReg;
    ///
    /// assert!(!FReg::F(3).is_double());
    /// assert!(FReg::F(4).is_double());
    /// assert!(FReg::F(0).is_double());
    ///
    /// assert!(!FReg::VirtualSingle(6).is_double());
    /// assert!(FReg::VirtualDouble(7).is_double());
    /// ```
    pub fn is_double(&self) -> bool {
        match self {
            Self::F(n) => n % 2 == 0,
            Self::VirtualSingle(_) => false,
            Self::VirtualDouble(_) => true,
        }
    }

    /// Returns `true` if the register is by convention preserved when calling a function.
    ///
    /// Virtual registers are considered to be preserved.
    pub fn is_saved(&self) -> bool {
        match *self {
            Self::F(0..=19) => false,
            Self::F(20..=31) => true,
            Self::F(n) => panic!("encountered nonexisting register $f{n}"),
            Self::VirtualSingle(_) | Self::VirtualDouble(_) => true,
        }
    }

    /// Returns `true` if this is a virtual FPU register.
    ///
    /// A virtual register is a register that yet has to be converted to a real register.
    /// There are an infinite number of virtual registers. They can be used in an initial pass that
    /// doesn't have to restrict itself to the limits imposed by real registers (e.g. that there're
    /// only 32, that only the even registers can be used for doubles, etc.). A later pass then has
    /// to convert them to real registers.
    pub fn is_virtual(&self) -> bool {
        !matches!(self, Self::F(_))
    }

    /// Returns the physical register number of this register. Panics if this is a virtual register.
    pub fn phy_num(&self) -> u8 {
        match self {
            Self::F(n) => *n,
            Self::VirtualSingle(_) | Self::VirtualDouble(_) => {
                panic!("virtual register doesn't have physical number")
            }
        }
    }

    pub fn stack_info(&self) -> StackInfo {
        if self.is_double() {
            StackInfo {
                signed: false,
                size: crate::size::DOUBLE as u128,
                alignment: crate::AlignBoundary::DOUBLE,
            }
        } else {
            StackInfo {
                signed: false,
                size: crate::size::WORD as u128,
                alignment: crate::AlignBoundary::WORD,
            }
        }
    }

    pub fn ffmt(&self) -> crate::FFmt {
        match self.is_double() {
            true => crate::FFmt::D,
            false => crate::FFmt::S,
        }
    }
}

impl std::fmt::Debug for FReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::fmt::Display for FReg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FReg::F(n @ 0..=31) => write!(f, "$f{n}"),
            FReg::F(n) => panic!("encountered nonexistent register $f{n}"),
            FReg::VirtualSingle(n) => write!(f, "@fs{n}"),
            FReg::VirtualDouble(n) => write!(f, "@fd{n}"),
        }
    }
}
