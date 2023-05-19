use crate::Label;
use vec1::Vec1;

/// Exponent `n` indicating data should be aligned at a `2.pow(n)`-byte boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AlignBoundary(pub u32);

impl AlignBoundary {
    pub const BYTE: Self = Self(0);
    pub const HALF: Self = Self(1);
    pub const WORD: Self = Self(2);
    pub const DOUBLE: Self = Self(3);

    /// Returns the actual align boundary in bytes. Equivalent to `2.pow(*self)`.
    pub fn bytes(self) -> u128 {
        1 << self.0
    }
}

impl std::ops::Deref for AlignBoundary {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for AlignBoundary {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Standard byte sizes.
pub mod size {
    pub const BYTE: u32 = 1;
    pub const HALF: u32 = 2;
    pub const WORD: u32 = 4;
    pub const DOUBLE: u32 = 8;
}

#[derive(Debug, Clone)]
pub enum DataDirective {
    Space(u128),
    Ascii(Vec<u8>),
    AsciiZ(Vec<u8>),
    Byte(u8),
    Half(u16),
    Word(u32),
    Float(f32),
    Double(f64),
    Bytes(Vec1<u8>),
    Halfs(Vec1<u16>),
    Words(Vec1<u32>),
    Floats(Vec1<f32>),
    Doubles(Vec1<f64>),
}

impl DataDirective {
    pub fn natural_align(&self) -> AlignBoundary {
        match self {
            DataDirective::Space(_) => AlignBoundary::BYTE,
            DataDirective::Ascii(_) => AlignBoundary::BYTE,
            DataDirective::AsciiZ(_) => AlignBoundary::BYTE,
            DataDirective::Byte(_) => AlignBoundary::BYTE,
            DataDirective::Half(_) => AlignBoundary::HALF,
            DataDirective::Word(_) => AlignBoundary::WORD,
            DataDirective::Float(_) => AlignBoundary::WORD,
            DataDirective::Double(_) => AlignBoundary::DOUBLE,
            DataDirective::Bytes(_) => AlignBoundary::BYTE,
            DataDirective::Halfs(_) => AlignBoundary::HALF,
            DataDirective::Words(_) => AlignBoundary::WORD,
            DataDirective::Floats(_) => AlignBoundary::WORD,
            DataDirective::Doubles(_) => AlignBoundary::DOUBLE,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GlobalData {
    label: Label,
    align: Option<AlignBoundary>,
    data: DataDirective,
}

impl GlobalData {
    pub fn new(label: Label, data: DataDirective) -> Self {
        Self {
            label,
            align: None,
            data,
        }
    }

    /// Returns `self` with the align set to the specified align if it is `Some`, otherwise the
    /// natural align of the data is used.
    pub fn with_align(self, align: Option<AlignBoundary>) -> Self {
        Self { align, ..self }
    }

    pub fn label(&self) -> &Label {
        &self.label
    }

    pub fn align(&self) -> AlignBoundary {
        self.align.unwrap_or(self.data.natural_align())
    }

    pub fn data(&self) -> &DataDirective {
        &self.data
    }
}

impl std::fmt::Display for GlobalData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(align) = self.align {
            if self.data.natural_align() != align {
                writeln!(f, "\t.align\t{}", *align)?;
            }
        }

        writeln!(f, "{}:", self.label)?;

        match &self.data {
            DataDirective::Space(n) => writeln!(f, "\t.space\t{n}"),
            DataDirective::Ascii(string) => writeln!(
                f,
                "\t.ascii\t\"{}\"",
                std::str::from_utf8(string).expect("unimplemented: escaping .ascii strings")
            ),
            DataDirective::AsciiZ(string) => writeln!(
                f,
                "\t.asciiz\t\"{}\"",
                std::str::from_utf8(string).expect("unimplemented: escaping .asciiz strings")
            ),
            DataDirective::Byte(x) => writeln!(f, "\t.byte\t{x}"),
            DataDirective::Half(x) => writeln!(f, "\t.half\t{x}"),
            DataDirective::Word(x) => writeln!(f, "\t.word\t{x}"),
            DataDirective::Float(x) => writeln!(f, "\t.float\t{x}"),
            DataDirective::Double(x) => writeln!(f, "\t.double\t{x}"),
            DataDirective::Bytes(xs) => {
                write!(f, "\t.byte\t{}", xs.first())?;
                for x in xs.iter().skip(1) {
                    write!(f, ", {x}")?;
                }
                writeln!(f)
            }
            DataDirective::Halfs(xs) => {
                write!(f, "\t.half\t{}", xs.first())?;
                for x in xs.iter().skip(1) {
                    write!(f, ", {x}")?;
                }
                writeln!(f)
            }
            DataDirective::Words(xs) => {
                write!(f, "\t.word\t{}", xs.first())?;
                for x in xs.iter().skip(1) {
                    write!(f, ", {x}")?;
                }
                writeln!(f)
            }
            DataDirective::Floats(xs) => {
                write!(f, "\t.float\t{}", xs.first())?;
                for x in xs.iter().skip(1) {
                    write!(f, ", {x}")?;
                }
                writeln!(f)
            }
            DataDirective::Doubles(xs) => {
                write!(f, "\t.double\t{}", xs.first())?;
                for x in xs.iter().skip(1) {
                    write!(f, ", {x}")?;
                }
                writeln!(f)
            }
        }
    }
}
