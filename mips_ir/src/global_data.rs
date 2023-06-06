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
    #[inline]
    pub fn bytes(self) -> u128 {
        1 << self.0
    }

    #[inline]
    pub fn next_multiple_from(&self, start: u32) -> u32 {
        // Based on the implementation of `core::u32::next_multiple_of`.
        match start % self.bytes() as u32 {
            0 => start,
            r => start + (self.bytes() as u32 - r),
        }
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
    LabelWord(Label),
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
            DataDirective::LabelWord(_) => AlignBoundary::WORD,
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

    /// Returns the global label of this data item. It's not possible to change this.
    pub fn label(&self) -> &Label {
        &self.label
    }

    /// Returns the align boundary if one was set, or the natural align of the data if not.
    pub fn align(&self) -> AlignBoundary {
        self.align.unwrap_or(self.data.natural_align())
    }

    /// Sets the align to the given align boundary, or to the natural alignment of the given align
    /// is `None`.
    pub fn set_align(&mut self, align: Option<AlignBoundary>) {
        self.align = align;
    }

    /// Returns a reference to the data directive of this global data item.
    pub fn data(&self) -> &DataDirective {
        &self.data
    }

    /// Sets the data to the given data directive.
    pub fn set_data(&mut self, data: DataDirective) {
        self.data = data;
    }
}
