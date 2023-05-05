use super::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct QualifiedTypeNode {
    pub span: Span,
    pub is_const: Option<Span>,
    // pub is_volitile: bool,
    // pub is_restrict: bool,
    pub unqualified: UnqualifiedTypeNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnqualifiedTypeNode {
    pub span: Span,
    pub data: UnqualifiedType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnqualifiedType {
    PointerType(Box<QualifiedTypeNode>),
    Void,
    // Floating types
    Float,
    Double,
    LongDouble,
    // Integer types
    Char,
    SignedChar,
    SignedShortInt,
    SignedInt,
    SignedLongInt,
    UnsignedChar,
    UnsignedShortInt,
    UnsignedInt,
    UnsignedLongInt,
}
