use super::{ExpressionNode, FunctionDeclaration, IdentNode, QualifiedTypeNode, Span};

#[derive(Debug, Clone, PartialEq)]
pub enum Declaration {
    Variable(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    pub type_name: QualifiedTypeNode,
    pub ident: IdentNode,
    pub array_parts: Vec<ArrayDeclarationNode>,
    pub initializer: Option<(Span, ExpressionNode)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayDeclarationNode {
    pub span: Span,
    pub data: ArrayDeclaration,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayDeclaration {
    /// `arr[];`
    Unknown,
    /// `arr[3];`
    Known(ExpressionNode),
}
