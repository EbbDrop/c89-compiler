use super::{ArrayDeclarationNode, BlockStatementNode, IdentNode, QualifiedTypeNode, Span};

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub prototype_span: Span,
    pub return_type: QualifiedTypeNode,
    pub ident: IdentNode,
    pub params: Vec<FunctionParamNode>,
    pub is_vararg: bool,
    pub body: BlockStatementNode,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    pub return_type: QualifiedTypeNode,
    pub ident: IdentNode,
    pub params: Vec<FunctionParamNode>,
    pub is_vararg: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParamNode {
    pub span: Span,
    pub type_name: QualifiedTypeNode,
    pub ident: Option<IdentNode>,
    pub array_parts: Vec<ArrayDeclarationNode>,
}
