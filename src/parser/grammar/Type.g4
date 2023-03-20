parser grammar Type;

typeName
    : ((qualifiers+=typeQualifier) | (specifiers+=typeSpecifier))+  # TypeNamePlain
    | inner=typeName STAR (ptr_qualifiers+=typeQualifier)*          # TypeNamePointer
    ;

typeQualifier
    : KW_CONST          # TypeQualifierConst
    ;

typeSpecifier
    : tp=primitiveType  # TypeSpecifierPrimitive
    ;

primitiveType
    : KW_CHAR           # PrimitiveTypeChar
    | KW_INT            # PrimitiveTypeInt
    | KW_FLOAT          # PrimitiveTypeFloat
    ;

identifier
    : value=IDENT
    ;
