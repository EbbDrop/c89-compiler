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
    | KW_LONG           # TypeSpecifierLong
    | KW_SHORT          # TypeSpecifierShort
    | KW_SIGNED         # TypeSpecifierSigned
    | KW_UNSIGNED       # TypeSpecifierUnsigned
    ;

primitiveType
    : KW_VOID           # PrimitiveTypeVoid
    | KW_CHAR           # PrimitiveTypeChar
    | KW_INT            # PrimitiveTypeInt
    | KW_FLOAT          # PrimitiveTypeFloat
    | KW_DOUBLE         # PrimitiveTypeDouble
    ;

identifier
    : value=IDENT
    ;
