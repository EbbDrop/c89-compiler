grammar Type;

typeName
    : ((qualifiers+=typeQualifier) | (specifiers+=typeSpecifier))+   # TypeNamePlain
    | inner=typeName '*' (ptr_qualifiers+=typeQualifier)*            # TypeNamePointer
    ;

typeQualifier
    : 'const'           # TypeQualifierConst
    ;

typeSpecifier
    : tp=primitiveType  # TypeSpecifierPrimitive
    ;

primitiveType
    : 'char'        # PrimitiveTypeChar
    | 'int'         # PrimitiveTypeInt
    | 'float'       # PrimitiveTypeFloat
    ;

identifier
    : value=IDENT
    ;

IDENT: [_a-zA-Z][_a-zA-Z0-9]*;

