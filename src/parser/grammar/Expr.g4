grammar Expr;
import Type;

expr
    : value=condExpr
    ;

condExpr
    : value=logicalOrExpr                                       # CondExprSingular
    | cond=logicalOrExpr '?' if_br=expr ':' else_br=condExpr    # CondExprTernary
    ;

logicalOrExpr
    : value=logicalAndExpr                              # LogicalOrExprSingular
    | lhs=logicalOrExpr op='||' rhs=logicalAndExpr      # LogicalOrExprComposed
    ;

logicalAndExpr
    : value=bitwiseOrExpr                               # LogicalAndExprSingular
    | lhs=logicalAndExpr op='&&' rhs=bitwiseOrExpr      # LogicalAndExprComposed
    ;

bitwiseOrExpr
    : value=bitwiseXorExpr                              # BitwiseOrExprSingular
    | lhs=bitwiseOrExpr op='|' rhs=bitwiseXorExpr       # BitwiseOrExprComposed
    ;

bitwiseXorExpr
    : value=bitwiseAndExpr                              # BitwiseXorExprSingular
    | lhs=bitwiseXorExpr op='^' rhs=bitwiseAndExpr      # BitwiseXorExprComposed
    ;


bitwiseAndExpr
    : value=equalityExpr                                # BitwiseAndExprSingular
    | lhs=bitwiseAndExpr op='&' rhs=equalityExpr        # BitwiseAndExprComposed
    ;

equalityExpr
    : value=inequalityExpr                                  # EqualityExprSingular
    | lhs=equalityExpr op=('==' | '!=') rhs=inequalityExpr  # EqualityExprComposed
    ;

inequalityExpr
    : value=arithExpr                                                   # InequalityExprSingular
    | lhs=inequalityExpr op=('<' | '>' | '<=' | '>=') rhs=arithExpr     # InequalityExprComposed
    ;

arithExpr
    : value=termExpr                                # ArithExprSingular
    | lhs=arithExpr op=('+' | '-') rhs=termExpr     # ArithExprComposed
    ;

termExpr
    : value=castExpr                                    # TermExprSingular
    | lhs=termExpr op=('*' | '/' | '%') rhs=castExpr    # TermExprComposed
    ;

castExpr
    : value=unaryExpr                                   # CastExprSingular
    | '(' type_name=typeName ')' value=castExpr         # CastExprComposed
    ;

unaryExpr
    : value=postfixExpr                                                     # UnaryExprPostfix
    | op=('++' | '--' | '!' | '+' | '-' | '&' | '*' | '~') value=castExpr   # UnaryExprPrefix
    ;

postfixExpr
    : value=primaryExpr                     # PostfixExprPrimary
    | value=postfixExpr op=('++' | '--')    # PostfixExprPostfix
    ;

primaryExpr
    : '(' inner=expr ')'    # PrimaryExprWrapped
    | value=literal         # PrimaryExprLiteral
    | ident=IDENT           # PrimaryExprIdent
    ;

literal
    : value=CHAR_LITERAL              # LiteralChar
    | value=FLOATING_POINT_LITERAL    # LiteralFloatingPoint
    | value=integerLiteral            # LiteralInteger
    ;

integerLiteral
    : value=DECIMAL_LITERAL           # IntegerLiteralDecimal
    | value=OCTAL_LITERAL             # IntegerLiteralOctal
    | value=HEXADECIMAL_LITERAL       # IntegerLiteralHexadecimalL
    ;

CHAR_LITERAL : '\'' CHARACTER  '\'';
FLOATING_POINT_LITERAL: ( [0-9]* '.' [0-9]+ | [0-9]+ '.' [0-9]* ) ([eE] [+-]? [0-9]+)?;

OCTAL_LITERAL: '0' [0-7]+;
HEXADECIMAL_LITERAL: ('0x' | '0X') [1-9a-fA-F][0-9a-fA-F]*;
DECIMAL_LITERAL: '0' | [1-9][0-9]*;

CHARACTER
    : ~[\n\r\\']
    | '\\u' [0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]
    | '\\U' [0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]
    | '\\' ['"?\\abfnrtv]
    | '\\' [0-7]
    | '\\' [0-7][0-7]
    | '\\' [0-7][0-7][0-7]
    | '\\x' [0-9a-fA-F]+
    ;
