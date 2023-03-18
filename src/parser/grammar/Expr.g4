grammar Expr;
import Tokens, Type;

expr
    : value=condExpr
    ;

condExpr
    : value=logicalOrExpr                                       # CondExprSingular
    | cond=logicalOrExpr
      QUESTION_MARK if_br=expr
      COLON else_br=condExpr                                    # CondExprTernary
    ;

logicalOrExpr
    : value=logicalAndExpr                                      # LogicalOrExprSingular
    | lhs=logicalOrExpr op=DOUBLE_PIPE rhs=logicalAndExpr       # LogicalOrExprComposed
    ;

logicalAndExpr
    : value=bitwiseOrExpr                                       # LogicalAndExprSingular
    | lhs=logicalAndExpr op=DOUBLE_AMPERSAND rhs=bitwiseOrExpr  # LogicalAndExprComposed
    ;

bitwiseOrExpr
    : value=bitwiseXorExpr                                      # BitwiseOrExprSingular
    | lhs=bitwiseOrExpr op=PIPE rhs=bitwiseXorExpr              # BitwiseOrExprComposed
    ;

bitwiseXorExpr
    : value=bitwiseAndExpr                                      # BitwiseXorExprSingular
    | lhs=bitwiseXorExpr op=CARET rhs=bitwiseAndExpr            # BitwiseXorExprComposed
    ;


bitwiseAndExpr
    : value=equalityExpr                                        # BitwiseAndExprSingular
    | lhs=bitwiseAndExpr op=AMPERSAND rhs=equalityExpr          # BitwiseAndExprComposed
    ;

equalityExpr
    : value=inequalityExpr                                      # EqualityExprSingular
    | lhs=equalityExpr
      op=(DOUBLE_EQUALS | BANG_EQUALS)
      rhs=inequalityExpr                                        # EqualityExprComposed
    ;

inequalityExpr
    : value=arithExpr                                           # InequalityExprSingular
    | lhs=inequalityExpr
      op=(ANGLE_LEFT_EQUALS | ANGLE_RIGHT_EQUALS | ANGLE_LEFT | ANGLE_RIGHT)
      rhs=arithExpr                                             # InequalityExprComposed
    ;

arithExpr
    : value=termExpr                                            # ArithExprSingular
    | lhs=arithExpr op=(PLUS | MINUS) rhs=termExpr              # ArithExprComposed
    ;

termExpr
    : value=castExpr                                            # TermExprSingular
    | lhs=termExpr op=(STAR | SLASH | PERCENT) rhs=castExpr     # TermExprComposed
    ;

castExpr
    : value=unaryExpr                                           # CastExprSingular
    | PAREN_LEFT type_name=typeName PAREN_RIGHT value=castExpr  # CastExprComposed
    ;

unaryExpr
    : value=postfixExpr                                         # UnaryExprPostfix
    | op=(DOUBLE_PLUS | DOUBLE_MINUS | BANG | PLUS | MINUS | AMPERSAND | STAR | TILDE)
      value=castExpr                                            # UnaryExprPrefix
    ;

postfixExpr
    : value=primaryExpr                                         # PostfixExprPrimary
    | value=postfixExpr op=(DOUBLE_PLUS | DOUBLE_MINUS)         # PostfixExprPostfix
    ;

primaryExpr
    : PAREN_LEFT inner=expr PAREN_RIGHT                         # PrimaryExprWrapped
    | value=literal                                             # PrimaryExprLiteral
    | ident=identifier                                          # PrimaryExprIdentifier
    ;

literal
    : value=CHAR_LITERAL                                        # LiteralChar
    | value=FLOATING_POINT_LITERAL                              # LiteralFloatingPoint
    | value=integerLiteral                                      # LiteralInteger
    ;

integerLiteral
    : value=DECIMAL_LITERAL                                     # IntegerLiteralDecimal
    | value=OCTAL_LITERAL                                       # IntegerLiteralOctal
    | value=HEXADECIMAL_LITERAL                                 # IntegerLiteralHexadecimal
    ;

