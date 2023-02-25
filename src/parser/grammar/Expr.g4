grammar Expr;

fullExpr
    : value=condOr
    ;

condOr
    : value=condAnd                     # CondOrSingular
    | lhs=condOr op='||' rhs=condAnd    # CondOrComposed
    ;

condAnd
    : value=expr                        # CondAndSingular
    | lhs=condAnd op='&&' rhs=expr      # CondAndComposed
    ;

expr
    : value=exprArith                                                       # ExprSingular
    | lhs=expr op=('<' | '>' | '==' | '<=' | '>=' | '!=') rhs=exprArith     # ExprComposed
    ;

exprArith
    : value=exprTerm                                # ExprArithSingular
    | lhs=exprArith op=('+' | '-') rhs=exprTerm     # ExprArithComposed
    ;

exprTerm
    : value=exprFactor                              # ExprTermSingular
    | lhs=exprTerm op=('*' | '/') rhs=exprFactor    # ExprTermComposed
    ;

exprFactor
    : '(' inner=fullExpr ')'                    # ExprFactorWrapped
    | op=('!' | '+' | '-') value=exprFactor     # ExprFactorUnaryOp
    | value=INTEGER                             # ExprFactorLiteral
    ;

INTEGER: [+-]? [0-9]+;
