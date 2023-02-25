grammar expr;

full_expr
    : value=cond_or
    ;

cond_or
    : value=cond_and                    # CondOrSingular
    | lhs=cond_or op='||' rhs=cond_and  # CondOrComposed
    ;

cond_and
    : value=expr                        # CondAndSingular
    | lhs=cond_and op='&&' rhs=expr     # CondAndComposed
    ;

expr
    : value=expr_arith                                                      # ExprSingular
    | lhs=expr op=('<' | '>' | '==' | '<=' | '>=' | '!=') rhs=expr_arith    # ExprComposed
    ;

expr_arith
    : value=expr_term                               # ExprArithSingular
    | lhs=expr_arith op=('+' | '-') rhs=expr_term   # ExprArithComposed
    ;

expr_term
    : value=expr_factor                             # ExprTermSingular
    | lhs=expr_term op=('*' | '/') rhs=expr_factor  # ExprTermComposed
    ;

expr_factor
    : '(' inner=full_expr ')'                   # ExprFactorWrapped
    | op=('!' | '+' | '-') value=expr_factor    # ExprFactorUnaryOp
    | value=INTEGER                             # ExprFactorLiteral
    ;

INTEGER: [+-]? [0-9]+;
