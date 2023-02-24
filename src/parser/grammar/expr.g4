grammar expr;
full_expr: cond_or;
cond_or: cond_and | cond_or '||' cond_and;
cond_and: expr | cond_and '&&' expr;
expr:
	expr_arith
	| expr_arith ('<' | '>' | '==' | '<=' | '>=' | '!=') expr_arith;
expr_arith: expr_term | expr_arith ('+' | '-') expr_term;
expr_term: expr_factor | expr_term ('*' | '/') expr_factor;
expr_factor:
	'(' full_expr ')'
	| ('!' | '+' | '-') expr_factor
	| INTEGER;
INTEGER: [+-]? [0-9]+;
