lexer grammar CLexer;

channels { COMMENTS }

SINGLELINE_COMMENT: '//' ( ~[\n\r] )* -> channel(COMMENTS);
MULTILINE_COMMENT: '/*' ( . )*? '*/' -> channel(COMMENTS);

DECIMAL_LITERAL: '0' | [1-9][0-9]*;
OCTAL_LITERAL: '0' [0-7]+;
HEXADECIMAL_LITERAL: ('0x' | '0X') [0-9a-fA-F]+;
FLOATING_POINT_LITERAL: ( [0-9]* '.' [0-9]+ | [0-9]+ '.' [0-9]* ) ([eE] [+-]? [0-9]+)?;

CHAR_LITERAL: '\'' ( ~[\n\r\\'] | '\\' ~[\n\r] )+ '\'';

// 2-char symbols
DOUBLE_PIPE: '||';
DOUBLE_AMPERSAND: '&&';
DOUBLE_EQUALS: '==';
BANG_EQUALS: '!=';
ANGLE_LEFT_EQUALS: '<=';
ANGLE_RIGHT_EQUALS: '>=';
DOUBLE_PLUS: '++';
DOUBLE_MINUS: '--';
DOUBLE_ANGLE_LEFT: '<<';
DOUBLE_ANGLE_RIGHT: '>>';

// 1-char symbols
SEMICOLON: ';';
EQUALS: '=';
QUESTION_MARK: '?';
COLON: ':';
PIPE: '|';
CARET: '^';
AMPERSAND: '&';
ANGLE_LEFT: '<';
ANGLE_RIGHT: '>';
PLUS: '+';
MINUS: '-';
STAR: '*';
SLASH: '/';
PERCENT: '%';
BANG: '!';
TILDE: '~';
PAREN_LEFT: '(';
PAREN_RIGHT: ')';
BRACKET_LEFT: '[';
BRACKET_RIGHT: ']';
BRACE_LEFT: '{';
BRACE_RIGHT: '}';

// keywords
KW_CONST: 'const';
KW_CHAR: 'char';
KW_INT: 'int';
KW_FLOAT: 'float';
KW_PRINTF: 'printf';

IDENT: [_a-zA-Z][_a-zA-Z0-9]*;

WS: [ \n\t\r]+ -> skip;

ERROR_TOKEN: . ; // catch all
