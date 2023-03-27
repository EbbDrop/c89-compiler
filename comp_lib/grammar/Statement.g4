parser grammar Statement;
import Expr;

statement
    : value=expr? SEMICOLON                                     # StatementExpr
    | value=declarationStatement                                # StatementDeclaration
    | value=blockStatement                                      # StatementBlock
    ;

declarationStatement
    : type_name=typeName ident=identifier SEMICOLON             # DeclarationStatementWithoutInitializer
    | type_name=typeName ident=identifier
      op=EQUALS rhs=expr SEMICOLON                              # DeclarationStatementWithInitializer
    ;

blockStatement
    : BRACE_LEFT (content+=statement)* BRACE_RIGHT
    ;
