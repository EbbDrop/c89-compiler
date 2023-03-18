grammar Statement;
import Tokens, Expr;

statement
    : value=expr? SEMICOLON                                     # StatementExpr
    | value=declarationStatement                                # StatementDeclaration
    | value=assignmentStatement                                 # StatementAssignment
    | value=blockStatement                                      # StatementBlock
    ;

declarationStatement
    : type_name=typeName ident=identifier SEMICOLON             # DeclarationStatementWithoutInitializer
    | type_name=typeName ident=identifier
      EQUALS rhs=expr SEMICOLON                                 # DeclarationStatementWithInitializer
    ;

assignmentStatement
    : ident=identifier EQUALS rhs=expr SEMICOLON
    ;

blockStatement
    : BRACE_LEFT (content+=statement)* BRACE_RIGHT
    ;
