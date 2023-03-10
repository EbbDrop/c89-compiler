grammar Statement;
import Expr;

statement
    : value=expr? ';'             # StatementExpr
    | value=declarationStatement  # StatementDeclaration
    | value=assignmentStatement   # StatementAssignment
    | value=blockStatement        # StatementBlock
    ;

declarationStatement
    : type_name=typeName ident=identifier ';'               # DeclarationStatementWithoutInitializer
    | type_name=typeName ident=identifier '=' rhs=expr ';'  # DeclarationStatementWithInitializer
    ;

assignmentStatement
    : ident=identifier '=' rhs=expr ';'
    ;

blockStatement
    : '{' (content+=statement)* '}'
    ;
