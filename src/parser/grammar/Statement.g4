grammar Statement;
import Expr;

statement
    : value=expr? ';'             # StatementExpr
    | value=declarationStatement  # StatementDeclaration
    | value=assignmentStatement   # StatementAssignment
    | value=blockStatement        # StatementBlock
    ;

declarationStatement
    : type_name=typeName ident=IDENT ';'                   # DeclarationStatementWithoutInitializer
    | type_name=typeName ident=IDENT '=' rhs=expr ';'      # DeclarationStatementWithInitializer
    ;

assignmentStatement
    : ident=IDENT '=' rhs=expr ';'
    ;

blockStatement
    : '{' (content+=statement)* '}'
    ;
