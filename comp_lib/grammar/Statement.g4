parser grammar Statement;
import Expr;

statement
    : value=expr SEMICOLON                                      # StatementExpr
    | value=selectionStatement                                  # StatementSelection
    | value=iterationStatement                                  # StatementIteration
    | value=jumpStatement                                       # StatementJump
    | KW_PRINTF PAREN_LEFT value=expr PAREN_RIGHT SEMICOLON     # StatementPrintf
    | value=blockStatement                                      # StatementBlock
    | SEMICOLON                                                 # StatementEmpty
    ;

declarationStatement
    : type_name=typeName ident=identifier
      array=arrayDeclaration? SEMICOLON                         # DeclarationStatementWithoutInitializer
    | type_name=typeName ident=identifier
      array=arrayDeclaration? op=EQUALS rhs=expr SEMICOLON      # DeclarationStatementWithInitializer
    | value=functionDeclaration                                 # DeclarationStatementFunctionDeclaration
    ;

arrayDeclaration
    : BRACKET_LEFT BRACKET_RIGHT                                # ArrayDeclarationPlain
    | BRACKET_LEFT value=condExpr BRACKET_RIGHT                 # ArrayDeclarationExpr
    ;

selectionStatement
    : value=ifStatement                                         # SelectionStatementIf
    | value=switchStatement                                     # SelectionStatementSwitch
    ;

ifStatement
    : KW_IF PAREN_LEFT value=expr PAREN_RIGHT body=statement    # ifStatementIf
    | KW_IF PAREN_LEFT value=expr PAREN_RIGHT body_if=statement
      KW_ELSE body_else=statement                               # ifStatementIfElse
    ;

switchStatement
    : KW_SWITCH PAREN_LEFT value=expr PAREN_RIGHT
      BRACE_LEFT (body+=switchCase)* BRACE_RIGHT
    ;

switchCase
    : label=switchCaseExprLabel COLON (body+=statement)*        # SwitchCaseExpr
    | label=KW_DEFAULT COLON (body+=statement)*                 # SwitchCaseDefault
    ;

switchCaseExprLabel: KW_CASE value=expr;

iterationStatement
    : value=whileStatement                                      # IterationStatementWhile
    | value=forStatement                                        # IterationStatementFor

//    | KW_DO body=statement KW_WHILE
//      PAREN_LEFT value=expr PAREN_RIGHT SEMICOLON             # IterationStatementDo
   ;

whileStatement: KW_WHILE PAREN_LEFT value=expr PAREN_RIGHT body=statement;

forStatement
    : KW_FOR PAREN_LEFT init=expr? SEMICOLON cond=expr?
      SEMICOLON iter=expr? PAREN_RIGHT body=statement           # ForStatementExpr
    | KW_FOR PAREN_LEFT init=declarationStatement cond=expr?
      SEMICOLON iter=expr? PAREN_RIGHT body=statement           # ForStatementDecl
    ;

jumpStatement
    : KW_CONTINUE SEMICOLON                                     # JumpStatementContinue
    | KW_BREAK SEMICOLON                                        # JumpStatementBreak
    | KW_RETURN value=expr? SEMICOLON                           # JumpStatementReturn
    ;

blockItem
    : value=statement                                           # BlockItemStatement
    | value=declarationStatement                                # BlockItemDeclaration
    ;

blockStatement
    : BRACE_LEFT (content+=blockItem)* BRACE_RIGHT
    ;

functionDeclaration
    : prototype=functionPrototype SEMICOLON
    ;

functionDefinition
    : prototype=functionPrototype body=blockStatement
    ;

functionPrototype
    : type_name=typeName ident=identifier
      PAREN_LEFT (params+=functionParam)? (COMMA params+=functionParam)* PAREN_RIGHT
    ;

functionParam
    : type_name=typeName ident=identifier?
    ;
