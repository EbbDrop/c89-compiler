// Based on C11 draft N1570 (Annex A):
// https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf

parser grammar CParser;

options { tokenVocab = CLexer; }

import Statement;

translationUnit: (content+=externalDeclaration)* EOF;

externalDeclaration
    : value=declarationStatement                                # ExternalDeclarationStatement
    | value=functionDefinition                                  # ExternalDeclarationFunctionDefinition
    | value=INCLUDE                                             # ExternalDeclarationInclude
    ;
