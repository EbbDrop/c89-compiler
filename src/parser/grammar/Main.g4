// Based on C11 draft N1570 (Annex A):
// https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf

grammar Main;
import Statement;

translationUnit
    : (content+=statement)*
    ;

WS: [ \n\t\r]+ -> skip;
