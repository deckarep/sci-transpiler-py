// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar sexp;

sexpr
    : item* EOF
    ;

item
    : atom
    | list_
    | array_
    ;

list_
    : LPAREN item* RPAREN
    ;
    
array_
    : LBRACKET item* RBRACKET
    ;

atom
    : STRING
    | SYMBOL
    | NUMBER
    ;

STRING
    : '"' ('\\' . | ~ ('\\' | '"'))* '"'
    ;

WHITESPACE
    : (' ' | '\n' | '\t' | '\r')+ -> skip
    ;

NUMBER
    : ('+' | '-')? (DIGIT)+ ('.' (DIGIT)+)?
    ;

SYMBOL
    : SYMBOL_START (SYMBOL_START | DIGIT)*
    ;

LBRACKET
    : '['
    ;

RBRACKET
    : ']'
    ;

LPAREN
    : '('
    ;

RPAREN
    : ')'
    ;

LINE_COMMENT : ';' ~[\r\n]* -> channel(HIDDEN)
    ;

fragment SYMBOL_START
    : ('a' .. 'z')
    | ('A' .. 'Z')
    | '=='
    | '+='
    | '+'
    | '-='
    | '-'
    | '='
    | '*='
    | '*'
    | '/='
    | '/'
    | '<'
    | '<='
    | '>'
    | '>='
    | '.'
    | '@'
    | '#'
    | '&'
    | '&='
    | '$'
    | ':'
    | '!'
    ;

fragment DIGIT
    : ('0' .. '9')
    ;