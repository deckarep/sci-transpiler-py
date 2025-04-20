grammar sci;

sci
    : root_declaration* EOF
    ;

root_declaration
    : LPAREN 'script#' NUMBER RPAREN
    | LPAREN 'include' SYMBOL RPAREN
    | LPAREN 'use' SYMBOL RPAREN
    | LPAREN 'public' (SYMBOL NUMBER)+ RPAREN
    | LPAREN 'local' local_pair_decl+ RPAREN
    | LPAREN 'procedure' preamble_decl statement* RPAREN
    | LPAREN 'class' SYMBOL 'of' SYMBOL properties_declaration method_declaration* RPAREN
    | LPAREN 'instance' SYMBOL 'of' SYMBOL properties_declaration method_declaration* RPAREN
    ;

preamble_decl
    : LPAREN SYMBOL SYMBOL* preamble_tmp? RPAREN
    ;

preamble_tmp
    : '&tmp' SYMBOL+
    ;

statement
    : assign_statement
    | augmented_statement
    | prefix_statement
    | call_statement
    | if_statement
    | message_statement
    | cond_statement
    | switch_statement
    | return_statement
    ;

local_pair_decl
    : (SYMBOL | local_array_size_decl)
    ;

local_array_size_decl
    : LBRACKET SYMBOL NUMBER RBRACKET
    ;

properties_declaration
    : LPAREN 'properties' (SYMBOL atom)* RPAREN
    ;

method_declaration
    : LPAREN 'method' preamble_decl statement* RPAREN
    ;

prefix_statement
    : prefix_expression
    ;

assign_statement
    : assign_expression
    ;

augmented_statement
    : LPAREN ('*=' | '/=' | '+=' | '-=') lhs rhs RPAREN
    ;

call_statement
    : call_expression
    ;

return_statement
    : LPAREN 'return' expression RPAREN
    ;

if_statement
    : LPAREN 'if' expression statement* if_statement_else_clause? RPAREN
    ;

if_statement_else_clause
    : 'else' statement+
    ;

cond_statement
    : LPAREN 'cond' cond_branch+ RPAREN
    ;

cond_branch
    : LPAREN ('else' | expression) statement* RPAREN
    ;

switch_statement
    : LPAREN 'switch' expression switch_branch+ RPAREN
    ;

switch_branch
    :   LPAREN (atom | 'else') (atom | statement+) RPAREN
    ;

message_statement
    : message_expression
    ;

message_expression
    : LPAREN item msgkv msgkv* RPAREN
    ;

msgsend
    : item msgkv msgkv*
    ;

msgkv
    : SELECTOR item*
    ;

item
    : expression
    | array_
    ;

lhs
    : SYMBOL
    | array_
    ;

rhs
    : expression
    ;

expression
    //: negate_expression
    : op_expression
    | assign_expression
    | prefix_expression
    //| binary_expression
    //| conditional_expression
    | if_expression
    | message_expression
    | call_expression
    | switch_expression
    | atom
    ;

op_expression
    : LPAREN OP expression* RPAREN
    ;

OP
    : '+' | '-' | '*' | '/' | 'or' | 'and' | '==' | '!=' | 'not' | '-=' | '+=' | '<=' | '>=' | '<' | '>'
    ;

prefix_expression
    : LPAREN (DOUBLE_DASH | DOUBLE_PLUS) SYMBOL RPAREN
    ;

assign_expression
    : LPAREN ('=' | '-=' | '+=' | '<<=' | '>>=' | '*=' | '/=') lhs rhs RPAREN
    ;

switch_expression
    : LPAREN 'switch' atom arm+ RPAREN
    ;

arm
    : LPAREN atom atom RPAREN
    ;

left_exp
    : expression
    ;

right_exp
    : expression
    ;

if_expression
    : LPAREN 'if' expression expression 'else' expression RPAREN
    ;

call_expression
    : LPAREN SYMBOL expression* RPAREN
    ;

array_
    :  LBRACKET atom* RBRACKET // Verify the atom can't have expressions.
    ;

atom
    : STRING
    | SYMBOL
    | HEX_NUMBER
    | NUMBER
    ;

LINE_COMMENT : ';' ~[\r\n]* -> channel(HIDDEN)
    ;

WHITESPACE
    : (' ' | '\n' | '\t' | '\r')+ -> skip
    ;

SELECTOR
    : SYMBOL ':'
    ;

STRING
    : '"' ('\\' . | ~ ('\\' | '"'))* '"'
    | '{' ( ~[{}] | '{' ~[{}]* '}' )* '}' //special rule to match curly-brace strings.
    ;

SYMBOL
    : SYMBOL_START (SYMBOL_START | DIGIT)*
    | '-info-'
    | '-propDict-'
    | '-classScript-'
    | '-super-'
    ;

SYMBOL_START
    : ('a' .. 'z')
    | ('A' .. 'Z')
    | '.'
    | '#'
    | '&'
    | '@'
    ;

HEX_NUMBER
    : '$' [a-zA-Z0-9]+
    ;

NUMBER
    : ('+' | '-')? (DIGIT)+ ('.' (DIGIT)+)?
    ;

DIGIT
    : ('0' .. '9')
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

DOUBLE_DASH
    : '--'
    ;

DOUBLE_PLUS
    : '++'
    ;