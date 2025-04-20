grammar sci2;

// Parser Rules
program
    : scriptDeclaration includeDeclaration? useDeclaration* publicDeclaration? content EOF
    ;

scriptDeclaration
    : '(script#' NUMBER ')'
    ;

includeDeclaration
    : '(include' IDENTIFIER ('.' IDENTIFIER)* ')'
    ;

useDeclaration
    : '(use' IDENTIFIER ')'
    ;

publicDeclaration
    : '(public' (publicElement)+ ')'
    ;

publicElement
    : IDENTIFIER NUMBER
    ;

content
    : (procedureDefinition | classDefinition)*
    ;

procedureDefinition
    : '(procedure' '(' IDENTIFIER parameterList ')' procedureBody ')'
    ;

parameterList
    : /* empty */
    | IDENTIFIER+
    ;

procedureBody
    : statementBlock
    | localVarDeclaration statementBlock
    ;

localVarDeclaration
    : '&tmp' IDENTIFIER+
    ;

classDefinition
    : '(class' IDENTIFIER ('of' IDENTIFIER)? propertiesBlock? methodDefinition* ')'
    ;

propertiesBlock
    : '(properties' (propertyDefinition)* ')'
    ;

propertyDefinition
    : IDENTIFIER expression
    ;

methodDefinition
    : '(method' '(' IDENTIFIER parameterList ')' methodBody ')'
    ;

methodBody
    : statementBlock
    | localVarDeclaration statementBlock
    ;

statementBlock
    : statement*
    ;

statement
    : returnStatement
    | ifStatement
    | forStatement
    | assignmentStatement
    | messageStatement
    | expression
    ;

returnStatement
    : '(return' expression? ')'
    ;

ifStatement
    : ifExpression
    ;
    
ifExpression
    : '(if' expression thenBlock elseBlock? ')'
    ;

thenBlock
    : statement+
    ;

elseBlock
    : 'else' statement+
    ;

forStatement
    : '(for' '(' forInitializer ')' forCondition forIterator statement+ ')'
    ;

forInitializer
    : '(' expression ')'
    ;

forCondition
    : expression
    ;

forIterator
    : '(' expression ')'
    ;

assignmentStatement
    : '(' assignmentOperator variable expression ')'
    ;

assignmentOperator
    : '=' | '+=' | '-=' | '*=' | '/=' | '<<=' | '>>=' | '&=' | '%='
    ;

messageStatement
    : '(' receiver message arguments? ')'
    ;

receiver
    : IDENTIFIER | 'self' | variable
    ;

message
    : IDENTIFIER ':' 
    ;

arguments
    : expression+
    ;

expression
    : literal
    | variable
    | arrayAccess
    | propertyAccess
    | ifExpression
    | messageExpression
    | binaryExpression
    | unaryExpression
    | parenExpression
    | andExpression
    | orExpression
    ;

literal
    : NUMBER
    | STRING
    ;

variable
    : IDENTIFIER
    ;

arrayAccess
    : '[' IDENTIFIER expression ']'
    ;

propertyAccess
    : '(' variable propertyName ':' ')'
    ;

propertyName
    : IDENTIFIER
    ;

messageExpression
    : '(' receiver message arguments? ')'
    ;

binaryExpression
    : '(' binaryOperator expression expression ')'
    ;

binaryOperator
    : '+' | '-' | '*' | '/' | '>' | '<' | '>=' | '<=' | '==' | '!='
    ;

unaryExpression
    : '(' unaryOperator expression ')'
    ;

unaryOperator
    : '!' | '-' | '++' | '--'
    ;

parenExpression
    : '(' expression ')'
    ;

andExpression
    : '(and' expression+ ')'
    ;

orExpression
    : '(or' expression+ ')'
    ;

// Lexer Rules
COMMENT
    : ';' ~[\r\n]* -> channel(HIDDEN)
    ;

WS
    : [ \t\r\n]+ -> skip
    ;

NUMBER
    : [0-9]+
    | '-'? [0-9]+ ('.' [0-9]+)?
    ;

STRING
    : '"' (~["\r\n] | '\\"')* '"'
    ;

IDENTIFIER
    : [a-zA-Z_][a-zA-Z0-9_-]*
    | '&rest'
    ;