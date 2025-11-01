grammar L2Grammar;


// top level program
program
    : datatypeDecl* funcDecl* express EOF
    ;

datatypeDecl
    : 'data' typeCon '=' (dataCon (typeCon)*)* ;

funcDecl
    : funcVar ':' typeScheme funcVar VAR* '=' express ;

locatedType
    : typeCon '@' locRegion ;

typeScheme
    : (locatedType '->')* locatedType ;

val : VAR | concreteLoc | lit ;

express 
    : val 
    | funcVar '[' locRegion* ']' val*
    | dataCon locRegion val*
    | 'let' VAR ':' locatedType '=' express 'in' express
    | 'letloc' locRegion '=' locExpress 'in' express
    | 'letregion' regionVar 'in' express
    | 'case' val 'of' pat*
    | '(' express ')'
    | express binaryOp express
    ;

lit 
    : INTLIT
    | FLOATLIT
    | BOOLLIT
    | STRINGLIT
    ;

binaryOp 
    : '^' 
    | '*' | '/' | '`div`' | '`mod`' | '.*.' | './.'
    | '+' | '-' | '.+.' | '.-.'
    | '==' | '.==.' | '*==*' | '>' | '<' | '.>.' | '.<.' | '>=' | '<=' | '.<=.' | '.>=.' | '/='
    | '&&' | '||'
    ;

pat : dataCon '(' (val ':' locatedType)* ')' '->' express ;

locExpress 
    : '(' 'start' regionVar ')'
    | '(' locRegion '+' '1' ')'
    | '(' 'after' locatedType ')'
    ;


funcVar : VAR ;
regionVar  : VAR ;
locVar : VAR ;
indexVar : VAR ;
typeCon : VAR ;
dataCon : VAR ;

locRegion 
    : '(' locVar ',' regionVar ')'
    | '(' locVar ',' regionVar ',' indexVar ')'
    ;

concreteLoc :  '(' regionVar ',' indexVar ',' locRegion ')' ;

// Lexer rules
VAR : [a-z][a-zA-Z0-9_']* ;