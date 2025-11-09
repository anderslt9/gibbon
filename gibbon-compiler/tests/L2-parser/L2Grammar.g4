grammar L2Grammar;


// top level program
program
    : datatypeDecl* funcDecl* expr EOF
    ;

datatypeDecl
    : 'data' typeCon '=' (dataCon (typeCon)*) ('|' (dataCon (typeCon)*))* ;

funcDecl
    : funcVar ':' typeScheme funcVar '[' locRegion* ']' VAR* '=' expr ;

locatedType
    : typeCon '@' locRegion ;

typeScheme
    : ((locatedType|baseType) '->')* (locatedType|baseType) ;

val : VAR | concreteLoc | lit ;     // value can be variable, concrete location, or literal


// expression types
letExpress : 'let' VAR ':' locatedType '=' expr 'in' expr ;

letLocExpress : 'letloc' locRegion '=' locExpress 'in' expr ;

letRegionExpress : 'letregion' regionVar 'in' expr ;

caseExpress : 'case' val 'of' pat+ ;

funcAppExpress : funcVar '[' locRegion* ']' val* ;

dataConAppExpress : dataCon locRegion val* ;

// Expression grammar with operator precedence (avoid direct left recursion)
expr
    : logicalOrExpr
    ;

logicalOrExpr
    : logicalAndExpr ( '||' logicalAndExpr )*
    ;

logicalAndExpr
    : equalityExpr ( '&&' equalityExpr )*
    ;

equalityExpr
    : relationalExpr ( ( '==' | '.==.' | '*==*' | '/=' ) relationalExpr )*
    ;

relationalExpr
    : addExpr ( ( '>' | '<' | '.>.' | '.<.' | '>=' | '<=' | '.<=.' | '.>=.' ) addExpr )*
    ;

addExpr
    : mulExpr ( ( '+' | '-' | '.+.' | '.-.' ) mulExpr )*
    ;

mulExpr
    : powExpr ( ( '*' | '/' | '`div`' | '`mod`' | '.*.' | './.' ) powExpr )*
    ;

powExpr
    : atom ( ( '^' ) powExpr )*
    ;

atom
    : val
    | funcAppExpress                     // match function with located region(s)
    | dataConAppExpress                  // match data constructor with located region
    | letExpress                         // let expression
    | letLocExpress                      // letloc expression
    | letRegionExpress                   // letregion expression    
    | caseExpress                        // case expression
    | '(' expr ')'                       // parenthesized expression
    ;

baseType
    : 'Int' | 'Float' | 'Bool' | 'String'
    ;

lit 
    : INT
    | FLOAT
    | BOOL
    | STRING
    ;

// int : INT ;
// float : FLOAT ;
// bool : BOOL ;
// string : STRING ;


binaryOp 
    : '^' 
    | '*' | '/' | '`div`' | '`mod`' | '.*.' | './.'
    | '+' | '-' | '.+.' | '.-.'
    | '==' | '.==.' | '*==*' | '>' | '<' | '.>.' | '.<.' | '>=' | '<=' | '.<=.' | '.>=.' | '/='
    | '&&' | '||'
    ;

pat : dataCon '(' (val ':' locatedType)* ')' '->' expr ;

locExpress 
    : '(' 'start' regionVar ')'
    | '(' locRegion '+' INT ')'    // must specifically match 1
    | '(' 'after' locatedType ')'
    ;

locRegion 
    : '(' locVar ',' regionVar ')'
    | '(' locVar ',' regionVar ',' indexVar ')'
    ;

concreteLoc :  '(' regionVar ',' indexVar ',' locRegion ')' ;

// specific variable types
funcVar : VAR ;
regionVar  : VAR ;
locVar : VAR ;
indexVar : VAR ;
typeCon : VAR ;
dataCon : VAR ;

////// lexer rules //////
VAR : [a-z][a-zA-Z0-9_']* ;

// base data types
INT : [0-9]+ ;
FLOAT : [0-9]+ '.' [0-9]+ ;
BOOL : 'True' | 'False' ;
STRING : '"' (~["\r\n] | '\\' .)* '"' ;

// white space
WS : [ \t\r\n]+ -> skip ;