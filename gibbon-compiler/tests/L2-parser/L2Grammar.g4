grammar L2Grammar;


// top level program
program
    : datatypeDecl* funcDecl* express EOF
    ;

datatypeDecl
    : 'data' typeCon '=' (dataCon (typeCon)*)* ;

funcDecl
    : funcVar ':' typeScheme funcVar '[' locRegion* ']' VAR* '=' express ;

locatedType
    : typeCon '@' locRegion ;

typeScheme
    : ((locatedType|baseType) '->')* (locatedType|baseType) ;

val : VAR | concreteLoc | lit ;     // value can be variable, concrete location, or literal


// expression types
letExpress : 'let' VAR ':' locatedType '=' express 'in' express ;

letLocExpress : 'letloc' locRegion '=' locExpress 'in' express ;

letRegionExpress : 'letregion' regionVar 'in' express ;

caseExpress : 'case' val 'of' pat* ;

funcAppExpress : funcVar '[' locRegion* ']' val* ;

dataConAppExpress : dataCon locRegion val* ;

express 
    : val 
    | funcAppExpress                     // match function with located region(s)
    | dataConAppExpress                  // match data constructor with located region
    | letExpress                         // let expression
    | letLocExpress                      // letloc expression
    | letRegionExpress                   // letregion expression    
    | caseExpress                        // case expression
    | '(' express ')'                    // parenthesized expression
    | express binaryOp express           // binary operation
    ;

baseType
    : 'Int' | 'Float' | 'Bool' | 'String'
    ;

lit 
    : int
    | float
    | bool
    | string
    ;

int : INT ;
float : FLOAT ;
bool : BOOL ;
string : STRING ;


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