{
module Gibbon.L2ParserNative.Parser where
-- import Data.Char (isSpace, isAlpha, isDigit, isLower)
-- import Data.List (isPrefixOf, isSuffixOf)
-- import System.Environment (getArgs)
-- import Control.Monad (forM_, when)
-- import System.FilePath.Posix (takeBaseName)
-- import System.IO (writeFile, appendFile)
import Gibbon.L2ParserNative.Helper (makeRed, E(Ok, Failed))
import Gibbon.L2ParserNative.AST
import Gibbon.L2ParserNative.Tokens
import Gibbon.L2ParserNative.PrintAST
import Gibbon.L2ParserNative.Lexer
}

%name l2ParserNative
%tokentype { Token }
%error { parseError }
%monad { E } { thenE } { returnE }
-- %lexer { lexer } { TokenEOF }
%right '||'
%right '&&'
%nonassoc '==' '.==.' '*==*' '>' '<' '.>.' '.<.' '>=' '<=' '.>=.' '.<=.' '/='
%left '+' '-' '.-.' '.+.'
%left '*' '/' '`div`' '`mod`' '.*.' './.'
%left '^'

-- %lexer { L2LexerNative }

%token 
    -- data constructor
    data        { TokenData _ }
    
    -- symbols
    '='         { TokenAssign _ }
    ':'         { TokenColon _ }
    '['         { TokenLBracket _ }
    ']'         { TokenRBracket _ }
    '@'         { TokenAt _ }
    '->'        { TokenArrow _ }
    '|'         { TokenBar _ }
    ','         { TokenComma _ }
    '('         { TokenLParen _ }
    ')'         { TokenRParen _ }
    '--'        { TokenComment _ }
    
    -- common expr keywords
    let         { TokenLet _ }
    in          { TokenIn _ }
    letloc      { TokenLetLoc _ }
    letregion   { TokenLetRegion _ }
    case        { TokenCase _ }
    of          { TokenOf _ }
    start       { TokenStart _ }
    after       { TokenAfter _ }

    -- binary operations
    '^'         { TokenPow _ }
    '*'         { TokenMul _ }
    '/'         { TokenDiv _ }
    '`div`'     { TokenDivInline _ }
    '`mod`'     { TokenModInline _ }
    '.*.'       { TokenFMul _ } 
    './.'       { TokenFDiv _ }
    '+'         { TokenAdd _ }
    '-'         { TokenSub _ }
    '.+.'       { TokenFAdd _ }
    '.-.'       { TokenFSub _ }
    '=='        { TokenEq _ }
    '.==.'      { TokenFEq _ }
    '*==*'      { TokenCEq _ }
    '>'         { TokenGt _ }
    '<'         { TokenLt _ }
    '.>.'       { TokenFGt _ }
    '.<.'       { TokenFLt _ }
    '>='        { TokenGe _ }
    '<='        { TokenLe _ }
    '.>=.'      { TokenFGe _ }
    '.<=.'      { TokenFLe _ }
    '/='        { TokenNeq _ }
    '&&'        { TokenAnd _ }
    '||'        { TokenOr _ }

    -- base types
    Int         { TokenIntType _ }
    Float       { TokenFloatType _ }
    Bool        { TokenBoolType _ }
    String      { TokenStringType _ }

    -- variable tokens (lowercase identifiers vs uppercase constructors)
    IDENT_LC    { TokenIdentLower _ $$ }
    IDENT_UC    { TokenIdentUpper _ $$ }
    INT_LIT     { TokenIntLit _ $$ }
    FLOAT_LIT   { TokenFloatLit _ $$ }
    BOOL_LIT    { TokenBoolLit _ $$ }
    STRING_LIT  { TokenStringLit _ $$ }

    -- other
    main        { TokenMain _ }
    '\n'        { TokenNewLine _ }
    EOF         { TokenEOF _ }



%%
-- top-level program
Program :: { Program }
    : DataTypeDecls FuncDecls MainExpr EOF { Program (reverseList DataTypeDecls $1) (reverseList FuncDecls $2) $3 }

MainExpr :: { Expr }
    : main '=' Expr     { $3 }

-- data type declarations
DataTypeDecl :: { DataTypeDecl }
    : data TypeCon '=' DataFields { DataTypeDecl $2 (reverseList DataFields $4) }

DataField :: { DataField }
    : DataCon CombinedTypeCons { DataField $1 (reverseList CombinedTypeCons $2) }

CombinedTypeCon :: { CombinedTypeCon }
    : TypeCon   { CTCTypeCon $1 }
    | BaseType  { CTCBase $1 }

-- function declarations
-- TODO make sure function variables match and modify FuncDecl to only have one FuncVar
FuncDecl :: { FuncDecl }
    : FuncVar FuncDeclRest
        { $2 $1 }

FuncDeclRest
    : ':' TypeScheme '\n' FuncVar '[' LocRegions ']' Vars '=' Expr
        {\v -> FuncDecl v $2 $4 (reverseList LocRegions $6) (reverseList Vars $8) $10}

-- type expressions
LocatedType :: { LocatedType } 
    : CombinedLocType '@' LocRegion { LocatedType $1 $3 }

CombinedLocType :: { CombinedLocType }
    : TypeCon   { CLTTypeCon $1 }
    | BaseType  { CLTBase $1 }

TypeScheme :: { TypeScheme }
    : CombinedTypes { TypeScheme (reverseList CombinedTypes $1)}

CombinedType :: { CombinedType }
    : LocatedType                     { CTLocated $1 }
    | BaseType                        { CTBase $1 }

BaseType :: { BaseType }
    : Int          { Int }
    | Float        { Float }
    | Bool         { Bool }
    | String       { String }

-- location expressions
LocExpress :: { LocExpress }
    : '(' start RegionVar ')'          { LocExpressStart $3 }
    | '(' LocRegion '+' INT_LIT ')'    { LocExpressNext $2 $4 }
    | '(' after LocatedType ')'        { LocExpressAfter $3 }

LocRegion :: { LocRegion }
    : '(' LocVar ',' RegionVar ')'              { LocRegion $2 $4 (IndexVar "") }
    | '(' LocVar ',' RegionVar ',' IndexVar ')' { LocRegion $2 $4 $6 }  

-- identifiers/literals
Val :: { Val }
    : Var                            { ValVar $1 }
    | Lit                            { ValLit $1 }

Lit :: { Lit }
    : INT_LIT        { IntLit $1 }
    | FLOAT_LIT      { FloatLit $1 }
    | BOOL_LIT       { BoolLit $1 }
    | STRING_LIT     { StringLit $1 }

-- expressions
Expr :: { Expr }
    : Expr BinOp Expr                { ExprBinOp $2 $1 $3 }
    | Val                            { ExprVal $1 }
    | '(' Expr ')'                   { $2 }
    | ExprFuncApp                    { $1 }
    | ExprDataConApp                 { $1 }
    | ExprCase                       { $1 }
    | ExprLet                        { $1 }
    | ExprLetLoc                     { $1 }
    | ExprLetRegion                  { $1 }

ExprLetRegion :: { Expr }
    : letregion RegionVar in Expr   { ExprLetRegion $2 $4 }

ExprLetLoc :: { Expr }
    : letloc LocRegion '=' LocExpress in Expr   { ExprLetLoc $2 $4 $6 }

ExprLet :: { Expr }
    : let Var ':' CombinedType '=' Expr in Expr   { ExprLet $2 $4 $6 $8 }

ExprFuncApp :: { Expr }
    : FuncVar '[' LocRegions ']' Exprs   { ExprFuncApp $1 (reverseList LocRegions $3) (reverseList Exprs $5)}

ExprDataConApp :: { Expr }
    : DataCon LocRegion Exprs    { ExprDataConApp $1 $2 (reverseList Exprs $3)}

-- TODO change this so Val is Expr
ExprCase :: { Expr }
    : case Val of Pats    { ExprCase $2 (reverseList Pats $4) }

Pat :: { Pat }
    : DataCon PatMatches '->' Expr      { Pat $1 (reverseList PatMatches $2) $4 }

PatMatch :: { PatMatch }
    : Val ':' LocatedType       { PatMatch $1 $3}

BinOp :: { BinOp }
    : '+'         { Add }
    | '-'         { Sub }
    | '.+.'       { FAdd }
    | '.-.'       { FSub }
    | '*'         { Mul }
    | '/'         { Div }
    | '.*.'       { FMul }
    | './.'       { FDiv }
    | '^'         { Pow }
    | '=='        { Eq }
    | '.==.'      { FEq }
    | '*==*'      { CEq }
    | '>'         { Gt }
    | '<'         { Lt }
    | '.>.'       { FGt }
    | '.<.'       { FLt }
    | '>='        { Ge }
    | '<='        { Le }
    | '.>=.'      { FGe }
    | '.<=.'      { FLe }
    | '/='        { Neq }
    | '&&'        { And }
    | '||'        { Or }

-- specific variable types
FuncVar :: { FuncVar }
    : IDENT_LC       { FuncVar $1 }

RegionVar :: { RegionVar }
    : IDENT_LC       { RegionVar $1 }

LocVar :: { LocVar }
    : IDENT_LC       { LocVar $1 }

IndexVar :: { IndexVar }
    : IDENT_LC       { IndexVar $1 }

TypeCon :: { TypeCon }
    : IDENT_UC       { TypeCon $1 }

DataCon :: { DataCon }
    : IDENT_UC       { DataCon $1 }

Var :: { Var }
    : IDENT_LC       { Var $1 }

-- repeated productions to model * operator
    -- lists of identifiers
Vars :: { [Var] }
    : {- empty -}            { [] }
    | Var                    { [$1] }
    | Vars Var               { $2 : $1 }

DataFields :: { [DataField] }
    : {- empty -}                   { [] }
    | DataField                     { [ $1 ] }
    | DataFields '|' DataField      { $3 : $1 }

CombinedTypeCons :: { [CombinedTypeCon] }
    : {- empty -}                           { [] }
    | CombinedTypeCon                       { [$1] }
    | CombinedTypeCons CombinedTypeCon      { $2 : $1 }

-- TypeCons :: { [TypeCon] }
--     : {- empty -}                   { [] }
--     | TypeCon                       { [$1] }
--     | TypeCons TypeCon              { $2 : $1 }

Exprs :: { [Expr] }
    : Expr                          { [$1] }
    | Exprs Expr                    { $2 : $1 }

Vals :: { [Val] }
    : {- empty -}                   { [] }
    | Val                           { [$1] }
    | Vals Val                      { $2 : $1 }

Pats :: { [Pat] }
    : Pat           { [$1] }
    | Pats Pat      { $2 : $1 }

PatMatches :: { [PatMatch] }
    : {- empty -}                   { [] }
    | '(' PatMatch ')'              { [$2] }
    | PatMatches '(' PatMatch ')'   { $3 : $1 }

    -- lists of other productions
DataTypeDecls :: { [DataTypeDecl] }
    : {- empty -}                      { [] }
    | DataTypeDecl '\n'                { [$1] }
    | DataTypeDecls DataTypeDecl '\n'  { $2 : $1 }

FuncDecls :: { [FuncDecl] }
    : {- empty -}                   { [] }
    | FuncDecl '\n'                 { [$1] }
    | FuncDecls FuncDecl '\n'       { $2 : $1 }

LocRegions :: { [LocRegion ] }
    : {- empty -}                   { [] }
    | LocRegion                     { [$1] }
    | LocRegions LocRegion          { $2 : $1 }

CombinedTypes :: { [CombinedType ] }
    : {- empty -}                        { [] }
    | CombinedType                       { [$1] }
    | CombinedType '->' CombinedTypes    { $1 : $3 }

{
parseError :: [Token] -> E a
parseError [] = failE "Parse error"
parseError (tok:_) = failE . makeRed $
        "Parse error at " ++ showPos (pos tok) ++
        "\nUnexpected token: " ++ show tok

-- data E a = Ok a | Failed String deriving Show
-- -- data ParseResult a = Ok a | Failed String deriving Show
-- -- type E a = String -> ParseResult a

-- instance Functor E where
--     fmap f (Ok x)      = Ok (f x)
--     fmap _ (Failed e)  = Failed e

-- instance Applicative E where
--     pure = Ok
--     (Ok f) <*> (Ok x)     = Ok (f x)
--     (Failed e) <*> _      = Failed e
--     _ <*> (Failed e)      = Failed e

-- instance Monad E where
--     (Ok x) >>= f = f x
--     (Failed e) >>= _ = Failed e

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
    case m of
        Ok a     -> k a
        Failed e -> Failed e

-- thenE :: E a -> (a -> E b) -> E b
-- m `thenE` k = \s ->
--    case m s of
--        Ok a     -> k a s
--        Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a
-- returnE :: a -> E a
-- returnE a = \s -> Ok a


failE :: String -> E a
failE err = Failed err
-- failE :: String -> E a
-- failE err = \s -> Failed err


catchE :: E a -> (String -> E a) -> E a
catchE m k =
    case m of
        Ok a     -> Ok a
        Failed e -> k e
-- catchE :: E a -> (String -> E a) -> E a
-- catchE m k = \s ->
--    case m s of
--       Ok a     -> Ok a
--       Failed e -> k e s
reverseList :: ([a] -> b) -> [a] -> b
reverseList f = f . reverse

}