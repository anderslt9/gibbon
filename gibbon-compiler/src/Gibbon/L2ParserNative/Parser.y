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
%left '+' '-' '.-.' '.+.' '*' '/' '`div`' '`mod`' '.*.' './.' '^'

-- technically more correct but doesn't match current compiler
-- %left '+' '-' '.-.' '.+.' 
-- %left '*' '/' '`div`' '`mod`' '.*.' './.'
-- %left '^'

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
    if          { TokenIf _ }
    then        { TokenThen _ }
    else        { TokenElse _ }
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
    Char        { TokenCharType _ }
    String      { TokenStringType _ }

    -- variable tokens (lowercase identifiers vs uppercase constructors)
    IDENT_LC    { TokenIdentLower _ $$ }
    IDENT_UC    { TokenIdentUpper _ $$ }
    INT_LIT     { TokenIntLit _ $$ }
    FLOAT_LIT   { TokenFloatLit _ $$ }
    BOOL_LIT    { TokenBoolLit _ $$ }
    CHAR_LIT    { TokenCharLit _ $$ }
    STRING_LIT  { TokenStringLit _ $$ }

    -- other
    main        { TokenMain _ }
    '\n'        { TokenNewLine _ }
    EOF         { TokenEOF _ }



%%
-- top-level program
Program :: { Program }
    : DataTypeDeclStar FuncDeclStar MainExpr EOF { Program (reverseList DataTypeDecls $1) (reverseList FuncDecls $2) $3 }

MainExpr :: { Expr }
    : main '=' Expr     { $3 }

-- data type declarations
DataTypeDecl :: { DataTypeDecl }
    : data UVar '=' DataFieldStar { DataTypeDecl (TypeCon $2) (reverseList DataFields $4) }

DataField :: { DataField }
    : UVar CombinedTypeConStar { DataField (DataCon $1) (reverseList CombinedTypeCons $2) }

CombinedTypeCon :: { CombinedTypeCon }
    : UVar   { CTCTypeCon (TypeCon $1) }
    | BaseType  { CTCBase $1 }

-- function declarations
-- TODO make sure function variables match and modify FuncDecl to only have one FuncVar
FuncDecl :: { FuncDecl }
    : LVar FuncDeclRest
        { $2 (FuncVar $1) }

FuncDeclRest
    : ':' TypeScheme '\n' LVar '[' LocRegionStar ']' LVarStar '=' Expr
        {\v -> FuncDecl v $2 (FuncVar $4) (reverseList LocRegions $6) (reverseList Vars $ map (\x -> Var x) $8) $10}

-- type expressions
LocatedType :: { LocatedType } 
    : CombinedTypeCon '@' LocRegion { LocatedType $1 $3 }

-- CombinedLocType :: { CombinedLocType }
--     : UVar   { CLTTypeCon (TypeCon $1) }
--     | BaseType  { CLTBase $1 }

TypeScheme :: { TypeScheme }
    : CombinedTypeStar { TypeScheme (CombinedTypes $1)}

CombinedType :: { CombinedType }
    : LocatedType                     { CTLocated $1 }
    | BaseType                        { CTBase $1 }

BaseType :: { BaseType }
    : Int          { Int }
    | Float        { Float }
    | Bool         { Bool }
    | Char         { Char }
    | String       { String }

-- location expressions
LocExpress :: { LocExpress }
    : '(' start LVar ')'               { LocExpressStart (RegionVar $3) }
    | '(' LocRegion '+' INT_LIT ')'    { LocExpressNext $2 $4 }
    | '(' after LocatedType ')'        { LocExpressAfter $3 }

LocRegion :: { LocRegion }
    : '(' LVar ',' LVar ')'              { LocRegion (LocVar $2) (RegionVar $4) (IndexVar "") }
    | '(' LVar ',' LVar ',' LVar ')'     { LocRegion (LocVar $2) (RegionVar $4) (IndexVar $6) }  

-- identifiers/literals
Val :: { Val }
    : LVar                           { ValVar (Var $1) }
    | Lit                            { ValLit $1 }

Lit :: { Lit }
    : INT_LIT        { IntLit $1 }
    | FLOAT_LIT      { FloatLit $1 }
    | BOOL_LIT       { BoolLit $1 }
    | CHAR_LIT       { CharLit $1 }
    | STRING_LIT     { StringLit $1 }

-- expressions
Expr :: { Expr }
    -- : ExprOr                      { $1 }
    : ExprBinOp                      { $1 }
    | Val                            { ExprVal $1 }
    | '(' Expr ')'                   { $2 }
    | ExprFuncApp                    { $1 }
    | ExprDataConApp                 { $1 }
    | ExprCase                       { $1 }
    | ExprLet                        { $1 }
    | ExprLetLoc                     { $1 }
    | ExprLetRegion                  { $1 }
    | ExprIf                         { $1 }

-- ExprPrimOp :: { Expr }

-- ExprOr :: { Expr }
--     : ExprAnd                 { $1 }
--     | ExprOr '||' ExprAnd     { ExprBinOp Or $1 $3 }

-- ExprAnd :: { Expr }
--     : ExprEq                   { $1 }
--     | ExprAnd '&&' ExprEq      { ExprBinOp And $1 $3 }

-- ExprEq :: { Expr }
--     : ExprRel                   { $1 }
--     | ExprEq '==' ExprRel       { ExprBinOp Eq $1 $3 }
--     | ExprEq '.==.' ExprRel     { ExprBinOp FEq $1 $3 }
--     | ExprEq '*==*' ExprRel     { ExprBinOp CEq $1 $3 }
--     | ExprEq '/=' ExprRel       { ExprBinOp Neq $1 $3 }

-- ExprRel :: { Expr }
--     : ExprAdd                   { $1 } 
--     | ExprRel '>' ExprAdd       { ExprBinOp Gt $1 $3 }
--     | ExprRel '<' ExprAdd       { ExprBinOp Lt $1 $3 }
--     | ExprRel '.>.' ExprAdd     { ExprBinOp FGt $1 $3 }
--     | ExprRel '.<.' ExprAdd     { ExprBinOp FLt $1 $3 }
--     | ExprRel '>=' ExprAdd      { ExprBinOp Ge $1 $3 }
--     | ExprRel '<=' ExprAdd      { ExprBinOp Le $1 $3 }
--     | ExprRel '.>=.' ExprAdd    { ExprBinOp FGe $1 $3 }
--     | ExprRel '.<=.' ExprAdd    { ExprBinOp FLe $1 $3 }

-- ExprAdd :: { Expr }
--     : ExprMul                   { $1 } 
--     | ExprAdd '+' ExprMul       { ExprBinOp Add $1 $3 }
--     | ExprAdd '-' ExprMul       { ExprBinOp Sub $1 $3 }
--     | ExprAdd '.+.' ExprMul     { ExprBinOp FAdd $1 $3 }
--     | ExprAdd '.-.' ExprMul     { ExprBinOp FSub $1 $3 }

-- ExprMul :: { Expr }
--     : ExprPow                   { $1 }
--     | ExprMul '*' ExprPow       { ExprBinOp Mul $1 $3 }
--     | ExprMul '/' ExprPow       { ExprBinOp Div $1 $3 }
--     | ExprMul '`div`' ExprPow   { ExprBinOp Div $1 $3 }
--     | ExprMul '`mod`' ExprPow   { ExprBinOp Mod $1 $3 }
--     | ExprMul '.*.' ExprPow     { ExprBinOp FMul $1 $3 }
--     | ExprMul './.' ExprPow     { ExprBinOp FDiv $1 $3 }

-- ExprPow :: { Expr }
--     : ExprAtom                  { $1 }
--     | ExprPow '^' ExprAtom      { ExprBinOp Pow $1 $3 }

-- ExprAtom :: { Expr }
--     : Val                            { ExprVal $1 }
--     | '(' Expr ')'                   { $2 }
--     | ExprFuncApp                    { $1 }
--     | ExprDataConApp                 { $1 }
--     | ExprCase                       { $1 }
--     | ExprLet                        { $1 }
--     | ExprLetLoc                     { $1 }
--     | ExprLetRegion                  { $1 }
--     | ExprIf                         { $1 }

ExprBinOp :: { Expr }
    : Expr '+' Expr         { ExprBinOp Add $1 $3 }
    | Expr '-' Expr         { ExprBinOp Sub $1 $3 }
    | Expr '.+.' Expr       { ExprBinOp FAdd $1 $3 }
    | Expr '.-.' Expr       { ExprBinOp FSub $1 $3 }
    | Expr '*' Expr         { ExprBinOp Mul $1 $3 }
    | Expr '/' Expr         { ExprBinOp Div $1 $3 }
    | Expr '.*.' Expr       { ExprBinOp FMul $1 $3 }
    | Expr './.' Expr       { ExprBinOp FDiv $1 $3 }
    | Expr '^' Expr         { ExprBinOp Pow $1 $3 }
    | Expr '==' Expr        { ExprBinOp Eq $1 $3 }
    | Expr '.==.' Expr      { ExprBinOp FEq $1 $3 }
    | Expr '*==*' Expr      { ExprBinOp CEq $1 $3 }
    | Expr '>' Expr         { ExprBinOp Gt $1 $3 }
    | Expr '<' Expr         { ExprBinOp Lt $1 $3 }
    | Expr '.>.' Expr       { ExprBinOp FGt $1 $3 }
    | Expr '.<.' Expr       { ExprBinOp FLt $1 $3 }
    | Expr '>=' Expr        { ExprBinOp Ge $1 $3 }
    | Expr '<=' Expr        { ExprBinOp Le $1 $3 }
    | Expr '.>=.' Expr      { ExprBinOp FGe $1 $3 }
    | Expr '.<=.' Expr      { ExprBinOp FLe $1 $3 }
    | Expr '/=' Expr        { ExprBinOp Neq $1 $3 }
    | Expr '&&' Expr        { ExprBinOp And $1 $3 }
    | Expr '||' Expr        { ExprBinOp Or $1 $3 }

ExprIf :: { Expr }
    : if Expr then Expr else Expr     { ExprIf $2 $4 $6 }

ExprLetRegion :: { Expr }
    : letregion LVar in Expr   { ExprLetRegion (RegionVar $2) $4 }

ExprLetLoc :: { Expr }
    : letloc LocRegion '=' LocExpress in Expr   { ExprLetLoc $2 $4 $6 }

-- TODO support let bindings
ExprLet :: { Expr }
    : let LVar ':' CombinedType '=' Expr in Expr   { ExprLet (Var $2) $4 $6 $8 }

-- LetBinding :: { Expr -> ExprLet }
--     : Var ':' CombinedType '=' Expr   { ExprLet $1 $3 $5 }

-- LetBindings :: { Expr -> ExprLet }
--     : {- empty -}                                   { \expr -> ExprLet undefined undefined expr }
--     | LetBinding                                    { \expr -> $1 }
--     | LetBindings LetBinding                        { \expr -> ExprLet $2 $4 $6 $7 }

ExprFuncApp :: { Expr }
    : LVar '[' LocRegionStar ']' ExprsPlus   { ExprFuncApp (FuncVar $1) (reverseList LocRegions $3) (reverseList Exprs $5)}

ExprDataConApp :: { Expr }
    : UVar LocRegion ExprsPlus    { ExprDataConApp (DataCon $1) $2 (reverseList Exprs $3)}

-- TODO change this so Val is Expr
ExprCase :: { Expr }
    : case Val of PatPlus    { ExprCase $2 (reverseList Pats $4) }

Pat :: { Pat }
    : UVar PatMatchStar '->' Expr      { Pat (DataCon $1) (reverseList PatMatches $2) $4 }

PatMatch :: { PatMatch }
    : Val ':' LocatedType       { PatMatch $1 $3}

-- BinOp :: { BinOp }
--     : '+'         { Add }
--     | '-'         { Sub }
--     | '.+.'       { FAdd }
--     | '.-.'       { FSub }
--     | '*'         { Mul }
--     | '/'         { Div }
--     | '.*.'       { FMul }
--     | './.'       { FDiv }
--     | '^'         { Pow }
--     | '=='        { Eq }
--     | '.==.'      { FEq }
--     | '*==*'      { CEq }
--     | '>'         { Gt }
--     | '<'         { Lt }
--     | '.>.'       { FGt }
--     | '.<.'       { FLt }
--     | '>='        { Ge }
--     | '<='        { Le }
--     | '.>=.'      { FGe }
--     | '.<=.'      { FLe }
--     | '/='        { Neq }
--     | '&&'        { And }
--     | '||'        { Or }

-- specific variable types
-- FuncVar :: { FuncVar }
--     : IDENT_LC       { FuncVar $1 }

-- RegionVar :: { RegionVar }
--     : IDENT_LC       { RegionVar $1 }

-- LocVar :: { LocVar }
--     : IDENT_LC       { LocVar $1 }

-- IndexVar :: { IndexVar }
--     : IDENT_LC       { IndexVar $1 }

-- TypeCon :: { TypeCon }
--     : IDENT_UC       { TypeCon $1 }

-- DataCon :: { DataCon }
--     : IDENT_UC       { DataCon $1 }

-- Var :: { Var }
--     : IDENT_LC       { Var $1 }

LVar :: { String }
    : IDENT_LC       { $1 }

UVar :: { String }
    : IDENT_UC       { $1 }

-- repeated productions to model * operator
    -- lists of identifiers
LVarStar :: { [String] }
    : {- empty -}            { [] }
    | LVarPlus                { $1 }

LVarPlus :: { [String] }
    : LVar                    { [$1] }
    | LVarPlus LVar           { $2 : $1 }

DataFieldStar :: { [DataField] }
    : {- empty -}                           { [] }
    | DataFieldPlus                         { $1 }

DataFieldPlus :: { [DataField] }
    : DataField                             { [ $1 ] }
    | DataFieldPlus '|' DataField           { $3 : $1 }

CombinedTypeConStar :: { [CombinedTypeCon] }
    : {- empty -}                               { [] }
    | CombinedTypeConPlus                       { $1 }

CombinedTypeConPlus :: { [CombinedTypeCon] }
    : CombinedTypeCon                           { [$1] }
    | CombinedTypeConPlus CombinedTypeCon       { $2 : $1 }

-- TypeCons :: { [TypeCon] }
--     : {- empty -}                   { [] }
--     | TypeCon                       { [$1] }
--     | TypeCons TypeCon              { $2 : $1 }

ExprsPlus :: { [Expr] }
    : Expr                              { [$1] }
    | ExprsPlus Expr                    { $2 : $1 }

-- Vals :: { [Val] }
--     : {- empty -}                   { [] }    
--     | Vals2                         { $1 }

-- Vals2 :: { [Val] }
--     : Val                           { [$1] }
--     | Vals2 ONL Val                 { $3 : $1 }

PatPlus :: { [Pat] }
    : Pat                   { [$1] }
    | PatPlus Pat           { $2 : $1 }

PatMatchStar :: { [PatMatch] }
    : {- empty -}                           { [] }
    | PatMatchPlus                          { $1 }

PatMatchPlus :: { [PatMatch] }
    : '(' PatMatch ')'                      { [$2] }
    | PatMatchPlus  '(' PatMatch ')'        { $3 : $1 }

    -- lists of other productions
DataTypeDeclStar :: { [DataTypeDecl] }
    : {- empty -}                        { [] }
    | DataTypeDeclPlus                   { $1 }

DataTypeDeclPlus :: { [DataTypeDecl] }
    : DataTypeDecl '\n'                      { [$1] }
    | DataTypeDeclPlus DataTypeDecl '\n'     { $2 : $1 }

FuncDeclStar :: { [FuncDecl] }
    : {- empty -}                   { [] }
    | FuncDeclPlus                  { $1 }

FuncDeclPlus :: { [FuncDecl] }
    : FuncDecl '\n'                    { [$1] }
    | FuncDeclPlus FuncDecl '\n'       { $2 : $1 }

LocRegionStar :: { [LocRegion ] }
    : {- empty -}                   { [] }
    | LocRegionPlus                 { $1 }

LocRegionPlus :: { [LocRegion ] }
    : LocRegion                     { [$1] }
    | LocRegionPlus LocRegion       { $2 : $1 }

CombinedTypeStar :: { [CombinedType ] }
    : {- empty -}                                  { [] }
    | CombinedTypePlus                             { $1 }

CombinedTypePlus :: { [CombinedType ] }
    : CombinedType                                 { [$1] }
    | CombinedType '->' CombinedTypePlus   { $1 : $3 }

-- optional newlines
-- ONL :: { }
--     : {- empty -}                   { () }
--     | NL                            { () }

-- NL :: { }
--     : '\n'                          { () }
--     | NL '\n'                       { () }  
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