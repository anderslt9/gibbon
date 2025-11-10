{
module Main where
import Data.Char (isSpace, isAlpha, isDigit, isLower)
import System.Environment (getArgs)
import Control.Monad (forM_)
import System.FilePath.Posix (takeBaseName)
import AST
import Tokens
import PrintAST
}

%name l2ParserNative
%tokentype { Token }
%error { parseError }
%monad { E } { thenE } { returnE }
%right '||'
%right '&&'
%nonassoc '==' '.==.' '*==*' '>' '<' '.>.' '.<.' '>=' '<=' '.>=.' '.<=.' '/='
%left '+' '-' '.-.' '.+.'
%left '*' '/' '`div`' '`mod`' '.*.' './.'
%left '^'

-- %lexer { L2LexerNative }

%token 
    -- data constructor
    data        { TokenData }
    
    -- symbols
    '='         { TokenAssign }
    ':'         { TokenColon }
    '['         { TokenLBracket }
    ']'         { TokenRBracket }
    '@'         { TokenAt }
    '->'        { TokenArrow }
    '|'         { TokenBar }
    ','         { TokenComma }
    '('         { TokenLParen }
    ')'         { TokenRParen }
    
    -- common expr keywords
    let         { TokenLet }
    in          { TokenIn }
    letloc      { TokenLetLoc }
    letregion   { TokenLetRegion }
    case        { TokenCase }
    of          { TokenOf }
    start       { TokenStart }
    after       { TokenAfter }

    -- binary operations
    '^'         { TokenPow }
    '*'         { TokenMul }
    '/'         { TokenDiv }
    '`div`'     { TokenDivInline }
    '`mod`'     { TokenModInline }
    '.*.'       { TokenFMul } 
    './.'       { TokenFDiv }
    '+'         { TokenAdd }
    '-'         { TokenSub }
    '.+.'       { TokenFAdd }
    '.-.'       { TokenFSub }
    '=='        { TokenEq }
    '.==.'      { TokenFEq }
    '*==*'      { TokenCEq }
    '>'         { TokenGt }
    '<'         { TokenLt }
    '.>.'       { TokenFGt }
    '.<.'       { TokenFLt }
    '>='        { TokenGe }
    '<='        { TokenLe }
    '.>=.'      { TokenFGe }
    '.<=.'      { TokenFLe }
    '/='        { TokenNeq }
    '&&'        { TokenAnd }
    '||'        { TokenOr }

    -- base types
    Int         { TokenIntType }
    Float       { TokenFloatType }
    Bool        { TokenBoolType }
    String      { TokenStringType }

    -- variable tokens
    IDENT       { TokenIdent $$ }
    INT_LIT     { TokenIntLit $$ }
    FLOAT_LIT   { TokenFloatLit $$ }
    BOOL_LIT    { TokenBoolLit $$ }
    STRING_LIT  { TokenStringLit $$ }

    -- main
    main        { TokenMain}




%%
-- top-level program
Program :: { Program }
    : DataTypeDecls FuncDecls MainExpr { Program (DataTypeDecls $1) (FuncDecls $2) $3 }

MainExpr :: { Expr }
    : main '=' Expr     { $3 }

-- data type declarations
DataTypeDecl :: { DataTypeDecl }
    : data TypeCon '=' DataFields { DataTypeDecl $2 (DataFields $4) }

DataField :: { DataField }
    : DataCon TypeCons { DataField $1 (TypeCons $2) }

-- function declarations
FuncDecl :: { FuncDecl }
    : FuncVar FuncDeclRest
        { $2 $1 }

FuncDeclRest
    : ':' TypeScheme FuncVar '[' LocRegions ']' Vars '=' Expr
        {\v -> FuncDecl v $2 $3 (LocRegions $5) (Vars $7) $9}

-- type expressions
LocatedType :: { LocatedType } 
    : TypeCon '@' LocRegion { LocatedType $1 $3 }

TypeScheme :: { TypeScheme }
    : CombinedTypes { TypeScheme (CombinedTypes $1)}

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
    | '(' LocExpress '+' INT_LIT ')'    { LocExpressNext $2 }
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

ExprFuncApp :: { Expr }
    : FuncVar '[' LocRegions ']' Vals   { ExprFuncApp $1 (LocRegions $3) (Vals $5)}

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
    : IDENT       { FuncVar $1 }

RegionVar :: { RegionVar }
    : IDENT       { RegionVar $1 }

LocVar :: { LocVar }
    : IDENT       { LocVar $1 }

IndexVar :: { IndexVar }
    : IDENT       { IndexVar $1 }

TypeCon :: { TypeCon }
    : IDENT       { TypeCon $1 }

DataCon :: { DataCon }
    : IDENT       { DataCon $1 }

Var :: { Var }
    : IDENT       { Var $1 }

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

TypeCons :: { [TypeCon] }
    : {- empty -}                   { [] }
    | TypeCon                       { [$1] }
    | TypeCons TypeCon              { $2 : $1 }

Vals :: { [Val] }
    : {- empty -}                   { [] }
    | Val                           { [$1] }
    | Vals Val                      { $2 : $1 }

    -- lists of other productions
DataTypeDecls :: { [DataTypeDecl] }
    : {- empty -}                      { [] }
    | DataTypeDecl                       { [$1] }
    | DataTypeDecls DataTypeDecl       { $2 : $1 }

FuncDecls :: { [FuncDecl] }
    : {- empty -}                   { [] }
    | FuncDecl                      { [$1] }
    | FuncDecls FuncDecl            { $2 : $1 }

LocRegions :: { [LocRegion ] }
    : {- empty -}                   { [] }
    | LocRegion                     { [$1] }
    | LocRegions LocRegion          { $2 : $1 }

CombinedTypes :: { [CombinedType ] }
    : {- empty -}                        { [] }
    | CombinedType                       { [$1] }
    | CombinedType '->' CombinedTypes    { $1 : $3 }

-- repeated productions to model + operator






{
-- parseError :: [Token] -> a
parseError tokens = failE "Parse error"

data E a = Ok a | Failed String deriving Show

instance Functor E where
    fmap f (Ok x)      = Ok (f x)
    fmap _ (Failed e)  = Failed e

instance Applicative E where
    pure = Ok
    (Ok f) <*> (Ok x)     = Ok (f x)
    (Failed e) <*> _      = Failed e
    _ <*> (Failed e)      = Failed e

instance Monad E where
    (Ok x) >>= f = f x
    (Failed e) >>= _ = Failed e

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
    case m of
        Ok a     -> k a
        Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
    case m of
        Ok a     -> Ok a
        Failed e -> k e

-- actual lexer
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
    | isSpace c = lexer cs
    | isAlpha c = lexVar (c:cs)
    | isDigit c = lexNum (c:cs)
    | c == '"' = 
        let (str, rest) = span (/= '"') cs
        in TokenStringLit str : lexer (tail rest)
lexer ('=':'=':cs)   =  TokenEq : lexer cs
lexer ('=':cs)       =  TokenAssign : lexer cs
lexer (':':cs)       =  TokenColon : lexer cs
lexer ('[':cs)       =  TokenLBracket : lexer cs
lexer (']':cs)       =  TokenRBracket : lexer cs
lexer ('@':cs)       =  TokenAt : lexer cs
lexer ('-':'>':cs)   =  TokenArrow : lexer cs
lexer ('|':'|':cs)   =  TokenOr : lexer cs
lexer ('|':cs)       =  TokenBar : lexer cs
lexer (',':cs)       =  TokenComma : lexer cs
lexer ('(':cs)       =  TokenLParen : lexer cs
lexer (')':cs)       =  TokenRParen : lexer cs
lexer ('^':cs)       =  TokenPow : lexer cs
lexer ('*':'=':'=':'*':cs) = TokenCEq : lexer cs
lexer ('*':cs)       =  TokenMul : lexer cs
lexer ('/':'=':cs)   =  TokenNeq : lexer cs
lexer ('/':cs)       =  TokenDiv : lexer cs
lexer ('`':'d':'i':'v':'`':cs) = TokenDivInline : lexer cs
lexer ('`':'m':'o':'d':'`':cs) = TokenModInline : lexer cs
lexer ('.':'*':'.':cs) = TokenFMul : lexer cs
lexer ('.':'/':'.':cs) = TokenFDiv : lexer cs
lexer ('+':cs)       =  TokenAdd : lexer cs
lexer ('-':cs)       =  TokenSub : lexer cs
lexer ('.':'+':'.':cs) = TokenFAdd : lexer cs
lexer ('.':'-':'.':cs) = TokenFSub : lexer cs
lexer ('>':'=':cs)   =  TokenGe : lexer cs
lexer ('>':cs)       =  TokenGt : lexer cs
lexer ('<':'=':cs)   =  TokenLe : lexer cs
lexer ('<':cs)       =  TokenLt : lexer cs
lexer ('.':'>':'.':cs) = TokenFGt : lexer cs
lexer ('.':'<':'.':cs) = TokenFLt : lexer cs
lexer ('.':'>':'=':'.':cs) = TokenFGe : lexer cs
lexer ('.':'<':'=':'.':cs) = TokenFLe : lexer cs
lexer ('&':'&':cs)   =  TokenAnd : lexer cs

lexNum cs = 
    case span isDigit cs of
        (num, "")   -> [TokenIntLit (read num)]
        (num, rest) -> if head rest == '.'
                       then case span isDigit (tail rest) of
                            (num2, rest2) -> TokenFloatLit (read (num ++ "." ++ num2)) : lexer rest2
                            -- otherwise error
                       else TokenIntLit (read num) : lexer rest


matchVar cs = 
    case span isValidChar cs of
        ((c:word), rest) -> if isLower c 
                            then TokenIdent (c:word) : lexer rest
                            else [] -- TODO ADD ERROR HERE
        (_, rest) -> []   
    where isValidChar = (\n -> n `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['`'] ++ ['0'..'9']))

lexVar cs =
    case span isAlpha cs of
        ("data", rest) -> TokenData : lexer rest
        ("let", rest)  -> TokenLet : lexer rest
        ("in", rest)   -> TokenIn : lexer rest
        ("letloc", rest) -> TokenLetLoc : lexer rest
        ("letregion", rest) -> TokenLetRegion : lexer rest
        ("case", rest) -> TokenCase : lexer rest
        ("of", rest)   -> TokenOf : lexer rest
        ("start", rest) -> TokenStart : lexer rest
        ("after", rest) -> TokenAfter : lexer rest
        ("Int", rest)  -> TokenIntType : lexer rest
        ("Float", rest) -> TokenFloatType : lexer rest
        ("Bool", rest) -> TokenBoolType : lexer rest
        ("String", rest) -> TokenStringType : lexer rest
        ("True", rest)  -> TokenBoolLit True : lexer rest
        ("False", rest) -> TokenBoolLit False : lexer rest
        ("main", rest) -> TokenMain : lexer rest
        (var, rest)    -> matchVar cs


printTest testFile = do
    putStrLn $ "Running Test: " ++ (takeBaseName testFile)
    contents <- readFile testFile
    let tokens = lexer contents
    putStrLn "== Tokens =="
    print tokens

    let ast = l2ParserNative tokens
        parsed_str = fmap (printAST 0) ast
    putStrLn "\n== Raw Parse Result =="
    print ast
    
    putStrLn "\n== Pretty Parse Result =="
    case parsed_str of
        Ok x -> putStrLn x
        Failed e -> print e


main = do 
    args <- getArgs 
    putStrLn $ show args
    forM_ args printTest
    -- contents <- readFile "/home/anderslt/gibbon-compiler/gibbon-compiler/tests/L2-Parser-Native/tests/0-Add1.hs" 
    -- let tokens = lexer contents
    -- putStrLn "== Tokens =="
    -- print tokens
    
    -- putStrLn "\n== Parse Result =="
    -- let ast = l2ParserNative tokens
    -- print ast
    -- getContents >>= print . l2ParserNative . lexer
}