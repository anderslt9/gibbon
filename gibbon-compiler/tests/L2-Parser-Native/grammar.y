{
module Main where
import Data.Char (isSpace, isAlpha, isDigit, isLower)
import Data.List (break)
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
    : DataTypeDecls FuncDecls MainExpr EOF { Program (DataTypeDecls $1) (FuncDecls $2) $3 }

MainExpr :: { Expr }
    : main '=' Expr     { $3 }

-- data type declarations
DataTypeDecl :: { DataTypeDecl }
    : data TypeCon '=' DataFields { DataTypeDecl $2 (DataFields $4) }

DataField :: { DataField }
    : DataCon CombinedTypeCons { DataField $1 (CombinedTypeCons $2) }

CombinedTypeCon :: { CombinedTypeCon }
    : TypeCon   { CTCTypeCon $1 }
    | BaseType  { CTCBase $1 }

-- function declarations
FuncDecl :: { FuncDecl }
    : FuncVar FuncDeclRest
        { $2 $1 }

FuncDeclRest
    : ':' TypeScheme '\n' FuncVar '[' LocRegions ']' Vars '=' Expr
        {\v -> FuncDecl v $2 $4 (LocRegions $6) (Vars $8) $10}

-- type expressions
LocatedType :: { LocatedType } 
    : CombinedLocType '@' LocRegion { LocatedType $1 $3 }

CombinedLocType :: { CombinedLocType }
    : TypeCon   { CLTTypeCon $1 }
    | BaseType  { CLTBase $1 }

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
    | '(' LocRegion '+' INT_LIT ')'    { LocExpressNext $2 }
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
    : FuncVar '[' LocRegions ']' Exprs   { ExprFuncApp $1 (LocRegions $3) (Exprs $5)}

ExprDataConApp :: { Expr }
    : DataCon LocRegion Exprs    { ExprDataConApp $1 $2 (Exprs $3)}

ExprCase :: { Expr }
    : case Val of Pats    { ExprCase $2 (Pats $4) }

Pat :: { Pat }
    : DataCon PatMatches '->' Expr      { Pat $1 (PatMatches $2) $4 }

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
parseError (tok:_) = failE $
        "Parse error at " ++ showPos (pos tok) ++
        "\nUnexpected token: " ++ show tok

data E a = Ok a | Failed String deriving Show
-- data ParseResult a = Ok a | Failed String deriving Show
-- type E a = String -> ParseResult a

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


lexer :: String -> [Token]
lexer input = lexer' (Pos 1 1) input

-- actual lexer
lexer' :: Pos -> String -> [Token]
lexer' p [] = [TokenEOF p]
lexer' p (c:cs)
    | c == '\n' = 
        if isSpace $ head cs 
        then lexer' (advance p c) cs
        else TokenNewLine p : lexer' (advance p c) cs
    | isSpace c = lexer' (advance p c) cs
    | isAlpha c = lexVar p (c:cs)
    | isDigit c = lexNum p (c:cs)
    | c == '"' = 
        let (str, rest) = span (/= '"') cs
        in TokenStringLit p str : lexer' (advance p c) (tail rest)
lexer' p ('=':'=':cs)   =  TokenEq p : lexer' (advanceStr p "==") cs
lexer' p ('=':cs)       =  TokenAssign p : lexer' (advance p '=') cs
lexer' p (':':cs)       =  TokenColon p : lexer' (advance p ':') cs
lexer' p ('[':cs)       =  TokenLBracket p : lexer' (advance p '[') cs
lexer' p (']':cs)       =  TokenRBracket p : lexer' (advance p ']') cs
lexer' p ('@':cs)       =  TokenAt p : lexer' (advance p '@') cs
lexer' p ('-':'>':cs)   =  TokenArrow p : lexer' (advanceStr p "->") cs
lexer' p ('|':'|':cs)   =  TokenOr p : lexer' (advanceStr p "||") cs
lexer' p ('|':cs)       =  TokenBar p : lexer' (advance p '|') cs
lexer' p (',':cs)       =  TokenComma p : lexer' (advance p ',') cs
lexer' p ('(':cs)       =  TokenLParen p : lexer' (advance p '(') cs
lexer' p (')':cs)       =  TokenRParen p : lexer' (advance p ')') cs
lexer' p ('-':'-':cs)   =  lexer' (advance p '\n') (lexComment cs)
lexer' p ('^':cs)       =  TokenPow p : lexer' (advance p '^') cs
lexer' p ('*':'=':'=':'*':cs) = TokenCEq p : lexer' (advanceStr p "*===") cs
lexer' p ('*':cs)       =  TokenMul p : lexer' (advance p '*') cs
lexer' p ('/':'=':cs)   =  TokenNeq p : lexer' (advanceStr p "/=") cs
lexer' p ('/':cs)       =  TokenDiv p : lexer' (advance p '/') cs
lexer' p ('`':'d':'i':'v':'`':cs) = TokenDivInline p : lexer' (advanceStr p "`div`") cs
lexer' p ('`':'m':'o':'d':'`':cs) = TokenModInline p : lexer' (advanceStr p "`mod`") cs
lexer' p ('.':'*':'.':cs) = TokenFMul p : lexer' (advanceStr p ".*.") cs
lexer' p ('.':'/':'.':cs) = TokenFDiv p : lexer' (advanceStr p "./.") cs
lexer' p ('+':cs)       =  TokenAdd p : lexer' (advance p '+') cs
lexer' p ('-':cs)       =  TokenSub p : lexer' (advance p '-') cs
lexer' p ('.':'+':'.':cs) = TokenFAdd p : lexer' (advanceStr p ".*.") cs
lexer' p ('.':'-':'.':cs) = TokenFSub p : lexer' (advanceStr p ".*.") cs
lexer' p ('>':'=':cs)   =  TokenGe p : lexer' (advanceStr p ">=") cs
lexer' p ('>':cs)       =  TokenGt p : lexer' (advance p '>') cs
lexer' p ('<':'=':cs)   =  TokenLe p : lexer' (advanceStr p "<=") cs
lexer' p ('<':cs)       =  TokenLt p : lexer' (advance p '<') cs
lexer' p ('.':'>':'.':cs) = TokenFGt p : lexer' (advanceStr p ".>.") cs
lexer' p ('.':'<':'.':cs) = TokenFLt p : lexer' (advanceStr p ".<.") cs
lexer' p ('.':'>':'=':'.':cs) = TokenFGe p : lexer' (advanceStr p ".>=") cs
lexer' p ('.':'<':'=':'.':cs) = TokenFLe p : lexer' (advanceStr p ".<=") cs
lexer' p ('&':'&':cs)   =  TokenAnd p : lexer' (advanceStr p "&&") cs

advanceStr :: Pos -> String -> Pos
advanceStr p [] = p
advanceStr p (c:cs) = advanceStr (advance p c) cs

advance :: Pos -> Char -> Pos
advance (Pos l c) '\n' = Pos (l + 1) 1
advance (Pos l c) _    = Pos l (c + 1)

lexComment :: String -> String
lexComment cs =
    case break (== '\n') cs of 
        (_, [])     -> ""
        (_, _:rest) -> rest

lexNum p cs = 
    case span isDigit cs of
        (num, "")   -> TokenIntLit p (read num) : [TokenEOF (advanceStr p num)]
        (num, rest) ->  if head rest == '.'
                        then case span isDigit (tail rest) of
                            (num2, rest2) -> TokenFloatLit p (read (num ++ "." ++ num2)) : lexer' (advanceStr p (num ++ "." ++ num2)) rest2
                            -- otherwise error
                        else TokenIntLit p (read num) : lexer' (advanceStr p num) rest

matchVar p cs =  
    case span isValidChar cs of
        (var, rest) ->  if isLower . head $ var
                        then TokenIdentLower p var : lexer' (advanceStr p var) rest
                        else if isValidStartChar (head var) 
                            then TokenIdentUpper p var : lexer' (advanceStr p var) rest
                            else []
        (var, _) -> [TokenEOF (advanceStr p var)]
        (_,_) -> []
    where   isValidChar = (\n -> n `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['`'] ++ ['0'..'9']))
            isValidStartChar = (\n -> n `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['`']))

lexVar p cs =
    case span isAlpha cs of
        ("data", rest) -> TokenData p : lexer' (advanceStr p "data") rest
        ("let", rest)  -> TokenLet p : lexer' (advanceStr p "let") rest
        ("in", rest)   -> TokenIn p : lexer' (advanceStr p "in") rest
        ("letloc", rest) -> TokenLetLoc p : lexer' (advanceStr p "letloc") rest
        ("letregion", rest) -> TokenLetRegion p : lexer' (advanceStr p "letregion") rest
        ("case", rest) -> TokenCase p : lexer' (advanceStr p "case") rest
        ("of", rest)   -> TokenOf p : lexer' (advanceStr p "of") rest
        ("start", rest) -> TokenStart p : lexer' (advanceStr p "start") rest
        ("after", rest) -> TokenAfter p : lexer' (advanceStr p "after") rest
        ("Int", rest)  -> TokenIntType p : lexer' (advanceStr p "Int") rest
        ("Float", rest) -> TokenFloatType p : lexer' (advanceStr p "Float") rest
        ("Bool", rest) -> TokenBoolType p : lexer' (advanceStr p "Bool") rest
        ("String", rest) -> TokenStringType p : lexer' (advanceStr p "String") rest
        ("True", rest)  -> TokenBoolLit p True : lexer' (advanceStr p "True") rest
        ("False", rest) -> TokenBoolLit p False : lexer' (advanceStr p "False") rest
        ("main", rest) -> TokenMain p : lexer' (advanceStr p "main") rest
        (var, rest)    -> matchVar p cs
        (var, _)     -> [TokenEOF (advanceStr p var)]
        (_,_)       -> []


printTest testFile = do
    putStrLn $ "Running Test: " ++ (takeBaseName testFile)
    contents <- readFile testFile
    let tokens = lexer contents
    putStrLn "== Tokens =="
    forM_ tokens $ \token -> putStrLn (show token)
    -- print tokens

    let ast = l2ParserNative tokens
        parsed_str = fmap (printAST 0) ast
    putStrLn "\n== Raw Parse Result =="
    print ast
    
    putStrLn "\n== Pretty Parse Result =="
    case parsed_str of
        Ok x -> putStrLn x
        Failed e -> putStrLn e


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