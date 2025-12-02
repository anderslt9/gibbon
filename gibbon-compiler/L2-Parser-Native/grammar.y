{
module Main where
-- import Data.Char (isSpace, isAlpha, isDigit, isLower)
import Data.List (break, isPrefixOf, isSuffixOf)
import System.Environment (getArgs)
import Control.Monad (forM_, when)
import System.FilePath.Posix (takeBaseName)
import System.IO (writeFile, appendFile)
import AST
import Tokens
import PrintAST
import Lexer
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
parseError (tok:_) = failE . makeRed $
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

makeGreen :: String -> String
makeGreen s = "\x1b[32m" ++ s ++ "\x1b[0m"

makeRed :: String -> String
makeRed s = "\x1b[31m" ++ s ++ "\x1b[0m"

makeBold :: String -> String
makeBold s = "\x1b[1m" ++ s ++ "\x1b[0m"

type Args = [String]
type LastParsed = String
data Result a = Success a | Failure String deriving Show

data SimpleCfg = SimpleCfg {
                    inFiles :: [String],
                    outFiles :: [String],
                    showTokens :: Bool,
                    showRaw :: Bool
                    } deriving Show

baseCfg :: SimpleCfg 
baseCfg = SimpleCfg {   inFiles = [],
                        outFiles = [],
                        showTokens = True,
                        showRaw = False
}

printTest:: SimpleCfg -> IO ()
printTest config = do
    forM_ (zip [1..] (inFiles config)) $ \(i, testFile) -> do
        when (i <= length (outFiles config)) $ writeFile (outFiles config !! (i - 1)) ""
        
        let testName = takeBaseName testFile
        putStrLn . makeBold $ "\nRunning Test " ++ show i ++ ": " ++ testName

        -- read given file
        contents <- readFile testFile
        
        -- gets/prints tokens
        let tokens = lexer contents
        when (showTokens config) $ do
            if length (outFiles config) >= i
                then do
                    appendFile (outFiles config !! (i - 1)) "== Tokens ==\n"
                    forM_ tokens $ \token -> appendFile (outFiles config !! (i - 1)) (show token ++ "\n")
                    putStrLn $ "Wrote Tokens to: " ++ (makeBold $ outFiles config !! (i - 1))
                else do
                    putStrLn "\n== Tokens =="
                    forM_ tokens $ \token -> putStrLn (show token)
                    putStrLn $ "Tokens written to console (no file specified)."

        -- runs/prints parser
        let ast = l2ParserNative tokens
            parsed_str = fmap (printAST 0) ast
        when (showRaw config) $ do
            if length (outFiles config) >= i
                then do
                    appendFile (outFiles config !! (i - 1)) "\n== Raw Parse Result ==\n"
                    appendFile (outFiles config !! (i - 1)) (show ast)
                    putStrLn $ "Wrote Raw Parse Result to: " ++ (makeBold $ outFiles config !! (i - 1))
                else do 
                    putStrLn "\n== Raw Parse Result =="
                    print ast
                    putStrLn $ "Raw Parse Result written to console (no file specified)."
        
        case parsed_str of
            Ok x -> if length (outFiles config) >= i
                        then do
                            appendFile (outFiles config !! (i - 1)) "\n== Pretty Parse Result ==\n"
                            appendFile (outFiles config !! (i - 1)) x
                            putStrLn $ "Wrote Pretty Parse Result to: " ++ (makeBold $ outFiles config !! (i - 1))
                        else do 
                            putStrLn "\n== Pretty Parse Result =="
                            putStrLn x
                            putStrLn $ "Pretty Parse Result written to console (no file specified)."
            Failed e -> putStrLn . makeRed $ e


setConfig :: Args -> Result SimpleCfg
setConfig [] = Success baseCfg
setConfig args = setConfig' args baseCfg ""
    where   
        setConfig' :: Args -> SimpleCfg -> LastParsed -> Result SimpleCfg
        -- empty arg list means we're done
        setConfig' [] cfg _ = if length (inFiles cfg) < length (outFiles cfg)
                                then Failure "Error: Number of input files is less than number of output files."
                                else Success cfg 
        
        -- if -i or -o seen, set this flag
        setConfig' ("-i":rest) cfg _ = setConfig' rest cfg "-i"
        setConfig' ("-o":rest) cfg _ = setConfig' rest cfg "-o"
        
        setConfig' (arg:rest) cfg lastParsed
            -- boolean flags
            | "--show-tokens" `isPrefixOf` arg = setConfig' rest (cfg {showTokens = getBoolean arg}) ""
            | "--show-raw" `isPrefixOf` arg = setConfig' rest (cfg {showRaw = getBoolean arg}) "" 
            
            -- file arguments
            | lastParsed == "-i" = if checkValidFile arg
                                    then setConfig' rest (cfg {inFiles = inFiles cfg ++ [arg]}) "-i"
                                    else Failure $ "Invalid input file: " ++ arg
            | lastParsed == "-o" = setConfig' rest (cfg {outFiles = outFiles cfg ++ [arg]}) "-o"
            | otherwise = if checkValidFile arg
                                    then setConfig' rest (cfg {inFiles = inFiles cfg ++ [arg]}) "-i"
                                    else Failure $ "Unknown command line argument: " ++ arg

        getBoolean :: String -> Bool
        getBoolean s
            | "=true" `isSuffixOf` s = True
            | "=false" `isSuffixOf` s = False
            | otherwise = True
        
        checkValidFile :: String -> Bool
        checkValidFile f = ".hs" `isSuffixOf` f || ".gib" `isSuffixOf` f

main = do 
    args <- getArgs
    let config = setConfig args
    putStrLn $ show args
    case config of
        Failure e -> putStrLn $ "Error in command line arguments: " ++ e
        Success cfg -> printTest cfg
}