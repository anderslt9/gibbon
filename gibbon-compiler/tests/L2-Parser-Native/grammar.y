{
module Main where
import Data.Char (isSpace, isAlpha, isDigit)
}

%name l2ParserNative
%tokentype { Token }
%error { parseError }
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




%%
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
    | CombinedTypes '->' CombinedType    { $3 : $1 }

-- repeated productions to model + operator




-- top-level program
Program :: { Program }
    : DataTypeDecls FuncDecls Expr { Program (DataTypeDecls $1) (FuncDecls $2) $3 }

-- data type declarations
DataTypeDecl :: { DataTypeDecl }
    : data TypeCon '=' DataFields { DataTypeDecl $2 (DataFields $4) }

DataField :: { DataField }
    : DataCon TypeCons { DataField $1 (TypeCons $2) }

-- function declarations
FuncDecl :: { FuncDecl }
    : FuncVar ':' TypeScheme FuncVar '[' LocRegions ']' Vars '=' Expr
        { FuncDecl $1 $3 $4 (LocRegions $6) (Vars $8) $10 }

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
    : Val                            { ExprVal $1 }
    | '(' Expr ')'                   { $2 }
    | Expr BinOp Expr                { ExprBinOp $2 $1 $3 }

BinOp :: { BinOp }
    : '+'         { Add }
    | '-'         { Sub }
    | '.*.'       { FAdd }
    | '.-.'       { FSub }
    | '*'         { Mul }
    | '/'         { Div }
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

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

-- specific variable types
newtype FuncVar = FuncVar String deriving Show
newtype RegionVar = RegionVar String deriving Show
newtype LocVar = LocVar String deriving Show
newtype IndexVar = IndexVar String deriving Show
newtype TypeCon = TypeCon String deriving Show
newtype DataCon = DataCon String deriving Show
newtype Var = Var String deriving Show

-- repeated productions to model * operator
newtype Vars = Vars [Var] deriving Show
newtype DataFields = DataFields [DataField] deriving Show
newtype TypeCons = TypeCons [TypeCon] deriving Show
newtype DataTypeDecls = DataTypeDecls [DataTypeDecl] deriving Show
newtype FuncDecls = FuncDecls [FuncDecl] deriving Show
newtype LocRegions = LocRegions [LocRegion] deriving Show
newtype CombinedTypes = CombinedTypes [CombinedType] deriving Show

-- top-level program
data Program = Program DataTypeDecls FuncDecls Expr deriving Show

-- data type declarations
data DataTypeDecl = DataTypeDecl TypeCon DataFields deriving Show
data DataField = DataField DataCon TypeCons deriving Show

-- function declarations
data FuncDecl = FuncDecl FuncVar TypeScheme FuncVar LocRegions Vars Expr deriving Show

-- type expressions
data LocatedType = LocatedType TypeCon LocRegion deriving Show
newtype TypeScheme = TypeScheme CombinedTypes deriving Show
data CombinedType = CTLocated LocatedType | CTBase BaseType deriving Show
data BaseType = Int | Float | Bool | String deriving Show

-- location expressions
data LocExpress = LocExpressStart RegionVar | LocExpressNext LocExpress | LocExpressAfter LocatedType deriving Show
data LocRegion = LocRegion LocVar RegionVar IndexVar deriving Show

-- identifiers/literals
data Val = ValVar Var | ValLit Lit deriving Show
data Lit = IntLit Int | FloatLit Float | BoolLit Bool | StringLit String deriving Show

-- expressions
data Expr = ExprVal Val | ExprBinOp BinOp Expr Expr deriving Show
data BinOp = Add | Sub | FAdd | FSub | Mul | Div | FDiv | Pow
           | Eq | FEq | CEq | Gt | Lt | FGt | FLt | Ge | Le | FGe | FLe | Neq | And | Or deriving Show

-- list all tokens
data Token 
    = TokenData
    | TokenAssign
    | TokenColon
    | TokenLBracket
    | TokenRBracket
    | TokenAt
    | TokenArrow
    | TokenBar
    | TokenComma
    | TokenLParen
    | TokenRParen
    | TokenLet
    | TokenIn
    | TokenLetLoc
    | TokenLetRegion
    | TokenCase
    | TokenOf
    | TokenStart
    | TokenAfter
    | TokenPow
    | TokenMul
    | TokenDiv
    | TokenDivInline
    | TokenModInline
    | TokenFMul
    | TokenFDiv
    | TokenAdd
    | TokenSub
    | TokenFAdd
    | TokenFSub
    | TokenEq
    | TokenFEq
    | TokenCEq
    | TokenGt
    | TokenLt
    | TokenFGt
    | TokenFLt
    | TokenGe
    | TokenLe
    | TokenFGe
    | TokenFLe
    | TokenNeq
    | TokenAnd
    | TokenOr
    | TokenIntType
    | TokenFloatType
    | TokenBoolType
    | TokenStringType
    | TokenIdent String
    | TokenIntLit Int
    | TokenFloatLit Float
    | TokenBoolLit Bool
    | TokenStringLit String
    deriving Show


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
        (num, rest) -> if head rest == '.'
                       then case span isDigit (tail rest) of
                            (num2, rest2) -> TokenFloatLit (read (num ++ "." ++ num2)) : lexer rest2
                            -- otherwise error
                       else TokenIntLit (read num) : lexer rest


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
        (var, rest)    -> TokenIdent var : lexer rest


main = getContents >>= print . l2ParserNative . lexer
}