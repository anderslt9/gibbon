module Gibbon.L2ParserNative.Tokens where 
data Pos = Pos { line :: Int, column :: Int } deriving Show

-- list all tokens
data Token 
    -- data constructor
    = TokenData Pos
    
    -- symbols
    | TokenAssign Pos
    | TokenColon Pos
    | TokenLBracket Pos
    | TokenRBracket Pos
    | TokenAt Pos
    | TokenArrow Pos
    | TokenBar Pos
    | TokenComma Pos
    | TokenLParen Pos
    | TokenRParen Pos
    | TokenComment Pos

    -- common expr keywords
    | TokenLet Pos
    | TokenIn Pos
    | TokenIf Pos
    | TokenThen Pos
    | TokenElse Pos
    | TokenLetLoc Pos
    | TokenLetRegion Pos
    | TokenCase Pos
    | TokenOf Pos
    | TokenStart Pos
    | TokenAfter Pos

    -- binary operators
    | TokenPow Pos
    | TokenMul Pos
    | TokenDiv Pos
    | TokenDivInline Pos
    | TokenModInline Pos
    | TokenFMul Pos
    | TokenFDiv Pos
    | TokenAdd Pos
    | TokenSub Pos
    | TokenFAdd Pos
    | TokenFSub Pos
    | TokenEq Pos
    | TokenFEq Pos
    | TokenCEq Pos
    | TokenGt Pos
    | TokenLt Pos
    | TokenFGt Pos
    | TokenFLt Pos
    | TokenGe Pos
    | TokenLe Pos
    | TokenFGe Pos
    | TokenFLe Pos
    | TokenNeq Pos
    | TokenAnd Pos
    | TokenOr Pos

    -- base types
    | TokenIntType Pos
    | TokenFloatType Pos
    | TokenBoolType Pos
    | TokenCharType Pos
    | TokenStringType Pos

    -- variable tokens
    | TokenIdentLower Pos String
    | TokenIdentUpper Pos String
    | TokenIntLit Pos Int
    | TokenFloatLit Pos Float
    | TokenBoolLit Pos Bool
    | TokenCharLit Pos Char
    | TokenStringLit Pos String

    -- primitive application tokens
    | TokenPrintInt Pos
    | TokenPrintChar Pos
    | TokenPrintFloat Pos
    | TokenPrintBool Pos

    | TokenReadPackedFile Pos
    | TokenWritePackedFile Pos

    -- other
    | TokenMain Pos
    | TokenNewLine Pos
    | TokenEOF Pos
    deriving Show

showPos :: Pos -> String
showPos (Pos l c) = "line: " ++ show l ++ ", column: " ++ show c

pos :: Token -> Pos
-- data constructor
pos (TokenData p)       = p

-- symbols
pos (TokenAssign p)     = p
pos (TokenColon p)      = p
pos (TokenLBracket p)   = p
pos (TokenRBracket p)   = p
pos (TokenAt p)         = p
pos (TokenArrow p)      = p
pos (TokenBar p)        = p
pos (TokenComma p)      = p
pos (TokenLParen p)     = p
pos (TokenRParen p)     = p
pos (TokenComment p)    = p

-- common expr keywords
pos (TokenLet p)        = p
pos (TokenIn p)         = p
pos (TokenIf p)         = p
pos (TokenThen p)       = p
pos (TokenElse p)       = p
pos (TokenLetLoc p)     = p
pos (TokenLetRegion p)  = p
pos (TokenCase p)       = p
pos (TokenOf p)         = p
pos (TokenStart p)      = p
pos (TokenAfter p)      = p

-- binary operators
pos (TokenPow p)        = p
pos (TokenMul p)        = p
pos (TokenDiv p)        = p
pos (TokenDivInline p)  = p
pos (TokenModInline p)  = p
pos (TokenFMul p)       = p
pos (TokenFDiv p)       = p
pos (TokenAdd p)        = p
pos (TokenSub p)        = p
pos (TokenFAdd p)       = p
pos (TokenFSub p)       = p
pos (TokenEq p)         = p
pos (TokenFEq p)        = p
pos (TokenCEq p)        = p
pos (TokenGt p)         = p
pos (TokenLt p)         = p
pos (TokenFGt p)        = p
pos (TokenFLt p)        = p
pos (TokenGe p)         = p
pos (TokenLe p)         = p
pos (TokenFGe p)        = p
pos (TokenFLe p)        = p
pos (TokenNeq p)        = p
pos (TokenAnd p)        = p
pos (TokenOr p)         = p

-- base types
pos (TokenIntType p)    = p
pos (TokenFloatType p)  = p
pos (TokenBoolType p)   = p
pos (TokenCharType p)   = p
pos (TokenStringType p) = p

-- variable tokens
pos (TokenIdentLower p _)    = p
pos (TokenIdentUpper p _)    = p
pos (TokenIntLit p _)   = p
pos (TokenFloatLit p _) = p
pos (TokenBoolLit p _)  = p
pos (TokenCharLit p _)  = p
pos (TokenStringLit p _) = p

-- primitive application tokens
pos (TokenPrintInt p) = p
pos (TokenPrintChar p) = p
pos (TokenPrintFloat p) = p
pos (TokenPrintBool p) = p

pos (TokenReadPackedFile p) = p
pos (TokenWritePackedFile p) = p

-- other
pos (TokenMain p)       = p
pos (TokenNewLine p)    = p
pos (TokenEOF p)        = p

