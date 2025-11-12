module Tokens where 

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
    | TokenComment
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
    | TokenMain
    | TokenNewLine
    deriving Show