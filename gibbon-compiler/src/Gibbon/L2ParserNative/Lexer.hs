module Gibbon.L2ParserNative.Lexer where
import Data.Char (isAlpha, isDigit, isSpace, isLower)
import Gibbon.L2ParserNative.Tokens

lexer :: String -> [Token]
lexer = lexer' (Pos 1 1)

-- actual lexer
lexer' :: Pos -> String -> [Token]
lexer' p [] = [TokenEOF p]
lexer' p (c:cs)
    | c == '\n' = 
        case cs of 
            (d:_) | isSpace d -> lexer' (advance p c) cs
            _  -> TokenNewLine p : lexer' (advance p c) cs
    | isSpace c = lexer' (advance p c) cs
    | isAlpha c = lexVar p (c:cs)
    | isDigit c = lexNum p (c:cs)
    | c == '"' = 
        let (str, rest) = span (/= '"') cs
        in case rest of 
            ('"':tailRest) -> TokenStringLit p str : lexer' (advanceStr p ('"':str ++ "\"")) tailRest
            [] -> [TokenStringLit p str, TokenEOF (advanceStr p str)]
            _ -> []
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
lexer' _ _ = []  -- unrecognized character, could also raise an error

advanceStr :: Pos -> String -> Pos
advanceStr p cs = foldl advance p cs
-- advanceStr p [] = p
-- advanceStr p (c:cs) = advanceStr (advance p c) cs

advance :: Pos -> Char -> Pos
advance (Pos l _) '\n' = Pos (l + 1) 1
advance (Pos l c) _    = Pos l (c + 1)

lexComment :: String -> String
lexComment cs =
    case break (== '\n') cs of 
        (_, [])     -> ""
        (_, _:rest) -> rest

lexNum :: Pos -> [Char] -> [Token]
lexNum p cs = 
    case span isDigit cs of
        (num, "")   -> TokenIntLit p (read num) : [TokenEOF (advanceStr p num)]
        (num,rest)  -> case rest of 
            ('.':ds) -> case span isDigit ds of
                            (num2, rest2) -> 
                                let floatStr = num ++ "." ++ num2 
                                in TokenFloatLit p (read floatStr) : lexer' (advanceStr p floatStr) rest2
                            -- otherwise error
            _ -> TokenIntLit p (read num) : lexer' (advanceStr p num) rest

matchVar :: Pos -> [Char] -> [Token]
matchVar p cs =  
    case span isValidChar cs of
        (v@(c:_), rest) ->  
            if isLower c
                        then TokenIdentLower p v : lexer' (advanceStr p v) rest
                        else if isValidStartChar c
                            then TokenIdentUpper p v : lexer' (advanceStr p v) rest
                            else []
        _ -> []
    where   isValidChar n = n `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['`'] ++ ['0'..'9'])
            isValidStartChar n = n `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['`'])

lexVar :: Pos -> [Char] -> [Token]
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
        (_, _)    -> matchVar p cs