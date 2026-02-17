module ParseIntoL2 where

import qualified Gibbon.L2.Syntax as L2
import Lexer (lexer)
import Parser (l2ParserNative)
import qualified ConvertToTypedAST (inferProgram)
import qualified ConvertToL2AST (convertToL2AST)
import Helper
import qualified Passes as Pass

parseIntoL2 :: FilePath -> IO L2.Prog2
parseIntoL2 filePath = do
    contents <- readFile filePath
    let tokens = lexer contents
        ast = l2ParserNative tokens
        programPasses = [Pass.replaceLocRegionNames, Pass.replaceLocRegionInAfterExprs]
        ast' = ast >>= Pass.runProgramPasses programPasses
        typed_ast = ast' >>= ConvertToTypedAST.inferProgram
        l2_ast = typed_ast >>= ConvertToL2AST.convertToL2AST
    case l2_ast of
        Ok l2Ast -> return l2Ast
        Failed err -> error $ "Error parsing into L2 AST: " ++ err