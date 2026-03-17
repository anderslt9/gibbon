module Gibbon.L2Frontend where

import Gibbon.L2.Syntax
import Gibbon.L2ParserNative.Lexer (lexer)
import Gibbon.L2ParserNative.Parser (l2ParserNative)
import qualified Gibbon.L2ParserNative.ConvertToTypedAST as ToTyped
import qualified Gibbon.L2ParserNative.ConvertToL2AST as ToL2AST
import Gibbon.L2ParserNative.Helper
import qualified Gibbon.L2ParserNative.Passes as Pass
import Gibbon.Common

parseL2 :: Config -> FilePath -> IO (PassM Prog2)
parseL2 config filePath = do
    contents <- readFile filePath
    let tokens = lexer contents
        ast = l2ParserNative tokens
        programPasses = [Pass.replaceLocRegionNames, Pass.replaceLocRegionInAfterExprs]
        ast' = ast >>= Pass.runProgramPasses programPasses
        typed_ast = ast' >>= ToTyped.inferProgram
        l2_ast = typed_ast >>= ToL2AST.convertToL2AST
    case l2_ast of
        Ok l2Ast -> return . pure $ l2Ast
        Failed err -> error $ "Error parsing into L2 AST: " ++ err
