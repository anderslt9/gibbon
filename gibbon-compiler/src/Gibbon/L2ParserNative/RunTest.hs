module Gibbon.L2ParserNative.RunTest where

import Data.List (isPrefixOf, isSuffixOf)
-- import System.Environment (getArgs)
import Control.Monad (forM_, when)
import System.FilePath.Posix (takeBaseName)
-- import System.IO (writeFile, appendFile)
import Gibbon.L2ParserNative.Lexer (lexer)
import Gibbon.L2ParserNative.PrintAST (printAST)
import Gibbon.L2ParserNative.Helper (makeRed, makeBold, E(Ok, Failed))
-- import L2ParserNative (l2ParserNative)
import Gibbon.L2ParserNative.Parser (l2ParserNative)
import Gibbon.L2ParserNative.ConvertToTypedAST (inferProgram, tType, tNode)
import Gibbon.L2ParserNative.ConvertToL2AST (convertToL2AST)
-- import Gibbon.Common (sdoc)
import qualified Gibbon.L2ParserNative.Passes as Pass
import qualified Gibbon.L2.Syntax as L2
import Control.DeepSeq (force)
import GHC.IO (evaluate)
-- import Control.Monad.Trans.Class (lift)
import Gibbon.Pretty (pprender)
-- import TestRunner (Test(name))

type Args = [String]
type LastParsed = String
data Result a = Success a | Failure String deriving Show

data SimpleCfg = SimpleCfg {
                    inFiles :: [String],
                    outFiles :: [String],
                    showTokens :: Bool,
                    showRaw :: Bool,
                    showInitialParse :: Bool,
                    showTyped :: Bool,
                    showAfterPasses :: Bool,
                    showL2AST :: Bool
                    } deriving Show

baseCfg :: SimpleCfg 
baseCfg = SimpleCfg {   inFiles = [],
                        outFiles = [],
                        showTokens = True,
                        showRaw = False,
                        showInitialParse = True,
                        showAfterPasses = False,
                        showTyped = False,
                        showL2AST = True
}

displayResult :: E a -> String -> Int -> SimpleCfg -> (a -> String) -> (SimpleCfg -> Bool) -> IO ()
displayResult (Ok x) namedResult i config func toShow = do
    when (toShow config) $ do
        let paddingSize = 50
            lengthAcross = ((paddingSize - length namedResult) `div` 2) - 1
            header = (replicate lengthAcross '=') ++ " " ++ namedResult ++ " " ++ (replicate (lengthAcross + (length namedResult `mod` 2)) '=') ++ "\n"
            padding = (replicate paddingSize '=') ++ "\n"
            resultStr = func x ++ "\n\n"
            totalStr = padding ++ header ++ padding ++ resultStr

        if length (outFiles config) >= i
        then do
            appendFile (outFiles config !! (i - 1)) totalStr
            putStrLn $ "Wrote " ++ namedResult ++ " to: " ++ makeBold (outFiles config !! (i - 1))
        else do
            putStrLn totalStr 
            putStrLn (namedResult ++ " written to console (no file specified).")
displayResult (Failed e) namedResult _ _ _ _ = putStrLn . makeRed $ namedResult ++ " Failed: " ++ e

printL2AST:: L2.Prog2 -> IO String
printL2AST prog = do 
    temp_prog <- evaluate $ force prog
    return $ pprender temp_prog

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
        displayResult (Ok tokens) "Tokens" i config (foldl (\acc token -> acc ++ show token ++ "\n") "") showTokens

        -- runs/prints parser
        let ast = l2ParserNative tokens
            parsed_str = fmap (printAST 0) ast
            -- gets typed AST
            -- programPasses = [Pass.replaceLocRegionNames, Pass.replaceLocRegionInAfterExprs]
        ast' <- Pass.runProgramPasses Pass.allPasses ast
        let typed_ast = ast' >>= inferProgram
            -- gets L2 AST
            l2_ast = typed_ast >>= convertToL2AST
        
        -- displays raw result
        displayResult ast "Raw Parse Result" i config show showRaw
        
        -- displays pretty result
        displayResult parsed_str "Pretty Parse Result" i config id showInitialParse

        -- displays result after passes
        displayResult ast' "After Passes" i config (printAST 0) showAfterPasses
        
        -- displays typed AST
        displayResult typed_ast "Typed AST" i config (\t_ast -> "Return type: " ++ show (tType t_ast) ++ "\n" ++ printAST 0 (tNode t_ast)) showTyped
        
        -- displays L2 AST
        l2_ast_str <- do
            case l2_ast of
                Ok temp_ast -> printL2AST temp_ast
                Failed e -> return $ "Failed to convert to L2 AST: " ++ e
        displayResult (Ok l2_ast_str) "L2 AST" i config id showL2AST


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
            | "--show-initial-parse" `isPrefixOf` arg = setConfig' rest (cfg {showInitialParse = getBoolean arg}) ""
            | "--show-after-passes" `isPrefixOf` arg = setConfig' rest (cfg {showAfterPasses = getBoolean arg}) ""
            | "--show-typed" `isPrefixOf` arg = setConfig' rest (cfg {showTyped = getBoolean arg}) ""
            | "--show-l2ast" `isPrefixOf` arg = setConfig' rest (cfg {showL2AST = getBoolean arg}) ""

            -- file arguments
            | lastParsed == "-i" =  if checkValidFile arg
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