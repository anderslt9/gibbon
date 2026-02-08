module RunTest where

import Data.List (isPrefixOf, isSuffixOf)
-- import System.Environment (getArgs)
import Control.Monad (forM_, when)
import System.FilePath.Posix (takeBaseName)
import System.IO (writeFile, appendFile)
import Lexer (lexer)
import PrintAST (printAST)
import Helper (makeRed, makeBold, makeGreen, E(Ok, Failed))
-- import L2ParserNative (l2ParserNative)
import Parser (l2ParserNative)
import ConvertToTypedAST (inferProgram, tType, tNode)
import ConvertToL2AST (convertToL2AST)
import Gibbon.Common (sdoc)
import qualified Passes as Pass

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
                    showL2AST :: Bool
                    } deriving Show

baseCfg :: SimpleCfg 
baseCfg = SimpleCfg {   inFiles = [],
                        outFiles = [],
                        showTokens = True,
                        showRaw = False,
                        showInitialParse = True,
                        showTyped = False,
                        showL2AST = True
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
                putStrLn $ "Wrote Tokens to: " ++ makeBold (outFiles config !! (i - 1))
            else do
                putStrLn "\n== Tokens =="
                forM_ tokens $ \token -> print token
                putStrLn "Tokens written to console (no file specified)."

        -- runs/prints parser
        let ast = l2ParserNative tokens
            parsed_str = fmap (printAST 0) ast
            -- gets typed AST
            ast' = ast >>= \f -> Pass.runProgramPass f Pass.replaceLocRegionNames
            ast'' = ast' >>= \f -> Pass.runProgramPass f Pass.replaceLocRegionInAfterExprs
            typed_ast = ast'' >>= inferProgram
            -- gets L2 AST
            l2_ast = typed_ast >>= convertToL2AST
        
        -- displays raw result
        when (showRaw config) $ do
            if length (outFiles config) >= i
            then do
                appendFile (outFiles config !! (i - 1)) "\n== Raw Parse Result ==\n"
                appendFile (outFiles config !! (i - 1)) (show ast)
                putStrLn $ "Wrote Raw Parse Result to: " ++ makeBold (outFiles config !! (i - 1))
            else do 
                putStrLn "\n== Raw Parse Result =="
                print ast
                putStrLn "Raw Parse Result written to console (no file specified)."
        
        -- displays pretty result
        case parsed_str of
            Ok x -> when (showInitialParse config) $ do 
                    if length (outFiles config) >= i
                    then do
                        appendFile (outFiles config !! (i - 1)) "\n== Pretty Parse Result ==\n"
                        appendFile (outFiles config !! (i - 1)) x
                        putStrLn $ "Wrote Pretty Parse Result to: " ++ makeBold (outFiles config !! (i - 1))
                    else do 
                        putStrLn "\n== Pretty Parse Result =="
                        putStrLn x
                        putStrLn "Pretty Parse Result written to console (no file specified)."
            Failed e -> putStrLn . makeRed $  "Parsing Failed: " ++ e
        
        -- displays typed AST
        case typed_ast of
            Ok t_ast -> when (showTyped config) $ do
                        let typedASTPretty = printAST 0 (tNode t_ast)
                        if length (outFiles config) >= i
                        then do
                            appendFile (outFiles config !! (i - 1)) "\n== Typed AST ==\n"
                            appendFile (outFiles config !! (i - 1)) ("Return type: " ++ show (tType t_ast) ++ "\n" ++ typedASTPretty)
                            putStrLn $ "Wrote Typed AST to: " ++ makeBold (outFiles config !! (i - 1))
                        else do 
                            putStrLn "\n== Typed AST =="
                            putStrLn ("Return type: " ++ show (tType t_ast) ++ "\n" ++ typedASTPretty)
                            putStrLn "Typed AST written to console (no file specified)."
            Failed e -> putStrLn . makeRed $ "Type Inference Failed: " ++ e
        
        -- displays L2 AST
        case l2_ast of
            Ok l2p -> when (showL2AST config) $ do 
                        if length (outFiles config) >= i
                        then do
                            appendFile (outFiles config !! (i - 1)) "\n== L2 AST ==\n"
                            appendFile (outFiles config !! (i - 1)) (sdoc l2p)
                            putStrLn $ "Wrote L2 AST to: " ++ makeBold (outFiles config !! (i - 1))
                        else do 
                            putStrLn "\n== L2 AST =="
                            putStrLn (sdoc l2p)
                            putStrLn "L2 AST written to console (no file specified)."
            Failed e -> putStrLn . makeRed $ "Conversion to L2 AST Failed: " ++ e


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