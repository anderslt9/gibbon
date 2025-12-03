module RunTest where

import Data.List (isPrefixOf, isSuffixOf)
-- import System.Environment (getArgs)
import Control.Monad (forM_, when)
import System.FilePath.Posix (takeBaseName)
import System.IO (writeFile, appendFile)
import Lexer (lexer)
import PrintAST (printAST)
import Helper (makeRed, makeBold, E(Ok, Failed))
-- import L2ParserNative (l2ParserNative)
import Parser (l2ParserNative)

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