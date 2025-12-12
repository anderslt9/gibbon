import Language.Haskell.Exts
import Language.Haskell.Exts.CPP

main :: IO ()
main = do
    let cppOptions = defaultCpphsOptions  -- Default CPP options
    let parseMode = defaultParseMode      -- Default parsing mode
    sourceCode <- readFile "gibbon-compiler/examples/add1.hs"   -- Read Haskell source file
    result <- parseFileContentsWithCommentsAndCPP cppOptions parseMode sourceCode
    case result of
        ParseOk (mod, comments) -> do
            putStrLn "Parsing succeeded!"
            print mod
            print comments
        ParseFailed loc err -> do
            putStrLn $ "Parsing failed at " ++ show loc ++ ": " ++ err
