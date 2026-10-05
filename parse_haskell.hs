{-# LANGUAGE CPP #-}

import           Data.Data
import           Data.Maybe (catMaybes)
import           Language.Haskell.Exts.Extension
import           Language.Haskell.Exts.Parser
import           Language.Haskell.Exts.Pretty
import           Language.Haskell.Exts.Comments (Comment(..))
import           Language.Haskell.Exts.CPP
import qualified Text.PrettyPrint as PP
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn, stderr)

parseMode :: ParseMode
parseMode = defaultParseMode
  { extensions = [ EnableExtension ScopedTypeVariables
                 , EnableExtension CPP
                 , EnableExtension TypeApplications
                 ] ++ extensions defaultParseMode
  }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> parsePath path
    _ -> do
      hPutStrLn stderr "Usage: parse_haskell.hs FILE"
      exitFailure

parsePath :: FilePath -> IO ()
parsePath path = do
  source <- readFile path
  parsed <- parseFileContentsWithCommentsAndCPP defaultCpphsOptions parseMode source
  case parsed of
    ParseOk (ast, comments) -> do
      putStrLn "AST:"
      putStrLn (prettyAst (fmap (const ()) ast))
      putStrLn "Comments:"
      mapM_ printComment comments
    ParseFailed location message -> do
      hPutStrLn stderr $ "Parse error at " ++ prettyPrint location ++ ": " ++ message
      exitFailure

printComment (Comment isBlock _ text) = do
  putStrLn $ (if isBlock then "Block comment: " else "Line comment: ") ++ text

prettyAst :: Data a => a -> String
prettyAst = PP.render . renderNode

renderNode :: Data a => a -> PP.Doc
renderNode value
  | Just atom <- primitive value = PP.text atom
  | isList value = PP.brackets (PP.vcat (renderList value))
  | null fields = PP.text constructor
  | otherwise = PP.text constructor PP.$$ PP.nest 2 (PP.vcat fields)
  where
    constructor = showConstr (toConstr value)
    fields = catMaybes (gmapQ renderChild value)
    renderChild child
      | isUnit child = Nothing
      | otherwise = Just (renderNode child)

isUnit :: Data a => a -> Bool
isUnit value = case cast value of
  Just () -> True
  Nothing -> False

isList :: Data a => a -> Bool
isList value =
  let constructor = showConstr (toConstr value)
  in constructor == "[]" || constructor == "(:)"

renderList :: Data a => a -> [PP.Doc]
renderList value
  | showConstr (toConstr value) == "[]" = []
  | otherwise = concat (gmapQ renderListChild value)
  where
    renderListChild child
      | isList child = renderList child
      | otherwise = [renderNode child]

primitive :: Data a => a -> Maybe String
primitive value =
  case cast value of
    Just stringValue -> Just (show (stringValue :: String))
    Nothing -> case cast value of
      Just intValue -> Just (show (intValue :: Int))
      Nothing -> case cast value of
        Just integerValue -> Just (show (integerValue :: Integer))
        Nothing -> case cast value of
          Just boolValue -> Just (show (boolValue :: Bool))
          Nothing -> case cast value of
            Just charValue -> Just (show (charValue :: Char))
            Nothing -> case cast value of
              Just floatValue -> Just (show (floatValue :: Float))
              Nothing -> case cast value of
                Just doubleValue -> Just (show (doubleValue :: Double))
                Nothing -> Nothing