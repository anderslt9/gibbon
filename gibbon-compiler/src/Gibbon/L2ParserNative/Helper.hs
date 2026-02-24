{-# LANGUAGE InstanceSigs #-}
module Gibbon.L2ParserNative.Helper where
import Control.Monad.IO.Class (MonadIO, liftIO)

makeGreen :: String -> String
makeGreen s = "\x1b[32m" ++ s ++ "\x1b[0m"

makeRed :: String -> String
makeRed s = "\x1b[31m" ++ s ++ "\x1b[0m"

makeBold :: String -> String
makeBold s = "\x1b[1m" ++ s ++ "\x1b[0m"

data E a = Ok a | Failed String deriving Show
-- data ParseResult a = Ok a | Failed String deriving Show
-- type E a = String -> ParseResult a

takeAlphaNum :: String -> String
takeAlphaNum [] = []
takeAlphaNum (x:xs)
    | x `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_") = x : takeAlphaNum xs
    | otherwise = []

checkAllSame :: (Eq a) => (a -> a -> Bool) -> [a] -> Bool 
checkAllSame _ [] = True
checkAllSame eq (x:xs) = all (eq x) xs

safeHead :: [a] -> E a
safeHead []    = Failed "Empty list"
safeHead (x:_) = Ok x

splitLast :: [a] -> E ([a], a)
splitLast [] = Failed "splitLast: Empty list has no last element"
splitLast [x] = return ([], x)
splitLast (x:xs) = do
    (rest, lastElem) <- splitLast xs
    return (x:rest, lastElem)
instance Functor E where
    fmap f (Ok x)      = Ok (f x)
    fmap _ (Failed e)  = Failed e

instance Applicative E where
    pure = Ok
    (Ok f) <*> (Ok x)     = Ok (f x)
    (Failed e) <*> _      = Failed e
    _ <*> (Failed e)      = Failed e

instance Monad E where
    (>>=) :: E a -> (a -> E b) -> E b
    (Ok x) >>= f = f x
    (Failed e) >>= _ = Failed e

instance MonadIO E where
    liftIO a = do liftIO a
        