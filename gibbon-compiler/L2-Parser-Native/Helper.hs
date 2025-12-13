{-# LANGUAGE InstanceSigs #-}
module Helper where

makeGreen :: String -> String
makeGreen s = "\x1b[32m" ++ s ++ "\x1b[0m"

makeRed :: String -> String
makeRed s = "\x1b[31m" ++ s ++ "\x1b[0m"

makeBold :: String -> String
makeBold s = "\x1b[1m" ++ s ++ "\x1b[0m"

data E a = Ok a | Failed String deriving Show
-- data ParseResult a = Ok a | Failed String deriving Show
-- type E a = String -> ParseResult a

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