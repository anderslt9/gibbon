{-# OPTIONS_GHC -w #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Main where
-- import Data.Char (isSpace, isAlpha, isDigit, isLower)
import Data.List (break, isPrefixOf, isSuffixOf)
import System.Environment (getArgs)
import Control.Monad (forM_, when)
import System.FilePath.Posix (takeBaseName)
import System.IO (writeFile, appendFile)
import AST
import Tokens
import PrintAST
import Lexer
import qualified Control.Monad as Happy_Prelude
import qualified Data.Bool as Happy_Prelude
import qualified Data.Function as Happy_Prelude
import qualified Data.Int as Happy_Prelude
import qualified Data.List as Happy_Prelude
import qualified Data.Maybe as Happy_Prelude
import qualified Data.String as Happy_Prelude
import qualified Data.Tuple as Happy_Prelude
import qualified GHC.Err as Happy_Prelude
import qualified GHC.Num as Happy_Prelude
import qualified Text.Show as Happy_Prelude
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 2.1.7

data HappyAbsSyn t11
        = HappyTerminal (Token)
        | HappyErrorToken Happy_Prelude.Int
        | HappyAbsSyn5 (Program)
        | HappyAbsSyn6 (Expr)
        | HappyAbsSyn7 (DataTypeDecl)
        | HappyAbsSyn8 (DataField)
        | HappyAbsSyn9 (CombinedTypeCon)
        | HappyAbsSyn10 (FuncDecl)
        | HappyAbsSyn11 t11
        | HappyAbsSyn12 (LocatedType)
        | HappyAbsSyn13 (CombinedLocType)
        | HappyAbsSyn14 (TypeScheme)
        | HappyAbsSyn15 (CombinedType)
        | HappyAbsSyn16 (BaseType)
        | HappyAbsSyn17 (LocExpress)
        | HappyAbsSyn18 (LocRegion)
        | HappyAbsSyn19 (Val)
        | HappyAbsSyn20 (Lit)
        | HappyAbsSyn28 (Pat)
        | HappyAbsSyn29 (PatMatch)
        | HappyAbsSyn30 (BinOp)
        | HappyAbsSyn31 (FuncVar)
        | HappyAbsSyn32 (RegionVar)
        | HappyAbsSyn33 (LocVar)
        | HappyAbsSyn34 (IndexVar)
        | HappyAbsSyn35 (TypeCon)
        | HappyAbsSyn36 (DataCon)
        | HappyAbsSyn37 (Var)
        | HappyAbsSyn38 ([Var])
        | HappyAbsSyn39 ([DataField])
        | HappyAbsSyn40 ([CombinedTypeCon])
        | HappyAbsSyn41 ([Expr])
        | HappyAbsSyn42 ([Val])
        | HappyAbsSyn43 ([Pat])
        | HappyAbsSyn44 ([PatMatch])
        | HappyAbsSyn45 ([DataTypeDecl])
        | HappyAbsSyn46 ([FuncDecl])
        | HappyAbsSyn47 ([LocRegion ])
        | HappyAbsSyn48 ([CombinedType ])

{-# NOINLINE happyTokenStrings #-}
happyTokenStrings = ["data","'='","':'","'['","']'","'@'","'->'","'|'","','","'('","')'","'--'","let","in","letloc","letregion","case","of","start","after","'^'","'*'","'/'","'`div`'","'`mod`'","'.*.'","'./.'","'+'","'-'","'.+.'","'.-.'","'=='","'.==.'","'*==*'","'>'","'<'","'.>.'","'.<.'","'>='","'<='","'.>=.'","'.<=.'","'/='","'&&'","'||'","Int","Float","Bool","String","IDENT_LC","IDENT_UC","INT_LIT","FLOAT_LIT","BOOL_LIT","STRING_LIT","main","'\\n'","EOF","%eof"]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x0b\x00\x00\x00\x0b\x00\x00\x00\xc8\xff\xff\xff\xff\xff\xff\xff\x05\x00\x00\x00\x54\x00\x00\x00\x73\x00\x00\x00\x00\x00\x00\x00\x57\x00\x00\x00\x58\x00\x00\x00\xb3\x00\x00\x00\xd9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x72\x00\x00\x00\x85\x00\x00\x00\xbe\x00\x00\x00\x00\x00\x00\x00\x3e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8f\x00\x00\x00\x00\x00\x00\x00\x3e\x00\x00\x00\xbb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\x00\x00\x00\x8b\x00\x00\x00\xde\x00\x00\x00\xe1\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\x00\x00\x00\xea\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x00\x00\xc3\x00\x00\x00\xee\x00\x00\x00\xcb\x00\x00\x00\x17\x00\x00\x00\xf9\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x00\x00\x00\xcc\x00\x00\x00\xf5\x00\x00\x00\xd7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd9\x00\x00\x00\x08\x01\x00\x00\x00\x00\x00\x00\xfc\x00\x00\x00\x00\x00\x00\x00\x01\x01\x00\x00\x00\x00\x00\x00\x0e\x01\x00\x00\x11\x01\x00\x00\x5d\x00\x00\x00\x2a\x00\x00\x00\x07\x01\x00\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xb2\x00\x00\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x8c\x00\x00\x00\x0c\x01\x00\x00\x2a\x00\x00\x00\xe2\x00\x00\x00\x17\x01\x00\x00\x19\x01\x00\x00\x00\x00\x00\x00\x02\x01\x00\x00\xa9\x00\x00\x00\x00\x00\x00\x00\x2b\x01\x00\x00\x03\x01\x00\x00\xb2\x00\x00\x00\x2a\x01\x00\x00\x82\x00\x00\x00\x37\x01\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x00\x00\x2a\x00\x00\x00\x2a\x00\x00\x00\x1f\x01\x00\x00\x15\x01\x00\x00\x8c\x00\x00\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x60\x00\x00\x00\xf1\x00\x00\x00\x16\x01\x00\x00\x50\x00\x00\x00\x1a\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x46\x01\x00\x00\x2f\x01\x00\x00\x2a\x00\x00\x00\xf1\x00\x00\x00\xb2\x00\x00\x00\x40\x01\x00\x00\x00\x00\x00\x00\x42\x01\x00\x00\x26\x01\x00\x00\x7d\x00\x00\x00\x2a\x00\x00\x00\x43\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x01\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\xbc\x01\x00\x00\x00\x00\x00\x00\x2a\x00\x00\x00\x51\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb2\x00\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x28\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x3b\x00\x00\x00\x41\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5a\x01\x00\x00\xad\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x44\x00\x00\x00\x00\x00\x00\x00\x4e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x48\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x55\x01\x00\x00\x00\x00\x00\x00\x1e\x01\x00\x00\x4e\x01\x00\x00\x62\x01\x00\x00\x58\x01\x00\x00\xdb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4c\x00\x00\x00\x56\x01\x00\x00\x65\x01\x00\x00\x48\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x59\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x01\x00\x00\xdd\x00\x00\x00\x1e\x00\x00\x00\x31\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x01\x00\x00\x00\x00\x00\x00\x74\x01\x00\x00\x69\x01\x00\x00\x44\x01\x00\x00\x00\x00\x00\x00\xae\x00\x00\x00\x77\x01\x00\x00\x57\x01\x00\x00\x20\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6b\x01\x00\x00\x78\x01\x00\x00\x00\x00\x00\x00\x60\x01\x00\x00\x32\x00\x00\x00\x6f\x01\x00\x00\x00\x00\x00\x00\x87\x01\x00\x00\x00\x00\x00\x00\x7c\x01\x00\x00\x00\x00\x00\x00\xf4\x00\x00\x00\x6a\x01\x00\x00\x7d\x01\x00\x00\x00\x00\x00\x00\x7b\x01\x00\x00\xda\x00\x00\x00\x90\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa2\x00\x00\x00\x6e\x00\x00\x00\x00\x00\x00\x00\x7e\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa3\x01\x00\x00\xd2\x01\x00\x00\x7f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x01\x00\x00\xb6\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x01\x00\x00\x00\x00\x00\x00\xdc\x00\x00\x00\x00\x00\x00\x00\xc9\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x7f\x01\x00\x00\x00\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x9f\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x9c\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb9\xff\xff\xff\x9e\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf8\xff\xff\xff\x96\xff\xff\xff\x9b\xff\xff\xff\x9d\xff\xff\xff\xaf\xff\xff\xff\xae\xff\xff\xff\xac\xff\xff\xff\xfc\xff\xff\xff\xb4\xff\xff\xff\xf2\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x95\xff\xff\xff\xf1\xff\xff\xff\xf5\xff\xff\xff\xf3\xff\xff\xff\xf0\xff\xff\xff\xef\xff\xff\xff\xee\xff\xff\xff\xed\xff\xff\xff\x00\x00\x00\x00\x9a\xff\xff\xff\xfe\xff\xff\xff\xe0\xff\xff\xff\xe6\xff\xff\xff\xfd\xff\xff\xff\xd9\xff\xff\xff\xda\xff\xff\xff\xdb\xff\xff\xff\xde\xff\xff\xff\xdd\xff\xff\xff\xdc\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe7\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb9\xff\xff\xff\xe5\xff\xff\xff\xe4\xff\xff\xff\xe3\xff\xff\xff\xe2\xff\xff\xff\x96\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xab\xff\xff\xff\xf9\xff\xff\xff\xfa\xff\xff\xff\xfb\xff\xff\xff\xaa\xff\xff\xff\xad\xff\xff\xff\xf6\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x94\xff\xff\xff\x00\x00\x00\x00\xb3\xff\xff\xff\x00\x00\x00\x00\xb8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\xff\xff\xff\x00\x00\x00\x00\xc8\xff\xff\xff\xcc\xff\xff\xff\xcb\xff\xff\xff\xca\xff\xff\xff\xc9\xff\xff\xff\xd0\xff\xff\xff\xcf\xff\xff\xff\xce\xff\xff\xff\xcd\xff\xff\xff\xc7\xff\xff\xff\xc6\xff\xff\xff\xc5\xff\xff\xff\xc4\xff\xff\xff\xc3\xff\xff\xff\xc2\xff\xff\xff\xc1\xff\xff\xff\xc0\xff\xff\xff\xbf\xff\xff\xff\xbe\xff\xff\xff\xbd\xff\xff\xff\xbc\xff\xff\xff\xbb\xff\xff\xff\xba\xff\xff\xff\xe1\xff\xff\xff\x98\xff\xff\xff\x00\x00\x00\x00\xa9\xff\xff\xff\xd4\xff\xff\xff\xdf\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x99\xff\xff\xff\x00\x00\x00\x00\xb7\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xa4\xff\xff\xff\xa2\xff\xff\xff\xd3\xff\xff\xff\xd8\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa8\xff\xff\xff\x97\xff\xff\xff\x00\x00\x00\x00\xd5\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa3\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb2\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe9\xff\xff\xff\xb1\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd7\xff\xff\xff\x00\x00\x00\x00\xf4\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xec\xff\xff\xff\xea\xff\xff\xff\x00\x00\x00\x00\xd2\xff\xff\xff\xa1\xff\xff\xff\x00\x00\x00\x00\xb0\xff\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb6\xff\xff\xff\xe8\xff\xff\xff\xf7\xff\xff\xff\xd1\xff\xff\xff\xa0\xff\xff\xff\xeb\xff\xff\xff\xd6\xff\xff\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\xff\xff\x02\x00\x00\x00\x3a\x00\x00\x00\x02\x00\x00\x00\x0b\x00\x00\x00\x0c\x00\x00\x00\x03\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x33\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x39\x00\x00\x00\x0b\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x02\x00\x00\x00\x0d\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x38\x00\x00\x00\x33\x00\x00\x00\x3a\x00\x00\x00\x3b\x00\x00\x00\x0b\x00\x00\x00\x33\x00\x00\x00\x17\x00\x00\x00\x0e\x00\x00\x00\x34\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x02\x00\x00\x00\x0d\x00\x00\x00\x1f\x00\x00\x00\x05\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x26\x00\x00\x00\x03\x00\x00\x00\x2a\x00\x00\x00\x17\x00\x00\x00\x33\x00\x00\x00\x03\x00\x00\x00\x35\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x38\x00\x00\x00\x28\x00\x00\x00\x1f\x00\x00\x00\x04\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x1a\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x1e\x00\x00\x00\x0b\x00\x00\x00\x0a\x00\x00\x00\x2a\x00\x00\x00\x0c\x00\x00\x00\x33\x00\x00\x00\x34\x00\x00\x00\x35\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x38\x00\x00\x00\x1f\x00\x00\x00\x29\x00\x00\x00\x2b\x00\x00\x00\x22\x00\x00\x00\x1f\x00\x00\x00\x08\x00\x00\x00\x0c\x00\x00\x00\x1e\x00\x00\x00\x0b\x00\x00\x00\x1e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x23\x00\x00\x00\x34\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x03\x00\x00\x00\x2b\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x0f\x00\x00\x00\x0b\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x3c\x00\x00\x00\x3a\x00\x00\x00\x3a\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x04\x00\x00\x00\x3b\x00\x00\x00\x01\x00\x00\x00\x06\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x05\x00\x00\x00\x0b\x00\x00\x00\x0b\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x04\x00\x00\x00\x0a\x00\x00\x00\x0b\x00\x00\x00\x18\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\x3a\x00\x00\x00\x34\x00\x00\x00\x03\x00\x00\x00\x20\x00\x00\x00\x34\x00\x00\x00\x09\x00\x00\x00\x3a\x00\x00\x00\x1e\x00\x00\x00\x1a\x00\x00\x00\x16\x00\x00\x00\x17\x00\x00\x00\x18\x00\x00\x00\x07\x00\x00\x00\x1e\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x21\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x26\x00\x00\x00\x27\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x07\x00\x00\x00\x08\x00\x00\x00\x0b\x00\x00\x00\x08\x00\x00\x00\x0b\x00\x00\x00\x07\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x05\x00\x00\x00\x0b\x00\x00\x00\x33\x00\x00\x00\x1a\x00\x00\x00\x1e\x00\x00\x00\x0b\x00\x00\x00\x1e\x00\x00\x00\x20\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x33\x00\x00\x00\x33\x00\x00\x00\x0b\x00\x00\x00\x24\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x34\x00\x00\x00\x33\x00\x00\x00\x05\x00\x00\x00\x1a\x00\x00\x00\x13\x00\x00\x00\x0f\x00\x00\x00\x03\x00\x00\x00\x0b\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x04\x00\x00\x00\x34\x00\x00\x00\x0b\x00\x00\x00\x24\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x0b\x00\x00\x00\x0a\x00\x00\x00\x33\x00\x00\x00\x1a\x00\x00\x00\x35\x00\x00\x00\x36\x00\x00\x00\x37\x00\x00\x00\x38\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x33\x00\x00\x00\x0b\x00\x00\x00\x34\x00\x00\x00\x1a\x00\x00\x00\x0f\x00\x00\x00\x03\x00\x00\x00\x0c\x00\x00\x00\x1d\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x33\x00\x00\x00\x33\x00\x00\x00\x04\x00\x00\x00\x1a\x00\x00\x00\x0c\x00\x00\x00\x33\x00\x00\x00\x0c\x00\x00\x00\x0c\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x35\x00\x00\x00\x0c\x00\x00\x00\x0c\x00\x00\x00\x1a\x00\x00\x00\x1e\x00\x00\x00\x06\x00\x00\x00\x19\x00\x00\x00\x0d\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x20\x00\x00\x00\x0d\x00\x00\x00\x1a\x00\x00\x00\x1a\x00\x00\x00\x0d\x00\x00\x00\x1b\x00\x00\x00\x19\x00\x00\x00\x1c\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x0d\x00\x00\x00\x19\x00\x00\x00\x0c\x00\x00\x00\x1a\x00\x00\x00\x0d\x00\x00\x00\x1b\x00\x00\x00\x27\x00\x00\x00\x19\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x0d\x00\x00\x00\x19\x00\x00\x00\x1b\x00\x00\x00\x1a\x00\x00\x00\x19\x00\x00\x00\x20\x00\x00\x00\xff\xff\xff\xff\x1d\x00\x00\x00\x1f\x00\x00\x00\x20\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1a\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1f\x00\x00\x00\x20\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1a\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1f\x00\x00\x00\x20\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1a\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1f\x00\x00\x00\x20\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\x10\x00\x00\x00\x11\x00\x00\x00\x12\x00\x00\x00\x13\x00\x00\x00\x14\x00\x00\x00\x15\x00\x00\x00\x16\x00\x00\x00\x0e\x00\x00\x00\x0f\x00\x00\x00\xff\xff\xff\xff\x1a\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x1f\x00\x00\x00\x20\x00\x00\x00\x18\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x31\x00\x00\x00\x32\x00\x00\x00\xff\xff\xff\xff\x34\x00\x00\x00\xff\xff\xff\xff\x20\x00\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x00\x00\x05\x00\x00\x00\x0e\x00\x00\x00\x02\x00\x00\x00\xb3\xff\xff\xff\xb3\xff\xff\xff\xa9\x00\x00\x00\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\x0d\x00\x00\x00\x05\x00\x00\x00\x86\x00\x00\x00\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\x11\x00\x00\x00\x4a\x00\x00\x00\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\x05\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x6e\x00\x00\x00\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\xb3\xff\xff\xff\x0d\x00\x00\x00\xb3\xff\xff\xff\xb3\xff\xff\xff\x35\x00\x00\x00\x4e\x00\x00\x00\x7c\x00\x00\x00\x36\x00\x00\x00\x08\x00\x00\x00\x37\x00\x00\x00\x38\x00\x00\x00\x39\x00\x00\x00\x08\x00\x00\x00\x6e\x00\x00\x00\x7d\x00\x00\x00\x09\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x1c\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x7e\x00\x00\x00\x16\x00\x00\x00\x6f\x00\x00\x00\x8c\x00\x00\x00\x4e\x00\x00\x00\x47\x00\x00\x00\x3b\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x3e\x00\x00\x00\x03\x00\x00\x00\x7d\x00\x00\x00\x42\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x0a\x00\x00\x00\x1d\x00\x00\x00\x1e\x00\x00\x00\x1f\x00\x00\x00\x43\x00\x00\x00\x92\x00\x00\x00\x7b\x00\x00\x00\x93\x00\x00\x00\x3a\x00\x00\x00\x1a\x00\x00\x00\x3b\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x3e\x00\x00\x00\x17\x00\x00\x00\x0b\x00\x00\x00\x20\x00\x00\x00\x18\x00\x00\x00\x17\x00\x00\x00\x98\x00\x00\x00\x73\x00\x00\x00\x1f\x00\x00\x00\x99\x00\x00\x00\x44\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x45\x00\x00\x00\x08\x00\x00\x00\x57\x00\x00\x00\x58\x00\x00\x00\x59\x00\x00\x00\x16\x00\x00\x00\x4b\x00\x00\x00\x5a\x00\x00\x00\x5b\x00\x00\x00\x5c\x00\x00\x00\x5d\x00\x00\x00\x5e\x00\x00\x00\x5f\x00\x00\x00\x60\x00\x00\x00\x61\x00\x00\x00\x62\x00\x00\x00\x63\x00\x00\x00\x64\x00\x00\x00\x65\x00\x00\x00\x66\x00\x00\x00\x67\x00\x00\x00\x68\x00\x00\x00\x69\x00\x00\x00\x6a\x00\x00\x00\x6b\x00\x00\x00\x6c\x00\x00\x00\x6d\x00\x00\x00\xa0\x00\x00\x00\x4a\x00\x00\x00\x93\x00\x00\x00\x94\x00\x00\x00\xff\xff\xff\xff\x15\x00\x00\x00\x14\x00\x00\x00\x57\x00\x00\x00\x58\x00\x00\x00\x59\x00\x00\x00\x8a\x00\x00\x00\x8b\x00\x00\x00\x5a\x00\x00\x00\x5b\x00\x00\x00\x5c\x00\x00\x00\x5d\x00\x00\x00\x5e\x00\x00\x00\x5f\x00\x00\x00\x60\x00\x00\x00\x61\x00\x00\x00\x62\x00\x00\x00\x63\x00\x00\x00\x64\x00\x00\x00\x65\x00\x00\x00\x66\x00\x00\x00\x67\x00\x00\x00\x68\x00\x00\x00\x69\x00\x00\x00\x6a\x00\x00\x00\x6b\x00\x00\x00\x6c\x00\x00\x00\x6d\x00\x00\x00\x46\x00\x00\x00\x28\x00\x00\x00\x0e\x00\x00\x00\x90\x00\x00\x00\x95\x00\x00\x00\x29\x00\x00\x00\x0f\x00\x00\x00\x43\x00\x00\x00\x4a\x00\x00\x00\x1a\x00\x00\x00\x1b\x00\x00\x00\x13\x00\x00\x00\x82\x00\x00\x00\x1e\x00\x00\x00\x96\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x27\x00\x00\x00\x08\x00\x00\x00\x26\x00\x00\x00\x33\x00\x00\x00\x1a\x00\x00\x00\x42\x00\x00\x00\x40\x00\x00\x00\x44\x00\x00\x00\x0a\x00\x00\x00\x57\x00\x00\x00\x58\x00\x00\x00\x59\x00\x00\x00\x41\x00\x00\x00\x1f\x00\x00\x00\x5a\x00\x00\x00\x5b\x00\x00\x00\x5c\x00\x00\x00\x5d\x00\x00\x00\x5e\x00\x00\x00\x5f\x00\x00\x00\x60\x00\x00\x00\x61\x00\x00\x00\x62\x00\x00\x00\x63\x00\x00\x00\x64\x00\x00\x00\x65\x00\x00\x00\x66\x00\x00\x00\x67\x00\x00\x00\x68\x00\x00\x00\x69\x00\x00\x00\x6a\x00\x00\x00\x6b\x00\x00\x00\x6c\x00\x00\x00\x6d\x00\x00\x00\x9a\x00\x00\x00\x1b\x00\x00\x00\xad\x00\x00\x00\x1b\x00\x00\x00\x9b\x00\x00\x00\x3f\x00\x00\x00\x9b\x00\x00\x00\xf4\xff\xff\xff\x4c\x00\x00\x00\x29\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x70\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x55\x00\x00\x00\x4a\x00\x00\x00\x4e\x00\x00\x00\x31\x00\x00\x00\x1f\x00\x00\x00\x4a\x00\x00\x00\x1f\x00\x00\x00\x33\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x50\x00\x00\x00\x0d\x00\x00\x00\x4a\x00\x00\x00\x71\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x70\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x1a\x00\x00\x00\x7a\x00\x00\x00\x78\x00\x00\x00\x31\x00\x00\x00\x77\x00\x00\x00\x76\x00\x00\x00\x75\x00\x00\x00\x4a\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x74\x00\x00\x00\x1a\x00\x00\x00\x82\x00\x00\x00\x86\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x2a\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x4a\x00\x00\x00\x7b\x00\x00\x00\x4e\x00\x00\x00\x31\x00\x00\x00\x3b\x00\x00\x00\x3c\x00\x00\x00\x3d\x00\x00\x00\x3e\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x52\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x50\x00\x00\x00\x8f\x00\x00\x00\x1a\x00\x00\x00\x31\x00\x00\x00\x8c\x00\x00\x00\x88\x00\x00\x00\xa6\x00\x00\x00\x9e\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x6d\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x50\x00\x00\x00\x4e\x00\x00\x00\xa7\x00\x00\x00\x31\x00\x00\x00\xa3\x00\x00\x00\xab\x00\x00\x00\xa2\x00\x00\x00\xb0\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x83\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\xa1\x00\x00\x00\xaf\x00\x00\x00\xac\x00\x00\x00\x31\x00\x00\x00\x06\x00\x00\x00\x11\x00\x00\x00\x55\x00\x00\x00\x53\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x7f\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x51\x00\x00\x00\x50\x00\x00\x00\x4a\x00\x00\x00\x31\x00\x00\x00\x48\x00\x00\x00\x4e\x00\x00\x00\x55\x00\x00\x00\x78\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x83\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x84\x00\x00\x00\x55\x00\x00\x00\x80\x00\x00\x00\x31\x00\x00\x00\x84\x00\x00\x00\x90\x00\x00\x00\x8d\x00\x00\x00\x55\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x9e\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x88\x00\x00\x00\x55\x00\x00\x00\x9c\x00\x00\x00\x31\x00\x00\x00\x55\x00\x00\x00\xa7\x00\x00\x00\x00\x00\x00\x00\xa9\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\x99\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\xa4\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\xb0\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\x28\x00\x00\x00\x29\x00\x00\x00\xac\x00\x00\x00\x2b\x00\x00\x00\x2c\x00\x00\x00\x2d\x00\x00\x00\x2e\x00\x00\x00\x2f\x00\x00\x00\x30\x00\x00\x00\x95\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x31\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x32\x00\x00\x00\x33\x00\x00\x00\xa3\x00\x00\x00\x22\x00\x00\x00\x23\x00\x00\x00\x24\x00\x00\x00\x25\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 107) [
        (1 , happyReduce_1),
        (2 , happyReduce_2),
        (3 , happyReduce_3),
        (4 , happyReduce_4),
        (5 , happyReduce_5),
        (6 , happyReduce_6),
        (7 , happyReduce_7),
        (8 , happyReduce_8),
        (9 , happyReduce_9),
        (10 , happyReduce_10),
        (11 , happyReduce_11),
        (12 , happyReduce_12),
        (13 , happyReduce_13),
        (14 , happyReduce_14),
        (15 , happyReduce_15),
        (16 , happyReduce_16),
        (17 , happyReduce_17),
        (18 , happyReduce_18),
        (19 , happyReduce_19),
        (20 , happyReduce_20),
        (21 , happyReduce_21),
        (22 , happyReduce_22),
        (23 , happyReduce_23),
        (24 , happyReduce_24),
        (25 , happyReduce_25),
        (26 , happyReduce_26),
        (27 , happyReduce_27),
        (28 , happyReduce_28),
        (29 , happyReduce_29),
        (30 , happyReduce_30),
        (31 , happyReduce_31),
        (32 , happyReduce_32),
        (33 , happyReduce_33),
        (34 , happyReduce_34),
        (35 , happyReduce_35),
        (36 , happyReduce_36),
        (37 , happyReduce_37),
        (38 , happyReduce_38),
        (39 , happyReduce_39),
        (40 , happyReduce_40),
        (41 , happyReduce_41),
        (42 , happyReduce_42),
        (43 , happyReduce_43),
        (44 , happyReduce_44),
        (45 , happyReduce_45),
        (46 , happyReduce_46),
        (47 , happyReduce_47),
        (48 , happyReduce_48),
        (49 , happyReduce_49),
        (50 , happyReduce_50),
        (51 , happyReduce_51),
        (52 , happyReduce_52),
        (53 , happyReduce_53),
        (54 , happyReduce_54),
        (55 , happyReduce_55),
        (56 , happyReduce_56),
        (57 , happyReduce_57),
        (58 , happyReduce_58),
        (59 , happyReduce_59),
        (60 , happyReduce_60),
        (61 , happyReduce_61),
        (62 , happyReduce_62),
        (63 , happyReduce_63),
        (64 , happyReduce_64),
        (65 , happyReduce_65),
        (66 , happyReduce_66),
        (67 , happyReduce_67),
        (68 , happyReduce_68),
        (69 , happyReduce_69),
        (70 , happyReduce_70),
        (71 , happyReduce_71),
        (72 , happyReduce_72),
        (73 , happyReduce_73),
        (74 , happyReduce_74),
        (75 , happyReduce_75),
        (76 , happyReduce_76),
        (77 , happyReduce_77),
        (78 , happyReduce_78),
        (79 , happyReduce_79),
        (80 , happyReduce_80),
        (81 , happyReduce_81),
        (82 , happyReduce_82),
        (83 , happyReduce_83),
        (84 , happyReduce_84),
        (85 , happyReduce_85),
        (86 , happyReduce_86),
        (87 , happyReduce_87),
        (88 , happyReduce_88),
        (89 , happyReduce_89),
        (90 , happyReduce_90),
        (91 , happyReduce_91),
        (92 , happyReduce_92),
        (93 , happyReduce_93),
        (94 , happyReduce_94),
        (95 , happyReduce_95),
        (96 , happyReduce_96),
        (97 , happyReduce_97),
        (98 , happyReduce_98),
        (99 , happyReduce_99),
        (100 , happyReduce_100),
        (101 , happyReduce_101),
        (102 , happyReduce_102),
        (103 , happyReduce_103),
        (104 , happyReduce_104),
        (105 , happyReduce_105),
        (106 , happyReduce_106),
        (107 , happyReduce_107)
        ]

happyRuleArr :: HappyAddr
happyRuleArr = HappyA# "\x00\x00\x00\x00\x04\x00\x00\x00\x01\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x04\x00\x00\x00\x03\x00\x00\x00\x02\x00\x00\x00\x04\x00\x00\x00\x01\x00\x00\x00\x04\x00\x00\x00\x01\x00\x00\x00\x05\x00\x00\x00\x02\x00\x00\x00\x06\x00\x00\x00\x0a\x00\x00\x00\x07\x00\x00\x00\x03\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x08\x00\x00\x00\x01\x00\x00\x00\x09\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0a\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x01\x00\x00\x00\x0b\x00\x00\x00\x01\x00\x00\x00\x0c\x00\x00\x00\x04\x00\x00\x00\x0c\x00\x00\x00\x05\x00\x00\x00\x0c\x00\x00\x00\x04\x00\x00\x00\x0d\x00\x00\x00\x05\x00\x00\x00\x0d\x00\x00\x00\x07\x00\x00\x00\x0e\x00\x00\x00\x01\x00\x00\x00\x0e\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x0f\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x03\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x03\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x10\x00\x00\x00\x01\x00\x00\x00\x11\x00\x00\x00\x04\x00\x00\x00\x12\x00\x00\x00\x06\x00\x00\x00\x13\x00\x00\x00\x08\x00\x00\x00\x14\x00\x00\x00\x05\x00\x00\x00\x15\x00\x00\x00\x03\x00\x00\x00\x16\x00\x00\x00\x04\x00\x00\x00\x17\x00\x00\x00\x04\x00\x00\x00\x18\x00\x00\x00\x03\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x19\x00\x00\x00\x01\x00\x00\x00\x1a\x00\x00\x00\x01\x00\x00\x00\x1b\x00\x00\x00\x01\x00\x00\x00\x1c\x00\x00\x00\x01\x00\x00\x00\x1d\x00\x00\x00\x01\x00\x00\x00\x1e\x00\x00\x00\x01\x00\x00\x00\x1f\x00\x00\x00\x01\x00\x00\x00\x20\x00\x00\x00\x01\x00\x00\x00\x21\x00\x00\x00\x00\x00\x00\x00\x21\x00\x00\x00\x01\x00\x00\x00\x21\x00\x00\x00\x02\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x22\x00\x00\x00\x01\x00\x00\x00\x22\x00\x00\x00\x03\x00\x00\x00\x23\x00\x00\x00\x00\x00\x00\x00\x23\x00\x00\x00\x01\x00\x00\x00\x23\x00\x00\x00\x02\x00\x00\x00\x24\x00\x00\x00\x01\x00\x00\x00\x24\x00\x00\x00\x02\x00\x00\x00\x25\x00\x00\x00\x00\x00\x00\x00\x25\x00\x00\x00\x01\x00\x00\x00\x25\x00\x00\x00\x02\x00\x00\x00\x26\x00\x00\x00\x01\x00\x00\x00\x26\x00\x00\x00\x02\x00\x00\x00\x27\x00\x00\x00\x00\x00\x00\x00\x27\x00\x00\x00\x03\x00\x00\x00\x27\x00\x00\x00\x04\x00\x00\x00\x28\x00\x00\x00\x00\x00\x00\x00\x28\x00\x00\x00\x02\x00\x00\x00\x28\x00\x00\x00\x03\x00\x00\x00\x29\x00\x00\x00\x00\x00\x00\x00\x29\x00\x00\x00\x02\x00\x00\x00\x29\x00\x00\x00\x03\x00\x00\x00\x2a\x00\x00\x00\x00\x00\x00\x00\x2a\x00\x00\x00\x01\x00\x00\x00\x2a\x00\x00\x00\x02\x00\x00\x00\x2b\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x00\x00\x01\x00\x00\x00\x2b\x00\x00\x00\x03\x00\x00\x00"#

happyCatchStates :: [Happy_Prelude.Int]
happyCatchStates = []

happy_n_terms = 61 :: Happy_Prelude.Int
happy_n_nonterms = 44 :: Happy_Prelude.Int

happy_n_starts = 1 :: Happy_Prelude.Int

happyReduce_1 = happyReduce 4# 0# happyReduction_1
happyReduction_1 (_ `HappyStk`
        (HappyAbsSyn6  happy_var_3) `HappyStk`
        (HappyAbsSyn46  happy_var_2) `HappyStk`
        (HappyAbsSyn45  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn5
                 (Program (DataTypeDecls happy_var_1) (FuncDecls happy_var_2) happy_var_3
        ) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  1# happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_3)
        _
        _
         =  HappyAbsSyn6
                 (happy_var_3
        )
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 4# 2# happyReduction_3
happyReduction_3 ((HappyAbsSyn39  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn35  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn7
                 (DataTypeDecl happy_var_2 (DataFields happy_var_4)
        ) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_2  3# happyReduction_4
happyReduction_4 (HappyAbsSyn40  happy_var_2)
        (HappyAbsSyn36  happy_var_1)
         =  HappyAbsSyn8
                 (DataField happy_var_1 (CombinedTypeCons happy_var_2)
        )
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 (HappyAbsSyn35  happy_var_1)
         =  HappyAbsSyn9
                 (CTCTypeCon happy_var_1
        )
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  4# happyReduction_6
happyReduction_6 (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn9
                 (CTCBase happy_var_1
        )
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5# happyReduction_7
happyReduction_7 (HappyAbsSyn11  happy_var_2)
        (HappyAbsSyn31  happy_var_1)
         =  HappyAbsSyn10
                 (happy_var_2 happy_var_1
        )
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 10# 6# happyReduction_8
happyReduction_8 ((HappyAbsSyn6  happy_var_10) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn38  happy_var_8) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn47  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn31  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn14  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn11
                 (\v -> FuncDecl v happy_var_2 happy_var_4 (LocRegions happy_var_6) (Vars happy_var_8) happy_var_10
        ) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  7# happyReduction_9
happyReduction_9 (HappyAbsSyn18  happy_var_3)
        _
        (HappyAbsSyn13  happy_var_1)
         =  HappyAbsSyn12
                 (LocatedType happy_var_1 happy_var_3
        )
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  8# happyReduction_10
happyReduction_10 (HappyAbsSyn35  happy_var_1)
         =  HappyAbsSyn13
                 (CLTTypeCon happy_var_1
        )
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  8# happyReduction_11
happyReduction_11 (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn13
                 (CLTBase happy_var_1
        )
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  9# happyReduction_12
happyReduction_12 (HappyAbsSyn48  happy_var_1)
         =  HappyAbsSyn14
                 (TypeScheme (CombinedTypes happy_var_1)
        )
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10# happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_1)
         =  HappyAbsSyn15
                 (CTLocated happy_var_1
        )
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  10# happyReduction_14
happyReduction_14 (HappyAbsSyn16  happy_var_1)
         =  HappyAbsSyn15
                 (CTBase happy_var_1
        )
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  11# happyReduction_15
happyReduction_15 _
         =  HappyAbsSyn16
                 (Int
        )

happyReduce_16 = happySpecReduce_1  11# happyReduction_16
happyReduction_16 _
         =  HappyAbsSyn16
                 (Float
        )

happyReduce_17 = happySpecReduce_1  11# happyReduction_17
happyReduction_17 _
         =  HappyAbsSyn16
                 (Bool
        )

happyReduce_18 = happySpecReduce_1  11# happyReduction_18
happyReduction_18 _
         =  HappyAbsSyn16
                 (String
        )

happyReduce_19 = happyReduce 4# 12# happyReduction_19
happyReduction_19 (_ `HappyStk`
        (HappyAbsSyn32  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn17
                 (LocExpressStart happy_var_3
        ) `HappyStk` happyRest

happyReduce_20 = happyReduce 5# 12# happyReduction_20
happyReduction_20 (_ `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn17
                 (LocExpressNext happy_var_2
        ) `HappyStk` happyRest

happyReduce_21 = happyReduce 4# 12# happyReduction_21
happyReduction_21 (_ `HappyStk`
        (HappyAbsSyn12  happy_var_3) `HappyStk`
        _ `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn17
                 (LocExpressAfter happy_var_3
        ) `HappyStk` happyRest

happyReduce_22 = happyReduce 5# 13# happyReduction_22
happyReduction_22 (_ `HappyStk`
        (HappyAbsSyn32  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn33  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn18
                 (LocRegion happy_var_2 happy_var_4 (IndexVar "")
        ) `HappyStk` happyRest

happyReduce_23 = happyReduce 7# 13# happyReduction_23
happyReduction_23 (_ `HappyStk`
        (HappyAbsSyn34  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn32  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn33  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn18
                 (LocRegion happy_var_2 happy_var_4 happy_var_6
        ) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_1  14# happyReduction_24
happyReduction_24 (HappyAbsSyn37  happy_var_1)
         =  HappyAbsSyn19
                 (ValVar happy_var_1
        )
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  14# happyReduction_25
happyReduction_25 (HappyAbsSyn20  happy_var_1)
         =  HappyAbsSyn19
                 (ValLit happy_var_1
        )
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  15# happyReduction_26
happyReduction_26 (HappyTerminal (TokenIntLit _ happy_var_1))
         =  HappyAbsSyn20
                 (IntLit happy_var_1
        )
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  15# happyReduction_27
happyReduction_27 (HappyTerminal (TokenFloatLit _ happy_var_1))
         =  HappyAbsSyn20
                 (FloatLit happy_var_1
        )
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  15# happyReduction_28
happyReduction_28 (HappyTerminal (TokenBoolLit _ happy_var_1))
         =  HappyAbsSyn20
                 (BoolLit happy_var_1
        )
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  15# happyReduction_29
happyReduction_29 (HappyTerminal (TokenStringLit _ happy_var_1))
         =  HappyAbsSyn20
                 (StringLit happy_var_1
        )
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  16# happyReduction_30
happyReduction_30 (HappyAbsSyn6  happy_var_3)
        (HappyAbsSyn30  happy_var_2)
        (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn6
                 (ExprBinOp happy_var_2 happy_var_1 happy_var_3
        )
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  16# happyReduction_31
happyReduction_31 (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn6
                 (ExprVal happy_var_1
        )
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  16# happyReduction_32
happyReduction_32 _
        (HappyAbsSyn6  happy_var_2)
        _
         =  HappyAbsSyn6
                 (happy_var_2
        )
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  16# happyReduction_33
happyReduction_33 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn6
                 (happy_var_1
        )
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  16# happyReduction_34
happyReduction_34 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn6
                 (happy_var_1
        )
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  16# happyReduction_35
happyReduction_35 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn6
                 (happy_var_1
        )
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  16# happyReduction_36
happyReduction_36 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn6
                 (happy_var_1
        )
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  16# happyReduction_37
happyReduction_37 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn6
                 (happy_var_1
        )
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  16# happyReduction_38
happyReduction_38 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn6
                 (happy_var_1
        )
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happyReduce 4# 17# happyReduction_39
happyReduction_39 ((HappyAbsSyn6  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn32  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn6
                 (ExprLetRegion happy_var_2 happy_var_4
        ) `HappyStk` happyRest

happyReduce_40 = happyReduce 6# 18# happyReduction_40
happyReduction_40 ((HappyAbsSyn6  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn17  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn18  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn6
                 (ExprLetLoc happy_var_2 happy_var_4 happy_var_6
        ) `HappyStk` happyRest

happyReduce_41 = happyReduce 8# 19# happyReduction_41
happyReduction_41 ((HappyAbsSyn6  happy_var_8) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn6  happy_var_6) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn15  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn37  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn6
                 (ExprLet happy_var_2 happy_var_4 happy_var_6 happy_var_8
        ) `HappyStk` happyRest

happyReduce_42 = happyReduce 5# 20# happyReduction_42
happyReduction_42 ((HappyAbsSyn41  happy_var_5) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn47  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn31  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn6
                 (ExprFuncApp happy_var_1 (LocRegions happy_var_3) (Exprs happy_var_5)
        ) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_3  21# happyReduction_43
happyReduction_43 (HappyAbsSyn41  happy_var_3)
        (HappyAbsSyn18  happy_var_2)
        (HappyAbsSyn36  happy_var_1)
         =  HappyAbsSyn6
                 (ExprDataConApp happy_var_1 happy_var_2 (Exprs happy_var_3)
        )
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happyReduce 4# 22# happyReduction_44
happyReduction_44 ((HappyAbsSyn43  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn19  happy_var_2) `HappyStk`
        _ `HappyStk`
        happyRest)
         = HappyAbsSyn6
                 (ExprCase happy_var_2 (Pats happy_var_4)
        ) `HappyStk` happyRest

happyReduce_45 = happyReduce 4# 23# happyReduction_45
happyReduction_45 ((HappyAbsSyn6  happy_var_4) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn44  happy_var_2) `HappyStk`
        (HappyAbsSyn36  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn28
                 (Pat happy_var_1 (PatMatches happy_var_2) happy_var_4
        ) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_3  24# happyReduction_46
happyReduction_46 (HappyAbsSyn12  happy_var_3)
        _
        (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn29
                 (PatMatch happy_var_1 happy_var_3
        )
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  25# happyReduction_47
happyReduction_47 _
         =  HappyAbsSyn30
                 (Add
        )

happyReduce_48 = happySpecReduce_1  25# happyReduction_48
happyReduction_48 _
         =  HappyAbsSyn30
                 (Sub
        )

happyReduce_49 = happySpecReduce_1  25# happyReduction_49
happyReduction_49 _
         =  HappyAbsSyn30
                 (FAdd
        )

happyReduce_50 = happySpecReduce_1  25# happyReduction_50
happyReduction_50 _
         =  HappyAbsSyn30
                 (FSub
        )

happyReduce_51 = happySpecReduce_1  25# happyReduction_51
happyReduction_51 _
         =  HappyAbsSyn30
                 (Mul
        )

happyReduce_52 = happySpecReduce_1  25# happyReduction_52
happyReduction_52 _
         =  HappyAbsSyn30
                 (Div
        )

happyReduce_53 = happySpecReduce_1  25# happyReduction_53
happyReduction_53 _
         =  HappyAbsSyn30
                 (FMul
        )

happyReduce_54 = happySpecReduce_1  25# happyReduction_54
happyReduction_54 _
         =  HappyAbsSyn30
                 (FDiv
        )

happyReduce_55 = happySpecReduce_1  25# happyReduction_55
happyReduction_55 _
         =  HappyAbsSyn30
                 (Pow
        )

happyReduce_56 = happySpecReduce_1  25# happyReduction_56
happyReduction_56 _
         =  HappyAbsSyn30
                 (Eq
        )

happyReduce_57 = happySpecReduce_1  25# happyReduction_57
happyReduction_57 _
         =  HappyAbsSyn30
                 (FEq
        )

happyReduce_58 = happySpecReduce_1  25# happyReduction_58
happyReduction_58 _
         =  HappyAbsSyn30
                 (CEq
        )

happyReduce_59 = happySpecReduce_1  25# happyReduction_59
happyReduction_59 _
         =  HappyAbsSyn30
                 (Gt
        )

happyReduce_60 = happySpecReduce_1  25# happyReduction_60
happyReduction_60 _
         =  HappyAbsSyn30
                 (Lt
        )

happyReduce_61 = happySpecReduce_1  25# happyReduction_61
happyReduction_61 _
         =  HappyAbsSyn30
                 (FGt
        )

happyReduce_62 = happySpecReduce_1  25# happyReduction_62
happyReduction_62 _
         =  HappyAbsSyn30
                 (FLt
        )

happyReduce_63 = happySpecReduce_1  25# happyReduction_63
happyReduction_63 _
         =  HappyAbsSyn30
                 (Ge
        )

happyReduce_64 = happySpecReduce_1  25# happyReduction_64
happyReduction_64 _
         =  HappyAbsSyn30
                 (Le
        )

happyReduce_65 = happySpecReduce_1  25# happyReduction_65
happyReduction_65 _
         =  HappyAbsSyn30
                 (FGe
        )

happyReduce_66 = happySpecReduce_1  25# happyReduction_66
happyReduction_66 _
         =  HappyAbsSyn30
                 (FLe
        )

happyReduce_67 = happySpecReduce_1  25# happyReduction_67
happyReduction_67 _
         =  HappyAbsSyn30
                 (Neq
        )

happyReduce_68 = happySpecReduce_1  25# happyReduction_68
happyReduction_68 _
         =  HappyAbsSyn30
                 (And
        )

happyReduce_69 = happySpecReduce_1  25# happyReduction_69
happyReduction_69 _
         =  HappyAbsSyn30
                 (Or
        )

happyReduce_70 = happySpecReduce_1  26# happyReduction_70
happyReduction_70 (HappyTerminal (TokenIdentLower _ happy_var_1))
         =  HappyAbsSyn31
                 (FuncVar happy_var_1
        )
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  27# happyReduction_71
happyReduction_71 (HappyTerminal (TokenIdentLower _ happy_var_1))
         =  HappyAbsSyn32
                 (RegionVar happy_var_1
        )
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  28# happyReduction_72
happyReduction_72 (HappyTerminal (TokenIdentLower _ happy_var_1))
         =  HappyAbsSyn33
                 (LocVar happy_var_1
        )
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  29# happyReduction_73
happyReduction_73 (HappyTerminal (TokenIdentLower _ happy_var_1))
         =  HappyAbsSyn34
                 (IndexVar happy_var_1
        )
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  30# happyReduction_74
happyReduction_74 (HappyTerminal (TokenIdentUpper _ happy_var_1))
         =  HappyAbsSyn35
                 (TypeCon happy_var_1
        )
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  31# happyReduction_75
happyReduction_75 (HappyTerminal (TokenIdentUpper _ happy_var_1))
         =  HappyAbsSyn36
                 (DataCon happy_var_1
        )
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  32# happyReduction_76
happyReduction_76 (HappyTerminal (TokenIdentLower _ happy_var_1))
         =  HappyAbsSyn37
                 (Var happy_var_1
        )
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_0  33# happyReduction_77
happyReduction_77  =  HappyAbsSyn38
                 ([]
        )

happyReduce_78 = happySpecReduce_1  33# happyReduction_78
happyReduction_78 (HappyAbsSyn37  happy_var_1)
         =  HappyAbsSyn38
                 ([happy_var_1]
        )
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_2  33# happyReduction_79
happyReduction_79 (HappyAbsSyn37  happy_var_2)
        (HappyAbsSyn38  happy_var_1)
         =  HappyAbsSyn38
                 (happy_var_2 : happy_var_1
        )
happyReduction_79 _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_0  34# happyReduction_80
happyReduction_80  =  HappyAbsSyn39
                 ([]
        )

happyReduce_81 = happySpecReduce_1  34# happyReduction_81
happyReduction_81 (HappyAbsSyn8  happy_var_1)
         =  HappyAbsSyn39
                 ([ happy_var_1 ]
        )
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_3  34# happyReduction_82
happyReduction_82 (HappyAbsSyn8  happy_var_3)
        _
        (HappyAbsSyn39  happy_var_1)
         =  HappyAbsSyn39
                 (happy_var_3 : happy_var_1
        )
happyReduction_82 _ _ _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_0  35# happyReduction_83
happyReduction_83  =  HappyAbsSyn40
                 ([]
        )

happyReduce_84 = happySpecReduce_1  35# happyReduction_84
happyReduction_84 (HappyAbsSyn9  happy_var_1)
         =  HappyAbsSyn40
                 ([happy_var_1]
        )
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_2  35# happyReduction_85
happyReduction_85 (HappyAbsSyn9  happy_var_2)
        (HappyAbsSyn40  happy_var_1)
         =  HappyAbsSyn40
                 (happy_var_2 : happy_var_1
        )
happyReduction_85 _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  36# happyReduction_86
happyReduction_86 (HappyAbsSyn6  happy_var_1)
         =  HappyAbsSyn41
                 ([happy_var_1]
        )
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_2  36# happyReduction_87
happyReduction_87 (HappyAbsSyn6  happy_var_2)
        (HappyAbsSyn41  happy_var_1)
         =  HappyAbsSyn41
                 (happy_var_2 : happy_var_1
        )
happyReduction_87 _ _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_0  37# happyReduction_88
happyReduction_88  =  HappyAbsSyn42
                 ([]
        )

happyReduce_89 = happySpecReduce_1  37# happyReduction_89
happyReduction_89 (HappyAbsSyn19  happy_var_1)
         =  HappyAbsSyn42
                 ([happy_var_1]
        )
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_2  37# happyReduction_90
happyReduction_90 (HappyAbsSyn19  happy_var_2)
        (HappyAbsSyn42  happy_var_1)
         =  HappyAbsSyn42
                 (happy_var_2 : happy_var_1
        )
happyReduction_90 _ _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_1  38# happyReduction_91
happyReduction_91 (HappyAbsSyn28  happy_var_1)
         =  HappyAbsSyn43
                 ([happy_var_1]
        )
happyReduction_91 _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_2  38# happyReduction_92
happyReduction_92 (HappyAbsSyn28  happy_var_2)
        (HappyAbsSyn43  happy_var_1)
         =  HappyAbsSyn43
                 (happy_var_2 : happy_var_1
        )
happyReduction_92 _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_0  39# happyReduction_93
happyReduction_93  =  HappyAbsSyn44
                 ([]
        )

happyReduce_94 = happySpecReduce_3  39# happyReduction_94
happyReduction_94 _
        (HappyAbsSyn29  happy_var_2)
        _
         =  HappyAbsSyn44
                 ([happy_var_2]
        )
happyReduction_94 _ _ _  = notHappyAtAll 

happyReduce_95 = happyReduce 4# 39# happyReduction_95
happyReduction_95 (_ `HappyStk`
        (HappyAbsSyn29  happy_var_3) `HappyStk`
        _ `HappyStk`
        (HappyAbsSyn44  happy_var_1) `HappyStk`
        happyRest)
         = HappyAbsSyn44
                 (happy_var_3 : happy_var_1
        ) `HappyStk` happyRest

happyReduce_96 = happySpecReduce_0  40# happyReduction_96
happyReduction_96  =  HappyAbsSyn45
                 ([]
        )

happyReduce_97 = happySpecReduce_2  40# happyReduction_97
happyReduction_97 _
        (HappyAbsSyn7  happy_var_1)
         =  HappyAbsSyn45
                 ([happy_var_1]
        )
happyReduction_97 _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_3  40# happyReduction_98
happyReduction_98 _
        (HappyAbsSyn7  happy_var_2)
        (HappyAbsSyn45  happy_var_1)
         =  HappyAbsSyn45
                 (happy_var_2 : happy_var_1
        )
happyReduction_98 _ _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_0  41# happyReduction_99
happyReduction_99  =  HappyAbsSyn46
                 ([]
        )

happyReduce_100 = happySpecReduce_2  41# happyReduction_100
happyReduction_100 _
        (HappyAbsSyn10  happy_var_1)
         =  HappyAbsSyn46
                 ([happy_var_1]
        )
happyReduction_100 _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_3  41# happyReduction_101
happyReduction_101 _
        (HappyAbsSyn10  happy_var_2)
        (HappyAbsSyn46  happy_var_1)
         =  HappyAbsSyn46
                 (happy_var_2 : happy_var_1
        )
happyReduction_101 _ _ _  = notHappyAtAll 

happyReduce_102 = happySpecReduce_0  42# happyReduction_102
happyReduction_102  =  HappyAbsSyn47
                 ([]
        )

happyReduce_103 = happySpecReduce_1  42# happyReduction_103
happyReduction_103 (HappyAbsSyn18  happy_var_1)
         =  HappyAbsSyn47
                 ([happy_var_1]
        )
happyReduction_103 _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_2  42# happyReduction_104
happyReduction_104 (HappyAbsSyn18  happy_var_2)
        (HappyAbsSyn47  happy_var_1)
         =  HappyAbsSyn47
                 (happy_var_2 : happy_var_1
        )
happyReduction_104 _ _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_0  43# happyReduction_105
happyReduction_105  =  HappyAbsSyn48
                 ([]
        )

happyReduce_106 = happySpecReduce_1  43# happyReduction_106
happyReduction_106 (HappyAbsSyn15  happy_var_1)
         =  HappyAbsSyn48
                 ([happy_var_1]
        )
happyReduction_106 _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_3  43# happyReduction_107
happyReduction_107 (HappyAbsSyn48  happy_var_3)
        _
        (HappyAbsSyn15  happy_var_1)
         =  HappyAbsSyn48
                 (happy_var_1 : happy_var_3
        )
happyReduction_107 _ _ _  = notHappyAtAll 

happyTerminalToTok term = case term of {
        TokenData _ -> 2#;
        TokenAssign _ -> 3#;
        TokenColon _ -> 4#;
        TokenLBracket _ -> 5#;
        TokenRBracket _ -> 6#;
        TokenAt _ -> 7#;
        TokenArrow _ -> 8#;
        TokenBar _ -> 9#;
        TokenComma _ -> 10#;
        TokenLParen _ -> 11#;
        TokenRParen _ -> 12#;
        TokenComment _ -> 13#;
        TokenLet _ -> 14#;
        TokenIn _ -> 15#;
        TokenLetLoc _ -> 16#;
        TokenLetRegion _ -> 17#;
        TokenCase _ -> 18#;
        TokenOf _ -> 19#;
        TokenStart _ -> 20#;
        TokenAfter _ -> 21#;
        TokenPow _ -> 22#;
        TokenMul _ -> 23#;
        TokenDiv _ -> 24#;
        TokenDivInline _ -> 25#;
        TokenModInline _ -> 26#;
        TokenFMul _ -> 27#;
        TokenFDiv _ -> 28#;
        TokenAdd _ -> 29#;
        TokenSub _ -> 30#;
        TokenFAdd _ -> 31#;
        TokenFSub _ -> 32#;
        TokenEq _ -> 33#;
        TokenFEq _ -> 34#;
        TokenCEq _ -> 35#;
        TokenGt _ -> 36#;
        TokenLt _ -> 37#;
        TokenFGt _ -> 38#;
        TokenFLt _ -> 39#;
        TokenGe _ -> 40#;
        TokenLe _ -> 41#;
        TokenFGe _ -> 42#;
        TokenFLe _ -> 43#;
        TokenNeq _ -> 44#;
        TokenAnd _ -> 45#;
        TokenOr _ -> 46#;
        TokenIntType _ -> 47#;
        TokenFloatType _ -> 48#;
        TokenBoolType _ -> 49#;
        TokenStringType _ -> 50#;
        TokenIdentLower _ happy_dollar_dollar -> 51#;
        TokenIdentUpper _ happy_dollar_dollar -> 52#;
        TokenIntLit _ happy_dollar_dollar -> 53#;
        TokenFloatLit _ happy_dollar_dollar -> 54#;
        TokenBoolLit _ happy_dollar_dollar -> 55#;
        TokenStringLit _ happy_dollar_dollar -> 56#;
        TokenMain _ -> 57#;
        TokenNewLine _ -> 58#;
        TokenEOF _ -> 59#;
        _ -> -1#;
        }
{-# NOINLINE happyTerminalToTok #-}

happyLex kend  _kmore []       = kend notHappyAtAll []
happyLex _kend kmore  (tk:tks) = kmore (happyTerminalToTok tk) tk tks
{-# INLINE happyLex #-}

happyNewToken action sts stk = happyLex (\tk -> happyDoAction 60# notHappyAtAll action sts stk) (\i tk -> happyDoAction i tk action sts stk)

happyReport 60# tk explist resume tks = happyReport' tks explist resume
happyReport _ tk explist resume tks = happyReport' (tk:tks) explist (\tks -> resume (Happy_Prelude.tail tks))


happyThen :: () => (E a) -> (a -> (E b)) -> (E b)
happyThen = (thenE)
happyReturn :: () => a -> (E a)
happyReturn = (returnE)
happyThen1 m k tks = (thenE) m (\a -> k a tks)
happyFmap1 f m tks = happyThen (m tks) (\a -> happyReturn (f a))
happyReturn1 :: () => a -> b -> (E a)
happyReturn1 = \a tks -> (returnE) a
happyReport' :: () => [(Token)] -> [Happy_Prelude.String] -> ([(Token)] -> (E a)) -> (E a)
happyReport' = (\tokens expected resume -> (parseError) tokens)

happyAbort :: () => [(Token)] -> (E a)
happyAbort = Happy_Prelude.error "Called abort handler in non-resumptive parser"

l2ParserNative tks = happySomeParser where
 happySomeParser = happyThen (happyDoParse 0# tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> E a
parseError [] = failE "Parse error"
parseError (tok:_) = failE . makeRed $
        "Parse error at " ++ showPos (pos tok) ++
        "\nUnexpected token: " ++ show tok

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
    (Ok x) >>= f = f x
    (Failed e) >>= _ = Failed e

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
    case m of
        Ok a     -> k a
        Failed e -> Failed e

-- thenE :: E a -> (a -> E b) -> E b
-- m `thenE` k = \s ->
--    case m s of
--        Ok a     -> k a s
--        Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a
-- returnE :: a -> E a
-- returnE a = \s -> Ok a


failE :: String -> E a
failE err = Failed err
-- failE :: String -> E a
-- failE err = \s -> Failed err


catchE :: E a -> (String -> E a) -> E a
catchE m k =
    case m of
        Ok a     -> Ok a
        Failed e -> k e
-- catchE :: E a -> (String -> E a) -> E a
-- catchE m k = \s ->
--    case m s of
--       Ok a     -> Ok a
--       Failed e -> k e s

makeGreen :: String -> String
makeGreen s = "\x1b[32m" ++ s ++ "\x1b[0m"

makeRed :: String -> String
makeRed s = "\x1b[31m" ++ s ++ "\x1b[0m"

makeBold :: String -> String
makeBold s = "\x1b[1m" ++ s ++ "\x1b[0m"

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

main = do 
    args <- getArgs
    let config = setConfig args
    putStrLn $ show args
    case config of
        Failure e -> putStrLn $ "Error in command line arguments: " ++ e
        Success cfg -> printTest cfg
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $

#if !defined(__GLASGOW_HASKELL__)
#  error This code isn't being built with GHC.
#endif

-- Get WORDS_BIGENDIAN (if defined)
#include "MachDeps.h"

-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Happy_Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Happy_Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Happy_Prelude.Bool)
#define PLUS(n,m) (n Happy_GHC_Exts.+# m)
#define MINUS(n,m) (n Happy_GHC_Exts.-# m)
#define TIMES(n,m) (n Happy_GHC_Exts.*# m)
#define NEGATE(n) (Happy_GHC_Exts.negateInt# (n))

type Happy_Int = Happy_GHC_Exts.Int#
data Happy_IntList = HappyCons Happy_Int Happy_IntList

#define INVALID_TOK -1#
#define ERROR_TOK 0#
#define CATCH_TOK 1#

#if defined(HAPPY_COERCE)
#  define GET_ERROR_TOKEN(x)  (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (happyInTok (x))
#else
#  define GET_ERROR_TOKEN(x)  (case x of { HappyErrorToken (Happy_GHC_Exts.I# i) -> i })
#  define MK_ERROR_TOKEN(i)   (HappyErrorToken (Happy_GHC_Exts.I# i))
#  define MK_TOKEN(x)         (HappyTerminal (x))
#endif

#if defined(HAPPY_DEBUG)
#  define DEBUG_TRACE(s)    (happyTrace (s)) Happy_Prelude.$
happyTrace string expr = Happy_System_IO_Unsafe.unsafePerformIO Happy_Prelude.$ do
    Happy_System_IO.hPutStr Happy_System_IO.stderr string
    Happy_Prelude.return expr
#else
#  define DEBUG_TRACE(s)    {- nothing -}
#endif

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyDoParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept ERROR_TOK tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) =
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

happyDoAction i tk st =
  DEBUG_TRACE("state: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++
              ",\ttoken: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++
              ",\taction: ")
  case happyDecodeAction (happyNextAction i st) of
    HappyFail             -> DEBUG_TRACE("failing.\n")
                             happyFail i tk st
    HappyAccept           -> DEBUG_TRACE("accept.\n")
                             happyAccept i tk st
    HappyReduce rule      -> DEBUG_TRACE("reduce (rule " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# rule) Happy_Prelude.++ ")")
                             (happyReduceArr Happy_Data_Array.! (Happy_GHC_Exts.I# rule)) i tk st
    HappyShift  new_state -> DEBUG_TRACE("shift, enter state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
                             happyShift new_state i tk st

{-# INLINE happyNextAction #-}
happyNextAction i st = case happyIndexActionTable i st of
  Happy_Prelude.Just (Happy_GHC_Exts.I# act) -> act
  Happy_Prelude.Nothing                      -> happyIndexOffAddr happyDefActions st

{-# INLINE happyIndexActionTable #-}
happyIndexActionTable i st
  | GTE(i, 0#), GTE(off, 0#), EQ(happyIndexOffAddr happyCheck off, i)
  -- i >= 0:   Guard against INVALID_TOK (do the default action, which ultimately errors)
  -- off >= 0: Otherwise it's a default action
  -- equality check: Ensure that the entry in the compressed array is owned by st
  = Happy_Prelude.Just (Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off))
  | Happy_Prelude.otherwise
  = Happy_Prelude.Nothing
  where
    off = PLUS(happyIndexOffAddr happyActOffsets st, i)

data HappyAction
  = HappyFail
  | HappyAccept
  | HappyReduce Happy_Int -- rule number
  | HappyShift Happy_Int  -- new state
  deriving Happy_Prelude.Show

{-# INLINE happyDecodeAction #-}
happyDecodeAction :: Happy_Int -> HappyAction
happyDecodeAction  0#                        = HappyFail
happyDecodeAction -1#                        = HappyAccept
happyDecodeAction action | LT(action, 0#)    = HappyReduce NEGATE(PLUS(action, 1#))
                         | Happy_Prelude.otherwise = HappyShift MINUS(action, 1#)

{-# INLINE happyIndexGotoTable #-}
happyIndexGotoTable nt st = happyIndexOffAddr happyTable off
  where
    off = PLUS(happyIndexOffAddr happyGotoOffsets st, nt)

{-# INLINE happyIndexOffAddr #-}
happyIndexOffAddr :: HappyAddr -> Happy_Int -> Happy_Int
happyIndexOffAddr (HappyA# arr) off =
#if __GLASGOW_HASKELL__ >= 901
  Happy_GHC_Exts.int32ToInt# -- qualified import because it doesn't exist on older GHC's
#endif
#ifdef WORDS_BIGENDIAN
  -- The CI of `alex` tests this code path
  (Happy_GHC_Exts.word32ToInt32# (Happy_GHC_Exts.wordToWord32# (Happy_GHC_Exts.byteSwap32# (Happy_GHC_Exts.word32ToWord# (Happy_GHC_Exts.int32ToWord32#
#endif
  (Happy_GHC_Exts.indexInt32OffAddr# arr off)
#ifdef WORDS_BIGENDIAN
  )))))
#endif

happyIndexRuleArr :: Happy_Int -> (# Happy_Int, Happy_Int #)
happyIndexRuleArr r = (# nt, len #)
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts
    offs = TIMES(MINUS(r,n_starts),2#)
    nt = happyIndexOffAddr happyRuleArr offs
    len = happyIndexOffAddr happyRuleArr PLUS(offs,1#)

data HappyAddr = HappyA# Happy_GHC_Exts.Addr#

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state ERROR_TOK tk st sts stk@(x `HappyStk` _) =
     -- See "Error Fixup" below
     let i = GET_ERROR_TOKEN(x) in
     DEBUG_TRACE("shifting the error token")
     happyDoAction i tk new_state (HappyCons st sts) stk

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons st sts) (MK_TOKEN(tk) `HappyStk` stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 nt fn j tk st sts stk
     = happySeq fn (happyGoto nt j tk st (HappyCons st sts) (fn `HappyStk` stk))

happySpecReduce_1 nt fn j tk old_st sts@(HappyCons st _) (v1 `HappyStk` stk')
     = let r = fn v1 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_2 nt fn j tk old_st
  (HappyCons _ sts@(HappyCons st _))
  (v1 `HappyStk` v2 `HappyStk` stk')
     = let r = fn v1 v2 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happySpecReduce_3 nt fn j tk old_st
  (HappyCons _ (HappyCons _ sts@(HappyCons st _)))
  (v1 `HappyStk` v2 `HappyStk` v3 `HappyStk` stk')
     = let r = fn v1 v2 v3 in
       happyTcHack old_st (happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk')))

happyReduce k nt fn j tk st sts stk
     = case happyDrop MINUS(k,(1# :: Happy_Int)) sts of
         sts1@(HappyCons st1 _) ->
                let r = fn stk in -- it doesn't hurt to always seq here...
                st `happyTcHack` happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk in
          j `happyTcHack` happyThen1 (fn stk tk)
                                     (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons st sts) of
        sts1@(HappyCons st1 _) ->
          let drop_stk = happyDropStk k stk
              off = happyIndexOffAddr happyGotoOffsets st1
              off_i = PLUS(off, nt)
              new_state = happyIndexOffAddr happyTable off_i
          in
            j `happyTcHack` happyThen1 (fn stk tk)
                                       (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l               = l
happyDrop n  (HappyCons _ t) = happyDrop MINUS(n,(1# :: Happy_Int)) t

happyDropStk 0# l                 = l
happyDropStk n  (x `HappyStk` xs) = happyDropStk MINUS(n,(1#::Happy_Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

happyGoto nt j tk st =
   DEBUG_TRACE(", goto state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
   happyDoAction j tk new_state
  where new_state = happyIndexGotoTable nt st

{- Note [Error recovery]
~~~~~~~~~~~~~~~~~~~~~~~~
When there is no applicable action for the current lookahead token `tk`,
happy enters error recovery mode. Depending on whether the grammar file
declares the two action form `%error { abort } { report }` for
    Resumptive Error Handling,
it works in one (not resumptive) or two phases (resumptive):

 1. Fixup mode:
    Try to see if there is an action for the error token ERROR_TOK. If there
    is, do *not* emit an error and pretend instead that an `error` token was
    inserted.
    When there is no ERROR_TOK action, report an error.

    In non-resumptive error handling, calling the single error handler
    (e.g. `happyError`) will throw an exception and abort the parser.
    However, in resumptive error handling we enter *error resumption mode*.

 2. Error resumption mode:
    After reporting the error (with `report`), happy will attempt to find
    a good state stack to resume parsing in.
    For each candidate stack, it discards input until one of the candidates
    resumes (i.e. shifts the current input).
    If no candidate resumes before the end of input, resumption failed and
    calls the `abort` function, to much the same effect as in non-resumptive
    error handling.

    Candidate stacks are declared by the grammar author using the special
    `catch` terminal and called "catch frames".
    This mechanism is described in detail in Note [happyResume].

The `catch` resumption mechanism (2) is what usually is associated with
`error` in `bison` or `menhir`. Since `error` is used for the Fixup mechanism
(1) above, we call the corresponding token `catch`.
Furthermore, in constrast to `bison`, our implementation of `catch`
non-deterministically considers multiple catch frames on the stack for
resumption (See Note [Multiple catch frames]).

Note [happyResume]
~~~~~~~~~~~~~~~~~~
`happyResume` implements the resumption mechanism from Note [Error recovery].
It is best understood by example. Consider

Exp :: { String }
Exp : '1'                { "1" }
    | catch              { "catch" }
    | Exp '+' Exp %shift { $1 Happy_Prelude.++ " + " Happy_Prelude.++ $3 } -- %shift: associate 1 + 1 + 1 to the right
    | '(' Exp ')'        { "(" Happy_Prelude.++ $2 Happy_Prelude.++ ")" }

The idea of the use of `catch` here is that upon encountering a parse error
during expression parsing, we can gracefully degrade using the `catch` rule,
still producing a partial syntax tree and keep on parsing to find further
syntax errors.

Let's trace the parser state for input 11+1, which will error out after shifting 1.
After shifting, we have the following item stack (growing downwards and omitting
transitive closure items):

  State 0: %start_parseExp -> . Exp
  State 5: Exp -> '1' .

(Stack as a list of state numbers: [5,0].)
As Note [Error recovery] describes, we will first try Fixup mode.
That fails because no production can shift the `error` token.
Next we try Error resumption mode. This works as follows:

  1. Pop off the item stack until we find an item that can shift the `catch`
     token. (Implemented in `pop_items`.)
       * State 5 cannot shift catch. Pop.
       * State 0 can shift catch, which would transition into
          State 4: Exp -> catch .
     So record the *stack* `[4,0]` after doing the shift transition.
     We call this a *catch frame*, where the top is a *catch state*,
     corresponding to an item in which we just shifted a `catch` token.
     There can be multiple such catch stacks, see Note [Multiple catch frames].

  2. Discard tokens from the input until the lookahead can be shifted in one
     of the catch stacks. (Implemented in `discard_input_until_exp` and
     `some_catch_state_shifts`.)
       * We cannot shift the current lookahead '1' in state 4, so we discard
       * We *can* shift the next lookahead '+' in state 4, but only after
         reducing, which pops State 4 and goes to State 3:
           State 3: %start_parseExp -> Exp .
                    Exp -> Exp . '+' Exp
         Here we can shift '+'.
     As you can see, to implement this machinery we need to simulate
     the operation of the LALR automaton, especially reduction
     (`happySimulateReduce`).

Note [Multiple catch frames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For fewer spurious error messages, it can be beneficial to trace multiple catch
items. Consider

Exp : '1'
    | catch
    | Exp '+' Exp %shift
    | '(' Exp ')'

Let's trace the parser state for input (;+1, which will error out after shifting (.
After shifting, we have the following item stack (growing downwards):

  State 0: %start_parseExp -> . Exp
  State 6: Exp -> '(' . Exp ')'

Upon error, we want to find items in the stack which can shift a catch token.
Note that both State 0 and State 6 can shift a catch token, transitioning into
  State 4: Exp -> catch .
Hence we record the catch frames `[4,6,0]` and `[4,0]` for possible resumption.

Which catch frame do we pick for resumption?
Note that resuming catch frame `[4,0]` will parse as "catch+1", whereas
resuming the innermost frame `[4,6,0]` corresponds to parsing "(catch+1".
The latter would keep discarding input until the closing ')' is found.
So we will discard + and 1, leading to a spurious syntax error at the end of
input, aborting the parse and never producing a partial syntax tree. Bad!

It is far preferable to resume with catch frame `[4,0]`, where we can resume
successfully on input +, so that is what we do.

In general, we pick the catch frame for resumption that discards the least
amount of input for a successful shift, preferring the topmost such catch frame.
-}

-- happyFail :: Happy_Int -> Token -> Happy_Int -> _
-- This function triggers Note [Error recovery].
-- If the current token is ERROR_TOK, phase (1) has failed and we might try
-- phase (2).
happyFail ERROR_TOK = happyFixupFailed
happyFail i         = happyTryFixup i

-- Enter Error Fixup (see Note [Error recovery]):
-- generate an error token, save the old token and carry on.
-- When a `happyShift` accepts the error token, we will pop off the error token
-- to resume parsing with the current lookahead `i`.
happyTryFixup i tk action sts stk =
  DEBUG_TRACE("entering `error` fixup.\n")
  happyDoAction ERROR_TOK tk action sts (MK_ERROR_TOKEN(i) `HappyStk` stk)
  -- NB: `happyShift` will simply pop the error token and carry on with
  --     `tk`. Hence we don't change `tk` in the call here

-- See Note [Error recovery], phase (2).
-- Enter resumption mode after reporting the error by calling `happyResume`.
happyFixupFailed tk st sts (x `HappyStk` stk) =
  let i = GET_ERROR_TOKEN(x) in
  DEBUG_TRACE("`error` fixup failed.\n")
  let resume   = happyResume i tk st sts stk
      expected = happyExpectedTokens st sts in
  happyReport i tk expected resume

-- happyResume :: Happy_Int -> Token -> Happy_Int -> _
-- See Note [happyResume]
happyResume i tk st sts stk = pop_items [] st sts stk
  where
    !(Happy_GHC_Exts.I# n_starts) = happy_n_starts   -- this is to test whether we have a start token
    !(Happy_GHC_Exts.I# eof_i) = happy_n_terms Happy_Prelude.- 1   -- this is the token number of the EOF token
    happy_list_to_list :: Happy_IntList -> [Happy_Prelude.Int]
    happy_list_to_list (HappyCons st sts)
      | LT(st, n_starts)
      = [(Happy_GHC_Exts.I# st)]
      | Happy_Prelude.otherwise
      = (Happy_GHC_Exts.I# st) : happy_list_to_list sts

    -- See (1) of Note [happyResume]
    pop_items catch_frames st sts stk
      | LT(st, n_starts)
      = DEBUG_TRACE("reached start state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", ")
        if Happy_Prelude.null catch_frames_new
          then DEBUG_TRACE("no resumption.\n")
               happyAbort
          else DEBUG_TRACE("now discard input, trying to anchor in states " Happy_Prelude.++ Happy_Prelude.show (Happy_Prelude.map (happy_list_to_list . Happy_Prelude.fst) (Happy_Prelude.reverse catch_frames_new)) Happy_Prelude.++ ".\n")
               discard_input_until_exp i tk (Happy_Prelude.reverse catch_frames_new)
      | (HappyCons st1 sts1) <- sts, _ `HappyStk` stk1 <- stk
      = pop_items catch_frames_new st1 sts1 stk1
      where
        !catch_frames_new
          | HappyShift new_state <- happyDecodeAction (happyNextAction CATCH_TOK st)
          , DEBUG_TRACE("can shift catch token in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", into state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# new_state) Happy_Prelude.++ "\n")
            Happy_Prelude.null (Happy_Prelude.filter (\(HappyCons _ (HappyCons h _),_) -> EQ(st,h)) catch_frames)
          = (HappyCons new_state (HappyCons st sts), MK_ERROR_TOKEN(i) `HappyStk` stk):catch_frames -- MK_ERROR_TOKEN(i) is just some dummy that should not be accessed by user code
          | Happy_Prelude.otherwise
          = DEBUG_TRACE("already shifted or can't shift catch in " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ "\n")
            catch_frames

    -- See (2) of Note [happyResume]
    discard_input_until_exp i tk catch_frames
      | Happy_Prelude.Just (HappyCons st (HappyCons catch_st sts), catch_frame) <- some_catch_state_shifts i catch_frames
      = DEBUG_TRACE("found expected token in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ " after shifting from " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# catch_st) Happy_Prelude.++ ": " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ "\n")
        happyDoAction i tk st (HappyCons catch_st sts) catch_frame
      | EQ(i,eof_i) -- is i EOF?
      = DEBUG_TRACE("reached EOF, cannot resume. abort parse :(\n")
        happyAbort
      | Happy_Prelude.otherwise
      = DEBUG_TRACE("discard token " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ "\n")
        happyLex (\eof_tk -> discard_input_until_exp eof_i eof_tk catch_frames) -- eof
                 (\i tk   -> discard_input_until_exp i tk catch_frames)         -- not eof

    some_catch_state_shifts _ [] = DEBUG_TRACE("no catch state could shift.\n") Happy_Prelude.Nothing
    some_catch_state_shifts i catch_frames@(((HappyCons st sts),_):_) = try_head i st sts catch_frames
      where
        try_head i st sts catch_frames = -- PRECONDITION: head catch_frames = (HappyCons st sts)
          DEBUG_TRACE("trying token " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ " in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ": ")
          case happyDecodeAction (happyNextAction i st) of
            HappyFail     -> DEBUG_TRACE("fail.\n")   some_catch_state_shifts i (Happy_Prelude.tail catch_frames)
            HappyAccept   -> DEBUG_TRACE("accept.\n") Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyShift _  -> DEBUG_TRACE("shift.\n")  Happy_Prelude.Just (Happy_Prelude.head catch_frames)
            HappyReduce r -> case happySimulateReduce r st sts of
              (HappyCons st1 sts1) -> try_head i st1 sts1 catch_frames

happySimulateReduce r st sts =
  DEBUG_TRACE("simulate reduction of rule " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# r) Happy_Prelude.++ ", ")
  let (# nt, len #) = happyIndexRuleArr r in
  DEBUG_TRACE("nt " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# nt) Happy_Prelude.++ ", len: " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# len) Happy_Prelude.++ ", new_st ")
  let !(sts1@(HappyCons st1 _)) = happyDrop len (HappyCons st sts)
      new_st = happyIndexGotoTable nt st1 in
  DEBUG_TRACE(Happy_Prelude.show (Happy_GHC_Exts.I# new_st) Happy_Prelude.++ ".\n")
  (HappyCons new_st sts1)

happyTokenToString :: Happy_Prelude.Int -> Happy_Prelude.String
happyTokenToString i = happyTokenStrings Happy_Prelude.!! (i Happy_Prelude.- 2) -- 2: errorTok, catchTok

happyExpectedTokens :: Happy_Int -> Happy_IntList -> [Happy_Prelude.String]
-- Upon a parse error, we want to suggest tokens that are expected in that
-- situation. This function computes such tokens.
-- It works by examining the top of the state stack.
-- For every token number that does a shift transition, record that token number.
-- For every token number that does a reduce transition, simulate that reduction
-- on the state state stack and repeat.
-- The recorded token numbers are then formatted with 'happyTokenToString' and
-- returned.
happyExpectedTokens st sts =
  DEBUG_TRACE("constructing expected tokens.\n")
  Happy_Prelude.map happyTokenToString (search_shifts st sts [])
  where
    search_shifts st sts shifts = Happy_Prelude.foldr (add_action st sts) shifts (distinct_actions st)
    add_action st sts (Happy_GHC_Exts.I# i, Happy_GHC_Exts.I# act) shifts =
      DEBUG_TRACE("found action in state " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# st) Happy_Prelude.++ ", input " Happy_Prelude.++ Happy_Prelude.show (Happy_GHC_Exts.I# i) Happy_Prelude.++ ", " Happy_Prelude.++ Happy_Prelude.show (happyDecodeAction act) Happy_Prelude.++ "\n")
      case happyDecodeAction act of
        HappyFail     -> shifts
        HappyAccept   -> shifts -- This would always be %eof or error... Not helpful
        HappyShift _  -> Happy_Prelude.insert (Happy_GHC_Exts.I# i) shifts
        HappyReduce r -> case happySimulateReduce r st sts of
          (HappyCons st1 sts1) -> search_shifts st1 sts1 shifts
    distinct_actions st
      -- The (token number, action) pairs of all actions in the given state
      = ((-1), (Happy_GHC_Exts.I# (happyIndexOffAddr happyDefActions st)))
      : [ (i, act) | i <- [begin_i..happy_n_terms], act <- get_act row_off i ]
      where
        row_off = happyIndexOffAddr happyActOffsets st
        begin_i = 2 -- +2: errorTok,catchTok
    get_act off (Happy_GHC_Exts.I# i) -- happyIndexActionTable with cached row offset
      | let off_i = PLUS(off,i)
      , GTE(off_i,0#)
      , EQ(happyIndexOffAddr happyCheck off_i,i)
      = [(Happy_GHC_Exts.I# (happyIndexOffAddr happyTable off_i))]
      | Happy_Prelude.otherwise
      = []

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Happy_Prelude.error "Internal Happy parser panic. This is not supposed to happen! Please open a bug report at https://github.com/haskell/happy/issues.\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions

happyTcHack :: Happy_Int -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}

-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Happy_GHC_Exts.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
