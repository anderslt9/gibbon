module ConvertToTypedAST where

import AST
import Gibbon.Language.Syntax as S

data MyEnv a b = MyEnv { tyEnv :: S.TyEnv a b 
                   , vEnv  :: S.VEnv a b
                   }

data MyProg

convertToTypedAST :: Program -> S.Prog S.Ty2
