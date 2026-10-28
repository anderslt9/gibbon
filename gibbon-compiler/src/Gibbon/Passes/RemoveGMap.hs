


-- | Remove GMap from the code.
-- I need to take an expression such as the following:
-- data TreeA = NodeA Int | EmptyA
-- data TreeB = NodeB Float | EmptyB


module Gibbon.Passes.RemoveGMap (rmGMap) where

import           Gibbon.Common
import           Gibbon.L0.Syntax

--------------------------------------------------------------------------------

rmGMap :: Prog0 -> PassM Prog0
rmGMap p@(Prog defs funs main) = do
    
    
    
    
    pure p



rmGmapFun :: FunDef0 -> PassM FunDef0
rmGmapFun (FunDef name args ty body meta) = do
    




rmGmapExp :: DDefs0 -> Exp0 -> PassM Exp0
rmGmapExp e = case e of