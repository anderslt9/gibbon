


-- | Remove GMap from the code.

module Gibbon.Passes.RemoveGMap (rmGMap) where

import           Gibbon.Common
import           Gibbon.L0.Syntax

--------------------------------------------------------------------------------

rmGMap :: Prog0 -> PassM Prog0
rmGMap p@(Prog defs funs main) = do
    
    
    
    
    pure p



rmGmapFun :: FunDef0 -> PassM FunDef0