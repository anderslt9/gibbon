


-- | Remove GMap from the code.
-- I need to take an expression such as the following:
-- data TreeA = NodeA Int TreeA TreeA | EmptyA
-- data TreeB = NodeB Float TreeB TreeB | EmptyB
-- 
-- f1 :: Int -> Float
-- f1 x = fromIntegral x + 0.5
-- 
-- TODO extend with default non-recursive constructor
-- let t1 = tlist [(NodeA, NodeB, f1)] 
--     t1' = gmap t1 (NodeA 5 (NodeA 3 EmptyA EmptyA) (NodeA 7 EmptyA EmptyA)) in
-- [printFunc] t1'
-- 
-- I will then transform it into the following:
-- data TreeA = NodeA Int TreeA TreeA | EmptyA
-- data TreeB = NodeB Float TreeB TreeB | EmptyB
-- 
-- f1 :: Int -> Float
-- f1 x = fromIntegral x + 0.5
-- 
-- transformTreeATreeB :: (Int -> Float) -> TreeA -> TreeB
-- transformTreeATreeB f (NodeA x l r) = NodeB (f x) EmptyB EmptyB
-- transformTreeATreeB EmptyA = EmptyB
-- 
-- transformTreeATreeB :: (TreeA -> TreeB) -> TreeA -> TreeB
-- transformTreeATreeB f a = case a of
    -- ta@(NodeA x l r) -> let tb = f ta in case tb of
--                           NodeB y _ _ -> (transformTreeATreeB f l) (transformTreeATreeB f r)
    -- EmptyA -> EmptyB
-- 
-- mapTreeATreeB :: TreeA -> TreeB
-- mapTreeATreeB ta = transformTreeATreeB f1 ta
-- 
-- {-# ANN mapTreeATreeB "OPT:MayVectorize" #-}
-- mapTreeATreeB :: TreeA -> TreeB
-- mapTreeATreeB (NodeA x l r) = NodeB (f1 x) (mapTreeATreeB l) (mapTreeATreeB r)
-- mapTreeATreeB EmptyA = EmptyB
-- 
-- let t1 = transformTreeATreeB
--     t1' = mapTreeATreeB f1 (NodeA 5 (NodeA 3 EmptyA EmptyA) (NodeA 7 EmptyA EmptyA)) in
-- [printFunc] t1'

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