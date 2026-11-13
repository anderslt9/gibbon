-- | Remove GMap from the code.
--
-- The first version of this pass lowers a GMap to a generated, recursive
-- first-order function.  Transform lists are deliberately kept as expressions
-- here; the generated cases currently require their transforms to be top-level
-- function references.
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

import           Control.Monad (foldM)
import           Data.List (find)
import qualified Data.Map as M

import           Gibbon.Common
import           Gibbon.L0.Syntax

-- type Fenv = TyEnv Var TyScheme
type MainExp = Maybe (Exp0, TyOf Exp0)

rmGMap :: Prog0 -> PassM Prog0
rmGMap (Prog ddefs funs main) = do
  let fenv = M.map funTy funs
  funs' <- foldM (rewriteFun ddefs fenv) funs (M.keys funs)
  (main', generated) <- case main of
    Nothing -> pure (Nothing, M.empty)
    Just (e, ty) -> do
      (e', fs) <- rmGmapExp ddefs (Env2 M.empty fenv) e
      pure (Just (e', ty), fs)
  pure $ Prog ddefs (M.union generated funs') main'
  where
    rewriteFun dds fenv0 funMap name =
      case M.lookup name funMap of
        Nothing -> pure funMap
        Just fd -> do
          (body', generated) <- rmGmapExp dds (Env2 M.empty fenv0) (funBody fd)
          let fd' = fd { funBody = body' }
              funMap' = M.insert name fd' (M.union generated funMap)
          pure funMap'

-- | Rewrite an expression and return any generated map functions.
rmGmapExp :: DDefs0 -> Env2 Var Ty0 -> Exp0 -> PassM (Exp0, FunDefs0)
rmGmapExp ddefs env = go
  where
    go ex =
      case ex of
        VarE{}       -> pure (ex, M.empty)
        LitE{}       -> pure (ex, M.empty)
        CharE{}      -> pure (ex, M.empty)
        FloatE{}     -> pure (ex, M.empty)
        LitSymE{}     -> pure (ex, M.empty)
        AppE f tr ls as -> mapArgs (\as' -> AppE f tr ls as') as
        PrimAppE p as -> mapArgs (PrimAppE p) as
        LetE (v, ls, ty, rhs) bod -> do
          (rhs', fs1) <- go rhs
          (bod', fs2) <- go bod
          pure (LetE (v, ls, ty, rhs') bod', M.union fs1 fs2)
        IfE a b c -> mapArgs3 IfE a b c
        MkProdE es -> mapList MkProdE es
        ProjE i e -> mapOne (ProjE i) e
        CaseE scrut alts -> do
          (scrut', fs0) <- go scrut
          (alts', fs) <- foldM rewriteAlt ([], fs0) alts
          pure (CaseE scrut' (reverse alts'), fs)
          where
            rewriteAlt (acc, fs0) (con, args, bod) = do
              (bod', fs1) <- go bod
              pure ((con, args, bod') : acc, M.union fs0 fs1)
        DataConE loc con es -> do
          (es', fs) <- mapMAccum es
          pure (DataConE loc con es', fs)
        TimeIt e ty b -> do
          (e', fs) <- go e
          pure (TimeIt e' ty b, fs)
        WithArenaE v e -> do
          (e', fs) <- go e
          pure (WithArenaE v e', fs)
        SpawnE f tys es -> do
          (es', fs) <- mapMAccum es
          pure (SpawnE f tys es', fs)
        SyncE -> pure (ex, M.empty)
        Ext ext ->
          case ext of
            GMap _ty transforms object -> do
              (transforms', fs1) <- go transforms
              (object', fs2) <- go object
              let (inTy, resultTy) = case recoverType ddefs env transforms' of
                           TransformListTy t t' -> (t, t')
                           t -> error $ "rmGMap: expected transform list, got " ++ sdoc t
                  -- resultTy = case recoverType ddefs env transforms' of
                  --           TransformListTy _ t -> t
                  --           t -> error $ "rmGMap: expected transform list, got " ++ sdoc t
              (mapFuncName, generated) <-
                createMapFunc ddefs env inTy resultTy transforms'
              pure (AppE mapFuncName NotTailRec [] [object'],
                    M.unions [generated, fs1, fs2])
            _ -> rewriteExt ext
        _ -> error $ "rmGMap: unhandled expression " ++ sdoc ex

    rewriteExt ext =
      case ext of
        LambdaE args bod -> do
          (bod', fs) <- go bod
          pure (Ext (LambdaE args bod'), fs)
        PolyAppE f a -> mapArgs (\[f', a'] -> Ext (PolyAppE f' a')) [f, a]
        FunRefE{} -> pure (Ext ext, M.empty)
        BenchE f tys as b -> do
          (as', fs) <- mapMAccum as
          pure (Ext (BenchE f tys as' b), fs)
        ParE0 es -> do
          (es', fs) <- mapMAccum es
          pure (Ext (ParE0 es'), fs)
        PrintPacked ty e -> mapOne (\e' -> Ext (PrintPacked ty e')) e
        CopyPacked ty e -> mapOne (\e' -> Ext (CopyPacked ty e')) e
        TravPacked ty e -> mapOne (\e' -> Ext (TravPacked ty e')) e
        L loc e -> mapOne (\e' -> Ext (L loc e')) e
        TransformList ls -> do
          (ls', fs) <- foldM rewriteTransform ([], M.empty) ls
          pure (Ext (TransformList (reverse ls')), fs)
          where
            rewriteTransform (acc, fs0) (a, b, e) = do
              (e', fs1) <- go e
              pure ((a, b, e') : acc, M.union fs0 fs1)
        LinearExt le -> do
          (le', fs) <- rewriteLinear le
          pure (Ext (LinearExt le'), fs)
        _ -> error $ "rmGMap: unhandled extension " ++ sdoc ext

    rewriteLinear le =
      case le of
        ReverseAppE a b -> do
          (a', fs1) <- go a
          (b', fs2) <- go b
          pure (ReverseAppE a' b', M.union fs1 fs2)
        LseqE a b -> do
          (a', fs1) <- go a
          (b', fs2) <- go b
          pure (LseqE a' b', M.union fs1 fs2)
        AliasE a -> do
          (a', fs) <- go a
          pure (AliasE a', fs)
        ToLinearE a -> do
          (a', fs) <- go a
          pure (ToLinearE a', fs)

    mapOne f e = do
      (e', fs) <- go e
      pure (f e', fs)

    mapArgs f es = do
      (es', fs) <- mapMAccum es
      pure (f es', fs)

    mapArgs3 f a b c = mapArgs (\[a', b', c'] -> f a' b' c') [a, b, c]

    mapList f es = mapArgs (f) es

    mapMAccum = foldM step ([], M.empty)
      where
        step (acc, fs0) e = do
          (e', fs1) <- go e
          pure (acc ++ [e'], M.union fs0 fs1)

-- | Generate a map function between 2 packed types.
createMapFunc :: DDefs0 -> Env2 Var Ty0 -> Ty0 -> Ty0 -> Exp0 -> PassM (Var, FunDefs0)
createMapFunc ddefs _env inTy resultTy transforms = do
  mapFuncName <- gensym "gmap"
  arg <- gensym "input"
  body <- createMapFuncBody ddefs mapFuncName arg inTy resultTy transforms
  let fd = FunDef mapFuncName [arg] (ForAll [] (ArrowTy [inTy] resultTy)) body
                  (FunMeta Rec NoInline False [] Nothing)
  pure (mapFuncName, M.singleton mapFuncName fd)

-- | Create a body which should just be a case expression
createMapFuncBody :: DDefs0 -> Var -> Var -> Ty0 -> Ty0 -> Exp0 -> PassM Exp0
createMapFuncBody ddefs mapFuncName arg inTy resultTy transforms =
  case (inTy, resultTy) of
    (PackedTy inCon _, PackedTy _ _) -> do
      let inDDef = lookupDDef ddefs inCon
      alts <- mapM mapPattern (dataCons inDDef)
      pure $ CaseE (VarE arg) alts
    _ -> error $ "rmGMap: GMap currently requires packed types, got "
              ++ sdoc (inTy, resultTy)
  where
    mapPattern (inDCon, fields) = do
      -- create variables for each field in given constructor
      vars <- mapM (const (gensym "field")) fields

      -- find transform corresponding to this constructor
      let match = find (\((i, _), _, _) -> i == inDCon) (transformEntries transforms)
      (_, outDCon, fn) <- case match of
        Just x -> pure x
        Nothing -> error $ "rmGMap: no transform for constructor " ++ sdoc inDCon
      args <- mapM (fieldExpr fn) (zip vars (map snd fields))
      pure (inDCon, zip vars (map snd fields), DataConE (ProdTy []) outDCon args)

    fieldExpr fn (v, ty)
      | recursive ty = pure $ AppE mapFuncName NotTailRec [] [VarE v]
      | otherwise = do
          f <- extractTransform fn
          pure $ AppE f NotTailRec [] [VarE v]

    recursive (PackedTy tc _) =
      case inTy of
        PackedTy inputTc _ -> tc == inputTc
        _ -> False
    recursive _ = False

transformEntries :: Exp0 -> [((DataCon, Ty0), DataCon, Exp0)]
transformEntries (Ext (TransformList ls)) =
  [((inCon, inTy), outCon, fn) | ((inCon, inTy), (outCon, _), fn) <- ls]
transformEntries ex =
  error $ "rmGMap: expected a transform list, got " ++ sdoc ex

extractTransform :: Exp0 -> PassM Var
extractTransform (Ext (FunRefE _ f)) = pure f
extractTransform ex =
  error $ "rmGMap: transform must be a top-level function reference, got "
       ++ sdoc ex
