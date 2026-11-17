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
type TransformEnv = M.Map Var Exp0

rmGMap :: Prog0 -> PassM Prog0
rmGMap (Prog ddefs funs main) = do
  let fenv = M.map funTy funs
  funs' <- foldM (rewriteFun ddefs fenv) funs (M.keys funs)
  (main', generated) <- case main of
    Nothing -> pure (Nothing, M.empty)
    Just (e, ty) -> do
      (e', fs) <- rmGmapExp ddefs (Env2 M.empty fenv) M.empty e
      pure (Just (e', ty), fs)
  pure $ Prog ddefs (M.union generated funs') main'
  where
    rewriteFun dds fenv0 funMap name =
      case M.lookup name funMap of
        Nothing -> pure funMap
        Just fd -> do
          (body', generated) <- rmGmapExp dds (Env2 M.empty fenv0) M.empty (funBody fd)
          let fd' = fd { funBody = body' }
              funMap' = M.insert name fd' (M.union generated funMap)
          pure funMap'

-- | Rewrite an expression and return any generated map functions.
rmGmapExp :: DDefs0 -> Env2 Var Ty0 -> TransformEnv -> Exp0 -> PassM (Exp0, FunDefs0)
rmGmapExp ddefs = go
  where
    go env tenv ex =
      case ex of
        VarE{}       -> pure (ex, M.empty)
        LitE{}       -> pure (ex, M.empty)
        CharE{}      -> pure (ex, M.empty)
        FloatE{}     -> pure (ex, M.empty)
        LitSymE{}     -> pure (ex, M.empty)
        AppE f tr ls as -> mapArgs (AppE f tr ls) as
        PrimAppE p as -> mapArgs (PrimAppE p) as
        LetE (v, ls, ty, rhs) bod -> do
          (rhs', fs1) <- go env tenv rhs
          let env' = extendVEnv v ty env
          let tenv' = M.insert v rhs' tenv
          (bod', fs2) <- go env' tenv' bod
          pure (LetE (v, ls, ty, rhs') bod', M.union fs1 fs2)
        IfE a b c -> mapArgs3 IfE a b c
        MkProdE es -> mapArgs MkProdE es
        ProjE i e -> mapOne (ProjE i) e
        CaseE scrut alts -> do
          (scrut', fs0) <- go env tenv scrut
          (alts', fs) <- foldM rewriteAlt ([], fs0) alts
          pure (CaseE scrut' (reverse alts'), fs)
          where
            rewriteAlt (acc, fs0) (con, args, bod) = do
              (bod', fs1) <- go env tenv bod
              pure ((con, args, bod') : acc, M.union fs0 fs1)
        DataConE loc con es -> do
          (es', fs) <- mapMAccum es
          pure (DataConE loc con es', fs)
        TimeIt e ty b -> do
          (e', fs) <- go env tenv e
          pure (TimeIt e' ty b, fs)
        WithArenaE v e -> do
          (e', fs) <- go env tenv e
          pure (WithArenaE v e', fs)
        SpawnE f tys es -> do
          (es', fs) <- mapMAccum es
          pure (SpawnE f tys es', fs)
        SyncE -> pure (ex, M.empty)
        Ext ext ->
          case ext of
            GMap _ty transforms object -> do
              (transforms', fs1) <- go env tenv transforms
              (object', fs2) <- go env tenv object
              let (inTy, resultTy) = case recoverType ddefs env transforms' of
                           TransformListTy t t' -> (t, t')
                           t -> error $ "rmGMap: expected transform list, got " ++ sdoc t
                  -- resultTy = case recoverType ddefs env transforms' of
                  --           TransformListTy _ t -> t
                  --           t -> error $ "rmGMap: expected transform list, got " ++ sdoc t
              (mapFuncName, generated) <-
                createMapFunc ddefs env tenv inTy resultTy transforms'
              pure (AppE mapFuncName NotTailRec [] [object'],
                    M.unions [generated, fs1, fs2])
            LambdaE args bod -> do
              (bod', fs) <- go env tenv bod
              pure (Ext (LambdaE args bod'), fs)
            PolyAppE f a -> mapArgs (\[f', a'] -> Ext (PolyAppE f' a')) [f, a]
            FunRefE{} -> pure (Ext ext, M.empty)
            BenchE f tys as b -> do
              (as', fs) <- mapMAccum as
              pure (Ext (BenchE f tys as' b), fs)
            ParE0 es -> do
              (es', fs) <- mapMAccum es
              pure (Ext (ParE0 es'), fs)
            PrintPacked ty e -> mapOne (Ext . PrintPacked ty) e
            CopyPacked ty e -> mapOne (Ext . CopyPacked ty) e
            TravPacked ty e -> mapOne (Ext . TravPacked ty) e
            L loc e -> mapOne (Ext . L loc) e
            TransformList inty outty ls -> do
              (ls', fs) <- foldM rewriteTransform ([], M.empty) ls
              pure (Ext (TransformList inty outty (reverse ls')), fs)
              where
                rewriteTransform (acc, fs0) (a, b, e) = do
                  (e', fs1) <- go env tenv e
                  pure ((a, b, e') : acc, M.union fs0 fs1)
            LinearExt le -> do
              (le', fs) <- rewriteLinear le
              pure (Ext (LinearExt le'), fs)
            -- _ -> error $ "rmGMap: unhandled extension " ++ sdoc ext
          where
            rewriteLinear le =
              case le of
                ReverseAppE a b -> do
                  (a', fs1) <- go env tenv a
                  (b', fs2) <- go env tenv b
                  pure (ReverseAppE a' b', M.union fs1 fs2)
                LseqE a b -> do
                  (a', fs1) <- go env tenv a
                  (b', fs2) <- go env tenv b
                  pure (LseqE a' b', M.union fs1 fs2)
                AliasE a -> do
                  (a', fs) <- go env tenv a
                  pure (AliasE a', fs)
                ToLinearE a -> do
                  (a', fs) <- go env tenv a
                  pure (ToLinearE a', fs)
        _ -> error $ "rmGMap: unhandled expression " ++ sdoc ex

      where
        mapOne f e = do
          (e', fs) <- go env tenv e
          pure (f e', fs)

        mapArgs f es = do
          (es', fs) <- mapMAccum es
          pure (f es', fs)

        mapArgs3 f a b c = mapArgs (\[a', b', c'] -> f a' b' c') [a, b, c]

        -- mapList = mapArgs

        mapMAccum = foldM step ([], M.empty)
          where
            step (acc, fs0) e = do
              (e', fs1) <- go env tenv e
              pure (acc ++ [e'], M.union fs0 fs1)

createFuncName :: Exp0 -> Var
createFuncName _ex = "gmap"

-- | Generate a map function between 2 packed types.
createMapFunc :: DDefs0 -> Env2 Var Ty0 -> TransformEnv -> Ty0 -> Ty0 -> Exp0 -> PassM (Var, FunDefs0)
createMapFunc ddefs env tenv inTy resultTy transforms = do
  mapFuncName <- gensym $ createFuncName transforms
  arg <- gensym "input"
  body <- createMapFuncBody ddefs env tenv mapFuncName arg inTy resultTy transforms
  let fd = FunDef mapFuncName [arg] (ForAll [] (ArrowTy [inTy] resultTy)) body
                  (FunMeta Rec NoInline False [] Nothing)
  pure (mapFuncName, M.singleton mapFuncName fd)

-- | Create a body which should just be a case expression
createMapFuncBody :: DDefs0 -> Env2 Var Ty0 -> TransformEnv -> Var -> Var -> Ty0 -> Ty0 -> Exp0 -> PassM Exp0
createMapFuncBody ddefs _env tenv mapFuncName arg inTy resultTy transforms =
  case (inTy, resultTy) of
    (PackedTy inCon _, PackedTy outCon _) -> do
      let inDDef = lookupDDef ddefs inCon
          outDDef = lookupDDef ddefs outCon
      alts <- mapM mapPattern (dataCons inDDef)
      pure $ CaseE (VarE arg) alts
    _ -> error $ "rmGMap: GMap currently requires packed types, got "
              ++ sdoc (inTy, resultTy)
  where
    mapPattern (inDCon, fields) = do
      -- create variables for each field in given constructor
      vars <- mapM (const (gensym "field")) fields

      -- find transform corresponding to this constructor
      let match = find (\(i, _, _) -> i == inDCon) (transformEntries tenv transforms)
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

    -- consLengthMatch :: [(IsBoxed, Ty0)] -> [(IsBoxed, Ty0)] -> Bool

-- TODO define a series of possible comparisons between recList objects 
    recList :: [(IsBoxed, Ty0)] -> ([Ty0], Int)
    recList [] = ([], 0)
    recList xs = recList' xs 0

    recList' :: [(IsBoxed, Ty0)] -> Int -> ([Ty0], Int)
    recList' [] x = ([], x)
    recList' ((_, ty):rest) n = if recursive ty then recList' rest (n + 1) else (ty : fst (recList' rest n), n)

    -- addImplicitTransform :: DataCon -> PassM (DataCon, DataCon, Maybe Exp0)
    addImplicitTransform inCon outDDef = do
      -- Implementation for adding implicit transforms
      mapM (addImplicitTransform' inCon) (dataCons outDDef)

    -- addImplicitTransform' :: DataCon -> (DataCon, [(IsBoxed, Ty0)]) -> PassM (DataCon, DataCon, Maybe Exp0)
    addImplicitTransform' inCon (outCon, fields) = do
      error ""

      -- let num_fields = 



transformEntries :: TransformEnv -> Exp0 -> [(DataCon, DataCon, Exp0)]
transformEntries tenv e = case e of 
  (Ext (TransformList _ _ ls)) -> ls
  (VarE v) -> transformEntries tenv (tenv # v)
  _ -> error $ "rmGMap: expected a transform list, got " ++ sdoc e
  
  -- where
    -- addImplicitTransforms

extractTransform :: Exp0 -> PassM Var
extractTransform (Ext (FunRefE _ f)) = pure f
extractTransform ex =
  error $ "rmGMap: transform must be a top-level function reference, got "
       ++ sdoc ex
