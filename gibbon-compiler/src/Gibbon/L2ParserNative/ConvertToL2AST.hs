module Gibbon.L2ParserNative.ConvertToL2AST (convertToL2AST) where

import qualified Gibbon.Common as C
import Gibbon.L2.Syntax as L2
import Gibbon.Language.Syntax as S
import qualified Data.Set.Internal as Set (empty)
import Gibbon.L2ParserNative.AST as AST
import Data.Map (fromList)
import Gibbon.L2ParserNative.ConvertToTypedAST as My
import Gibbon.L2ParserNative.Helper
import GHC.Float ( float2Double )
-- import TestRunner (Result(Fail))


-- class ConvertToL2AST a where
--     convertToL2AST :: a -> b

-- instance ConvertToL2AST Program where
--     convertToL2AST (Program dataTypeDecls funcDecls expr) =
--         L2Program (map convertToL2AST dataTypeDecls) (map convertToL2AST funcDecls) (convertToL2AST expr)

-- instance ConvertToL2AST DataTypeDecl wherex  
--     convertToL2AST (DataTypeDecl typeCon dataFields) =
-- getExpType :: Expr -> S.TyOf L2.Exp2
-- getExpType expr = case expr of 
--     (ExprVal val) -> getValType val
--     (ExprBinOp _ _ _) -> S.IntTy  -- assuming integer for now
--     _ -> error "getExpType: Not implemented for this expression type"


convertToL2AST :: (TypedNode LocRegion) Program -> E L2.Prog2
convertToL2AST (TypedNode locReg (Program dataTypeDecls funcDecls expr)) = do
    -- L2.Prog2 (convertDataTypeDecls dataTypeDecls) (map convertFuncDecl funcDecls) (convertExpr expr)
    newDataTypeDecls <- convertDataTypeDecls dataTypeDecls
    newExpr <- convertExpr expr
    newPType <- convertMyTyUrTy locReg
    newFuncDecls <- convertFuncDecls funcDecls
    return $ L2.Prog newDataTypeDecls newFuncDecls (Just (newExpr, newPType))

convertDataTypeDecls :: DataTypeDecls -> E (S.DDefs L2.Ty2)
convertDataTypeDecls (DataTypeDecls decls) = do
    newDecls <- mapM convertDataTypeDecl decls
    return $ S.fromListDD newDecls

-- setting this as linear for now, unsure how this works
convertDataTypeDecl :: DataTypeDecl -> E (S.DDef L2.Ty2)
convertDataTypeDecl (DataTypeDecl typeCon dataFields) = do
    newTypeCon <- convertTypeCon typeCon
    newDataFields <- convertDataFields dataFields
    return $ S.DDef (C.toVar newTypeCon) [] newDataFields S.Linear

convertDataFields :: DataFields -> E [(C.DataCon, [(S.IsBoxed, L2.Ty2)])]
convertDataFields (DataFields dataFields) = do
    mapM convertDataField dataFields

convertDataField :: DataField -> E (C.DataCon, [(S.IsBoxed, L2.Ty2)])
convertDataField (DataField dataCon combinedTypeCons) = do
    newDataCon <- convertDataCon dataCon
    newCombinedTypeCons <- convertCombinedTypeCons combinedTypeCons
    return (newDataCon, newCombinedTypeCons)

    where convertCombinedTypeCons :: CombinedTypeCons -> E [(S.IsBoxed, L2.Ty2)]
          convertCombinedTypeCons (CombinedTypeCons ts) = do
            mapM convertCombinedTypeCon ts

          convertCombinedTypeCon :: CombinedTypeCon -> E (S.IsBoxed, L2.Ty2)
          convertCombinedTypeCon (CTCTypeCon tc) = do
            newTypeCon <- convertTypeCon tc
            return (True, S.PackedTy newTypeCon (C.Single "l"))
        --   May switch to just setting to False later
          convertCombinedTypeCon (CTCBase baseType) = do
            newBaseType <- convertBaseType baseType
            return (False, newBaseType)

convertBaseType :: BaseType -> E L2.Ty2
convertBaseType baseType = case baseType of 
    Int    -> return S.IntTy
    Float  -> return S.FloatTy
    Bool   -> return S.BoolTy
    _      -> Failed "convertBaseType: Unsupported base type"
-- convertBaseType String = S.StringTy   Unsure how to deal with this for now

convertFuncDecls :: FuncDecls -> E (L2.FunDefs C.Var L2.Exp2)
convertFuncDecls (FuncDecls funcDecls) = do
    newDecls <- mapM convertFuncDecl funcDecls
    return $ fromList newDecls

convertFuncDecl :: FuncDecl -> E (C.Var, L2.FunDef2)
convertFuncDecl (FuncDecl (FuncVar f) typeScheme (FuncVar _innerF) _locRegions vars expr) = do
    newTypeScheme <- convertTypeScheme typeScheme
    newExpr <- convertExpr expr
    newVars <- convertVarsToVars vars
    -- TODO figure out how to actually set Rec and Inline and CanTriggerGC
    return (C.toVar f, S.FunDef (C.toVar f) newVars newTypeScheme newExpr (S.FunMeta S.Rec S.NoInline False) )

-- TODO look more into how I set locRets and has Parallelism
convertTypeScheme :: TypeScheme -> E (L2.ArrowTy2 L2.Ty2)
convertTypeScheme (TypeScheme (CombinedTypes combinedTypes)) = do
    (args, ret) <- splitLast combinedTypes
    locVarsArgs <- mapM (convertCombinedTypeToLRM L2.Input) . filter isLocatedType $ args
    locVarsRet <-  mapM (convertCombinedTypeToLRM L2.Output) . filter isLocatedType $ [ret]
    let locVars = locVarsArgs ++ locVarsRet
    inTypes <- mapM convertCombinedTypeToTy args
    outType <- convertCombinedTypeToTy ret
    -- TODO figure out how to set parallelism
    return $ L2.ArrowTy2 locVars inTypes Set.empty outType [] False
    where
        isLocatedType :: CombinedType -> Bool
        isLocatedType (CTLocated (LocatedType _ EmptyLocRegion)) = False
        isLocatedType (CTLocated (LocatedType (CLTBase _) _)) = False
        isLocatedType (CTLocated _) = True
        isLocatedType (CTBase _) = False

-- TODO figure out how index var works in this case
convertCombinedTypeToLRM :: L2.Modality -> CombinedType -> E LRM
convertCombinedTypeToLRM lrmModality (CTLocated (LocatedType _combinedLocType locRegion)) = do
    locVar <- convertLocRegionToLocVar locRegion
    regionVar <- convertLocRegionToRegVar locRegion
    -- indexVar <- convertLocRegionToIndexVar locRegion
    return $ L2.LRM (C.Single locVar) (L2.VarR regionVar) lrmModality
convertCombinedTypeToLRM _ _ = Failed "convertCombinedTypeToLRM: Mapping called on non-located type"

convertCombinedTypeToTy :: CombinedType -> E L2.Ty2
convertCombinedTypeToTy (CTLocated (LocatedType (CLTTypeCon (TypeCon typeCon)) l@(LocRegion {}))) = do
    locVar <- convertLocRegionToLocVar l
    return $ S.PackedTy typeCon (C.Single locVar)
convertCombinedTypeToTy (CTLocated (LocatedType (CLTBase baseType) _)) = convertBaseType baseType
convertCombinedTypeToTy (CTBase baseType) = convertBaseType baseType
convertCombinedTypeToTy _ = Failed "convertCombinedTypeToInputTy: Unsupported CombinedType"


-- TODO continue this
convertExpr :: Expr -> E L2.Exp2
convertExpr expr = do
    case expr of 
        (ExprVal val) -> convertVal val
        (ExprBinOp binOp e1 e2) -> do
            newE1 <- convertExpr e1
            newE2 <- convertExpr e2
            primOp <- convertBinOp binOp
            return $ S.PrimAppE primOp [newE1, newE2]
        (ExprFuncApp (FuncVar f) (LocRegions locRegions) (Exprs exprs)) -> do
            newExprs <- mapM convertExpr exprs
            newLocs <- mapM convertLocRegionToLocVar locRegions
            -- TODO figure out how to deal with tail-recursive stuff
            -- TODO calling function within function will register as recursive call
            return $ S.AppE (C.toVar f) S.NotTailRec (map C.Single newLocs) newExprs
        (ExprDataConApp (DataCon dataCon) locRegion (Exprs exprs)) -> do
            newExprs <- mapM convertExpr exprs
            locVar <- convertLocRegionToLocVar locRegion
            return $ S.DataConE (C.Single locVar) dataCon newExprs
        (ExprCase val pats) -> do
            newPats <- convertPats pats
            newVal <- convertVal val
            return $ L2.CaseE newVal newPats
        (ExprLet (Var var) combinedType e1 e2) -> do
            let newVar = C.toVar var
            newCombinedType <- convertCombinedTypeToTy combinedType
            newE1 <- convertExpr e1
            newE2 <- convertExpr e2
            return $ L2.LetE (newVar, [], newCombinedType, newE1) newE2
        (ExprLetLoc locRegion locExpress e) -> do
            locVar <- convertLocRegionToLocVar locRegion
            newE <- convertExpr e
            case locExpress of 
                (LocExpressStart (RegionVar regionVar)) -> do
                    return $ L2.Ext $ L2.LetLocE (C.Single locVar) (L2.StartOfRegionLE (L2.VarR . C.toVar $ regionVar)) newE
                (LocExpressNext nextLocRegion offset) -> do
                    nextLocVar <- convertLocRegionToLocVar nextLocRegion
                    return $ L2.Ext $ L2.LetLocE (C.Single locVar) (L2.AfterConstantLE offset (C.Single nextLocVar)) newE
                (LocExpressAfter (LocatedType (CLTTypeCon (TypeCon _typeCon)) lr@(LocRelativeVar relativeVar _locVar1 _regVar1 _iVar1))) -> do
                    locVarRel <- convertLocRegionToLocVar lr
                    -- Failed $ "relativeVar: " ++ show relativeVar ++ ", locVarRel: " ++ show locVarRel
                    return $ L2.Ext $ L2.LetLocE (C.Single locVar) (L2.AfterVariableLE (C.toVar relativeVar) (C.Single locVarRel) True) newE
                (LocExpressAfter _) -> do
                    Failed "convertExpr: Not implemented for this type of LocExpressAfter"
                -- _ -> Failed "convertExpr: Not implemented for this locExpress type"
        (ExprLetRegion (RegionVar regionVar) e) -> do
            newE <- convertExpr e
            -- TODO figure out mutable vs immutable
            return $ L2.Ext $ L2.LetRegionE (L2.VarR . C.toVar $ regionVar) L2.Undefined RegionImmutable Nothing newE
            -- Failed "convertExpr: Not implemented for letregion expressions"
        -- _ -> Failed "convertExpr: Not implemented for this expression type"


convertPats :: Pats -> E [(C.DataCon, [(C.Var, L2.LocVar)], L2.Exp2)]
convertPats (Pats pats) = do
    mapM convertPat pats

convertPat :: Pat -> E (C.DataCon, [(C.Var, L2.LocVar)], L2.Exp2)
convertPat (Pat (DataCon dataCon) (PatMatches patMatches) expr) = do
    newPatMatches <- mapM convertPatMatch patMatches
    newExpr <- convertExpr expr
    return (dataCon, newPatMatches, newExpr)

convertPatMatch :: PatMatch -> E (C.Var, L2.LocVar)
convertPatMatch (PatMatch (ValVar (AST.Var v)) (LocatedType _ locRegion)) = do
    locVar <- convertLocRegionToLocVar locRegion
    return (C.toVar v, C.Single locVar)
convertPatMatch _ = Failed "convertPatMatch: Unsupported pattern match"


convertVal :: Val -> E L2.Exp2
convertVal val = case val of 
    (ValVar (AST.Var v)) -> return $ L2.VarE (C.toVar v)
    (ValLit lit) -> convertLit lit

convertLit :: Lit -> E L2.Exp2
convertLit lit = case lit of 
    (IntLit n)    -> return $ S.LitE n
    (FloatLit f)  -> return $ S.FloatE (float2Double f)
    _             -> Failed "convertLit: Unsupported literal type"
-- convertLit (BoolLit b)   = L2.LitE b  currently no BoolE in L2
-- convertLit (StringLit s) = L2.LitE s  currently no StringE in L2

convertBinOp :: BinOp -> E (S.Prim a)
convertBinOp b = case b of 
    Add ->  return S.AddP
    Sub ->  return S.SubP
    FAdd -> return S.FAddP
    FSub -> return S.FSubP
    Mul ->  return S.MulP
    Div ->  return S.DivP
    FMul -> return S.FMulP
    FDiv -> return S.FDivP
    Pow ->  return S.ExpP
    Eq ->   return S.EqIntP
    FEq ->  return S.EqFloatP
    Gt ->   return S.GtP
    Lt ->   return S.LtP
    FGt ->  return S.FGtP
    FLt ->  return S.FLtP
    Ge ->   return S.GtEqP
    Le ->   return S.LtEqP
    FGe ->  return S.FGtEqP
    FLe ->  return S.FLtEqP
    -- Neq ->  C.NeqP  Currently no NeqP in Gibbon
    And ->  return S.AndP
    Or ->   return S.OrP
    _ ->   Failed "convertBinOp: Unsupported binary operator"

convertLocRegionToLocVar :: LocRegion -> E C.Var
convertLocRegionToLocVar (LocRegion (LocVar locVar) _ _) = return $ C.toVar locVar
convertLocRegionToLocVar (LocRelativeVar _ (LocVar locVar) _ _) = return $ C.toVar locVar
convertLocRegionToLocVar EmptyLocRegion = Failed "convertLocRegionToLocVar: EmptyLocRegion has no LocVar"

convertLocRegionToRegVar :: LocRegion -> E C.Var
convertLocRegionToRegVar (LocRegion _ (RegionVar regionVar) _) = return $ C.toVar regionVar
convertLocRegionToRegVar (LocRelativeVar _ _ (RegionVar regionVar) _) = return $ C.toVar regionVar
convertLocRegionToRegVar EmptyLocRegion = Failed "convertLocRegionToRegVar: EmptyLocRegion has no RegionVar"

convertLocRegionToIndexVar :: LocRegion -> E C.Var
convertLocRegionToIndexVar (LocRegion _ _ (IndexVar indexVar)) = return $ C.toVar indexVar
convertLocRegionToIndexVar (LocRelativeVar _ _ _ (IndexVar indexVar)) = return $ C.toVar indexVar
convertLocRegionToIndexVar EmptyLocRegion = Failed "convertLocRegionToIndexVar: EmptyLocRegion has no IndexVar"

convertLocRegionToRelLocVar :: LocRegion -> E C.Var
convertLocRegionToRelLocVar (LocRelativeVar relVar _ _ _) = return $ C.toVar relVar
convertLocRegionToRelLocVar _ = Failed "convertLocRegionToRelLocVar: Only LocRelativeVar has a relative location variable"

convertTypeCon :: TypeCon -> E S.TyCon
convertTypeCon (TypeCon typeCon) = do
    return typeCon

convertVarsToVars :: Vars -> E [C.Var]
convertVarsToVars (Vars vars) = return $ map (\(Var v) -> C.toVar v) vars

convertDataCon :: AST.DataCon -> E C.DataCon
convertDataCon (AST.DataCon dataCon) = return dataCon

-- TODO good amount here
convertMyTyUrTy :: My.MyTy LocRegion -> E (S.TyOf L2.Exp2)
convertMyTyUrTy myTy = case myTy of
    My.IntTy -> return S.IntTy
    My.FloatTy -> return S.FloatTy
    My.BoolTy -> return S.BoolTy
    My.PackedTy (TypeCon typeCon) locRegion -> do
        locVar <- convertLocRegionToLocVar locRegion
        return $ S.PackedTy typeCon (C.Single locVar)
    _ -> Failed "convertMyTyUrTy: Unsupported MyTy type"
