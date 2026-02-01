module ConvertToL2AST where

import qualified Gibbon.Common as C
import Gibbon.L2.Syntax as L2
import Gibbon.Language.Syntax as S
import qualified Data.Set.Internal as Set (empty)
import AST
import Data.Map (empty, fromList)
import ConvertToTypedAST as My
import Helper
import GHC.Float ( float2Double )
import Foreign (new)
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


convertToL2AST :: (TypedNode a) Program -> E L2.Prog2
convertToL2AST (TypedNode p_type (Program dataTypeDecls funcDecls expr)) = do
    -- L2.Prog2 (convertDataTypeDecls dataTypeDecls) (map convertFuncDecl funcDecls) (convertExpr expr)
    newDataTypeDecls <- convertDataTypeDecls dataTypeDecls
    newExpr <- convertExpr expr
    newPType <- convertMyTyUrTy p_type
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
convertFuncDecl (FuncDecl (FuncVar f) typeScheme (FuncVar innerF) locRegions vars expr) = do
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
convertCombinedTypeToLRM lrmModality (CTLocated (LocatedType combinedLocType locRegion)) = do
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
        (ExprFuncApp (FuncVar f) locRegions (Exprs exprs)) -> do
            newExprs <- mapM convertExpr exprs
            -- TODO insert locRegions into []
            return $ S.AppE (C.toVar f) [] newExprs
        (ExprDataConApp (DataCon dataCon) locRegion (Exprs exprs)) -> do
            newExprs <- mapM convertExpr exprs
            locVar <- convertLocRegionToLocVar locRegion
            return $ S.DataConE (C.Single locVar) dataCon newExprs
        (ExprCase val pats) -> do
            Failed "convertExpr: Not implemented for case expressions"
        (ExprLet var combinedType e1 e2) -> do
            Failed "convertExpr: Not implemented for let expressions"
        (ExprLetLoc locRegion locExpress e) -> do
            Failed "convertExpr: Not implemented for letloc expressions"
        (ExprLetRegion regionVar e) -> do
            Failed "convertExpr: Not implemented for letregion expressions"
        _ -> Failed "convertExpr: Not implemented for this expression type"


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
convertLocRegionToLocVar EmptyLocRegion = Failed "convertLocRegionToLocVar: EmptyLocRegion has no LocVar"

convertLocRegionToRegVar :: LocRegion -> E C.Var
convertLocRegionToRegVar (LocRegion _ (RegionVar regionVar) _) = return $ C.toVar regionVar
convertLocRegionToRegVar EmptyLocRegion = Failed "convertLocRegionToRegVar: EmptyLocRegion has no RegionVar"

convertLocRegionToIndexVar :: LocRegion -> E C.Var
convertLocRegionToIndexVar (LocRegion _ _ (IndexVar indexVar)) = return $ C.toVar indexVar
convertLocRegionToIndexVar EmptyLocRegion = Failed "convertLocRegionToIndexVar: EmptyLocRegion has no IndexVar"

convertTypeCon :: TypeCon -> E S.TyCon
convertTypeCon (TypeCon typeCon) = do
    return typeCon

convertVarsToVars :: Vars -> E [C.Var]
convertVarsToVars (Vars vars) = return $ map (\(Var v) -> C.toVar v) vars

convertDataCon :: AST.DataCon -> E C.DataCon
convertDataCon (AST.DataCon dataCon) = return dataCon

-- TODO good amount here
convertMyTyUrTy :: My.MyTy a -> E (S.TyOf L2.Exp2)
convertMyTyUrTy myTy = case myTy of
    My.IntTy -> return S.IntTy
    My.FloatTy -> return S.FloatTy
    My.BoolTy -> return S.BoolTy
    _ -> Failed "convertMyTyUrTy: Unsupported MyTy type"
