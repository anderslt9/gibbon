module ConvertToL2AST where

import Gibbon.Common as C
import Gibbon.L2.Syntax as L2
import Gibbon.Language.Syntax as S
import AST
import Data.Map (empty)
import ConvertToTypedAST as My
import Helper
import GHC.Float ( float2Double )


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
    return $ L2.Prog newDataTypeDecls empty (Just (newExpr, newPType))

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
        (ExprFuncApp (FuncVar f) locRegions exprs) -> do
            newExprs <- mapM convertExpr (let (Exprs es) = exprs in es)
            -- TODO insert locRegions into []
            return $ S.AppE (C.toVar f) [] newExprs
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

convertTypeCon :: TypeCon -> E S.TyCon
convertTypeCon (TypeCon typeCon) = do
    return $ typeCon

convertDataCon :: AST.DataCon -> E C.DataCon
convertDataCon (AST.DataCon dataCon) = return dataCon

-- TODO good amount here
convertMyTyUrTy :: My.MyTy a -> E (S.TyOf L2.Exp2)
convertMyTyUrTy myTy = case myTy of
    My.IntTy -> return S.IntTy
    My.FloatTy -> return S.FloatTy
    My.BoolTy -> return S.BoolTy
    _ -> Failed "convertMyTyUrTy: Unsupported MyTy type"
