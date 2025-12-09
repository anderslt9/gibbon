module ConvertToL2AST where

import Gibbon.Common as C
import Gibbon.L2.Syntax as L2
import Gibbon.Language.Syntax as S
import AST
import Data.Map (empty)

-- class ConvertToL2AST a where
--     convertToL2AST :: a -> b

-- instance ConvertToL2AST Program where
--     convertToL2AST (Program dataTypeDecls funcDecls expr) =
--         L2Program (map convertToL2AST dataTypeDecls) (map convertToL2AST funcDecls) (convertToL2AST expr)

-- instance ConvertToL2AST DataTypeDecl where
--     convertToL2AST (DataTypeDecl typeCon dataFields) =
-- getExpType :: Expr -> S.TyOf L2.Exp2
-- getExpType expr = case expr of 
--     (ExprVal val) -> getValType val
--     (ExprBinOp _ _ _) -> S.IntTy  -- assuming integer for now
--     _ -> error "getExpType: Not implemented for this expression type"


convertToL2AST :: Program -> L2.Prog2
convertToL2AST (Program dataTypeDecls funcDecls expr) =
    -- L2.Prog2 (convertDataTypeDecls dataTypeDecls) (map convertFuncDecl funcDecls) (convertExpr expr)
    L2.Prog (convertDataTypeDecls dataTypeDecls) empty (convertExpr expr)

convertDataTypeDecls :: DataTypeDecls -> S.DDefs L2.Ty2
convertDataTypeDecls (DataTypeDecls decls) = S.fromListDD (map convertDataTypeDecl decls)

-- setting this as linear for now, unsure how this works
convertDataTypeDecl :: DataTypeDecl -> S.DDef L2.Ty2
convertDataTypeDecl (DataTypeDecl typeCon dataFields) =
    S.DDef (convertTypeCon typeCon) [] (convertDataFields dataFields) S.Linear

convertDataFields :: DataFields -> [(C.DataCon, [(S.IsBoxed, L2.Ty2)])]
convertDataFields (DataFields dataFields) =
    map convertDataField dataFields

convertDataField :: DataField -> (C.DataCon, [(S.IsBoxed, L2.Ty2)])
convertDataField (DataField dataCon combinedTypeCons) =
    (convertDataCon dataCon, convertCombinedTypeCons combinedTypeCons)

    where convertCombinedTypeCons :: CombinedTypeCons -> [(S.IsBoxed, L2.Ty2)]
          convertCombinedTypeCons (CombinedTypeCons ts) = map convertCombinedTypeCon ts

          convertCombinedTypeCon :: CombinedTypeCon -> (S.IsBoxed, L2.Ty2)
          convertCombinedTypeCon (CTCTypeCon tc) = (True, S.PackedTy (convertTypeCon tc) (C.Single "l"))
        --   May switch to just setting to False later
          convertCombinedTypeCon (CTCBase baseType) = (False, convertBaseType baseType)

convertBaseType :: BaseType -> L2.Ty2
convertBaseType baseType = case baseType of 
    Int    -> S.IntTy
    Float  -> S.FloatTy
    Bool   -> S.BoolTy
-- convertBaseType String = S.StringTy   Unsure how to deal with this for now

-- TODO continue this
convertExpr :: Expr -> L2.Exp2
convertExpr expr = case expr of 
    (ExprVal val) -> convertVal val
    (ExprBinOp binOp e1 e2) -> L2.PrimAppE (convertBinOp binOp) [convertExpr e1, convertExpr e2]


convertVal :: Val -> L2.Exp2
convertVal val = case val of 
    (ValVar (AST.Var v)) -> L2.VarE v
    (ValLit lit) -> convertLit lit

convertLit :: Lit -> L2.Exp2
convertLit lit = case lit of 
    (IntLit n)    -> S.LitE n
    (FloatLit f)  -> S.FloatE f
-- convertLit (BoolLit b)   = L2.LitE b  currently no BoolE in L2
-- convertLit (StringLit s) = L2.LitE s  currently no StringE in L2

convertBinOp :: BinOp -> S.Prim a
convertBinOp b = case b of 
    Add ->  S.AddP
    Sub ->  S.SubP
    FAdd -> S.FAddP
    FSub -> S.FSubP
    Mul ->  S.MulP
    Div ->  S.DivP
    FMul -> S.FMulP
    FDiv -> S.FDivP
    Pow ->  S.ExpP
    Eq ->   S.EqIntP
    FEq ->  S.EqFloatP
    Gt ->   S.GtP
    Lt ->   S.LtP
    FGt ->  S.FGtP
    FLt ->  S.FLtP
    Ge ->   S.GtEqP
    Le ->   S.LtEqP
    FGe ->  S.FGtEqP
    FLe ->  S.FLtEqP
    -- Neq ->  C.NeqP  Currently no NeqP in Gibbon
    And ->  S.AndP
    Or ->   S.OrP

convertTypeCon :: TypeCon -> C.Var
convertTypeCon (TypeCon typeCon) = C.toVar typeCon

convertDataCon :: AST.DataCon -> C.DataCon
convertDataCon (AST.DataCon dataCon) = dataCon
