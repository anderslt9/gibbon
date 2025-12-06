module ConvertToL2AST where

import Gibbon.Common as C
import Gibbon.L2.Syntax as L2
import Gibbon.Language.Syntax as S
import AST

-- class ConvertToL2AST a where
--     convertToL2AST :: a -> b

-- instance ConvertToL2AST Program where
--     convertToL2AST (Program dataTypeDecls funcDecls expr) =
--         L2Program (map convertToL2AST dataTypeDecls) (map convertToL2AST funcDecls) (convertToL2AST expr)

-- instance ConvertToL2AST DataTypeDecl where
--     convertToL2AST (DataTypeDecl typeCon dataFields) =

convertToL2AST :: Program -> L2.Prog2
convertToL2AST (Program dataTypeDecls funcDecls expr) =
    -- L2.Prog2 (convertDataTypeDecls dataTypeDecls) (map convertFuncDecl funcDecls) (convertExpr expr)
    L2.Prog2 (convertDataTypeDecls dataTypeDecls) [] (convertExpr expr)

convertDataTypeDecls :: DataTypeDecls -> S.DDefs L2.Ty2
convertDataTypeDecls (DataTypeDecls decls) = C.fromListDD (map convertDataTypeDecl decls)

convertDataTypeDecl :: DataTypeDecl -> S.DDef L2.Ty2
convertDataTypeDecl (DataTypeDecl (TypeCon typeCon) (DataFields dataFields)) =
    S.DDef (convertTypeCon typeCon) [] (convertDataFields dataFields) S.Linear

convertDataFields :: DataFields -> [(C.DataCon, [(S.IsBoxed, L2.Ty2)])]
convertDataFields (DataFields dataFields) =
    map convertDataField dataFields

convertDataField :: DataField -> (C.DataCon, [(S.IsBoxed, L2.Ty2)])
convertDataField (DataField (DataCon dataCon) (CombinedTypeCons combinedTypeCons)) =
    (convertDataCon dataCon, convertCombinedTypeCons combinedTypeCons)

    where convertCombinedTypeCons :: CombinedTypeCons -> [(S.IsBoxed, L2.Ty2)]
          convertCombinedTypeCons (CombinedTypeCons ts) = map convertCombinedTypeCon ts

          convertCombinedTypeCon :: CombinedTypeCon -> (S.IsBoxed, L2.Ty2)
          convertCombinedTypeCon (CTCTypeCon (TypeCon tc)) = (S.Boxed, S.PackedTy (convertTypeCon tc) (C.Single "l"))
        --   May switch to just setting to False later
          convertCombinedTypeCon (CTCBase baseType) = (S.Unboxed, convertBaseType baseType)

convertBaseType :: BaseType -> L2.Ty2
convertBaseType Int    = S.IntTy
convertBaseType Float  = S.FloatTy
convertBaseType Bool   = S.BoolTy
-- convertBaseType String = S.StringTy   Unsure how to deal with this for now

-- TODO continue this
convertExpr :: Expr -> L2.PreExp L2.E2Ext L2.LocVar L2.Ty2
convertExpr (ExprVal val) = convertVal val
convertExpr (ExprBinOp binOp e1 e2) =
    L2.PrimAppE (convertBinOp binOp) [convertExpr e1, convertExpr e2]

convertVal :: Val -> L2.PreExp L2.E2Ext L2.LocVar L2.Ty2
convertVal (ValVar (Var v)) = L2.VarE (C.toVar v)
convertVal (ValLit lit) = convertLit lit

convertBinOp :: BinOp -> C.Prim2


convertTypeCon :: TypeCon -> C.Var
convertTypeCon (TypeCon typeCon) = C.toVar typeCon

convertDataCon :: DataCon -> C.DataCon
convertDataCon (DataCon dataCon) = dataCon
