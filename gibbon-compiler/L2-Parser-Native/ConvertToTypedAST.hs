module ConvertToTypedAST where

import AST
-- import Gibbon.Language.Syntax as S
import Control.Monad.Reader
import qualified Data.Map as M

type TyEnv a b = M.Map a b

emptyTyEnv :: TyEnv a b
emptyTyEnv = M.empty

data MyEnv = MyEnv   { dcEnv :: TyEnv DataCon ([Type], Type) 
                     , vEnv  :: TyEnv AST.Var Type
                     , fEnv  :: TyEnv FuncVar Type
                     }

type InferM = ReaderT MyEnv (Either String)

data TypedNode a = TypedNode
    { tType :: Type 
    , tNode :: a 
    } deriving Show

-- Lookup Functions
lookupVar :: AST.Var -> InferM Type 
lookupVar v = do
    env <- asks vEnv
    case Map.lookup v env of
        Just ty -> return ty
        Nothing -> lift . Left $ "Variable " ++ show v ++ " not found in environment"

lookupDataCon :: DataCon -> InferM ([Type], Type)
lookupDataCon dc = do
    env <- asks dcEnv
    case Map.lookup dc env of
        Just (fieldTys, resTy) -> return (fieldTys, resTy)
        Nothing -> lift . Left $ "Data constructor " ++ show dc ++ " not found in environment"

lookupFunc :: FuncVar -> InferM Type
lookupFunc fv = do
    env <- asks fEnv
    case Map.lookup fv env of
        Just ty -> return ty
        Nothing -> lift . Left $ "Function " ++ show fv ++ " not found in environment"

-- Extension Functions
extendDataConEnv :: DataCon -> ([Type], Type) -> InferM a -> InferM a
extendDataConEnv dc ty = local (\env -> env { dcEnv = M.insert dc ty (dcEnv env) })

extendVEnv :: AST.Var -> Type -> InferM a -> InferM a
extendVEnv v ty = local (\env -> env { vEnv = M.insert v ty (vEnv env) })

extendFEnv :: FuncVar -> Type -> InferM a -> InferM a
extendFEnv fv ty = local (\env -> env { fEnv = M.insert fv ty (fEnv env) })

-- Helper Construction Functions
createTypedNode :: Type -> a -> TypedNode a
createTypedNode ty node = TypedNode { tType = ty, tNode = node }

emptyEnv :: MyEnv
emptyEnv = MyEnv emptyTyEnv emptyTyEnv emptyTyEnv

-- Program Type Inference
inferProgram :: Program -> Either String (TypedNode Program)
inferProgram (Program dataDecls funcDecls mainExpr) = 
    runReaderT (do
        dataTypeEnv <- loadDataDecls dataDecls
    
    ) emptyEnv


loadDataDecls :: DataTypeDecls -> InferM Type
loadDataDecls (DataTypeDecls decls) = do
    mapM loadDataDecl decls

loadDataDecl :: DataTypeDecl -> InferM Type
loadDataDecl (DataTypeDecl typeCon dataFields) = do
    mapM loadDataField dataFields

    where loadDataField :: DataField -> InferM Type
          loadDataField (DataField dataCon combinedTypeCons) = do
            extendDataConEnv dataCon (getTypeFromCombinedTypeCon <$> combinedTypeCons) $ \ty -> do
                  return ()

          getTypeFromCombinedTypeCon :: CombinedTypeCon -> Type
          getTypeFromCombinedTypeCon (CTCTypeCon tc) = S.PackedTy (convertTypeCon tc) (C.Single "l")
          getTypeFromCombinedTypeCon (CTCBase baseType) = convertBaseType baseType 



-- convertToTypedAST :: Program -> S.Prog S.Ty2
