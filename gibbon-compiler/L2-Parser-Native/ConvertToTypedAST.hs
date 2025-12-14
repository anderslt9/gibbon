-- {-# LANGUAGE StandaloneDeriving #-}
module ConvertToTypedAST where

import AST
import Helper (E(..), takeAlphaNum)
-- import Gibbon.Language.Syntax as S
import Control.Monad (foldM)
import Control.Monad.Reader
import qualified Data.Map as M

type TyEnv a b = M.Map a b

emptyTyEnv :: TyEnv a b
emptyTyEnv = M.empty

data MyEnv = MyEnv   { dcEnv :: TyEnv DataCon ([MyTy], MyTy) -- Data constructor maps to type constructors and result type
                     , vEnv  :: TyEnv AST.Var MyTy
                     , fEnv  :: TyEnv FuncVar ([MyTy], MyTy) -- Function maps to argument types and return type
                     } deriving Show

type InferM = ReaderT MyEnv E

data TypedNode a = TypedNode
    { tType :: MyTy
    , tNode :: a 
    } deriving Show

-- all types
data MyTy 
    = IntTy
    | FloatTy
    | BoolTy
    | StringTy
    | PackedTy TypeCon
    deriving (Show, Eq, Ord)

-- deriving instance Show MyTy


-- Lookup Functions
lookupVar :: AST.Var -> InferM MyTy 
lookupVar v = do
    env <- asks vEnv
    case M.lookup v env of
        Just ty -> return ty
        Nothing -> lift . Failed $ "Variable " ++ show v ++ " not found in environment"

lookupDataCon :: DataCon -> InferM ([MyTy], MyTy)
lookupDataCon dc = do
    env <- asks dcEnv
    case M.lookup dc env of
        Just ty -> return ty
        Nothing -> lift . Failed $ "Data constructor " ++ show dc ++ " not found in environment"

lookupFunc :: FuncVar -> InferM ([MyTy], MyTy)
lookupFunc fv = do
    env <- asks fEnv
    case M.lookup fv env of
        Just ty -> return ty
        Nothing -> lift . Failed $ "Function " ++ show fv ++ " not found in environment"

-- Extension Functions
extendDataConEnv :: DataCon -> ([MyTy], MyTy) -> InferM a -> InferM a
extendDataConEnv dc ty = local (\env -> env { dcEnv = M.insert dc ty (dcEnv env) })

extendVEnv :: AST.Var -> MyTy -> InferM a -> InferM a
extendVEnv v ty = local (\env -> env { vEnv = M.insert v ty (vEnv env) })

extendFEnv :: FuncVar -> ([MyTy], MyTy) -> InferM a -> InferM a
extendFEnv fv ty = local (\env -> env { fEnv = M.insert fv ty (fEnv env) })

-- Helper Construction Functions
createTypedNode :: MyTy -> a -> TypedNode a
createTypedNode ty node = TypedNode { tType = ty, tNode = node }

emptyEnv :: MyEnv
emptyEnv = MyEnv emptyTyEnv emptyTyEnv emptyTyEnv

-- Program Type Inference
inferProgram :: Program -> E (TypedNode Expr)
inferProgram (Program dataDecls funcDecls mainExpr) = do
    env1 <- loadDataDecls dataDecls emptyEnv
    env2 <- loadFuncDecls funcDecls env1
    -- print dataTypeEnv
    -- env2 <- createTypedNode IntTy (Program dataDecls funcDecls mainExpr)
    runReaderT (inferExpr mainExpr) env2
    
    -- Failed (show env1) -- temporary to see env1

inferExpr :: Expr -> InferM (TypedNode Expr)
inferExpr expr = case expr of 
    iVal@(ExprVal val) -> do
        typedVal <- inferVal val
        return $ createTypedNode (tType typedVal) iVal
    iBinApp@(ExprBinOp binOp e1 e2) -> do
        typedE1 <- inferExpr e1
        typedE2 <- inferExpr e2
        -- TODO
        -- Can extend to support type promotion later
        -- Also, need to ensure operator is valid for types
        if tType typedE1 == tType typedE2
            then return $ createTypedNode (tType typedE1) iBinApp
            else lift . Failed $ "Type mismatch in binary operation: " ++ show (tType typedE1) ++ " vs " ++ show (tType typedE2)
    -- TODO deal with location region stuff (may consider type promotion too)
    iFuncApp@(ExprFuncApp funcVar locRegions (Exprs exprs)) -> do
        typedExprs <- mapM inferExpr exprs
        (fArgTypes, retTy) <- lookupFunc funcVar 
        let argTys = map tType typedExprs
            -- retTy = snd funcTy
            -- fArgTypes = fst funcTy
        if argTys == fArgTypes
            then return $ createTypedNode retTy iFuncApp
            else lift . Failed $ "Type mismatch in function application: " ++ show argTys ++ " vs " ++ show fArgTypes
    -- TODO deal with location region stuff
    iDataConApp@(ExprDataConApp dataCon locRegion (Exprs exprs)) -> do
        typedExprs <- mapM inferExpr exprs
        (fieldTys, resTy) <- lookupDataCon dataCon
        let argTys = map tType typedExprs
        if argTys == fieldTys
            then return $ createTypedNode resTy iDataConApp
            else lift . Failed $ "Type mismatch in data constructor application: " ++ show argTys ++ " vs " ++ show fieldTys
    iCase@(ExprCase val pats) -> do
        typedVal <- inferVal val
        -- TODO infer patterns and ensure types match
        -- For now, just return the type of the value being matched on
        return $ createTypedNode (tType typedVal) iCase

    _ -> lift . Failed $ "inferExpr: Not implemented for this expression type: " ++ takeAlphaNum (show expr)



inferVal :: Val -> InferM (TypedNode Val)
inferVal val = case val of 
    iValVar@(ValVar (AST.Var v)) -> do
        ty <- lookupVar (AST.Var v)
        return $ createTypedNode ty iValVar
    iValLit@(ValLit n) -> do
        typedLit <- inferLit n
        return $ createTypedNode (tType typedLit) iValLit
    _ -> lift . Failed $ "inferVal: Not implemented for this value type"

inferLit :: Lit -> InferM (TypedNode Lit)
inferLit lit = case lit of 
    node@(IntLit _) -> return $ createTypedNode IntTy node
    node@(FloatLit _) -> return $ createTypedNode FloatTy node
    node@(BoolLit _) -> return $ createTypedNode BoolTy node
    node@(StringLit _) -> return $ createTypedNode StringTy node
    _ -> lift . Failed $ "inferLit: Not implemented for this literal type"

-- Data Declaration Loading
extractDataCons :: DataTypeDecl -> [(DataCon, ([MyTy], MyTy))]
extractDataCons (DataTypeDecl typeCon (DataFields dataFields)) = map extractDataField dataFields
    where
        extractDataField :: DataField -> (DataCon, ([MyTy], MyTy))
        extractDataField (DataField dataCon (CombinedTypeCons combinedTypeCons)) =
            (dataCon, (map combinedTCToType combinedTypeCons, PackedTy typeCon))

loadDataDecls :: DataTypeDecls -> MyEnv -> E MyEnv
loadDataDecls (DataTypeDecls decls) env = foldM loadDataDecl env decls
    where
        loadDataDecl :: MyEnv -> DataTypeDecl -> E MyEnv
        loadDataDecl env2 decl = foldM loadCon env2 (extractDataCons decl)

        loadCon :: MyEnv -> (DataCon, ([MyTy], MyTy)) -> E MyEnv
        loadCon env2 (dataCon, (fieldTys, resTy)) 
            | M.member dataCon (dcEnv env2) = 
                Failed $ "Data constructor " ++ show dataCon ++ " already defined in environment"
            | otherwise = Ok env2 { dcEnv = M.insert dataCon (fieldTys, resTy) (dcEnv env2) }

-- Function Declaration Loading
extractFuncDecls :: FuncDecls -> [(FuncVar, ([MyTy], MyTy))]
extractFuncDecls (FuncDecls decls) = map extractFuncDecl decls
    where
        -- TODO deal with location regions
        extractFuncDecl :: FuncDecl -> (FuncVar, ([MyTy], MyTy))
        extractFuncDecl (FuncDecl funcVar1 (TypeScheme combinedTypes) funcVar2 locRegions vars expr) = 
            -- GET ARG TYPES AND RETURN TYPE FROM TYPESCHEME, ignore everything else
            case separateArgs combinedTypes of
                Nothing -> error $ "extractFuncDecl: Function " ++ show funcVar1 ++ " has invalid type scheme"
                Just (argTys, retTy) -> (funcVar1, (argTys, retTy))
        
        separateArgs :: CombinedTypes -> Maybe ([MyTy], MyTy)
        separateArgs (CombinedTypes []) = Nothing
        separateArgs (CombinedTypes [x]) = Just ([], combinedTToType x)
        separateArgs (CombinedTypes (x:xs)) = case separateArgs (CombinedTypes xs) of
            Nothing -> Nothing
            Just (argTys, retTy) -> Just (combinedTToType x : argTys, retTy)

loadFuncDecls :: FuncDecls -> MyEnv -> E MyEnv
loadFuncDecls (FuncDecls decls) env = foldM loadFuncDecl env decls
    where
        loadFuncDecl :: MyEnv -> FuncDecl -> E MyEnv
        loadFuncDecl env2 decl = 
            let (funcVar, (argTys, retTy)) = extractFuncDecls (FuncDecls [decl]) !! 0
            in if M.member funcVar (fEnv env2)
                then Failed $ "Function " ++ show funcVar ++ " already defined in environment"
                else Ok env2 { fEnv = M.insert funcVar (argTys, retTy) (fEnv env2) }


-- loadDataDecls :: DataTypeDecls -> InferM Type
-- loadDataDecls (DataTypeDecls decls) = do
    
--     mapM loadDataDecl decls

-- loadDataDecl :: DataTypeDecl -> InferM Type
-- loadDataDecl (DataTypeDecl typeCon dataFields) = do
--     mapM loadDataField dataFields

--     where loadDataField :: DataField -> InferM Type
--           loadDataField (DataField dataCon combinedTypeCons) = do
--             extendDataConEnv dataCon (getTypeFromCombinedTypeCon <$> combinedTypeCons) $ \ty -> do
--                   return ()

--           getTypeFromCombinedTypeCon :: CombinedTypeCon -> Type
--           getTypeFromCombinedTypeCon (CTCTypeCon tc) = PackedTy tc
--           getTypeFromCombinedTypeCon (CTCBase baseType) = convertBaseType baseType 

-- Type Conversion Helpers
convertBaseType :: BaseType -> MyTy
convertBaseType Int = IntTy
convertBaseType Float = FloatTy
convertBaseType Bool = BoolTy
convertBaseType String = StringTy

combinedTCToType :: CombinedTypeCon -> MyTy
combinedTCToType (CTCTypeCon tc) = PackedTy tc
combinedTCToType (CTCBase baseType) = convertBaseType baseType

combinedTToType :: CombinedType -> MyTy
combinedTToType (CTLocated (LocatedType combinedLocType locRegion)) = case combinedLocType of
    CLTTypeCon tc -> PackedTy tc
    CLTBase baseType -> convertBaseType baseType
combinedTToType (CTBase baseType) = convertBaseType baseType


-- convertToTypedAST :: Program -> S.Prog S.Ty2
