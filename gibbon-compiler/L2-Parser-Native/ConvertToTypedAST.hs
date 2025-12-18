-- {-# LANGUAGE StandaloneDeriving #-}
module ConvertToTypedAST where

import AST
import Helper
-- import Gibbon.Language.Syntax as S
import Control.Monad (foldM)
import Control.Monad.Reader
import qualified Data.Map as M
import System.Environment (getEnv)

type TyEnv a b = M.Map a b

emptyTyEnv :: TyEnv a b
emptyTyEnv = M.empty

data MyEnv = MyEnv   { dcEnv :: TyEnv String ([MyTy], MyTy) -- Data constructor maps to type constructors and result type
                     , vEnv  :: TyEnv String MyTy
                     , fEnv  :: TyEnv String ([MyTy], MyTy) -- Function maps to argument types and return type
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

data EnvType
    = EnvDataCon
    | EnvVar
    | EnvFunc
    deriving (Show, Eq)

-- Lookup Functions
lookupVar :: String -> InferM MyTy 
lookupVar v = do
    env <- asks vEnv
    case M.lookup v env of
        Just ty -> return ty
        Nothing -> lift . Failed $ "Variable " ++ show v ++ " not found in environment"

lookupDataCon :: String -> InferM ([MyTy], MyTy)
lookupDataCon dc = do
    env <- asks dcEnv
    case M.lookup dc env of
        Just ty -> return ty
        Nothing -> lift . Failed $ "Data constructor " ++ show dc ++ " not found in environment"

lookupFunc :: String -> InferM ([MyTy], MyTy)
lookupFunc fv = do
    env <- asks fEnv
    case M.lookup fv env of
        Just ty -> return ty
        Nothing -> lift . Failed $ "Function " ++ show fv ++ " not found in environment"

-- Extension Functions
extendDataConEnv :: String -> ([MyTy], MyTy) -> InferM a -> InferM a
extendDataConEnv dc ty = local (\env -> env { dcEnv = M.insert dc ty (dcEnv env) })

extendVEnv :: String -> MyTy -> InferM a -> InferM a
extendVEnv v ty = local (\env -> env { vEnv = M.insert v ty (vEnv env) })

extendVEnvs :: [(String, MyTy)] -> InferM a -> InferM a
extendVEnvs [] action = action
extendVEnvs ((v, ty):rest) action = extendVEnv v ty (extendVEnvs rest action)

extendFEnv :: String -> ([MyTy], MyTy) -> InferM a -> InferM a
extendFEnv fv ty = local (\env -> env { fEnv = M.insert fv ty (fEnv env) })

getEnvType :: String -> InferM (Maybe EnvType)
getEnvType v = do
    env <- asks vEnv
    case M.lookup v env of
        Just _ -> return $ Just EnvVar
        Nothing -> do
            envF <- asks fEnv
            case M.lookup v envF of
                Just _ -> return $ Just EnvFunc
                Nothing -> do
                    envDC <- asks dcEnv
                    case M.lookup v envDC of
                        Just _ -> return $ Just EnvDataCon
                        Nothing -> return Nothing

checkVarNameExists :: String -> InferM Bool
checkVarNameExists v = do
    envType <- getEnvType v
    case envType of
        Just _  -> return True
        Nothing -> return False

-- Helper Construction Functions
createTypedNode :: MyTy -> a -> TypedNode a
createTypedNode ty node = TypedNode { tType = ty, tNode = node }

emptyEnv :: MyEnv
emptyEnv = MyEnv emptyTyEnv emptyTyEnv emptyTyEnv

-- Program Type Inference
inferProgram :: Program -> E (TypedNode Program)
inferProgram (Program dataDecls funcDecls@(FuncDecls funcs) mainExpr) = do
    env1 <- loadDataDecls dataDecls emptyEnv
    env2 <- loadFuncDecls funcDecls env1
    -- print dataTypeEnv
    -- env2 <- createTypedNode IntTy (Program dataDecls funcDecls mainExpr)
    runReaderT (do
        typedMain <- inferExpr mainExpr
        typedFuncs <- mapM inferFunc funcs
        return $ createTypedNode (tType typedMain) (Program dataDecls (FuncDecls (map tNode typedFuncs)) (tNode typedMain))
        ) env2
    
    -- Failed (show env1) -- temporary to see env1

-- TODO need to handle location regions
-- We assume function declarations already loaded
inferFunc :: FuncDecl -> InferM (TypedNode FuncDecl)
inferFunc func@(FuncDecl (FuncVar funcVar) typeScheme funcVar2 locRegions v@(Vars vars) expr) = do
    -- case lookupFunc funcVar of 
    --     Nothing -> lift . Failed $ "inferFunc: Function " ++ show funcVar ++ " not found in environment"
    (argTys, retTy) <- lookupFunc funcVar
    -- -- this should never happen, but for completeness, I include it
    -- if length argTys /= length vars
    --     then lift . Failed $ "inferFunc: Argument length mismatch in function " ++ show funcVar
    --     else do
    let varsNames = map (\(Var vname) -> vname) vars
        varTyPairs = zip varsNames argTys
    typeExpr <- extendVEnvs varTyPairs (inferExpr expr) 
    if tType typeExpr == retTy
        then return $ createTypedNode retTy func
        else lift . Failed $ "inferFunc: Return type mismatch in function " ++ show funcVar ++ ": expected " ++ show retTy ++ ", got " ++ show (tType typeExpr)
                    
                    -- >>= \typedExpr -> 
                    --     if tType typedExpr == retTy
                    --         then return $ createTypedNode retTy (FuncDecl funcVar typeScheme funcVar2 locRegions vars (tNode typedExpr))
                    --         else lift . Failed $ "inferFunc: Return type mismatch in function " ++ show funcVar ++ ": expected " ++ show retTy ++ ", got " ++ show (tType typedExpr)
    

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
    iFuncApp@(ExprFuncApp (FuncVar funcVar) locRegions (Exprs exprs)) -> do
        typedExprs <- mapM inferExpr exprs
        (fArgTypes, retTy) <- lookupFunc funcVar 
        let argTys = map tType typedExprs
            -- retTy = snd funcTy
            -- fArgTypes = fst funcTy
        if argTys == fArgTypes
            then return $ createTypedNode retTy iFuncApp
            else lift . Failed $ "Type mismatch in function application: " ++ show argTys ++ " vs " ++ show fArgTypes
    -- TODO deal with location region stuff
    iDataConApp@(ExprDataConApp (DataCon dataCon) locRegion (Exprs exprs)) -> do
        typedExprs <- mapM inferExpr exprs
        (fieldTys, resTy) <- lookupDataCon dataCon
        let argTys = map tType typedExprs
        if argTys == fieldTys
            then return $ createTypedNode resTy iDataConApp
            else lift . Failed $ "Type mismatch in data constructor application: " ++ show argTys ++ " vs " ++ show fieldTys
    iCase@(ExprCase val p@(Pats pats)) -> do
        typedVal <- inferVal val
        typedPats <- mapM (inferPat (tType typedVal)) pats
        let patTypes = map tType typedPats
            typedPat = safeHead typedPats
        
        if checkAllSame patTypes 
            then case typedPat of 
                Ok tp -> return $ createTypedNode (tType tp) iCase
                Failed e -> lift $ Failed e
            else lift . Failed $ "Type mismatch in case patterns: " ++ show patTypes

        -- [X]TODO infer patterns and ensure types match
        -- For now, just return the type of the value being matched on
    iLet@(ExprLet (Var var) combinedType expr1 expr2) -> do        
        typedExpr1 <- inferExpr expr1
        let typedCombinedType = combinedTToType combinedType
            expr1Type = tType typedExpr1
        typedExpr2 <- extendVEnv var expr1Type (inferExpr expr2) 
        if typedCombinedType == expr1Type
            then return $ createTypedNode (tType typedExpr2) iLet
            else lift . Failed $ "Type mismatch in let binding: " ++ show typedCombinedType ++ " vs " ++ show expr1Type
    
    -- TODO deal with letloc and letregion
        -- return $ createTypedNode (tType typedExpr2) iLet

    _ -> lift . Failed $ "inferExpr: Not implemented for this expression type: " ++ takeAlphaNum (show expr)


-- TODO check if val should be var???  also need to deal with location regions
inferPatMatch :: PatMatch -> InferM (TypedNode PatMatch)
inferPatMatch pm@(PatMatch (ValVar (Var var)) (LocatedType combinedLocType locRegion)) = do
    -- BUG HERE: val doesn't have type, but need to make sure variable not in scope
    -- typedVal <- inferVal val
    varExists <- checkVarNameExists var
    if varExists
        then lift . Failed $ "inferPatMatch: Variable " ++ show var ++ " already defined in environment"
        else do
            -- let typedVal = createTypedNode (combinedLocToType combinedLocType) (ValVar (AST.Var var))
            let ty = combinedLocToType combinedLocType
            -- if tType typedVal == ty
            return $ createTypedNode ty pm
                -- else lift . Failed $ "inferPatMatch: Type mismatch in pattern match: " ++ show (tType typedVal) ++ " vs " ++ show ty
inferPatMatch (PatMatch (ValLit val) _) = do 
    lift . Failed $ "inferPatMatch: Requires variable for pattern match, received literal: " ++ show val

inferPat :: MyTy -> Pat -> InferM (TypedNode Pat)
inferPat myTy p@(Pat (DataCon dataCon) (PatMatches patMatches) expr) = do
    matchedTypes <- mapM inferPatMatch patMatches
    let argTypes = map tType matchedTypes
    (typeCons, result) <- lookupDataCon dataCon
    
    -- check to make sure actual type constructors match expected
    if argTypes /= typeCons
        then lift . Failed $ "inferPat: Type mismatch in pattern for data constructor " ++ show dataCon ++ ": " ++ show argTypes ++ " vs " ++ show typeCons
        else do
            -- check to make sure result type is correct
            if result /= myTy
                then lift . Failed $ "inferPat: Result type mismatch in pattern for data constructor " ++ show dataCon ++ ": " ++ show result ++ " vs " ++ show myTy
                else do
                    -- TODO need to change PatMatch to have var, not val
                    let varTypePairs = map (\(PatMatch (ValVar (Var v)) (LocatedType combinedLocType locRegion)) -> (v, combinedLocToType combinedLocType)) patMatches
                    typedExpr <- extendVEnvs varTypePairs (inferExpr expr)
                    return $ createTypedNode (tType typedExpr) p
    -- --
    -- let varTyPairs = map (\(TypedNode ty (PatMatch val _)) -> (val, ty)) matchedTypes
    -- patExpr <- extendVEnv

    -- where
        -- getPatMatchPairs :: PatMatches -> [(Val, MyTy)]
        -- getPatMatchPairs (PatMatches []) = []
        -- getPatMatchPairs (PatMatches (pm@(PatMatch val (LocatedType combinedLocType locRegion)):rest)) =
        --     let ty = combinedLocToType combinedLocType
        --     in (val, ty) : getPatMatchPairs (PatMatches rest)


inferVal :: Val -> InferM (TypedNode Val)
inferVal val = case val of 
    iValVar@(ValVar (AST.Var v)) -> do
        ty <- lookupVar v
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
        loadCon env2 ((DataCon dataCon), (fieldTys, resTy)) 
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
            let ((FuncVar funcVar), (argTys, retTy)) = extractFuncDecls (FuncDecls [decl]) !! 0
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

combinedLocToType :: CombinedLocType -> MyTy
combinedLocToType (CLTTypeCon tc) = PackedTy tc
combinedLocToType (CLTBase baseType) = convertBaseType baseType    
-- convertToTypedAST :: Program -> S.Prog S.Ty2
