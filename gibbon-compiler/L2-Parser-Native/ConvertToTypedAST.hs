-- {-# LANGUAGE StandaloneDeriving #-}
module ConvertToTypedAST where

import AST
import Helper
-- import Gibbon.Language.Syntax as S
import Control.Monad (foldM)
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Gibbon.Common as C
-- import System.Environment (getEnv)

-- This conversion does the following transformations:
-- 1. Annotates each node in the AST with its type (using MyTy)
-- 2. Ensures that all expressions are well-typed according to the type environment



type TyEnv a b = M.Map a b

emptyTyEnv :: TyEnv a b
emptyTyEnv = M.empty

data MyEnv a = MyEnv { dcEnv  :: TyEnv String ([MyTy a], MyTy a) -- Data constructor maps to type constructors and result type
                     , vEnv   :: TyEnv String (MyTy a)
                     , fEnv   :: TyEnv String (FuncInfo a) -- Function maps to argument types and return type
                     , locEnv :: TyEnv String (LocInfo a) -- Location variable maps to (type, region variable)
                     , regEnv :: TyEnv String RegInfo -- Region variable maps to type
                     } deriving Show

data FuncInfo a = FuncInfo
    { funcArgTypes :: [MyTy a]
    , funcRetType  :: MyTy a
    , funcInside   :: Bool
    } deriving Show

-- TODO maybe rework tuples into this style
data RegInfo = RegInfo
    { regStarted    :: Bool
    , regLocsUsed   :: S.Set String
    } deriving Show

data LocInfo a = LocInfo
    { getLocType :: MyTy a
    , locInitialized    :: Bool
    , getRegionFromLoc  :: String
    } deriving Show

type InferM a = ReaderT (MyEnv a) E

data TypedNode a b = TypedNode
    { tType :: MyTy a
    , tNode :: b
    } deriving Show

-- all types
data MyTy loc
    = IntTy
    | FloatTy
    | BoolTy
    | StringTy
    | PackedTy TypeCon loc -- loc (I will add loc at some point, but it will break a lot right now)
    | NoneTy
    deriving (Show, Ord)

instance Eq a => Eq (MyTy a) where
    (==) (PackedTy tc1 loc1) (PackedTy tc2 loc2) =
        tc1 == tc2 && loc1 == loc2
    (==) IntTy IntTy = True
    (==) FloatTy FloatTy = True
    (==) BoolTy BoolTy = True
    (==) StringTy StringTy = True
    (==) NoneTy NoneTy = True
    (==) _ _ = False

-- instance Eq a => Eq [MyTy a] where


-- emptyLocRegion :: LocRegion
-- emptyLocRegion = LocRegion (LocVar "") (RegionVar "") (IndexVar "")

data EnvType
    = EnvDataCon
    | EnvVar
    | EnvFunc
    | EnvLoc 
    | EnvReg
    deriving (Show, Eq)


-- Lookup Functions
lookupByVal :: (Eq a, Show a) => MyTy a -> InferM a String
lookupByVal val = do
    env <- asks vEnv
    case M.toList (M.filter (==^^ val) env) of
        ((k, _):_) -> return k
        []         -> lift . Failed $ "Value " ++ show val ++ " not found in environment"

lookupVar :: String -> InferM a (MyTy a)
lookupVar v = do
    env <- asks vEnv
    case M.lookup v env of
        Just ty -> return ty
        Nothing -> lift . Failed $ "Variable " ++ show v ++ " not found in environment"

lookupDataCon :: String -> InferM a ([MyTy a], MyTy a)
lookupDataCon dc = do
    env <- asks dcEnv
    case M.lookup dc env of
        Just ty -> return ty
        Nothing -> lift . Failed $ "Data constructor " ++ show dc ++ " not found in environment"

lookupModDataCon :: String -> LocRegion -> InferM LocRegion ([MyTy LocRegion], MyTy LocRegion)
lookupModDataCon dc loc = do
    env <- asks dcEnv
    case M.lookup dc env of
        Just (argTys, resTy) -> do
            let newResTy = case resTy of
                    PackedTy tc _ -> PackedTy tc loc
                    _             -> resTy
            return (argTys, newResTy)
        Nothing -> lift . Failed $ "Data constructor " ++ show dc ++ " not found in environment"

lookupFunc :: String -> InferM a (FuncInfo a)
lookupFunc fv = do
    env <- asks fEnv
    case M.lookup fv env of
        Just ty -> return ty
        Nothing -> lift . Failed $ "Function " ++ show fv ++ " not found in environment"

lookupLoc :: String -> InferM a (LocInfo a)
lookupLoc lv = do
    env <- asks locEnv
    case M.lookup lv env of
        Just ty -> return ty
        Nothing -> lift . Failed $ "Location variable " ++ show lv ++ " not found in environment"

lookupReg :: String -> InferM a RegInfo
lookupReg rv = do
    env <- asks regEnv
    case M.lookup rv env of
        Just ty -> return ty
        Nothing -> lift . Failed $ "Region variable " ++ show rv ++ " not found in environment"

-- Extension Functions
-- TODO go through and make sure variable not already in environment for all additions to environment
extendDataConEnv :: String -> ([MyTy a], MyTy a) -> (InferM a) b -> (InferM a) b
extendDataConEnv dc ty = local (\env -> env { dcEnv = M.insert dc ty (dcEnv env) })

extendVEnv :: String -> MyTy a -> (InferM a) b -> (InferM a) b
extendVEnv v ty = local (\env -> env { vEnv = M.insert v ty (vEnv env) })

extendVEnvs :: [(String, MyTy a)] -> (InferM a) b -> (InferM a) b
extendVEnvs [] action = action
extendVEnvs ((v, ty):rest) action = extendVEnv v ty (extendVEnvs rest action)

extendFEnv :: String -> FuncInfo a -> (InferM a) b -> (InferM a) b
extendFEnv fv ty = local (\env -> env { fEnv = M.insert fv ty (fEnv env) })

setAsCurrentFunc :: String -> (InferM a) b -> (InferM a) b
setAsCurrentFunc fv = local (\env -> 
    case M.lookup fv (fEnv env) of
        Just funcInfo -> let newFuncInfo = funcInfo { funcInside = True }
                         in env { fEnv = M.insert fv newFuncInfo (fEnv env) }
        Nothing -> env -- should never happen, but just in case
    )

extendLocEnv :: String -> LocInfo a -> (InferM a) b -> (InferM a) b
extendLocEnv lv ty = local (\env -> env { locEnv = M.insert lv ty (locEnv env) })

-- extendLocEnv :: String -> LocInfo -> InferM a -> InferM a
-- extendLocEnv lv ty = do
--     regionEnv
--     let reg = getRegionFromLoc ty
--     return $ addRegEnvLoc reg RegEnv (RegInfo { regStarted = False, regLocsUsed = S.empty }) (extendLocEnvOnly lv ty)

addRegEnvLoc :: String -> RegInfo -> String -> (InferM a) b -> (InferM a) b
addRegEnvLoc rv (RegInfo started locsUsed) newLoc = local (\env -> 
    let newLocs = S.insert newLoc locsUsed
        newRegInfo = RegInfo { regStarted = started, regLocsUsed = newLocs }
    in env { regEnv = M.insert rv newRegInfo (regEnv env) }
    )

extendRegEnv :: String -> RegInfo -> (InferM a) b -> (InferM a) b
extendRegEnv rv ty = local (\env -> env { regEnv = M.insert rv ty (regEnv env) })

getEnvType :: String -> (InferM a) (Maybe EnvType)
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
                        Nothing -> do
                            envL <- asks locEnv
                            case M.lookup v envL of
                                Just _ -> return $ Just EnvLoc
                                Nothing -> do
                                    envR <- asks regEnv
                                    case M.lookup v envR of
                                        Just _ -> return $ Just EnvReg
                                        Nothing -> return Nothing

checkVarNameExists :: String -> (InferM a) Bool
checkVarNameExists v = do
    envType <- getEnvType v
    case envType of
        Just _  -> return True
        Nothing -> return False

-- compare only by name of type
(==^^) :: (Eq a) => MyTy a -> MyTy a -> Bool
(==^^) (PackedTy (TypeCon tc1) _) (PackedTy (TypeCon tc2) _) =
    tc1 == tc2
(==^^) ty1 ty2 = ty1 == ty2

-- Helper Construction Functions
createTypedNode :: MyTy LocRegion -> b -> (TypedNode LocRegion) b
createTypedNode ty node = TypedNode { tType = ty, tNode = node }

emptyEnv :: MyEnv LocRegion
emptyEnv = MyEnv emptyTyEnv emptyTyEnv emptyTyEnv emptyTyEnv emptyTyEnv


----------------------------------------------------------------------------------------------
-- Program Type Inference
inferProgram :: Program -> E ((TypedNode LocRegion) Program)
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
inferFunc :: FuncDecl -> (InferM LocRegion) ((TypedNode LocRegion) FuncDecl)
inferFunc (FuncDecl (FuncVar funcVar) typeScheme funcVar2 locRegions v@(Vars vars) expr) = do
    -- case lookupFunc funcVar of 
    --     Nothing -> lift . Failed $ "inferFunc: Function " ++ show funcVar ++ " not found in environment"
    funcInfo <- lookupFunc funcVar
    -- -- this should never happen, but for completeness, I include it
    -- if length argTys /= length vars
    --     then lift . Failed $ "inferFunc: Argument length mismatch in function " ++ show funcVar
    --     else do
    let varsNames = map (\(Var vname) -> vname) vars
        varTyPairs = zip varsNames (funcArgTypes funcInfo)
    typeExpr <- setAsCurrentFunc funcVar (extendVEnvs varTyPairs (inferExpr expr)) 
    let newIFunc = FuncDecl (FuncVar funcVar) typeScheme funcVar2 locRegions v (tNode typeExpr)
    
    if tType typeExpr ==^^ funcRetType funcInfo
    then return $ createTypedNode (funcRetType funcInfo) newIFunc
    else lift . Failed $ "inferFunc: Return type mismatch in function " ++ show funcVar ++ ": expected " ++ show (funcRetType funcInfo) ++ ", got " ++ show (tType typeExpr)

                    -- >>= \typedExpr -> 
                    --     if tType typedExpr == retTy
                    --         then return $ createTypedNode retTy (FuncDecl funcVar typeScheme funcVar2 locRegions vars (tNode typedExpr))
                    --         else lift . Failed $ "inferFunc: Return type mismatch in function " ++ show funcVar ++ ": expected " ++ show retTy ++ ", got " ++ show (tType typedExpr)
    

inferExpr :: Expr -> (InferM LocRegion) ((TypedNode LocRegion) Expr)
inferExpr expr = case expr of 
    (ExprVal val) -> do
        typedVal <- inferVal val
        let newIVal = ExprVal (tNode typedVal)
        return $ createTypedNode (tType typedVal) newIVal
    (ExprBinOp binOp e1 e2) -> do
        typedE1 <- inferExpr e1
        typedE2 <- inferExpr e2
        let (arg1Ty, arg2Ty, retTy) = binOpToType binOp
            newIBinApp = ExprBinOp binOp (tNode typedE1) (tNode typedE2)
        -- TODO
            -- Can extend to support type promotion later
            -- maybe change to nested if statement where separate error for binary operation being wrong
        if tType typedE1 ==^^ tType typedE2 && tType typedE1 ==^^ arg1Ty && tType typedE2 ==^^ arg2Ty
        then return $ createTypedNode retTy newIBinApp
        else lift . Failed $ "Type mismatch in binary operation" ++ show binOp ++ ": " ++ show (tType typedE1) ++ " vs " ++ show (tType typedE2)
    
    -- TODO deal with location region stuff (may consider type promotion too)
    (ExprFuncApp (FuncVar funcVar) locRegions (Exprs exprs)) -> do
        typedExprs <- mapM inferExpr exprs
        funcInfo <- lookupFunc funcVar
        let argTys = map tType typedExprs
            oldFuncRetType = funcRetType funcInfo
            newIFuncApp = ExprFuncApp (FuncVar funcVar) locRegions (Exprs (map tNode typedExprs))

        -- TODO may need to check if recursive function requires same region for types, but for now, ignore
        -- check if function not being recursively called
        -- if not (funcInside funcInfo)
        -- then 
            -- check if arguments match (ignoring location regions)
        if and $ zipWith (==^^) argTys (funcArgTypes funcInfo)
        then return $ createTypedNode oldFuncRetType newIFuncApp
        else lift . Failed $ "Type mismatch in function application: " ++ show argTys ++ " vs " ++ show (funcArgTypes funcInfo)
        -- else
        --     if argTys == funcArgTypes funcInfo
        --     then return $ createTypedNode (funcRetType funcInfo) iFuncApp
        --     else lift . Failed $ "Type mismatch in function application: " ++ show argTys ++ " vs " ++ show (funcArgTypes funcInfo)
        where
            -- removes location region from return type
            -- modRetType :: MyTy LocRegion -> MyTy LocRegion
            -- modRetType (PackedTy ty _) = PackedTy ty EmptyLocRegion
            -- modRetType ty = ty

            -- checkArgTys :: [MyTy LocRegion] -> [MyTy LocRegion] -> Bool
            -- checkArgTys [] [] = True
            -- checkArgTys (x:xs) (y:ys) = checkArgTy x y && checkArgTys xs ys
            -- checkArgTys _ _ = False

            -- checkArgTy :: MyTy LocRegion -> MyTy LocRegion -> Bool
            -- checkArgTy (PackedTy ty1 _) (PackedTy ty2 _) = ty1 == ty2
            -- checkArgTy ty1 ty2 = ty1 == ty2
    -- TODO deal with location region stuff
    (ExprDataConApp (DataCon dataCon) locRegion (Exprs exprs)) -> do
        typedExprs <- mapM inferExpr exprs
        (fieldTys, resTy) <- lookupModDataCon dataCon locRegion
        let argTys = map tType typedExprs
            newIDataConApp = ExprDataConApp (DataCon dataCon) locRegion (Exprs (map tNode typedExprs))
        if and $ zipWith (==^^) argTys fieldTys
        then return $ createTypedNode resTy newIDataConApp
        else lift . Failed $ "Type mismatch in data constructor application: " ++ show argTys ++ " vs " ++ show fieldTys
    
    (ExprCase val (Pats pats)) -> do
        typedVal <- inferVal val
        typedPats <- mapM (inferPat (tType typedVal)) pats
        let patTypes = map tType typedPats
            typedPat = safeHead typedPats
            newICase = ExprCase (tNode typedVal) (Pats (map tNode typedPats))
        if checkAllSame (==^^) patTypes
        then case typedPat of
            Ok tp -> return $ createTypedNode (tType tp) newICase
            Failed e -> lift $ Failed e
        else lift . Failed $ "Type mismatch in case patterns: " ++ show patTypes

        -- [x] TODO infer patterns and ensure types match
        -- For now, just return the type of the value being matched on
    (ExprLet (Var var) combinedType expr1 expr2) -> do
        typedExpr1 <- inferExpr expr1
        let typedCombinedType = combinedTypeToType combinedType
            expr1Type = tType typedExpr1
        typedExpr2 <- extendVEnv var expr1Type (inferExpr expr2) 
        if typedCombinedType ==^^ expr1Type
        then return $ createTypedNode (tType typedExpr2) (ExprLet (Var var) combinedType (tNode typedExpr1) (tNode typedExpr2))
        else lift . Failed $ "Type mismatch in let binding: " ++ show typedCombinedType ++ " vs " ++ show expr1Type
    
    (ExprLetRegion (RegionVar rv) expr1) -> do
        typedExpr <- extendRegEnv rv (RegInfo { regStarted = False, regLocsUsed = S.empty }) (inferExpr expr1)
        let newILetRegion = ExprLetRegion (RegionVar rv) (tNode typedExpr)
        return $ createTypedNode (tType typedExpr) newILetRegion
    -- TODO still need to check index variables (just make sure they're in environment), may be too complicated for this
    (ExprLetLoc locreg@(LocRegion (LocVar locVar) (RegionVar regVar) (IndexVar indexVar)) locExpress expr1) -> do
        -- let locType = tType typedExpr1
        case locExpress of 
            LocExpressStart (RegionVar regVar2) -> do
                -- TODO create individual functions which check expressions (InferM Bool) for each (should be another InferM Bool already)
                -- This should help nesting and many checks.  May also create 1 function which does all of this using locExpress
                -- Next, mutate region environment as needed and then location expressions
                if regVar2 /= regVar
                then lift . Failed $ "inferExpr: Region variable mismatch in letloc: " ++ show regVar2 ++ " vs " ++ show regVar
                else do
                    regInfo <- lookupReg regVar2
                    if regStarted regInfo
                    then lift . Failed $ "inferExpr: Region " ++ show regVar2 ++ " already started in letloc"
                    else do
                        -- locInfo2 <- lookupLoc locVar2
                        typedExpr <- extendLocEnv locVar (LocInfo { getLocType = NoneTy, locInitialized = False, getRegionFromLoc = regVar2 }) (inferExpr expr1)
                        let newILetLoc = ExprLetLoc locreg (LocExpressStart (RegionVar regVar2)) (tNode typedExpr)
                        return $ createTypedNode (tType typedExpr) newILetLoc
            LocExpressNext (LocRegion (LocVar locVar2) (RegionVar regVar2) (IndexVar indexVar2)) offset -> do
                if regVar2 /= regVar
                then lift . Failed $ "inferExpr: Region variable mismatch in letloc: " ++ show regVar2 ++ " vs " ++ show regVar
                else do
                    -- locInfo2 <- lookupLoc locVar2
                    typedExpr <- extendLocEnv locVar (LocInfo { getLocType = NoneTy, locInitialized = False, getRegionFromLoc = regVar2 }) (inferExpr expr1)
                    let newILetLoc = ExprLetLoc locreg (LocExpressNext (LocRegion (LocVar locVar2) (RegionVar regVar2) (IndexVar indexVar2)) offset) (tNode typedExpr)
                    return $ createTypedNode (tType typedExpr) newILetLoc
            -- TODO need to check with others/paper if different types can be in same region (if not, more work is needed)
            -- TODO this may be an incorrect pattern
            LocExpressAfter (LocatedType combinedLocType lr@(LocRegion _ (RegionVar regVar2) _)) -> do
                -- TODO make sure locVar2 is in same region
                -- let locatedType = locatedTypeToType locatedType
                --     locRegion = getLocRegionFromLocType locatedType
                if regVar2 /= regVar
                then lift . Failed $ "inferExpr: Region variable mismatch in letloc: " ++ show regVar2 ++ " vs " ++ show regVar
                else do
                    typedExpr <- extendLocEnv locVar (LocInfo { getLocType = NoneTy, locInitialized = False, getRegionFromLoc = regVar2 }) (inferExpr expr1)
                    -- varAfter <- lookupByVal (locatedTypeToType lt)
                    let newILetLoc = ExprLetLoc locreg (LocExpressAfter (LocatedType combinedLocType lr)) expr1
                    -- lift . Failed $ show newILetLoc
                    return $ createTypedNode (tType typedExpr) newILetLoc
                    -- locInfo2 <- lookupLoc locVar2
            LocExpressAfter (LocatedType combinedLocType lr@(LocRelativeVar _ _ (RegionVar regVar2) _)) -> do
                if regVar2 /= regVar
                then lift . Failed $ "inferExpr: Region variable mismatch in letloc: " ++ show regVar2 ++ " vs " ++ show regVar
                else do
                    typedExpr <- extendLocEnv locVar (LocInfo { getLocType = NoneTy, locInitialized = False, getRegionFromLoc = regVar2 }) (inferExpr expr1)
                    let newILetLoc = ExprLetLoc locreg (LocExpressAfter (LocatedType combinedLocType lr)) expr1
                    return $ createTypedNode (tType typedExpr) newILetLoc
            -- TODO this only occurs when an empty location expression is given, ensure correctness
            _ -> lift . Failed $ "inferExpr: Not implemented for this locExpress type in letloc"
                    
    -- [x] TODO deal with letloc and letregion

    _ -> lift . Failed $ "inferExpr: Not implemented for this expression type: " ++ takeAlphaNum (show expr)

-- inferLocExpress :: LocExpress -> LocRegion -> InferM Bool
-- inferLocExpress locExpress (LocRegion (LocVar locVar) (RegionVar regVar) (IndexVar indexVar)) = case locExpress of
--     LocExpressStart (RegionVar regVar2) -> do
--         -- make sure region variables match
--         if regVar2 /= regVar
--         then lift . Failed $ "inferLocExpress: Region variable mismatch in letloc: " ++ show regVar2 ++ " vs " ++ show regVar
--         else do
--             -- make sure region not already started
--             regInfo <- lookupReg regVar2
--             if regStarted regInfo
--             then lift . Failed $ "inferLocExpress: Region " ++ show regVar2 ++ " already started in letloc"
--             else return True
--     -- TODO need to make sure no other location is already defined to be next location
--     LocExpressNext (LocRegion (LocVar locVar2) (RegionVar regVar2) (IndexVar indexVar2)) -> do
--         -- make sure region variables match
--         if regVar2 /= regVar
--         then lift . Failed $ "inferLocExpress: Region variable mismatch in letloc: " ++ show regVar2 ++ " vs " ++ show regVar
--         else do



-- TODO check if val should be var???  also need to deal with location regions
inferPatMatch :: PatMatch -> (InferM LocRegion) ((TypedNode LocRegion) PatMatch)
inferPatMatch (PatMatch (ValVar (Var var)) locatedType) = do
    -- [x]BUG HERE: val doesn't have type, but need to make sure variable not in scope
    -- typedVal <- inferVal val
    varExists <- checkVarNameExists var
    if varExists
        then lift . Failed $ "inferPatMatch: Variable " ++ show var ++ " already defined in environment"
        else do
            -- let typedVal = createTypedNode (combinedLocTypeToType combinedLocType) (ValVar (AST.Var var))
            let ty = locatedTypeToType locatedType
                newIPatMatch = PatMatch (ValVar (AST.Var var)) locatedType
            -- if tType typedVal == ty
            return $ createTypedNode ty newIPatMatch
                -- else lift . Failed $ "inferPatMatch: Type mismatch in pattern match: " ++ show (tType typedVal) ++ " vs " ++ show ty
inferPatMatch (PatMatch (ValLit val) _) = do 
    lift . Failed $ "inferPatMatch: Requires variable for pattern match, received literal: " ++ show val

inferPat :: MyTy LocRegion -> Pat -> (InferM LocRegion) ((TypedNode LocRegion) Pat)
inferPat myTy (Pat (DataCon dataCon) (PatMatches patMatches) expr) = do
    matchedTypes <- mapM inferPatMatch patMatches
    let argTypes = map tType matchedTypes
    (typeCons, result) <- lookupDataCon dataCon
    
    -- check to make sure actual type constructors match expected
    if not $ and $ zipWith (==^^) argTypes typeCons
        then lift . Failed $ "inferPat: Type mismatch in pattern for data constructor " ++ show dataCon ++ ": " ++ show argTypes ++ " vs " ++ show typeCons
        else do
            -- check to make sure result type is correct
            if not $ result ==^^ myTy
                then lift . Failed $ "inferPat: Result type mismatch in pattern for data constructor " ++ show dataCon ++ ": " ++ show result ++ " vs " ++ show myTy
                else do
                    -- TODO need to change PatMatch to have var, not val
                    let varTypePairs = map (\(PatMatch (ValVar (Var v)) locatedType) -> (v, locatedTypeToType locatedType)) patMatches
                    typedExpr <- extendVEnvs varTypePairs (inferExpr expr)
                    let newIPat = Pat (DataCon dataCon) (PatMatches (map tNode matchedTypes)) (tNode typedExpr)
                    return $ createTypedNode (tType typedExpr) newIPat

inferVal :: Val -> (InferM LocRegion) ((TypedNode LocRegion) Val)
inferVal val = case val of 
    (ValVar (AST.Var v)) -> do
        ty <- lookupVar v
        let newIValVar = ValVar (AST.Var v)
        return $ createTypedNode ty newIValVar
    (ValLit n) -> do
        typedLit <- inferLit n
        let newIValLit = ValLit n
        return $ createTypedNode (tType typedLit) newIValLit
    _ -> lift . Failed $ "inferVal: Not implemented for this value type"

inferLit :: Lit -> (InferM LocRegion) ((TypedNode LocRegion) Lit)
inferLit lit = case lit of 
    node@(IntLit _) -> return $ createTypedNode IntTy node
    node@(FloatLit _) -> return $ createTypedNode FloatTy node
    node@(BoolLit _) -> return $ createTypedNode BoolTy node
    node@(StringLit _) -> return $ createTypedNode StringTy node
    _ -> lift . Failed $ "inferLit: Not implemented for this literal type"

-- Data Declaration Loading
extractDataCons :: DataTypeDecl -> [(DataCon, ([MyTy LocRegion], MyTy LocRegion))]
extractDataCons (DataTypeDecl typeCon (DataFields dataFields)) = map extractDataField dataFields
    where
        extractDataField :: DataField -> (DataCon, ([MyTy LocRegion], MyTy LocRegion))
        extractDataField (DataField dataCon (CombinedTypeCons combinedTypeCons)) =
            (dataCon, (map combinedTypeConToType combinedTypeCons, PackedTy typeCon EmptyLocRegion))

loadDataDecls :: DataTypeDecls -> MyEnv LocRegion -> E (MyEnv LocRegion)
loadDataDecls (DataTypeDecls decls) env = foldM loadDataDecl env decls
    where
        loadDataDecl :: MyEnv LocRegion -> DataTypeDecl -> E (MyEnv LocRegion)
        loadDataDecl env2 decl = foldM loadCon env2 (extractDataCons decl)

        loadCon :: MyEnv LocRegion -> (DataCon, ([MyTy LocRegion], MyTy LocRegion)) -> E (MyEnv LocRegion)
        loadCon env2 (DataCon dataCon, (fieldTys, resTy)) 
            | M.member dataCon (dcEnv env2) = 
                Failed $ "Data constructor " ++ show dataCon ++ " already defined in environment"
            | otherwise = Ok env2 { dcEnv = M.insert dataCon (fieldTys, resTy) (dcEnv env2) }

-- Function Declaration Loading
extractFuncDecls :: FuncDecls -> [(FuncVar, FuncInfo LocRegion)]
extractFuncDecls (FuncDecls decls) = map extractFuncDecl decls
    where
        -- TODO deal with location regions
        extractFuncDecl :: FuncDecl -> (FuncVar, FuncInfo LocRegion)
        extractFuncDecl (FuncDecl funcVar1 (TypeScheme combinedTypes) funcVar2 locRegions vars expr) = 
            -- GET ARG TYPES AND RETURN TYPE FROM TYPESCHEME, ignore everything else
            case separateArgs combinedTypes of
                Nothing -> error $ "extractFuncDecl: Function " ++ show funcVar1 ++ " has invalid type scheme"
                Just funcInfo -> (funcVar1, funcInfo)
        
        separateArgs :: CombinedTypes -> Maybe (FuncInfo LocRegion)
        separateArgs (CombinedTypes []) = Nothing
        separateArgs (CombinedTypes [x]) = Just (FuncInfo [] (combinedTypeToType x) False)
        separateArgs (CombinedTypes (x:xs)) = case separateArgs (CombinedTypes xs) of
            Nothing -> Nothing
            Just (FuncInfo argTys retTy _) -> Just (FuncInfo (combinedTypeToType x : argTys) retTy False)

loadFuncDecls :: FuncDecls -> MyEnv LocRegion -> E (MyEnv LocRegion)
loadFuncDecls (FuncDecls decls) env = foldM loadFuncDecl env decls
    where
        loadFuncDecl :: MyEnv LocRegion -> FuncDecl -> E (MyEnv LocRegion)
        loadFuncDecl env2 decl = 
            let (FuncVar funcVar, funcInfo) = extractFuncDecls (FuncDecls [decl]) !! 0
            in if M.member funcVar (fEnv env2)
                then Failed $ "Function " ++ show funcVar ++ " already defined in environment"
                else Ok env2 { fEnv = M.insert funcVar funcInfo (fEnv env2) }

-- Type Conversion Helpers
baseTypeToType :: BaseType -> MyTy a
baseTypeToType Int = IntTy
baseTypeToType Float = FloatTy
baseTypeToType Bool = BoolTy
baseTypeToType String = StringTy

combinedTypeConToType :: CombinedTypeCon -> MyTy LocRegion
combinedTypeConToType (CTCTypeCon tc) = PackedTy tc EmptyLocRegion
combinedTypeConToType (CTCBase baseType) = baseTypeToType baseType

combinedTypeToType :: CombinedType -> MyTy LocRegion
combinedTypeToType (CTLocated (LocatedType combinedLocType locRegion)) = case combinedLocType of
    CLTTypeCon tc -> PackedTy tc locRegion
    CLTBase baseType -> baseTypeToType baseType
combinedTypeToType (CTBase baseType) = baseTypeToType baseType

-- combinedLocTypeToType :: CombinedLocType -> MyTy LocRegion
-- combinedLocTypeToType (CLTTypeCon tc) = PackedTy tc EmptyLocRegion
-- combinedLocTypeToType (CLTBase baseType) = baseTypeToType baseType    
-- convertToTypedAST :: Program -> S.Prog S.Ty2

-- TODO look at whether I need to bind base types to locRegion
locatedTypeToType :: LocatedType -> MyTy LocRegion
locatedTypeToType (LocatedType (CLTTypeCon tc) locRegion) = PackedTy tc locRegion -- deal with adding location at some point
locatedTypeToType (LocatedType (CLTBase baseType) locRegion) = baseTypeToType baseType

binOpToType :: BinOp -> (MyTy a, MyTy a, MyTy a)
binOpToType op = case op of
    Add -> (IntTy, IntTy, IntTy)
    Sub -> (IntTy, IntTy, IntTy)
    Mul -> (IntTy, IntTy, IntTy)
    Div -> (IntTy, IntTy, IntTy)
    FAdd -> (FloatTy, FloatTy, FloatTy)
    FSub -> (FloatTy, FloatTy, FloatTy)
    FMul -> (FloatTy, FloatTy, FloatTy)
    FDiv -> (FloatTy, FloatTy, FloatTy)
    Pow -> (FloatTy, FloatTy, FloatTy)
    Eq  -> (IntTy, IntTy, BoolTy)
    FEq -> (FloatTy, FloatTy, BoolTy)
    CEq -> (BoolTy, BoolTy, BoolTy)
    Gt  -> (IntTy, IntTy, BoolTy)
    Lt  -> (IntTy, IntTy, BoolTy)
    FGt -> (FloatTy, FloatTy, BoolTy)
    FLt -> (FloatTy, FloatTy, BoolTy)
    Ge  -> (IntTy, IntTy, BoolTy)
    Le  -> (IntTy, IntTy, BoolTy)
    FGe -> (FloatTy, FloatTy, BoolTy)
    FLe -> (FloatTy, FloatTy, BoolTy)
    Neq -> (IntTy, IntTy, BoolTy)
    And -> (BoolTy, BoolTy, BoolTy)
    Or  -> (BoolTy, BoolTy, BoolTy)
