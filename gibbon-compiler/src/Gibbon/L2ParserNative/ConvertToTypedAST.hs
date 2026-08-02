-- {-# LANGUAGE StandaloneDeriving #-}
module Gibbon.L2ParserNative.ConvertToTypedAST where

import Gibbon.L2ParserNative.AST as AST
import Gibbon.L2ParserNative.Helper
-- import Gibbon.Language.Syntax as S
import Control.Monad (foldM, zipWithM)
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Set as S
-- import qualified Gibbon.Common as C
-- import System.Environment (getEnv)

-- This conversion does the following transformations:
-- 1. Annotates each node in the AST with its type (using MyTy)
-- 2. Ensures that all expressions are well-typed according to the type environment



type TyEnv a b = M.Map a b

emptyTyEnv :: TyEnv a b
emptyTyEnv = M.empty

data MyEnv a = MyEnv { dcEnv  :: TyEnv String DataTypeInfo -- Data constructor maps to type constructors and result type
                     , vEnv   :: TyEnv String MyType
                     , fEnv   :: TyEnv String FuncInfo -- Function maps to argument types and return type
                     , locEnv :: TyEnv String LocInfo -- Location variable maps to (type, region variable)
                     , regEnv :: TyEnv String RegInfo -- Region variable maps to type
                     } deriving Show

data DataTypeInfo = DataTypeInfo 
    { dataTypeInfoCon :: MyType 
    , dataTypeInfoFields :: [MyType]
    , dataTypeInfoTyArgs :: [String]
    } deriving Show

data FuncInfo = FuncInfo
    { funcArgTypes :: [MyType]
    , funcRetType  :: MyType
    , funcInside   :: Bool
    } deriving Show

-- TODO maybe rework tuples into this style
data RegInfo = RegInfo
    { regStarted    :: Bool
    , regLocsUsed   :: S.Set String
    } deriving Show

data LocInfo = LocInfo
    { getLocType :: MyType
    , locInitialized    :: Bool
    , getRegionFromLoc  :: String
    } deriving Show

type InferM a = ReaderT (MyEnv a) E

data TypedNode a = TypedNode
    { tType :: MyType
    , tNode :: a
    } deriving Show

-- all types
-- data MyTy loc
--     = IntTy
--     | FloatTy
--     | BoolTy
--     | CharTy
--     | StringTy
--     | PackedTy TypeCon loc -- loc (I will add loc at some point, but it will break a lot right now)
--     | NoneTy
--     deriving (Show, Ord)

-- instance Eq a => Eq MyType where
--     (==) (PackedTy tc1 loc1) (PackedTy tc2 loc2) =
--         tc1 == tc2 && loc1 == loc2
--     (==) IntTy IntTy = True
--     (==) FloatTy FloatTy = True
--     (==) BoolTy BoolTy = True
--     (==) CharTy CharTy = True
--     (==) StringTy StringTy = True
--     (==) NoneTy NoneTy = True
--     (==) _ _ = False

-- instance Eq a => Eq [MyType] where


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
lookupByVal :: MyType -> InferM a String
lookupByVal val = do
    env <- asks vEnv
    case M.toList (M.filter (== val) env) of
        ((k, _):_) -> return k
        []         -> lift . Failed $ "Value " ++ show val ++ " not found in environment"

lookupVar :: String -> InferM a MyType
lookupVar v = do
    env <- asks vEnv
    case M.lookup v env of
        Just ty -> return ty
        Nothing -> lift . Failed $ "Variable " ++ show v ++ " not found in environment"

lookupDataCon :: String -> InferM a DataTypeInfo
lookupDataCon dc = do
    env <- asks dcEnv
    case M.lookup dc env of
        Just dataInfo -> return dataInfo
        Nothing -> lift . Failed $ "Data constructor " ++ show dc ++ " not found in environment"

lookupModDataCon :: String -> LocRegion -> InferM LocRegion DataTypeInfo
lookupModDataCon dc loc = do
    env <- asks dcEnv
    case M.lookup dc env of
        Just (DataTypeInfo dataTypeCon dataTypeFields dataTypeTyArgs) -> do
            let newDataTypeCon = case dataTypeCon of
                    PackedTy tc _ -> PackedTy tc loc
                    _             -> dataTypeCon
            return (DataTypeInfo newDataTypeCon dataTypeFields dataTypeTyArgs)
        Nothing -> lift . Failed $ "Data constructor " ++ show dc ++ " not found in environment"

lookupFunc :: String -> InferM a FuncInfo
lookupFunc fv = do
    env <- asks fEnv
    case M.lookup fv env of
        Just ty -> return ty
        Nothing -> lift . Failed $ "Function " ++ show fv ++ " not found in environment"

lookupLoc :: String -> InferM a LocInfo
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
extendDataConEnv :: String -> DataTypeInfo -> (InferM a) b -> (InferM a) b
extendDataConEnv dc ty = local (\env -> env { dcEnv = M.insert dc ty (dcEnv env) })

extendVEnv :: String -> MyType -> (InferM a) b -> (InferM a) b
extendVEnv v ty = local (\env -> env { vEnv = M.insert v ty (vEnv env) })

extendVEnvs :: [(String, MyType)] -> (InferM a) b -> (InferM a) b
extendVEnvs [] action = action
extendVEnvs ((v, ty):rest) action = extendVEnv v ty (extendVEnvs rest action)

extendFEnv :: String -> FuncInfo -> (InferM a) b -> (InferM a) b
extendFEnv fv ty = local (\env -> env { fEnv = M.insert fv ty (fEnv env) })

setAsCurrentFunc :: String -> (InferM a) b -> (InferM a) b
setAsCurrentFunc fv = local (\env -> 
    case M.lookup fv (fEnv env) of
        Just funcInfo -> let newFuncInfo = funcInfo { funcInside = True }
                         in env { fEnv = M.insert fv newFuncInfo (fEnv env) }
        Nothing -> env -- should never happen, but just in case
    )

extendLocEnv :: String -> LocInfo -> (InferM a) b -> (InferM a) b
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

-- checkDataTypeExists :: String -> (InferM a) Bool

checkVarNameExists :: String -> (InferM a) Bool
checkVarNameExists v = do
    envType <- getEnvType v
    case envType of
        Just _  -> return True
        Nothing -> return False

-- compare only by name of type
(==^^) :: MyType -> MyType -> Bool
(==^^) (PackedTy (TypeCon tc1) _) (PackedTy (TypeCon tc2) _) =
    tc1 == tc2
(==^^) ty1 ty2 = ty1 == ty2

-- Helper Construction Functions
createTypedNode :: MyType -> b -> TypedNode b
createTypedNode ty node = TypedNode { tType = ty, tNode = node }

emptyEnv :: MyEnv LocRegion
emptyEnv = MyEnv emptyTyEnv emptyTyEnv emptyTyEnv emptyTyEnv emptyTyEnv


----------------------------------------------------------------------------------------------
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
inferFunc :: FuncDecl -> (InferM LocRegion) (TypedNode FuncDecl)
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
    

inferExpr :: Expr -> (InferM LocRegion) (TypedNode Expr)
inferExpr expr = case expr of 
    (ExprVal val) -> do
        typedVal <- inferVal val
        let newIVal = ExprVal (tNode typedVal)
        return $ createTypedNode (tType typedVal) newIVal
    (ExprPrimApp primFunc (Exprs exprs)) -> do
        typedExprs <- mapM inferExpr exprs
        let (argTys, retTy) = primFuncToType primFunc
            newIPrimApp = ExprPrimApp primFunc (Exprs (map tNode typedExprs))
        -- TODO
            -- Can extend to support type promotion later
            -- maybe change to nested if statement where separate error for binary operation being wrong
        if and $ zipWith (==^^) (map tType typedExprs) argTys
        then return $ createTypedNode retTy newIPrimApp
        else lift . Failed $ "Type mismatch in primitive application " ++ show primFunc ++ ": " ++ show (map tType typedExprs) ++ " vs " ++ show argTys

    -- TODO deal with location region stuff (may consider type promotion too)
    (ExprFuncApp (FuncVar funcVar) lrs@(LocRegions locRegions) (Exprs exprs)) -> do
        typedExprs <- mapM inferExpr exprs
        funcInfo <- lookupFunc funcVar
        let argTys = map tType typedExprs
            newIFuncApp = ExprFuncApp (FuncVar funcVar) lrs (Exprs (map tNode typedExprs))
        oldFuncRetType <- case funcRetType funcInfo of
            PackedTy tc _ -> do
                let retLoc = safeLast locRegions
                case retLoc of 
                    Ok r -> return $ PackedTy tc r
                    Failed _ -> lift . Failed $ "inferExpr: Function application for " ++ show funcVar ++ " has packed return type but no location regions provided"
            ty -> return ty

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
            -- modRetType :: MyType -> MyType
            -- modRetType (PackedTy ty _) = PackedTy ty EmptyLocRegion
            -- modRetType ty = ty

            -- checkArgTys :: [MyType] -> [MyType] -> Bool
            -- checkArgTys [] [] = True
            -- checkArgTys (x:xs) (y:ys) = checkArgTy x y && checkArgTys xs ys
            -- checkArgTys _ _ = False

            -- checkArgTy :: MyType -> MyType -> Bool
            -- checkArgTy (PackedTy ty1 _) (PackedTy ty2 _) = ty1 == ty2
            -- checkArgTy ty1 ty2 = ty1 == ty2
    -- TODO deal with location region stuff
    (ExprDataConApp (DataCon dataCon) locRegion (Exprs exprs)) -> do
        typedExprs <- mapM inferExpr exprs
        (DataTypeInfo resTy fieldTys _) <- lookupModDataCon dataCon locRegion
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
    (ExprLet patDeconstruct combinedType expr1 expr2) -> do
        typedExpr1 <- inferExpr expr1
        let expr1Type = tType typedExpr1
        varTypePairs <- getVarTypePairsFromPatDeconstruct patDeconstruct combinedType
        typedExpr2 <- extendVEnvs varTypePairs (inferExpr expr2)
        if combinedType ==^^ expr1Type
        then return $ createTypedNode (tType typedExpr2) (ExprLet patDeconstruct combinedType (tNode typedExpr1) (tNode typedExpr2))
        else lift . Failed $ "Type mismatch in let binding: " ++ show combinedType ++ " vs " ++ show expr1Type
    
    (ExprLetRegion (RegionVar rv) expr1) -> do
        typedExpr <- extendRegEnv rv (RegInfo { regStarted = False, regLocsUsed = S.empty }) (inferExpr expr1)
        let newILetRegion = ExprLetRegion (RegionVar rv) (tNode typedExpr)
        return $ createTypedNode (tType typedExpr) newILetRegion
    -- TODO still need to check index variables (just make sure they're in environment), may be too complicated for this
    (ExprLetLoc locreg@(LocRegion (LocVar locVar) (RegionVar regVar) (IndexVar _indexVar)) locExpress expr1) -> do
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
            LocExpressAfter (PackedTy typeCon lr@(LocRegion _ (RegionVar regVar2) _)) -> do
                -- TODO make sure locVar2 is in same region
                -- let locatedType = locatedTypeToType locatedType
                --     locRegion = getLocRegionFromLocType locatedType
                if regVar2 /= regVar
                then lift . Failed $ "inferExpr: Region variable mismatch in letloc: " ++ show regVar2 ++ " vs " ++ show regVar
                else do
                    typedExpr <- extendLocEnv locVar (LocInfo { getLocType = NoneTy, locInitialized = False, getRegionFromLoc = regVar2 }) (inferExpr expr1)
                    -- varAfter <- lookupByVal (locatedTypeToType lt)
                    let newILetLoc = ExprLetLoc locreg (LocExpressAfter (PackedTy typeCon lr)) expr1
                    -- lift . Failed $ show newILetLoc
                    return $ createTypedNode (tType typedExpr) newILetLoc
                    -- locInfo2 <- lookupLoc locVar2
            LocExpressAfter (PackedTy typeCon lr@(LocRelativeVar _ _ (RegionVar regVar2) _)) -> do
                if regVar2 /= regVar
                then lift . Failed $ "inferExpr: Region variable mismatch in letloc: " ++ show regVar2 ++ " vs " ++ show regVar
                else do
                    typedExpr <- extendLocEnv locVar (LocInfo { getLocType = NoneTy, locInitialized = False, getRegionFromLoc = regVar2 }) (inferExpr expr1)
                    let newILetLoc = ExprLetLoc locreg (LocExpressAfter (PackedTy typeCon lr)) expr1
                    return $ createTypedNode (tType typedExpr) newILetLoc
            -- TODO this only occurs when an empty location expression is given, ensure correctness
            _ -> lift . Failed $ "inferExpr: Not implemented for this locExpress type in letloc"
                    
    (ExprIf condExpr thenExpr elseExpr) -> do
        typedCond <- inferExpr condExpr
        typedThen <- inferExpr thenExpr
        typedElse <- inferExpr elseExpr
        let newIIf = ExprIf (tNode typedCond) (tNode typedThen) (tNode typedElse)
        if tType typedCond ==^^ BoolTy EmptyLocRegion
        then if tType typedThen ==^^ tType typedElse
            then return $ createTypedNode (tType typedThen) newIIf
            else lift . Failed $ "Type mismatch in branches of if expression: " ++ show (tType typedThen) ++ " vs " ++ show (tType typedElse)
        else lift . Failed $ "Condition in if expression must be of type Bool, got: " ++ show (tType typedCond)
    _ -> lift . Failed $ "inferExpr: Not implemented for this expression type: " ++ takeAlphaNum (show expr)
    where 
        getVarTypePairsFromPatDeconstruct :: PatDeconstruct -> MyType -> (InferM LocRegion) [(String, MyType)]
        getVarTypePairsFromPatDeconstruct (PatVar (Var v)) myType = return [(v, myType)]
        getVarTypePairsFromPatDeconstruct (PatTuple (PatDeconstructs patDeconstructs)) (ProdTy (MyTypes myTypes)) = mapVarTypePairs' patDeconstructs myTypes
        getVarTypePairsFromPatDeconstruct _ _ = error "Unsupported pattern deconstruct or type in getVarTypePairsFromPatDeconstruct"

        mapVarTypePairs' :: [PatDeconstruct] -> [MyType] -> (InferM LocRegion) [(String, MyType)]
        mapVarTypePairs' [] [] = return []
        mapVarTypePairs' (PatVar (Var v) : restDeconstructs) (ty : restTypes) = do
            let newVarTypePair = (v, ty)
            nextDeconstructs <- mapVarTypePairs' restDeconstructs restTypes
            return $ newVarTypePair : nextDeconstructs
        mapVarTypePairs' (PatTuple (PatDeconstructs patDeconstructs) : restDeconstructs) (ProdTy (MyTypes myTypes) : restTypes) = do
            currDeconstructs <- mapVarTypePairs' patDeconstructs myTypes
            nextDeconstructs <- mapVarTypePairs' restDeconstructs restTypes
            return $ currDeconstructs ++ nextDeconstructs
        mapVarTypePairs' _ _ = lift . Failed $ "mapVarTypePairs': Not implemented for this pattern deconstruct and type combination"
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
inferPatMatch :: PatMatch -> (InferM LocRegion) (TypedNode PatMatch)
inferPatMatch (PatMatch (PatVar (Var var)) locatedType) = do
    varExists <- checkVarNameExists var
    if varExists
        then lift . Failed $ "inferPatMatch: Variable " ++ show var ++ " already defined in environment"
        else do
            -- let typedVal = createTypedNode (combinedLocTypeToType combinedLocType) (ValVar (AST.Var var))
            let newIPatMatch = PatMatch (PatVar (AST.Var var)) locatedType
            -- if tType typedVal == ty
            return $ createTypedNode locatedType newIPatMatch
                -- else lift . Failed $ "inferPatMatch: Type mismatch in pattern match: " ++ show (tType typedVal) ++ " vs " ++ show ty
-- inferPatMatch (PatMatch (ValLit val) _) = do 
--     lift . Failed $ "inferPatMatch: Requires variable for pattern match, received literal: " ++ show val
inferPatMatch (PatMatch (PatTuple (PatDeconstructs patDeconstructs)) (ProdTy (MyTypes innerTypes))) = do
    innerNodes <- zipWithM inferPatDeconstruct innerTypes patDeconstructs
    let newIPatMatch = PatMatch (PatTuple (PatDeconstructs (map tNode innerNodes))) (ProdTy (MyTypes (map tType innerNodes)))
    return $ createTypedNode (ProdTy (MyTypes (map tType innerNodes))) newIPatMatch
    -- lift . Failed $ "inferPatMatch: Tuple patterns not supported yet: " ++ show patDeconstructs
inferPatMatch _ = lift . Failed $ "inferPatMatch: Not implemented for this pattern match type"

inferPatDeconstruct :: MyType -> PatDeconstruct -> (InferM LocRegion) (TypedNode PatDeconstruct)
inferPatDeconstruct myTy (PatVar (Var var)) = do
    varExists <- checkVarNameExists var
    if varExists
        then lift . Failed $ "inferPatDeconstruct: Variable " ++ show var ++ " already defined in environment"
        else do
            let newIPatDeconstruct = PatVar (AST.Var var)
            return $ createTypedNode myTy newIPatDeconstruct -- TODO need to figure out type for this
inferPatDeconstruct (ProdTy (MyTypes tys)) (PatTuple (PatDeconstructs patDeconstructs)) = do
    if length tys /= length patDeconstructs
    then lift . Failed $ "inferPatDeconstruct: Tuple pattern length mismatch: " ++ show (length tys) ++ " vs " ++ show (length patDeconstructs)
    else do
        innerNodes <- zipWithM inferPatDeconstruct tys patDeconstructs
        let newIPatDeconstruct = PatTuple (PatDeconstructs (map tNode innerNodes))
        return $ createTypedNode (ProdTy (MyTypes (map tType innerNodes))) newIPatDeconstruct

inferPatDeconstruct _ _ = lift . Failed $ "inferPatDeconstruct: Not implemented for this pattern deconstruct type"

inferPat :: MyType -> Pat -> (InferM LocRegion) (TypedNode Pat)
inferPat myTy (Pat (DataCon dataCon) (PatMatches patMatches) expr) = do
    matchedTypes <- mapM inferPatMatch patMatches
    let argTypes = map tType matchedTypes
    (DataTypeInfo result typeCons _) <- lookupDataCon dataCon
    
    -- check to make sure actual type constructors match expected
    if not $ and $ zipWith (==^^) argTypes typeCons
        then lift . Failed $ "inferPat: Type mismatch in pattern for data constructor " ++ show dataCon ++ ": " ++ show argTypes ++ " vs " ++ show typeCons
        else do
            -- check to make sure result type is correct
            if not $ result ==^^ myTy
                then lift . Failed $ "inferPat: Result type mismatch in pattern for data constructor " ++ show dataCon ++ ": " ++ show result ++ " vs " ++ show myTy
                else do
                    varTypePairs <- mapVarTypePairs patMatches
                    typedExpr <- extendVEnvs varTypePairs (inferExpr expr)
                    let newIPat = Pat (DataCon dataCon) (PatMatches (map tNode matchedTypes)) (tNode typedExpr)
                    return $ createTypedNode (tType typedExpr) newIPat

    where   
        mapVarTypePairs :: [PatMatch] -> (InferM LocRegion) [(String, MyType)]
        mapVarTypePairs [] = return []
        mapVarTypePairs ((PatMatch patDeconstruct locatedType : rest)) = do
            pairs <- mapVarTypePairs' [patDeconstruct] [locatedType]
            restPairs <- mapVarTypePairs rest
            return $ pairs ++ restPairs
        -- mapVarTypePairs ((PatMatch (PatTuple (PatDeconstructs patDeconstructs)) (ProdTy (MyTypes myTypes))) : rest) = do
            
            
            -- let innerVarTypePairs = concat $ zipWith mapVarTypePairs (map (\(PatDeconstruct patDeconstruct) -> PatMatch patDeconstruct) patDeconstructs) (map (\ty -> case ty of
            --         PackedTy tc _ -> PackedTy tc EmptyLocRegion
            --         ty -> ty) myTypes)
            -- in innerVarTypePairs ++ mapVarTypePairs rest
        
        mapVarTypePairs' :: [PatDeconstruct] -> [MyType] -> (InferM LocRegion) [(String, MyType)]
        mapVarTypePairs' [] [] = return []
        mapVarTypePairs' (PatVar (Var v) : restDeconstructs) (ty : restTypes) = do
            let newVarTypePair = (v, ty)
            nextDeconstructs <- mapVarTypePairs' restDeconstructs restTypes
            return $ newVarTypePair : nextDeconstructs
        mapVarTypePairs' (PatTuple (PatDeconstructs patDeconstructs) : restDeconstructs) (ProdTy (MyTypes myTypes) : restTypes) = do
            currDeconstructs <- mapVarTypePairs' patDeconstructs myTypes
            nextDeconstructs <- mapVarTypePairs' restDeconstructs restTypes
            return $ currDeconstructs ++ nextDeconstructs
        mapVarTypePairs' _ _ = lift . Failed $ "mapVarTypePairs': Not implemented for this pattern deconstruct and type combination"
            

inferVal :: Val -> (InferM LocRegion) (TypedNode Val)
inferVal val = case val of 
    (ValVar (AST.Var v)) -> do
        ty <- lookupVar v
        let newIValVar = ValVar (AST.Var v)
        return $ createTypedNode ty newIValVar
    (ValLit n) -> do
        typedLit <- inferLit n
        let newIValLit = ValLit n
        return $ createTypedNode (tType typedLit) newIValLit
    (ValTuple (Exprs exprs)) -> do
        typedExprs <- mapM inferExpr exprs
        let exprTypes = map tType typedExprs
            newITuple = ValTuple (Exprs (map tNode typedExprs))
        -- For now, just use NoneTy for tuples, but may want to create actual tuple types later
        return $ createTypedNode (ProdTy (MyTypes exprTypes)) newITuple
    -- _ -> lift . Failed $ "inferVal: Not implemented for this value type"

inferLit :: Lit -> (InferM LocRegion) (TypedNode Lit)
inferLit lit = case lit of 
    node@(IntLit _) -> return $ createTypedNode (IntTy EmptyLocRegion) node
    node@(FloatLit _) -> return $ createTypedNode (FloatTy EmptyLocRegion) node
    node@(BoolLit _) -> return $ createTypedNode (BoolTy EmptyLocRegion) node
    node@(CharLit _) -> return $ createTypedNode (CharTy EmptyLocRegion) node -- TODO may want to have separate CharTy, but for now, just use StringTy
    node@(StringLit _) -> return $ createTypedNode (StringTy EmptyLocRegion) node
    -- _ -> lift . Failed $ "inferLit: Not implemented for this literal type"

-- Data Declaration Loading
extractDataCons :: DataTypeDecl -> [(DataCon, DataTypeInfo)]
extractDataCons (DataTypeDecl typeCon (TypeArgs typeArgs) (DataFields dataFields)) = map extractDataField dataFields
    where
        extractDataField :: DataField -> (DataCon, DataTypeInfo)
        extractDataField (DataField dataCon (MyTypes myTys)) =
            (dataCon, DataTypeInfo (PackedTy typeCon EmptyLocRegion) myTys typeArgs)

loadDataDecls :: DataTypeDecls -> MyEnv LocRegion -> E (MyEnv LocRegion)
loadDataDecls (DataTypeDecls decls) env = foldM loadDataDecl env decls
    where
        loadDataDecl :: MyEnv LocRegion -> DataTypeDecl -> E (MyEnv LocRegion)
        loadDataDecl env2 decl = foldM loadCon env2 (extractDataCons decl)

        loadCon :: MyEnv LocRegion -> (DataCon, DataTypeInfo) -> E (MyEnv LocRegion)
        loadCon env2 (DataCon dataCon, dataTypeInfo) 
            | M.member dataCon (dcEnv env2) = 
                Failed $ "Data constructor " ++ show dataCon ++ " already defined in environment"
            | otherwise = Ok env2 { dcEnv = M.insert dataCon dataTypeInfo (dcEnv env2) }

-- Function Declaration Loading
extractFuncDecls :: FuncDecls -> [(FuncVar, FuncInfo)]
extractFuncDecls (FuncDecls decls) = map extractFuncDecl decls
    where
        -- TODO deal with location regions
        extractFuncDecl :: FuncDecl -> (FuncVar, FuncInfo)
        extractFuncDecl (FuncDecl funcVar1 (TypeScheme combinedTypes) _funcVar2 _locRegions _vars _expr) = 
            -- GET ARG TYPES AND RETURN TYPE FROM TYPESCHEME, ignore everything else
            case separateArgs combinedTypes of
                Nothing -> error $ "extractFuncDecl: Function " ++ show funcVar1 ++ " has invalid type scheme"
                Just funcInfo -> (funcVar1, funcInfo)
        
        separateArgs :: MyTypes -> Maybe FuncInfo
        separateArgs (MyTypes []) = Nothing
        separateArgs (MyTypes [x]) = Just (FuncInfo [] x False)
        separateArgs (MyTypes (x:xs)) = case separateArgs (MyTypes xs) of
            Nothing -> Nothing
            Just (FuncInfo argTys retTy _) -> Just (FuncInfo (x : argTys) retTy False)

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
-- baseTypeToType :: BaseType -> MyType
-- baseTypeToType Int = IntTy
-- baseTypeToType Float = FloatTy
-- baseTypeToType Bool = BoolTy
-- baseTypeToType Char = CharTy
-- baseTypeToType String = StringTy

-- combinedTypeConToType :: CombinedTypeCon -> MyType
-- combinedTypeConToType (CTCTypeCon tc) = PackedTy tc EmptyLocRegion
-- combinedTypeConToType (CTCBase baseType) = baseTypeToType baseType

-- combinedTypeToType :: CombinedType -> MyType
-- combinedTypeToType (CTLocated (LocatedType combinedTypeCon locRegion)) = case combinedTypeCon of
--     CTCTypeCon tc -> PackedTy tc locRegion
--     CTCBase baseType -> baseTypeToType baseType
-- combinedTypeToType (CTBase baseType) = baseTypeToType baseType

-- combinedLocTypeToType :: CombinedLocType -> MyType
-- combinedLocTypeToType (CLTTypeCon tc) = PackedTy tc EmptyLocRegion
-- combinedLocTypeToType (CLTBase baseType) = baseTypeToType baseType    
-- convertToTypedAST :: Program -> S.Prog S.Ty2

-- TODO look at whether I need to bind base types to locRegion
-- locatedTypeToType :: LocatedType -> MyType
-- locatedTypeToType (LocatedType (CTCTypeCon tc) locRegion) = PackedTy tc locRegion -- deal with adding location at some point
-- locatedTypeToType (LocatedType (CTCBase baseType) _locRegion) = baseTypeToType baseType

primFuncToType :: PrimFunc -> ([MyType], MyType)
primFuncToType op = case op of
    Add -> ([IntTy EmptyLocRegion, IntTy EmptyLocRegion], IntTy EmptyLocRegion)
    Sub -> ([IntTy EmptyLocRegion, IntTy EmptyLocRegion], IntTy EmptyLocRegion)
    Mul -> ([IntTy EmptyLocRegion, IntTy EmptyLocRegion], IntTy EmptyLocRegion)
    Div -> ([IntTy EmptyLocRegion, IntTy EmptyLocRegion], IntTy EmptyLocRegion)
    FAdd -> ([FloatTy EmptyLocRegion, FloatTy EmptyLocRegion], FloatTy EmptyLocRegion)
    FSub -> ([FloatTy EmptyLocRegion, FloatTy EmptyLocRegion], FloatTy EmptyLocRegion)
    FMul -> ([FloatTy EmptyLocRegion, FloatTy EmptyLocRegion], FloatTy EmptyLocRegion)
    FDiv -> ([FloatTy EmptyLocRegion, FloatTy EmptyLocRegion], FloatTy EmptyLocRegion)
    Pow -> ([IntTy EmptyLocRegion, IntTy EmptyLocRegion], IntTy EmptyLocRegion)
    Eq  -> ([IntTy EmptyLocRegion, IntTy EmptyLocRegion], BoolTy EmptyLocRegion)
    FEq -> ([FloatTy EmptyLocRegion, FloatTy EmptyLocRegion], BoolTy EmptyLocRegion)
    CEq -> ([BoolTy EmptyLocRegion, BoolTy EmptyLocRegion], BoolTy EmptyLocRegion)
    Gt  -> ([IntTy EmptyLocRegion, IntTy EmptyLocRegion], BoolTy EmptyLocRegion)
    Lt  -> ([IntTy EmptyLocRegion, IntTy EmptyLocRegion], BoolTy EmptyLocRegion)
    FGt -> ([FloatTy EmptyLocRegion, FloatTy EmptyLocRegion], BoolTy EmptyLocRegion)
    FLt -> ([FloatTy EmptyLocRegion, FloatTy EmptyLocRegion], BoolTy EmptyLocRegion)
    Ge  -> ([IntTy EmptyLocRegion, IntTy EmptyLocRegion], BoolTy EmptyLocRegion)
    Le  -> ([IntTy EmptyLocRegion, IntTy EmptyLocRegion], BoolTy EmptyLocRegion)
    FGe -> ([FloatTy EmptyLocRegion, FloatTy EmptyLocRegion], BoolTy EmptyLocRegion)
    FLe -> ([FloatTy EmptyLocRegion, FloatTy EmptyLocRegion], BoolTy EmptyLocRegion)
    Neq -> ([IntTy EmptyLocRegion, IntTy EmptyLocRegion], BoolTy EmptyLocRegion)
    And -> ([BoolTy EmptyLocRegion, BoolTy EmptyLocRegion], BoolTy EmptyLocRegion)
    Or  -> ([BoolTy EmptyLocRegion, BoolTy EmptyLocRegion], BoolTy EmptyLocRegion)

    -- print primitives
    PrintInt -> ([IntTy EmptyLocRegion], ProdTy (MyTypes []))
    PrintChar -> ([CharTy EmptyLocRegion], ProdTy (MyTypes []))
    PrintFloat -> ([FloatTy EmptyLocRegion], ProdTy (MyTypes []))
    PrintBool -> ([BoolTy EmptyLocRegion], ProdTy (MyTypes []))

    -- file primitives
    ReadPackedFile (Just _) (TypeCon _) _ ty -> ([], ty)
    WritePackedFile _ _ -> ([], ProdTy (MyTypes []))

    _ -> error $ "primFuncToType: Not implemented for this primitive function: " ++ show op

