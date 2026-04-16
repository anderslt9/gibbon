{-# LANGUAGE LambdaCase #-}
module Gibbon.L2ParserNative.Passes where 
import Gibbon.L2ParserNative.AST
import Gibbon.L2ParserNative.ConvertToTypedAST
import Control.Monad (foldM)
-- import Control.Monad.IO.Class (liftIO)
import Gibbon.L2ParserNative.Helper (E)
import Control.Monad.Reader (ReaderT(runReaderT))

data PassNamed = PassNamed 
    { passName :: String
    , passFunc :: Pass
    }

data Pass = Pass 
    { onProgram :: Program -> (InferM LocRegion) Program
    , onDataTypeDecl :: DataTypeDecl -> (InferM LocRegion) DataTypeDecl
    , onDataField :: DataField -> (InferM LocRegion) DataField
    , onCombinedTypeCon :: CombinedTypeCon -> (InferM LocRegion) CombinedTypeCon
    , onFuncDecl :: FuncDecl -> (InferM LocRegion) FuncDecl
    , onLocatedType :: LocatedType -> (InferM LocRegion) LocatedType
    , onCombinedLocType :: CombinedLocType -> (InferM LocRegion) CombinedLocType
    , onTypeScheme :: TypeScheme -> (InferM LocRegion) TypeScheme
    , onCombinedType :: CombinedType -> (InferM LocRegion) CombinedType
    , onBaseType :: BaseType -> (InferM LocRegion) BaseType
    , onLocExpress :: LocExpress -> (InferM LocRegion) LocExpress
    , onLocRegion :: LocRegion -> (InferM LocRegion) LocRegion
    , onVal :: Val -> (InferM LocRegion) Val
    , onLit :: Lit -> (InferM LocRegion) Lit
    , onExpr :: Expr -> (InferM LocRegion) Expr
    , onPat :: Pat -> (InferM LocRegion) Pat
    , onPatMatch :: PatMatch -> (InferM LocRegion) PatMatch
    , onBinOp :: BinOp -> (InferM LocRegion) BinOp
    , onFuncVar :: FuncVar -> (InferM LocRegion) FuncVar
    , onRegionVar :: RegionVar -> (InferM LocRegion) RegionVar
    , onLocVar :: LocVar -> (InferM LocRegion) LocVar
    , onIndexVar :: IndexVar -> (InferM LocRegion) IndexVar
    , onTypeCon :: TypeCon -> (InferM LocRegion) TypeCon
    , onDataCon :: DataCon -> (InferM LocRegion) DataCon
    , onVar :: Var -> (InferM LocRegion) Var
    , onVars :: Vars -> (InferM LocRegion) Vars
    , onDataFields :: DataFields -> (InferM LocRegion) DataFields
    , onCombinedTypeCons :: CombinedTypeCons -> (InferM LocRegion) CombinedTypeCons
    , onExprs :: Exprs -> (InferM LocRegion) Exprs
    , onVals :: Vals -> (InferM LocRegion) Vals
    , onPats :: Pats -> (InferM LocRegion) Pats
    , onPatMatches :: PatMatches -> (InferM LocRegion) PatMatches
    , onDataTypeDecls :: DataTypeDecls -> (InferM LocRegion) DataTypeDecls
    , onFuncDecls :: FuncDecls -> (InferM LocRegion) FuncDecls
    , onLocRegions :: LocRegions -> (InferM LocRegion) LocRegions
    , onCombinedTypes :: CombinedTypes -> (InferM LocRegion) CombinedTypes
    }

idPass :: Pass
idPass = Pass return return return return return return return return return return
              return return return return return return return return return return
              return return return return return return return return return return
              return return return return return return
    -- { onProgram = return . createTypedNode (LocRelativeVar "idPassProgram") . id
    -- , onDataTypeDecl = return . createTypedNode (LocRelativeVar

runProgramPass' :: Program -> Pass -> E Program
runProgramPass' program pass = walkProgram pass program

runProgramPasses' :: Program -> [Pass] -> E Program
runProgramPasses' = foldM runProgramPass'

runProgramPass :: Program -> PassNamed -> E Program
runProgramPass program (PassNamed _name pass) = do
    -- liftIO $ putStrLn $ "Running pass: " ++ name
    runProgramPass' program pass

runProgramPasses :: [PassNamed] -> Program -> E Program
runProgramPasses passes program = foldM runProgramPass program passes

----------------------------------------------------------------------------------------
------------------------ Passes --------------------------------------------------------
all_passes :: [PassNamed]
all_passes = [replaceLocRegionNames, replaceLocRegionInAfterExprs, replaceNeq]

replaceLocRegionNames :: PassNamed
replaceLocRegionNames = PassNamed "Replace Location Region Names" $ idPass
    { onLocRegion = \case
        LocRegion (LocVar lVar) regionVar (IndexVar iVar) -> do
            if iVar == ""
            then return (LocRegion (LocVar lVar) regionVar (IndexVar iVar))
            else do
                let newLocVar = LocVar (lVar ++ "_" ++ iVar)
                    newLocRegion = LocRegion newLocVar regionVar (IndexVar "")
                return newLocRegion
        e -> return e
    }

-- TODO : implement ability to write as reference to variable in top level language
replaceLocRegionInAfterExprs :: PassNamed
replaceLocRegionInAfterExprs = PassNamed "Replace Location Region Names in After Expressions" $ idPass
    { onLocExpress = \case
        LocExpressAfter lt@(LocatedType combinedLocType (LocRegion lv rv iv)) -> do
            varAfter <- lookupByVal (locatedTypeToType lt)
            let newLocExpress = LocExpressAfter (LocatedType combinedLocType (LocRelativeVar varAfter lv rv iv))
            return newLocExpress
        e -> return e
    }

replaceNeq :: PassNamed
replaceNeq = PassNamed "Replace Not Equal with If-Then-Else" $ idPass
    { onExpr = \case
        ExprBinOp Neq expr1 expr2 -> do
            let newExpr = ExprIf (ExprBinOp Eq expr1 expr2) (ExprVal $ ValLit $ BoolLit False) (ExprVal $ ValLit $ BoolLit True)
            return newExpr
        e -> return e
    }

----------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------

walkProgram :: Pass -> Program -> E Program
walkProgram pass (Program dataTypeDecls fd@(FuncDecls funcDecls) expr) = do
    env1 <- loadDataDecls dataTypeDecls emptyEnv
    env2 <- loadFuncDecls fd env1
    
    runReaderT (do
        main' <- walkExpr pass expr
        funcs' <- mapM (walkFuncDecl pass) funcDecls
        let newProgram = Program dataTypeDecls (FuncDecls funcs') main'
        onProgram pass newProgram
        ) env2

walkDataTypeDecl :: Pass -> DataTypeDecl -> (InferM LocRegion) DataTypeDecl
walkDataTypeDecl pass (DataTypeDecl typeCon dataFields) = do
    typeCon' <- walkTypeCon pass typeCon
    dataFields' <- walkDataFields pass dataFields
    let newDataTypeDecl = DataTypeDecl typeCon' dataFields'
    onDataTypeDecl pass newDataTypeDecl

walkDataField :: Pass -> DataField -> (InferM LocRegion) DataField
walkDataField pass (DataField dataCon combinedTypeCons) = do
    dataCon' <- walkDataCon pass dataCon
    combinedTypeCons' <- walkCombinedTypeCons pass combinedTypeCons
    let newDataField = DataField dataCon' combinedTypeCons'
    onDataField pass newDataField

walkCombinedTypeCon :: Pass -> CombinedTypeCon -> (InferM LocRegion) CombinedTypeCon
walkCombinedTypeCon pass (CTCTypeCon typeCon) = do
    typeCon' <- walkTypeCon pass typeCon
    let newCombinedTypeCon = CTCTypeCon typeCon'
    onCombinedTypeCon pass newCombinedTypeCon
walkCombinedTypeCon pass (CTCBase baseType) = do
    baseType' <- walkBaseType pass baseType
    let newCombinedTypeCon = CTCBase baseType'
    onCombinedTypeCon pass newCombinedTypeCon

walkFuncDecl :: Pass -> FuncDecl -> (InferM LocRegion) FuncDecl
walkFuncDecl pass (FuncDecl fv@(FuncVar funcVar) typeScheme funcVar2 locRegions vs@(Vars vars) expr) = do
    funcInfo <- lookupFunc funcVar
    let varsNames = map (\(Var vname) -> vname) vars
        varTyPairs = zip varsNames (funcArgTypes funcInfo)
    expr' <- setAsCurrentFunc funcVar (extendVEnvs varTyPairs (walkExpr pass expr))
    funcVar' <- walkFuncVar pass fv
    typeScheme' <- walkTypeScheme pass typeScheme
    funcVar2' <- walkFuncVar pass funcVar2
    locRegions' <- walkLocRegions pass locRegions
    vars' <- walkVars pass vs
    
    let newFuncDecl = FuncDecl funcVar' typeScheme' funcVar2' locRegions' vars' expr'
    onFuncDecl pass newFuncDecl

walkLocatedType :: Pass -> LocatedType -> (InferM LocRegion) LocatedType
walkLocatedType pass (LocatedType combinedLocType locRegion) = do
    combinedLocType' <- walkCombinedLocType pass combinedLocType
    locRegion' <- walkLocRegion pass locRegion
    let newLocatedType = LocatedType combinedLocType' locRegion'
    onLocatedType pass newLocatedType

walkCombinedLocType :: Pass -> CombinedLocType -> (InferM LocRegion) CombinedLocType
walkCombinedLocType pass (CLTTypeCon typeCon) = do
    typeCon' <- walkTypeCon pass typeCon
    let newCombinedLocType = CLTTypeCon typeCon'
    onCombinedLocType pass newCombinedLocType
walkCombinedLocType pass (CLTBase baseType) = do
    baseType' <- walkBaseType pass baseType
    let newCombinedLocType = CLTBase baseType'
    onCombinedLocType pass newCombinedLocType

walkTypeScheme :: Pass -> TypeScheme -> (InferM LocRegion) TypeScheme
walkTypeScheme pass (TypeScheme combinedTypes) = do
    combinedTypes' <- walkCombinedTypes pass combinedTypes
    let newTypeScheme = TypeScheme combinedTypes'
    onTypeScheme pass newTypeScheme

walkCombinedType :: Pass -> CombinedType -> (InferM LocRegion) CombinedType
walkCombinedType pass (CTLocated locatedType) = do
    locatedType' <- walkLocatedType pass locatedType
    let newCombinedType = CTLocated locatedType'
    onCombinedType pass newCombinedType
walkCombinedType pass (CTBase baseType) = do
    baseType' <- walkBaseType pass baseType
    let newCombinedType = CTBase baseType'
    onCombinedType pass newCombinedType

walkBaseType :: Pass -> BaseType -> (InferM LocRegion) BaseType
walkBaseType = onBaseType

walkLocExpress :: Pass -> LocExpress -> (InferM LocRegion) LocExpress
walkLocExpress pass (LocExpressStart regionVar) = do
    regionVar' <- walkRegionVar pass regionVar
    let newLocExpress = LocExpressStart regionVar'
    onLocExpress pass newLocExpress
walkLocExpress pass (LocExpressNext locRegion offset) = do
    locRegion' <- walkLocRegion pass locRegion
    let newLocExpress = LocExpressNext locRegion' offset
    onLocExpress pass newLocExpress
walkLocExpress pass (LocExpressAfter locatedType) = do
    locatedType' <- walkLocatedType pass locatedType
    let newLocExpress = LocExpressAfter locatedType'
    onLocExpress pass newLocExpress

walkLocRegion :: Pass -> LocRegion -> (InferM LocRegion) LocRegion
walkLocRegion pass (LocRegion locVar regionVar indexVar) = do
    locVar' <- walkLocVar pass locVar
    regionVar' <- walkRegionVar pass regionVar
    indexVar' <- walkIndexVar pass indexVar
    let newLocRegion = LocRegion locVar' regionVar' indexVar'
    onLocRegion pass newLocRegion
walkLocRegion pass (LocRelativeVar s locVar regionVar indexVar) = do
    locVar' <- walkLocVar pass locVar
    regionVar' <- walkRegionVar pass regionVar
    indexVar' <- walkIndexVar pass indexVar
    let newLocRegion = LocRelativeVar s locVar' regionVar' indexVar'
    onLocRegion pass newLocRegion
walkLocRegion pass EmptyLocRegion = onLocRegion pass EmptyLocRegion

walkVal :: Pass -> Val -> (InferM LocRegion) Val
walkVal pass (ValVar n) = do
    n' <- walkVar pass n
    let newVal = ValVar n'
    onVal pass newVal
walkVal pass (ValLit lit) = do
    lit' <- walkLit pass lit
    let newVal = ValLit lit'
    onVal pass newVal

walkLit :: Pass -> Lit -> (InferM LocRegion) Lit
walkLit pass (IntLit n) = onLit pass (IntLit n)
walkLit pass (FloatLit f) = onLit pass (FloatLit f)
walkLit pass (BoolLit b) = onLit pass (BoolLit b)
walkLit pass (CharLit c) = onLit pass (CharLit c)
walkLit pass (StringLit s) = onLit pass (StringLit s)

walkExpr :: Pass -> Expr -> (InferM LocRegion) Expr
walkExpr pass (ExprVal val) = do
    val' <- walkVal pass val
    let newExpr = ExprVal val'
    onExpr pass newExpr
walkExpr pass (ExprBinOp binOp expr1 expr2) = do
    binOp' <- walkBinOp pass binOp
    expr1' <- walkExpr pass expr1
    expr2' <- walkExpr pass expr2
    let newExpr = ExprBinOp binOp' expr1' expr2'
    onExpr pass newExpr
walkExpr pass (ExprFuncApp funcVar locRegions exprs) = do
    funcVar' <- walkFuncVar pass funcVar
    locRegions' <- walkLocRegions pass locRegions
    exprs' <- walkExprs pass exprs
    let newExpr = ExprFuncApp funcVar' locRegions' exprs'
    onExpr pass newExpr
walkExpr pass (ExprDataConApp dataCon locRegion exprs) = do
    dataCon' <- walkDataCon pass dataCon
    locRegion' <- walkLocRegion pass locRegion
    exprs' <- walkExprs pass exprs
    let newExpr = ExprDataConApp dataCon' locRegion' exprs'
    onExpr pass newExpr
walkExpr pass (ExprCase val pats) = do
    val' <- walkVal pass val
    pats' <- walkPats pass pats
    let newExpr = ExprCase val' pats'
    onExpr pass newExpr
walkExpr pass (ExprLet v@(Var var) combinedType expr1 expr2) = do
    let exprType = combinedTypeToType combinedType
    var' <- walkVar pass v
    combinedType' <- walkCombinedType pass combinedType
    expr1' <- walkExpr pass expr1
    expr2' <- extendVEnv var exprType (walkExpr pass expr2) 
    let newExpr = ExprLet var' combinedType' expr1' expr2'
    onExpr pass newExpr
walkExpr pass (ExprLetLoc locRegion locExpress expr) = do
    locRegion' <- walkLocRegion pass locRegion
    locExpress' <- walkLocExpress pass locExpress
    expr' <- walkExpr pass expr
    let newExpr = ExprLetLoc locRegion' locExpress' expr'
    onExpr pass newExpr
walkExpr pass (ExprLetRegion regionVar expr) = do
    regionVar' <- walkRegionVar pass regionVar
    expr' <- walkExpr pass expr
    let newExpr = ExprLetRegion regionVar' expr'
    onExpr pass newExpr
walkExpr pass (ExprIf cond thenExpr elseExpr) = do
    cond' <- walkExpr pass cond
    thenExpr' <- walkExpr pass thenExpr
    elseExpr' <- walkExpr pass elseExpr
    let newExpr = ExprIf cond' thenExpr' elseExpr'
    onExpr pass newExpr

walkPat :: Pass -> Pat -> (InferM LocRegion) Pat
walkPat pass (Pat dc@(DataCon _dataCon) pms@(PatMatches patMatches) expr) = do
    let varTypePairs = map (\(PatMatch (ValVar (Var v)) locatedType) -> (v, locatedTypeToType locatedType)) patMatches
    expr' <- extendVEnvs varTypePairs (walkExpr pass expr)
    dataCon' <- walkDataCon pass dc
    patMatches' <- walkPatMatches pass pms
    let newPat = Pat dataCon' patMatches' expr'
    onPat pass newPat

walkPatMatch :: Pass -> PatMatch -> (InferM LocRegion) PatMatch
walkPatMatch pass (PatMatch val locatedType) = do
    val' <- walkVal pass val
    locatedType' <- walkLocatedType pass locatedType
    let newPatMatch = PatMatch val' locatedType'
    onPatMatch pass newPatMatch

walkBinOp :: Pass -> BinOp -> (InferM LocRegion) BinOp
walkBinOp pass binOp = onBinOp pass binOp

walkFuncVar :: Pass -> FuncVar -> (InferM LocRegion) FuncVar
walkFuncVar pass funcVar = onFuncVar pass funcVar

walkRegionVar :: Pass -> RegionVar -> (InferM LocRegion) RegionVar
walkRegionVar pass regionVar = onRegionVar pass regionVar

walkLocVar :: Pass -> LocVar -> (InferM LocRegion) LocVar
walkLocVar pass locVar = onLocVar pass locVar

walkIndexVar :: Pass -> IndexVar -> (InferM LocRegion) IndexVar
walkIndexVar pass indexVar = onIndexVar pass indexVar

walkTypeCon :: Pass -> TypeCon -> (InferM LocRegion) TypeCon
walkTypeCon pass typeCon = onTypeCon pass typeCon

walkDataCon :: Pass -> DataCon -> (InferM LocRegion) DataCon
walkDataCon pass dataCon = onDataCon pass dataCon

walkVar :: Pass -> Var -> (InferM LocRegion) Var
walkVar pass var = onVar pass var

walkVars :: Pass -> Vars -> (InferM LocRegion) Vars
walkVars pass (Vars vars) = do
    vars' <- mapM (walkVar pass) vars
    let newVars = Vars vars'
    onVars pass newVars

walkDataFields :: Pass -> DataFields -> (InferM LocRegion) DataFields
walkDataFields pass (DataFields dataFields) = do
    dataFields' <- mapM (walkDataField pass) dataFields
    let newDataFields = DataFields dataFields'
    onDataFields pass newDataFields

walkCombinedTypeCons :: Pass -> CombinedTypeCons -> (InferM LocRegion) CombinedTypeCons
walkCombinedTypeCons pass (CombinedTypeCons combinedTypeCons) = do
    combinedTypeCons' <- mapM (walkCombinedTypeCon pass) combinedTypeCons
    let newCombinedTypeCons = CombinedTypeCons combinedTypeCons'
    onCombinedTypeCons pass newCombinedTypeCons

walkExprs :: Pass -> Exprs -> (InferM LocRegion) Exprs
walkExprs pass (Exprs exprs) = do
    exprs' <- mapM (walkExpr pass) exprs
    let newExprs = Exprs exprs'
    onExprs pass newExprs

walkVals :: Pass -> Vals -> (InferM LocRegion) Vals
walkVals pass (Vals vals) = do
    vals' <- mapM (walkVal pass) vals
    let newVals = Vals vals'
    onVals pass newVals

walkPats :: Pass -> Pats -> (InferM LocRegion) Pats
walkPats pass (Pats pats) = do
    pats' <- mapM (walkPat pass) pats
    let newPats = Pats pats'
    onPats pass newPats

walkPatMatches :: Pass -> PatMatches -> (InferM LocRegion) PatMatches
walkPatMatches pass (PatMatches patMatches) = do
    patMatches' <- mapM (walkPatMatch pass) patMatches
    let newPatMatches = PatMatches patMatches'
    onPatMatches pass newPatMatches

walkDataTypeDecls :: Pass -> DataTypeDecls -> (InferM LocRegion) DataTypeDecls
walkDataTypeDecls pass (DataTypeDecls dataTypeDecls) = do
    dataTypeDecls' <- mapM (walkDataTypeDecl pass) dataTypeDecls
    let newDataTypeDecls = DataTypeDecls dataTypeDecls'
    onDataTypeDecls pass newDataTypeDecls

walkFuncDecls :: Pass -> FuncDecls -> (InferM LocRegion) FuncDecls
walkFuncDecls pass (FuncDecls funcDecls) = do
    funcDecls' <- mapM (walkFuncDecl pass) funcDecls
    let newFuncDecls = FuncDecls funcDecls'
    onFuncDecls pass newFuncDecls

walkLocRegions :: Pass -> LocRegions -> (InferM LocRegion) LocRegions
walkLocRegions pass (LocRegions locRegions) = do
    locRegions' <- mapM (walkLocRegion pass) locRegions
    let newLocRegions = LocRegions locRegions'
    onLocRegions pass newLocRegions

walkCombinedTypes :: Pass -> CombinedTypes -> (InferM LocRegion) CombinedTypes
walkCombinedTypes pass (CombinedTypes combinedTypes) = do
    combinedTypes' <- mapM (walkCombinedType pass) combinedTypes
    let newCombinedTypes = CombinedTypes combinedTypes'
    onCombinedTypes pass newCombinedTypes