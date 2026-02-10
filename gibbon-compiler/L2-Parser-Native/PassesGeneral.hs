module PassesGeneral where
import AST
import ConvertToTypedAST
import Helper (E)
import Control.Monad.Reader (ReaderT(runReaderT))
import Passes (walkDataTypeDecl)

data PassGen a = PassGen
    { onProgramGen :: Program -> (InferM LocRegion) a
    , onDataTypeDeclGen :: DataTypeDecl -> (InferM LocRegion) a
    , onDataFieldGen :: DataField -> (InferM LocRegion) a
    , onCombinedTypeConGen :: CombinedTypeCon -> (InferM LocRegion) a
    , onFuncDeclGen :: FuncDecl -> (InferM LocRegion) a
    , onLocatedTypeGen :: LocatedType -> (InferM LocRegion) a
    , onCombinedLocTypeGen :: CombinedLocType -> (InferM LocRegion) a
    , onTypeSchemeGen :: TypeScheme -> (InferM LocRegion) a
    , onCombinedTypeGen :: CombinedType -> (InferM LocRegion) a
    , onBaseTypeGen :: BaseType -> (InferM LocRegion) a
    , onLocExpressGen :: LocExpress -> (InferM LocRegion) a
    , onLocRegionGen :: LocRegion -> (InferM LocRegion) a
    , onValGen :: Val -> (InferM LocRegion) a
    , onLitGen :: Lit -> (InferM LocRegion) a
    , onExprGen :: Expr -> (InferM LocRegion) a
    , onPatGen :: Pat -> (InferM LocRegion) a
    , onPatMatchGen :: PatMatch -> (InferM LocRegion) a
    , onBinOpGen :: BinOp -> (InferM LocRegion) a
    , onFuncVarGen :: FuncVar -> (InferM LocRegion) a
    , onRegionVarGen :: RegionVar -> (InferM LocRegion) a
    , onLocVarGen :: LocVar -> (InferM LocRegion) a
    , onIndexVarGen :: IndexVar -> (InferM LocRegion) a
    , onTypeConGen :: TypeCon -> (InferM LocRegion) a
    , onDataConGen :: DataCon -> (InferM LocRegion) a
    , onVarGen :: Var -> (InferM LocRegion) a
    , onVarsGen :: Vars -> (InferM LocRegion) a
    , onDataFieldsGen :: DataFields -> (InferM LocRegion) a
    , onCombinedTypeConsGen :: CombinedTypeCons -> (InferM LocRegion) a
    , onExprsGen :: Exprs -> (InferM LocRegion) a
    , onValsGen :: Vals -> (InferM LocRegion) a
    , onPatsGen :: Pats -> (InferM LocRegion) a
    , onPatMatchesGen :: PatMatches -> (InferM LocRegion) a
    , onDataTypeDeclsGen :: DataTypeDecls -> (InferM LocRegion) a
    , onFuncDeclsGen :: FuncDecls -> (InferM LocRegion) a
    , onLocRegionsGen :: LocRegions -> (InferM LocRegion) a
    , onCombinedTypesGen :: CombinedTypes -> (InferM LocRegion) a
    , joinFunction :: [a] -> a 
    }

runProgramPassGen' :: Program -> PassGen a -> E a
runProgramPassGen' program pass = walkProgramGen pass program

walkProgramGen :: PassGen a -> Program -> E a
walkProgramGen pass (Program dataTypeDecls fd@(FuncDecls funcDecls) expr) = do
    env1 <- loadDataDecls dataTypeDecls emptyEnv
    env2 <- loadFuncDecls fd env1
    
    runReaderT (do
        main' <- walkExprGen pass expr
        funcs' <- mapM (walkFuncDeclGen pass) funcDecls
        let newProgram = joinFunction [funcs', main']
        onProgramGen pass newProgram
        ) env2

walkDataTypeDeclGen :: PassGen a -> DataTypeDecl -> (InferM LocRegion) a
walkDataTypeDeclGen pass (DataTypeDecl typeCon dataFields) = do
    dataCons' <- mapM (walkCombinedTypeConGen pass) dataCons