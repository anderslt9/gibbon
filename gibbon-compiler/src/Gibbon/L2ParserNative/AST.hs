module Gibbon.L2ParserNative.AST where 

-- top-level program
data Program = Program DataTypeDecls FuncDecls Expr deriving (Show,Eq)

-- data type declarations
data DataTypeDecl = DataTypeDecl TypeCon DataFields deriving (Show,Eq)
data DataField = DataField DataCon CombinedTypeCons deriving (Show,Eq)
data CombinedTypeCon = CTCTypeCon TypeCon | CTCBase BaseType deriving (Show,Eq)

-- function declarations
data FuncDecl = FuncDecl FuncVar TypeScheme FuncVar LocRegions Vars Expr deriving (Show,Eq)

-- type expressions
data LocatedType = LocatedType CombinedTypeCon LocRegion deriving (Show, Eq)
-- data CombinedLocType = CLTTypeCon TypeCon | CLTBase BaseType deriving (Show, Eq)
newtype TypeScheme = TypeScheme CombinedTypes deriving (Show,Eq)
data CombinedType = CTLocated LocatedType | CTBase BaseType deriving (Show,Eq)
data BaseType = Int | Float | Bool | Char | String deriving (Show,Eq)

-- location expressions
data LocExpress = LocExpressStart RegionVar | LocExpressNext LocRegion Int | LocExpressAfter LocatedType deriving (Show,Eq)
data LocRegion = LocRegion LocVar RegionVar IndexVar | LocRelativeVar String LocVar RegionVar IndexVar | EmptyLocRegion deriving (Show, Ord)

instance Eq LocRegion where
    (==) EmptyLocRegion EmptyLocRegion = True
    (==) (LocRegion {}) EmptyLocRegion = True
    (==) EmptyLocRegion (LocRegion {}) = True
    (==) (LocRegion lv1 rv1 (IndexVar "")) (LocRegion lv2 rv2 _) =
        lv1 == lv2 && rv1 == rv2
    (==) (LocRegion lv1 rv1 _) (LocRegion lv2 rv2 (IndexVar "")) =
        lv1 == lv2 && rv1 == rv2
    (==) (LocRegion lv1 rv1 iv1) (LocRegion lv2 rv2 iv2) =
        lv1 == lv2 && rv1 == rv2 && iv1 == iv2
    (==) (LocRelativeVar s1 _ _ _) (LocRelativeVar s2 _ _ _) = s1 == s2
    (==) _ _ = False

-- identifiers/literals
data Val = ValVar Var | ValLit Lit deriving (Show,Eq)
data Lit = IntLit Int | FloatLit Float | BoolLit Bool | CharLit Char | StringLit String deriving (Show,Eq)

-- expressions
data Expr = ExprVal Val | ExprPrimApp PrimFunc Exprs | ExprFuncApp FuncVar LocRegions Exprs | ExprDataConApp DataCon LocRegion Exprs
            | ExprCase Val Pats | ExprLet Var CombinedType Expr Expr | ExprLetLoc LocRegion LocExpress Expr
            | ExprLetRegion RegionVar Expr | ExprIf Expr Expr Expr deriving (Show,Eq)
data Pat = Pat DataCon PatMatches Expr deriving (Show,Eq)
data PatMatch = PatMatch Val LocatedType deriving (Show,Eq)
data PrimFunc = 
        Add | Sub | FAdd | FSub | FMul | Mul | Div | FDiv | Pow
        | Eq | FEq | CEq | Gt | Lt | FGt | FLt | Ge | Le | FGe | 
        FLe | Neq | And | Or 
        
        -- | PrintInt | PrintChar | PrintFloat | PrintBool
        deriving (Show,Eq)

-- specific variable types
-- newtype LVar = LVar String deriving (Show, Eq, Ord)
-- newtype UVar = UVar String deriving (Show, Eq, Ord)
-- data LVar = FV FuncVar | RV RegionVar | LV LocVar | IV IndexVar | VAR Var deriving (Show, Eq, Ord)
-- data UVar = TC TypeCon | DC DataCon deriving (Show, Eq, Ord)
newtype FuncVar = FuncVar String deriving (Show, Eq, Ord)
newtype RegionVar = RegionVar String deriving (Show, Eq, Ord)
newtype LocVar = LocVar String deriving (Show, Eq, Ord)
newtype IndexVar = IndexVar String deriving (Show, Eq, Ord)
newtype TypeCon = TypeCon String deriving (Show, Eq, Ord)
newtype DataCon = DataCon String deriving (Show, Eq, Ord)
newtype Var = Var String deriving (Show, Eq, Ord)

-- repeated productions to model * operator
newtype Vars = Vars [Var] deriving (Show,Eq)
newtype DataFields = DataFields [DataField] deriving (Show,Eq)
newtype CombinedTypeCons = CombinedTypeCons [CombinedTypeCon] deriving (Show,Eq)
newtype Exprs = Exprs [Expr] deriving (Show,Eq)
newtype Vals = Vals [Val] deriving (Show,Eq)
newtype Pats = Pats [Pat] deriving (Show,Eq)
newtype PatMatches = PatMatches [PatMatch] deriving (Show,Eq)
newtype DataTypeDecls = DataTypeDecls [DataTypeDecl] deriving (Show,Eq)
newtype FuncDecls = FuncDecls [FuncDecl] deriving (Show,Eq)
newtype LocRegions = LocRegions [LocRegion] deriving (Show,Eq)
newtype CombinedTypes = CombinedTypes [CombinedType] deriving (Show,Eq)