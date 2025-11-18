module AST where 

-- top-level program
data Program = Program DataTypeDecls FuncDecls Expr deriving Show

-- data type declarations
data DataTypeDecl = DataTypeDecl TypeCon DataFields deriving Show
data DataField = DataField DataCon CombinedTypeCons deriving Show
data CombinedTypeCon = CTCTypeCon TypeCon | CTCBase BaseType deriving Show

-- function declarations
data FuncDecl = FuncDecl FuncVar TypeScheme FuncVar LocRegions Vars Expr deriving Show

-- type expressions
data LocatedType = LocatedType TypeCon LocRegion deriving Show
newtype TypeScheme = TypeScheme CombinedTypes deriving Show
data CombinedType = CTLocated LocatedType | CTBase BaseType deriving Show
data BaseType = Int | Float | Bool | String deriving Show

-- location expressions
data LocExpress = LocExpressStart RegionVar | LocExpressNext LocExpress | LocExpressAfter LocatedType deriving Show
data LocRegion = LocRegion LocVar RegionVar IndexVar deriving Show

-- identifiers/literals
data Val = ValVar Var | ValLit Lit deriving Show
data Lit = IntLit Int | FloatLit Float | BoolLit Bool | StringLit String deriving Show

-- expressions
data Expr = ExprVal Val | ExprBinOp BinOp Expr Expr | ExprFuncApp FuncVar LocRegions Vals | ExprDataConApp DataCon LocRegion Vals
            | ExprCase Val Pats deriving Show
data Pat = Pat DataCon PatMatches Expr deriving Show
data PatMatch = PatMatch Val LocatedType deriving Show
data BinOp = Add | Sub | FAdd | FSub | FMul | Mul | Div | FDiv | Pow
            | Eq | FEq | CEq | Gt | Lt | FGt | FLt | Ge | Le | FGe | FLe | Neq | And | Or deriving Show

-- specific variable types
newtype FuncVar = FuncVar String deriving Show
newtype RegionVar = RegionVar String deriving Show
newtype LocVar = LocVar String deriving Show
newtype IndexVar = IndexVar String deriving Show
newtype TypeCon = TypeCon String deriving Show
newtype DataCon = DataCon String deriving Show
newtype Var = Var String deriving Show

-- repeated productions to model * operator
newtype Vars = Vars [Var] deriving Show
newtype DataFields = DataFields [DataField] deriving Show
-- newtype TypeCons = TypeCons [TypeCon] deriving Show
newtype CombinedTypeCons = CombinedTypeCons [CombinedTypeCon] deriving Show
newtype Vals = Vals [Val] deriving Show
newtype Pats = Pats [Pat] deriving Show
newtype PatMatches = PatMatches [PatMatch] deriving Show
newtype DataTypeDecls = DataTypeDecls [DataTypeDecl] deriving Show
newtype FuncDecls = FuncDecls [FuncDecl] deriving Show
newtype LocRegions = LocRegions [LocRegion] deriving Show
newtype CombinedTypes = CombinedTypes [CombinedType] deriving Show