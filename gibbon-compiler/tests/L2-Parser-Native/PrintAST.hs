module PrintAST where
import AST
-- import Control.Monad (forM_)
import Data.List (intercalate)

type Depth = Int
type ListName = String

indent :: Depth -> String -> String
indent depth line = replicate (depth * 4) ' ' ++ line

mergeStrings :: [String] -> String
mergeStrings strings = foldl (\acc c -> acc ++ c ++ "\n") "" strings

mergeStrings2 :: [String] -> String
mergeStrings2 strings = intercalate "\n" strings

addList :: PrintAST a => Depth -> ListName -> [a] -> String
addList depth listName items = 
    if length items == 0
    then indent depth $ listName ++ " []\n"
    else
        let header = indent depth $ listName ++ " ["
            e1 = printAST (depth + 1) items
            footer = indent depth "]"
        in mergeStrings2 [header, e1, footer]

class PrintAST a where
    printAST :: Int -> a -> String

instance PrintAST a => PrintAST [a] where
    printAST depth xs = mergeStrings2 $ map (\x -> printAST depth x) xs

--------- top-level program --------- 
-- program defined
instance PrintAST Program where
    printAST depth (Program dataTypeDecls funcDecls expr) = 
        let header = indent depth "Program ("
            e1 = printAST (depth + 1) dataTypeDecls
            e2 = printAST (depth + 1) funcDecls
            e3 = printAST (depth + 1) expr 
            footer = indent depth ")"
        in mergeStrings2 [header, e1, e2, e3, footer]

--------- data type and function declarations --------- 
-- TODO
instance PrintAST DataTypeDecl where
    printAST depth dataTypeDecl = indent depth $ (show dataTypeDecl)

-- TODO
instance PrintAST DataField where
    printAST depth temp = indent depth $ (show temp)

-- TODO (maybe update by separating out parts of function declaration)
instance PrintAST FuncDecl where
    printAST depth (FuncDecl funcVar1 typeScheme funcVar2 locRegions vars expr) =
        let header = indent depth "Function Declaration ("
            e1 = printAST (depth + 1) funcVar1
            e2 = printAST (depth + 1) typeScheme
            e3 = printAST (depth + 1) funcVar2
            e4 = printAST (depth + 1) locRegions
            e5 = printAST (depth + 1) vars
            e6 = printAST (depth + 1) expr
            footer = indent depth ")"
        in mergeStrings2 [header, e1, e2, e3, e4, e5, e6, footer]

--------- type expressions ---------
-- TODO
instance PrintAST LocatedType where
    printAST depth temp = indent depth $ (show temp)

-- TODO
instance PrintAST TypeScheme where
    printAST depth temp = indent depth $ (show temp)

-- TODO
instance PrintAST CombinedType where
    printAST depth temp = indent depth $ (show temp)

-- TODO
instance PrintAST BaseType where
    printAST depth temp = indent depth $ (show temp)

--------- location expressions ---------
-- TODO
instance PrintAST LocExpress where
    printAST depth temp = indent depth $ (show temp)

-- TODO
instance PrintAST LocRegion where
    printAST depth temp = indent depth $ (show temp)

--------- identifiers/literals ---------
-- TODO
instance PrintAST Val where
    printAST depth temp = indent depth $ (show temp)

-- TODO
instance PrintAST Lit where
    printAST depth temp = indent depth $ (show temp)

--------- expressions ---------
instance PrintAST Expr where 
    printAST depth (ExprVal val) = printAST depth val
    printAST depth (ExprBinOp binOp expr1 expr2) = 
        let header = indent depth "Binary Operation [" ++ (printAST 0 binOp) ++ "] ("
            e1 = printAST (depth + 1) expr1
            e2 = printAST (depth + 1) expr2
            footer = indent depth ")"
        in mergeStrings2 [header, e1, e2, footer]
    printAST depth (ExprFuncApp funcVar locRegions vals) =
        let header = indent depth "Function Application ("
            e1 = printAST (depth + 1) funcVar
            e2 = printAST (depth + 1) locRegions
            e3 = printAST (depth + 1) vals
            footer = indent depth ")"
        in mergeStrings2 [header, e1, e2, e3, footer]
    -- printAST depth expr = indent depth $ (show expr)

instance PrintAST BinOp where
    printAST depth binOp = indent depth $ (show binOp)

--------- specific variable types ---------
instance PrintAST FuncVar where
    printAST depth (FuncVar var) = indent depth $ "Function Variable (\"" ++ var ++ "\")"

instance PrintAST RegionVar where
    printAST depth (RegionVar var) = indent depth $ "Region Variable (\"" ++ var ++ "\")"

instance PrintAST LocVar where
    printAST depth (LocVar var) = indent depth $ "Location Variable (\"" ++ var ++ "\")"

instance PrintAST IndexVar where
    printAST depth (IndexVar var) = indent depth $ "Index Variable (\"" ++ var ++ "\")"

instance PrintAST TypeCon where
    printAST depth (TypeCon var) = indent depth $ "Type Constructor (\"" ++ var ++ "\")"

instance PrintAST DataCon where
    printAST depth (DataCon var) = indent depth $ "Data Constructor (\"" ++ var ++ "\")"

instance PrintAST Var where
    printAST depth (Var var) = indent depth $ "Variable (\"" ++ var ++ "\")"

--------- list declarations ---------
instance PrintAST Vars where
    printAST depth (Vars vars) = addList depth "Vars" vars

instance PrintAST DataFields where
    printAST depth (DataFields dataFields) = addList depth "Data Fields" dataFields

instance PrintAST TypeCons where
    printAST depth (TypeCons typeCons) = addList depth "Type Constructors" typeCons

instance PrintAST Vals where
    printAST depth (Vals vals) = addList depth "Vals" vals

instance PrintAST DataTypeDecls where
    printAST depth (DataTypeDecls dataTypeDecls) = addList depth "Data Type Declarations" dataTypeDecls

instance PrintAST FuncDecls where 
    printAST depth (FuncDecls funcDecls) = addList depth "Function Declarations" funcDecls

instance PrintAST LocRegions where
    printAST depth (LocRegions locRegions) = addList depth "Location Regions" locRegions

instance PrintAST CombinedTypes where
    printAST depth (CombinedTypes combinedTypes) = addList depth "Combined Types" combinedTypes