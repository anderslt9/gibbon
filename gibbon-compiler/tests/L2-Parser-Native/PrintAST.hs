-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

module PrintAST where
import AST
-- import Control.Monad (forM_)
import Data.List (intercalate)

type Depth = Int
type ListName = String

indent :: Depth -> String -> String
indent depth line = replicate (depth * 4) ' ' ++ line

-- mergeStrings :: [String] -> String
-- mergeStrings = foldl (\acc c -> acc ++ c ++ "\n") ""

mergeStrings2 :: [String] -> String
mergeStrings2 = intercalate "\n"

getFullExpr :: Depth -> String -> [String] -> String
getFullExpr depth name children =
    let header = indent depth $ name ++ " ("
        footer = indent depth ")"
    in mergeStrings2 $ header : children ++ [footer]

addList :: PrintAST a => Depth -> ListName -> [a] -> String
addList depth listName items =
    if null items
    then indent depth $ listName ++ " []\n"
    else
        let header = indent depth $ listName ++ " ["
            e1 = printAST (depth + 1) items
            footer = indent depth "]"
        in mergeStrings2 [header, e1, footer]

class PrintAST a where
    printAST :: Depth -> a -> String

instance PrintAST a => PrintAST [a] where
    printAST depth xs = mergeStrings2 $ map (printAST depth) xs

-- children class to help with displaying
class Children r where
    children :: Int -> [String] -> r

instance Children [String] where
    children _ acc = reverse acc

instance (PrintAST a, Children r) => Children (a -> r) where
    children depth acc x =
        children depth (printAST (depth + 1) x : acc)

getChildren :: Children r => Depth -> r
getChildren depth = children depth []

--------- top-level program --------- 
-- program defined
instance PrintAST Program where
    printAST depth (Program dataTypeDecls funcDecls expr) =
        getFullExpr depth "Program" (getChildren depth dataTypeDecls funcDecls expr)

--------- data type and function declarations --------- 
instance PrintAST DataTypeDecl where
    printAST depth (DataTypeDecl typeCon dataFields) =
        getFullExpr depth "Data Type Declaration" (getChildren depth typeCon dataFields)

instance PrintAST DataField where
    printAST depth (DataField dataCon combinedTypeCons) =
        getFullExpr depth "Data Field" (getChildren depth dataCon combinedTypeCons)

-- TODO
instance PrintAST CombinedTypeCon where
    printAST depth temp = indent depth (show temp)    

-- TODO (maybe update by separating out parts of function declaration)
instance PrintAST FuncDecl where
    printAST depth (FuncDecl funcVar1 typeScheme funcVar2 locRegions vars expr) =
        getFullExpr depth "Function Declaration" (getChildren depth funcVar1 typeScheme funcVar2 locRegions vars expr)
        -- let header = indent depth "Function Declaration ("
        --     e1 = printAST (depth + 1) funcVar1
        --     e2 = printAST (depth + 1) typeScheme
        --     e3 = printAST (depth + 1) funcVar2
        --     e4 = printAST (depth + 1) locRegions
        --     e5 = printAST (depth + 1) vars
        --     e6 = printAST (depth + 1) expr
        --     footer = indent depth ")"
        -- in mergeStrings2 [header, e1, e2, e3, e4, e5, e6, footer]

--------- type expressions ---------
-- TODO
instance PrintAST LocatedType where
    printAST depth temp = indent depth (show temp)

-- TODO
instance PrintAST CombinedLocType where
    printAST depth temp = indent depth (show temp)

-- TODO
instance PrintAST TypeScheme where
    printAST depth temp = indent depth (show temp)

-- TODO
instance PrintAST CombinedType where
    printAST depth temp = indent depth (show temp)

-- TODO
instance PrintAST BaseType where
    printAST depth temp = indent depth (show temp)

--------- location expressions ---------
-- TODO
instance PrintAST LocExpress where
    printAST depth temp = indent depth (show temp)

-- TODO
instance PrintAST LocRegion where
    printAST depth temp = indent depth (show temp)

--------- identifiers/literals ---------
-- TODO
instance PrintAST Val where
    printAST depth temp = indent depth (show temp)

-- TODO
instance PrintAST Lit where
    printAST depth temp = indent depth (show temp)

--------- expressions ---------
instance PrintAST Expr where
    printAST depth (ExprVal val) = printAST depth val
    
    printAST depth (ExprBinOp binOp expr1 expr2) =
        getFullExpr depth "Binary Operation" (getChildren depth binOp expr1 expr2)
    
    printAST depth (ExprFuncApp funcVar locRegions exprs) =
        getFullExpr depth "Function Application" (getChildren depth funcVar locRegions exprs)

    printAST depth (ExprDataConApp dataCon locRegion exprs) =
        getFullExpr depth "Data Constructor Application" (getChildren depth dataCon locRegion exprs)

    printAST depth (ExprCase val pats) =
        getFullExpr depth "Case Statement" (getChildren depth val pats)
    
    -- TODO maybe rework to look better and not just be super nested
    printAST depth (ExprLet var combinedType expr1 expr2) =
        getFullExpr depth "Let Expression" (getChildren depth var combinedType expr1 expr2)

    printAST depth (ExprLetLoc locRegion locExpress expr) =
        getFullExpr depth "Let Location Expression" (getChildren depth locRegion locExpress expr)

    printAST depth (ExprLetRegion regionVar expr) =
        getFullExpr depth "Let Region Expression" (getChildren depth regionVar expr)

instance PrintAST Pat where
    printAST depth (Pat dataCon patMatches expr) = 
        getFullExpr depth "Pattern" (getChildren depth dataCon patMatches expr)

instance PrintAST PatMatch where
    printAST depth (PatMatch val locatedType) = 
        getFullExpr depth "Pattern Match" (getChildren depth val locatedType)

instance PrintAST BinOp where
    printAST depth binOp = indent depth (show binOp)

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

-- instance PrintAST TypeCons where
--     printAST depth (TypeCons typeCons) = addList depth "Type Constructors" typeCons

instance PrintAST CombinedTypeCons where
    printAST depth (CombinedTypeCons combinedTypeCons) = addList depth "Combined Type Constructor" combinedTypeCons

instance PrintAST Exprs where
    printAST depth (Exprs exprs) = addList depth "Expressions" exprs

instance PrintAST Vals where
    printAST depth (Vals vals) = addList depth "Vals" vals

instance PrintAST Pats where
    printAST depth (Pats pats) = addList depth "Patterns" pats

instance PrintAST PatMatches where
    printAST depth (PatMatches patMatches) = addList depth "Pattern Matches" patMatches

instance PrintAST DataTypeDecls where
    printAST depth (DataTypeDecls dataTypeDecls) = addList depth "Data Type Declarations" dataTypeDecls

instance PrintAST FuncDecls where
    printAST depth (FuncDecls funcDecls) = addList depth "Function Declarations" funcDecls

instance PrintAST LocRegions where
    printAST depth (LocRegions locRegions) = addList depth "Location Regions" locRegions

instance PrintAST CombinedTypes where
    printAST depth (CombinedTypes combinedTypes) = addList depth "Combined Types" combinedTypes