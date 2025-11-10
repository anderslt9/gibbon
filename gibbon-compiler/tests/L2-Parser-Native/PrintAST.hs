module PrintAST where
import AST
import Control.Monad (forM_)

type Depth = Int
type ListName = String

indent :: Depth -> String -> String
indent depth line = replicate (depth * 2) ' ' ++ line

mergeStrings :: [String] -> String
mergeStrings strings = foldl (\acc c -> acc ++ c ++ "\n") "" strings

addList :: PrintAST a => Depth -> ListName -> [a] -> String
addList depth listName items = 
    if length items == 0
    then indent depth $ listName ++ " []\n"
    else
        let header = indent depth $ listName ++ " ["
            e1 = printAST (depth + 1) items
            footer = indent depth "]"
        in mergeStrings [header, e1, footer]

class PrintAST a where
    printAST :: Int -> a -> String

instance PrintAST a => PrintAST [a] where
    printAST depth xs = concatMap (\x -> printAST depth x ++ "\n") xs

-- program defined
instance PrintAST Program where
    printAST depth (Program dataTypeDecls funcDecls expr) = 
        let header = indent depth "Program ("
            e1 = printAST (depth + 1) dataTypeDecls
            e2 = printAST (depth + 1) funcDecls
            e3 = printAST (depth + 1) expr 
            footer = indent depth ")"
        in mergeStrings [header, e1, e2, e3, footer]

-- data type and function declarations
-- TODO
instance PrintAST DataTypeDecl where
    printAST depth dataTypeDecl = indent depth $ (show dataTypeDecl)

-- TODO
instance PrintAST FuncDecl where
    printAST depth funcDecl = indent depth $ (show funcDecl)

-- TODO
instance PrintAST Expr where 
    printAST depth expr = indent depth $ (show expr) ++ "\n"

-- list declarations
instance PrintAST DataTypeDecls where
    printAST depth (DataTypeDecls dataTypeDecls) = addList depth "Data Type Declarations" dataTypeDecls

instance PrintAST FuncDecls where 
    printAST depth (FuncDecls funcDecls) = addList depth "Function Declarations" funcDecls
