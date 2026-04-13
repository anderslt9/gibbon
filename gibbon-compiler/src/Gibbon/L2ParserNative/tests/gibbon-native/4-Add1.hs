data Tree = Leaf Int | Node Tree Tree

add1 :: Tree -> Tree
add1 t = case t of
            Leaf n     -> Leaf (n + 1)
            Node left right -> Node (add1 left) (add1 right)

gibbon_main = let 
    result = add1 (Node (Leaf 1) (Leaf 2))
 in 0