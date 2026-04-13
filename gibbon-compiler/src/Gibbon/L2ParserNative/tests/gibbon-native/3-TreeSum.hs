data Tree = Leaf Int | Node Tree Tree

sum :: Tree -> Int
sum t = case t of
            Leaf n     -> n
            Node a b -> (sum a) + (sum b)

gibbon_main = sum (Node (Leaf 1) (Leaf 2))