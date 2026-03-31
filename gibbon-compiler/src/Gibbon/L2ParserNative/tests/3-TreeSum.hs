data Tree = Leaf Int | Node Tree Tree

sum : Tree@(l,r) -> Int
sum [(l,r)] t = case t of 
                    Leaf (n : Int@(l,r,n)) -> n 
                    Node (a : Tree@(l,r,a)) (b : Tree@(l,r,b)) ->
                        (sum [(l,r,a)] a) + (sum [(l,r,b)] b)

main = 
    letregion r in
    letloc (l,r) = (start r) in
    -- let root : Tree@(l,r) = Leaf (l,r) 3 in
    -- sum [(l,r)] root
    sum [(l,r)] (Leaf (l,r) 3)