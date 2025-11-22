data Tree = Leaf Int | Node Tree Tree

add1 : Tree@(l5,r2) -> Tree@(l6,r3)
add1 [(l5,r2) (l6,r3)] tr1 = case tr1 of
                Leaf (n : Int@(l5,r2,n)) -> 
                    let v : Int = n + 1 in 
                    let lf : Tree@(l6,r3) = Leaf (l6,r3) v in
                    lf
                Node (left : Tree@(l5,r2,left)) (right : Tree@(l5,r2,right)) ->
                    letloc (l7,r3) = ((l6,r3) + 1) in
                    let x2 : Tree@(l7,r3) = add1 [(l5,r2) (l7,r3)] left in
                    letloc (l8,r3) = (after Tree@(l7,r3)) in
                    let y2 : Tree@(l8,r3) = add1 [(l5,r2) (l8,r3)] right in
                    let z2 : Tree@(l6,r3) = Node (l6,r3) x2 y2 in
                    z2

-- computes "main = Add1 (Node (Leaf 1) (Leaf 2))"
main = 
    -- defines region for input 
    letregion r in 
    
    -- sets locations for root and left nodes
    letloc (l1,r) = (start r) in
    letloc (l2,r) = ((l1,r) + 1)  in
    
    -- initializes left node
    let left : Tree@(l2,r) = Leaf (l2,r) 1 in
    
    -- sets right node location
    letloc (l3,r) = (after Tree@(l2,r)) in
    
    -- initializes right node and root node
    let y : Tree@(l3,r) = Leaf (l3,r) 2 in 
    let z : Tree@(l1,r) = Node (l1,r) left y in 
    
    -- defines region and location for output of function
    letregion r1 in
    letloc (l4,r1) = (start r1) in 
    
    -- computes add1 on input tree
    let a : Tree@(l4,r1) = add1 [(l1,r) (l4,r1)] z in
    a