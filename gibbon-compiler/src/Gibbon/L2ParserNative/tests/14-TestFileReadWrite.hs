data Tree = Leaf Int | Node Tree Tree

main =
    letregion r1 in
    
    -- initialize tree of form Node (Leaf 1) (Node (Leaf 4) (Leaf 8))
    letloc (l1,r1) = (start r1) in
    letloc (l2,r1) = ((l1,r1) + 1) in
    let left : Tree@(l2,r1) = Leaf (l2,r1) 1 in
    
    letloc (l3,r1) = (after Tree@(l2,r1)) in
    letloc (l4,r1) = ((l3,r1) + 1) in
    
    let rightLeft : Tree@(l4,r1) = Leaf (l4,r1) 4 in
    letloc (l5,r1) = (after Tree@(l4,r1)) in
    
    let rightRight : Tree@(l5,r1) = Leaf (l5,r1) 8 in
    let right : Tree@(l3,r1) = Node (l3,r1) rightLeft rightRight in

    let tree : Tree@(l1,r1) = Node (l1,r1) left right in
    let x : () = writePackedFile "test.packed" Tree@(l1,r1) tree in
        ()
