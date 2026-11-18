import Gibbon.List
data TreeA = NodeA Int TreeA TreeA | EmptyA
data TreeB = NodeB Float TreeB TreeB | EmptyB

f :: Int -> Float
f x = (intToFloat x) .*. 1.5

g :: TreeA -> TreeB
g a = gmap (tlist [(NodeA, NodeB, f)]) a

gibbon_main = 
    let tr1 = NodeA 1 (NodeA 2 EmptyA EmptyA) (NodeA 3 EmptyA EmptyA)
        tr2 = g tr1
        _ = printPacked tr1
        _ = print_newline()
        _ = printPacked tr2
    in ()