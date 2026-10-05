data TreeA = NodeA Int TreeA TreeA | EmptyA
data TreeB = NodeB Float TreeB TreeB | EmptyB

f :: Int -> Float
f x = fromIntegral x * 1.5

gibbon_main = 
    let tr1 = NodeA 1 (NodeA 2 EmptyA EmptyA) (NodeA 3 EmptyA EmptyA)
        tr2 = map f tr1
    in 0