-- import Gibbon.List
data ListInt = ConsInt Int (ListInt) | NilInt
data ListFloat = ConsFloat Float (ListFloat) | NilFloat
data TreeA = NodeA (ListInt) TreeA TreeA | EmptyA
data TreeB = NodeB (ListFloat) TreeB TreeB | EmptyB

f :: ListInt -> ListFloat
f ls = case ls of
    NilInt -> NilFloat
    ConsInt x xs -> ConsFloat ((intToFloat x) .*. 1.5) (f xs)

-- I think that I could make it work recursively
-- h :: int -> float
-- h x = (intToFloat x) .*. 1.5
-- gmap (tlist [(NodeA, NodeB, tlist [ConsInt, ConstFloat, h])]) a
g :: TreeA -> TreeB
g a = gmap (tlist [(NodeA, NodeB, f)]) a

gibbon_main = 
    let tr1 = NodeA (ConsInt 1 (ConsInt 2 (ConsInt 3 NilInt))) (NodeA (ConsInt 4 (ConsInt 5 (ConsInt 6 NilInt))) EmptyA EmptyA) (NodeA (ConsInt 7 (ConsInt 8 (ConsInt 9 NilInt))) EmptyA EmptyA)
        tr2 = g tr1
        _ = printPacked tr1
        -- _ = print_newline
        _ = printPacked tr2
    in ()