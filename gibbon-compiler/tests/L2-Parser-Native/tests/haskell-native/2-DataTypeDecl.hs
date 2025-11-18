data Tree = Leaf Int | Node Int Tree Tree

addTree :: Tree
addTree = Leaf 1

main :: Tree
main = addTree