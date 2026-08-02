data Tree = Leaf Int | Node Tree Tree

mkTree :: Int -> Tree
mkTree i = 
  if i <= 0
  then Leaf 1
  else
      let x = mkTree (i-1)
          y = mkTree (i-1)
      in Node x y

gibbon_main = 
    let tree = mkTree 3 in
    let _ = writePackedFile "test.gpkd" tree in
        ()
