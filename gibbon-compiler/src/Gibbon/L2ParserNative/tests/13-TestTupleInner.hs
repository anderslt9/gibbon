main =  let (x,y) : ((Int, Int), Int) = ((1,2),4) in 
        let ((a,b),c) : ((Int, Int), Int) = (x,y) in 
        (a * b + c) / b