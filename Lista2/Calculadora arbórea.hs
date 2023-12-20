data Ops = SUM | MUL | SUB
           deriving (Read, Eq)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read) 

evalTree :: IntTree -> Int 
evalTree (Nilt a ) = a 
evalTree (Node ops (Nilt a) (Nilt b))  | ops == SUM =  a + b
                                       | ops == MUL = a * b
                                       | ops == SUB = a - b
evalTree (Node ops sub1 sub2) | ops == SUM =  evalTree sub1 +  evalTree sub2
                              | ops == MUL =  evalTree sub1 *  evalTree sub2
                              | ops == SUB =  evalTree sub1 -  evalTree sub2

evalTree (Node ops sub1 (Nilt sub2)) | ops == SUM = evalTree sub1 + sub2
                                     | ops == MUL = evalTree sub1 * sub2
                                     | ops == SUB = evalTree sub1 - sub2

evalTree (Node ops (Nilt sub1) sub2) | ops == SUM = sub1 + evalTree sub2
                                     | ops == MUL = sub1 * evalTree sub2
                                     | ops == SUB = sub1 - evalTree sub2

main = do
    s <- getLine
    let result = evalTree (read s)
    print result 
