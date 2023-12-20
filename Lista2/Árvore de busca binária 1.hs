data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Eq, Ord, Read)


collapse :: Tree t -> [t]
collapse Nilt = []
collapse (Node valor sub1 sub2) = [valor] ++ collapse sub1 ++ collapse sub2


isBST :: Ord t => Tree t -> Bool
isBST Nilt = True
isBST (Node op Nilt Nilt) = True
isBST (Node op sub1 Nilt) | (foldr (max) op(collapse sub1) == op)  && isBST sub1 = True
                          | otherwise = False

isBST (Node op Nilt sub2) | (foldr (min) op(collapse sub2) == op) && isBST sub2 = True
                          | otherwise = False

isBST (Node op sub1 sub2) | (foldr (max) op(collapse sub1) == op) &&  (foldr (min) op(collapse sub2) == op) && isBST sub1 && isBST sub2 = True
                          | otherwise = False

main = do
       s <- getLine
       let result = isBST (read s::Tree Int)
       print result
