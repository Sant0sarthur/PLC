data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read, Ord, Eq)

depth :: (Num t, Ord t) => Tree t -> Int
depth Nilt = 0
depth (Node _ t1 t2) = 1 + max (depth t1) (depth t2)

diameter :: (Num t, Ord t) => Tree t -> Int
diameter Nilt = 0
diameter (Node _ t1 t2) = max (depth t1 + depth t2 + 1) (max (diameter t1) (diameter t2))

main :: IO ()
main = do
    s <- getLine
    let result = diameter (read s :: Tree Int)
    print result
