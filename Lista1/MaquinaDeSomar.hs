maquinaSomar :: [Int] -> [Int]
maquinaSomar [] = []
maquinaSomar (x:xs)  | x == 0 && oraculo xs = []
                     | x == 0 = [] ++ maquinaSomar xs
                     | otherwise = [] ++ [adder (x:xs)] ++ maquinaSomar (oraculo2 (x:xs))

adder :: [Int] -> Int
adder [] = 0
adder (0:_) = 0
adder (x:xs) = x + adder xs

oraculo :: [Int] -> Bool
oraculo [] = False
oraculo (x:xs) | x == 0 = True
               | otherwise = False
               

oraculo2 :: [Int] -> [Int]
oraculo2 [] = []
oraculo2 (x:xs) | x == 0 && oraculo xs = (x:xs)
                | x == 0 = xs
                | otherwise = oraculo2 xs

main = do
       lista <- getLine
       print $ maquinaSomar (read lista :: [Int])
       
       
