fifi :: Int -> [Int]
fifi 0 = [0]
fifi 1 = [0,1]
fifi n = fifi (n-1) ++ [last (fifi(n-1)) + last (fifi(n-2))]

pegapar :: [Int] -> [Int]
pegapar [] = []
pegapar (x:xs) | mod x 2 == 0 = (x : pegapar xs)
               | otherwise = pegapar xs 

func1 :: String -> String 
func1 x = x ++ show (foldr (+) 0 (pegapar (fifi(length x - 1))))

main = do 
    input <- getLine 
    let result = func1 input
    print result
