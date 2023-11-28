minMaxCartao :: String -> (Double, Double)
minMaxCartao xs = maxmin (conversor( skipEveryThree  (splitBySemicolon xs)))

splitBySemicolon :: String -> [String]
splitBySemicolon [] = [""]
splitBySemicolon (x:xs)
  | x == ';'  = "" : rest
  | otherwise = (x : head rest) : tail rest
  where
    rest = splitBySemicolon xs
    
skipEveryThree :: [String] -> [String]
skipEveryThree [] = []
skipEveryThree (_:_:x:xs) = x : skipEveryThree xs
skipEveryThree (_:xs) = xs

conversor :: [String] -> [Double]
conversor [] = []
conversor (x:xs) = read x : conversor xs

maxmin :: [Double] -> (Double, Double)
maxmin valores = (minimum valores, maximum valores)

main :: IO ()
main = do
    a <- getLine
    let result = minMaxCartao a
    print result
