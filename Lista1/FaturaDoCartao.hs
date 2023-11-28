logMes :: String -> String -> Double
logMes _ [] = 0
logMes [] _  = 0
logMes mes fatura = foldl (+) 0 (verificaTodos mes (zip (pegames (splitBySemicolon fatura)) (conversor (pegavalor (splitBySemicolon fatura)))))


splitBySemicolon :: String -> [String]
splitBySemicolon [] = [""]
splitBySemicolon (x:xs)
  | x == ';'  = "" : rest
  | otherwise = (x : head rest) : tail rest
  where
    rest = splitBySemicolon xs
    
pegavalor :: [String] -> [String]
pegavalor [] = []
pegavalor (_:_:x:xs) = x : pegavalor xs
pegavalor (_:xs) = xs

pegames :: [String] -> [String]
pegames [] = []
pegames (x:_:_:xs) = drop 3 x : pegames xs
pegames (_:xs) = xs

conversor :: [String] -> [Double]
conversor [] = []
conversor (x:xs) = read x : conversor xs

verificaTodos :: String -> [(String, Double)] -> [Double]
verificaTodos _ [] = []
verificaTodos x ((a, b):xs) | x == a = b : verificaTodos x xs
                            | otherwise = verificaTodos x xs

main :: IO ()
main = do
  a <- getLine
  b <- getLine
  let result = logMes a b
  print result
