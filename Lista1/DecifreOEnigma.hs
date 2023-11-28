decEnigma :: String -> [(Char, Char)] -> String
decEnigma "" _ =  ""
decEnigma (f:l) ((a,b):xs) | f == a = b : decEnigma l ((a,b):xs)
                           | f /= b = oraculo (f:l) xs ((a,b):xs)

oraculo :: String -> [(Char, Char)] -> [(Char, Char)] -> String
oraculo (f:l) ((a,b):xs) dicio | f == a = b : decEnigma l dicio
                               | f /= b = oraculo (f:l) xs dicio
                               | otherwise = decEnigma l dicio
 
main = do
  a <- getLine
  b <- getLine
  let result = decEnigma a (read b) 
  print result


