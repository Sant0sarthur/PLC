import Data.Char 

sumNumbers :: String -> Int 
sumNumbers "" = 0  
sumNumbers (x:xs) | (x >= '0') && (x <= '9') = ord x - 48 + sumNumbers xs
                  | otherwise = sumNumbers xs  

main :: IO ()
main = do
  a <- getLine
  let result = sumNumbers a
  print result
