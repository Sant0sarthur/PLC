import Data.Char (digitToInt)

btoi :: String -> Int
btoi = foldl (\acc x -> acc * 2 + digitToInt x) 0

main :: IO ()
main = do
  s <- getLine
  let result = btoi s
  print result
