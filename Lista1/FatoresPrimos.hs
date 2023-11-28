fatPrime :: Int -> [(Int, Int)]
fatPrime 1 = []
fatPrime x |ehPrimo x == True = [(x, 1)]
           |mod x 2 == 0 = [(2, cont x 2)] ++ fatPrime(div x (2 ^ (cont x 2)))
           |mod x 3 == 0 = [(3, cont x 3)] ++ fatPrime(div x (3 ^ (cont x 3)))
           |mod x 5 == 0 = [(5, cont x 5)] ++ fatPrime(div x (5 ^ (cont x 5)))
           |mod x 7 == 0 = [(7, cont x 7)] ++ fatPrime(div x (7 ^ (cont x 7)))
           |mod x 11 == 0 = [(11, cont x 11)] ++ fatPrime(div x (11 ^ (cont x 11)))
           |mod x 13 == 0 = [(13, cont x 13)] ++ fatPrime(div x (13 ^ (cont x 13)))
           |mod x 17 == 0 = [(17, cont x 17)] ++ fatPrime(div x (17 ^ (cont x 17)))
           |mod x 23 == 0 = [(23, cont x 23)] ++ fatPrime(div x (23 ^ (cont x 23)))

cont :: Int -> Int -> Int
cont a y | (mod a y) == 0 = 1 + cont (div a y) y
         | otherwise = 0 

testaPrimo:: Int->Int->Bool
testaPrimo x y  | y == 1 = True
                | mod x y == 0 = False
                | otherwise = testaPrimo x (y-1)

ehPrimo :: Int -> Bool
ehPrimo 1 = False
ehPrimo n = testaPrimo n (n-1)

main = do
      a <- getLine
      let result = fatPrime (read a :: Int)
      print result
