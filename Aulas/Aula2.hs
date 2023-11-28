
fat :: Int-> Int
fat 0 = 1
fat n = n * fat (n-1)

exOr :: Bool -> Bool -> Bool
exOr x y = (x || y) && not (x && y)

vendas :: Int->Int
vendas semana | semana == 0 = 30
              | semana == 1 = 50
              | semana == 2 = 80
              | semana == 3 = 150
              | semana == 4 = 200
              | semana == 5 = 50
              | semana == 6 = 150
              | semana == 7 = 50 

totalVendas :: Int->Int
totalVendas n | n == 0 = vendas 0
              | otherwise = totalVendas (n-1) + vendas n

maxi :: Int->Int->Int
maxi x y | x>y = x
         | otherwise = y

maxVendas :: Int -> Int
maxVendas n | n == 0 = vendas 0
            | otherwise = maxi (maxVendas(n-1)) (vendas n)

addEspacos :: Int->String
addEspacos 1 = " "
addEspacos n = " " ++ addEspacos(n-1)

paraDireita :: Int->String->String
paraDireita n s = addEspacos n  ++ s

imprimeSemanas :: Int->String
imprimeSemanas 0 = show (vendas 0) ++ addEspacos 7 ++ "0"
imprimeSemanas n = imprimeSemanas(n-1) ++ "\n" ++ show (vendas n )++ addEspacos 7 ++ show n

mediaVendas :: Int->Float
mediaVendas n = fromIntegral (totalVendas n)/ fromIntegral (n+1)

imprimeTabela :: Int -> IO()
imprimeTabela n = putStr ("Vendas da Semanas\n" ++ imprimeSemanas n ++ "\n" ++ show (totalVendas n) ++ "\n" ++ show (mediaVendas n)++ "\n")

-- div, mod
-- show x = "x"
-- "x" ++ "y" = "xy"
-- read ("2345") = Int = 2345
-- floor,celing,round -> para divisao de floats/double = /
