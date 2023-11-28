main = do
    lista <- getLine
    let readList = read lista :: [Int]
    num <- getLine
    let readNum = read num :: Int
    let result = somarMultiplos readList readNum
    print result
    
    
somarMultiplos :: [Int] -> Int -> [Int]
somarMultiplos []_ = []  -- Caso base
somarMultiplos (x:xs) y = (somasoma x y):somarMultiplos xs y

somasoma :: Int -> Int -> Int  -- Funcao de soma de elementos
somasoma x y |y > x = 0 -- Caso base 1, o verificador maior que o numero 
             |x == 0 = 0 -- Caso base 2, o valor ser 0
             |y == 0 = 0 -- Caso base 3, o valor ser multiplo de 0 
             |mod x y == 0  = x + somasoma (x-1) y
             |otherwise = somasoma (x - 1) y
