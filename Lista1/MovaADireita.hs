paraDireita :: Int -> String -> String
paraDireita num frase | num == 0 = frase
                      | num /= 0 = addEspacos num ++ frase

addEspacos :: Int -> String
addEspacos 0 = "" -- Caso base para encerrar a recursão
addEspacos x | x /= 0 = " " ++ addEspacos (x - 1)

parseInput :: String -> (Int, String)
parseInput str = let [n, s] = words str
                 in (read n, s)

main :: IO ()
main = interact $ uncurry paraDireita . parseInput
