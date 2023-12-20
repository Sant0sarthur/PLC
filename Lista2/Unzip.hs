unzip1:: [(a,b)] -> ([a],[b])
unzip1 [] = ([],[])
unzip1 ((x, y): xs) = ((x: fst(unzip1 xs)), (y: snd(unzip1 xs)))
main = interact $ show . unzip1 . (read :: String -> [(Int,Int)])
