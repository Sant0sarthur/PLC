data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Eq, Read, Show)

faces :: Direction -> [Command] -> Direction
faces direcao (Forward x:xs) = faces direcao xs
faces direcao (Backward x:xs) = faces direcao xs
faces direcao [] = direcao
faces direcao (TurnLeft:xs) | direcao == North = faces West xs 
                            | direcao == West = faces South xs 
                            | direcao == South = faces East xs 
                            | direcao == East = faces North xs 

faces direcao (TurnRight:xs) | direcao == North = faces East xs 
                             | direcao == East = faces South xs 
                             | direcao == South = faces West xs 
                             | direcao == West = faces North xs 

main = do
       a <- getLine
       b <- getLine
       let result = faces (read a) (read b)
       print result
