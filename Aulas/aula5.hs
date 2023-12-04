data Expr = Lit Int
        | Add Expr Expr
        | Sub Expr Expr
    deriving Show

teste1, teste2 :: Expr
teste1 = Add (Sub (Lit 10) (Lit 20)) (Sub (Lit 20)(Lit 5)) 
teste2 = Sub teste1 (Lit 100)


eval :: Expr -> Int
eval (Lit n) = n 
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)

showExpr :: Expr -> String 
showExpr (Lit value) = show value 
showExpr (Add subexp1 subexp2) = "(" ++ showExpr subexp1 ++ "+" ++ showExpr subexp2 ++ ")" 
showExpr (Sub subexp1 subexp2) = "(" ++ showExpr subexp1 ++ "+" ++ showExpr subexp2 ++ ")" 

data List t = Nil | Cons t (List t)

teste3 :: List Int
teste3 = Cons 1 (Cons 2 (Cons 3 Nil))

tolist :: List t -> [t]
tolist Nil = []
tolist (Cons x xs) = x : tolist xs

data Tree t = Leaf | Node t (Tree t) (Tree t)
    deriving Show
data Tree2 t = Leaf2 t | Node2 (Tree2 t) (Tree2 t)
    deriving Show

teste4, teste5 :: Tree String 
teste4 = Node "abc" (Node "xy" Leaf Leaf) (Node "st" Leaf Leaf)
teste5 = Node "aei" Leaf teste4 

depth :: Tree t -> Int 
depth Leaf = 0 
depth (Node valor t1 t2) = 1 + max (depth t1) (depth t2)
