    -- 1
data Arv a = Vazia | No a (Arv a) (Arv a)
            deriving Show

    -- a)
profound :: Arv a -> Int
profound Vazia = 0
profound (No x y z) = 1 + max (profound y) (profound z)

    -- b)
tamanhoArv :: Arv a -> Int
tamanhoArv Vazia = 0
tamanhoArv (No x y z) = 1 + tamanhoArv y + tamanhoArv z

    -- c)
eq_struct :: Arv a -> Arv b -> Bool
eq_struct   Vazia           Vazia       = True
eq_struct   Vazia       (No x' y' z')   = False
eq_struct (No x y z)        Vazia       = False
eq_struct (No x y z)    (No x' y' z')   = eq_struct y y' && eq_struct z z'

    -- 2
    -- a)
balanceada :: Arv a -> Bool
balanceada Vazia = True
balanceada (No x y z) = (abs(profound y - profound z) <= 1) && balanceada y && balanceada z

    -- b)
balancear :: [a] -> Arv a
balancear [] = Vazia
balancear (x:xs) = No x (balancear (take h xs)) (balancear (drop h xs))
                    where h = div (length xs) 2

    -- 3
data Expr = Val Int | Soma Expr Expr

    -- a)
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Soma e e') = g (folde f g e) (folde f g e')

    -- b)
valor :: Expr -> Int
valor (Val x) = x
valor (Soma e e') = valor e + valor e'

tamanhoExpr :: Expr -> Int
tamanhoExpr (Val x) = 1
tamanhoExpr (Soma e e') = tamanhoExpr e + tamanhoExpr e'

    -- 4
    -- a)
-- data Node
-- data Grafo a = 
--     deriving Show
