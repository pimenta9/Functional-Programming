data Arv a  = No (Arv a) (Arv a)
            | Vazia

altura :: Arv a -> Int
altura Vazia = 0
altura (No _ esq dir) = 1 + max (altura esq) (altura dir)

desvio :: Arv a -> Int
desvio Vazia = 0
desvio (No _ esq dir) = altura esq - altura dir

pesquisaAVL :: Ord a => a -> Arv a -> Bool
pesquisaAVL x Vazia = False
pesquisaAVL x (No y esq dir)
    | x == y = True
    | x < y = pesquisaAVL esq
    | x > y = pesquisaAVL dir

