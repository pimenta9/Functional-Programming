    -- 1
data Stack a = Stk [a]
    deriving (Show)

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _           = error "Stack.pop: empty stack"

top :: Stack a -> a
top (Stk (x:xs)) = x
top _           = error "Stack.top: empty stack"

empty_ :: Stack a
empty_ = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk []) = True
isEmpty _        = False

    -- 1 (a)

calc :: Stack Float -> String -> Stack Float
calc stk str
    | str == "+" = push (top stk + top (pop stk)) (pop (pop (stk)))
    | str == "-" = push (top stk - top (pop stk)) (pop (pop (stk)))
    | str == "*" = push (top stk * top (pop stk)) (pop (pop (stk)))
    | str == "/" = push (top stk / top (pop stk)) (pop (pop (stk)))
    | otherwise = push (read str) stk

    -- 1 (b)

calcular_ :: Stack Float -> [String] -> Float
calcular_ stk [] = top stk
calcular_ stk (x:xs) = calcular_ (calc stk x) xs

calcular :: String -> Float
calcular str = calcular_ empty_ (words str)

    -- 1 (c)

main = do
    putStr "Digite a expressão:\n"
    str <- getLine
    putStr ("Resultado: " ++ show (calcular str) ++ "\n")

    -- 2

data Set a = Node a (Set a) (Set a)
            | Empty
            deriving (Show)

empty :: Set a
empty = Empty

insert :: Ord a => a -> Set a -> Set a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y = Node y (insert x left) right
    | x > y = Node y left (insert x right)
    | x == y = (Node y left right)

member :: Ord a => a -> Set a -> Bool
member x Empty = False
member x (Node y left right)
    | x == y = True
    | x < y = member x left
    | x > y = member x right

    -- 3

union, intersect, difference :: Ord a => Set a -> Set a -> Set a

union Empty s = s
union s Empty = s
union s (Node x left right) = union (insert x s) (union left right)

intersect Empty _ = Empty
intersect _ Empty = Empty
intersect (Node x left right) s
    | member x s = insert x (union (intersect left s) (intersect right s))
    | otherwise = (union (intersect left s) (intersect right s))

difference s Empty = s
difference Empty _ = Empty
difference (Node x left right) s
    | member x s = difference (union left right) s
    | otherwise = insert x (difference (union left right) s)
