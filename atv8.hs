    -- 2

module Set (Set, 
            empty, insert, member)
            where

data Set a = Node a (Set a) (Set a)
            | Empty
            deriving (Show)

empty :: Set a
empty = Empty

insert :: Ord a => a -> Set a -> Set a
insert x (Node y left right)
    | x < y = insert x left
    | x > y = insert x right
insert x Empty = Node x Empty Empty

member :: Ord a => a -> Set a -> Bool
member x Empty = False
member x (Node y left right)
    | x == y = True
    | x < y = member x left
    | x > y = member x right

    -- 3

union :: Ord a => Set a -> Set a -> Set a
union Empty s = s
union s Empty = s
union s (Node x left right) = insert x (union s (union left right))
