    -- 1
aux1 :: Int -> Int -> Int
aux1 a b = 10 * a + b

dec2int :: [Int] -> Int
dec2int (x:xs) = foldl aux1 x xs

    -- 2 (a)
myMaximum :: Ord a => [a] -> a
myMaximum xs = foldl1 max xs

myMaximum2 :: Ord a => [a] -> a
myMaximum2 xs = foldr1 max xs

myMinimum :: Ord a => [a] -> a
myMinimum xs = foldl1 min xs

myMinimum2 :: Ord a => [a] -> a
myMinimum2 xs = foldr1 min xs

    -- 2 (b)
-- myFoldr1 :: Foldable t => (a -> a -> a) -> t a -> a
myFoldr1 f (x:xs) = foldr f x xs

myFoldl1 f (x:xs) = foldl f x xs

    -- 3 (a)
-- (+++) :: [a] -> [a] -> [a]
-- (+++) (x:xs) (y:ys) = foldr 

    -- 4
isZero x = if x == 0 then True else False

-- mdc a b = until isZero (\x -> mod x b) a

    -- 5 (a)
shift :: [a] -> [a]
shift (x:xs) = xs ++ [x]

    -- 5 (b)
rotate :: [a] -> [[a]]
rotate xs = foldr .......
