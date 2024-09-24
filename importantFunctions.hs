    -- and
and2 :: [Bool] -> Bool
and2 [] = True
and2 (x:xs) = x && and2 xs

    -- or
or2 :: [Bool] -> Bool
or2 [] = False
or2 (x:xs) = x || or2 xs

    -- concat
concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 (x:xs) = x ++ concat2 xs

    -- replicate
replicate2 :: Int -> a -> [a]
replicate2 0 x = []
replicate2 n x = x : replicate2 (n-1) x

    -- (!!)
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = (!!!) xs (n-1)

    -- elem
elem2 :: Eq a => a -> [a] -> Bool
elem2 _   []   = False
elem2 y (x:xs)  | (x == y) = True
                | otherwise = elem2 y xs

    -- nub
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub [y | y <- xs, y /= x]
