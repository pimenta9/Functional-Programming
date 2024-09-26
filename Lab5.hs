    -- test function
f :: Integer -> Integer
f x = x - 3
    
    -- 1
anyZero :: (Integer -> Integer) -> Integer -> Bool
anyZero f 0 =   if f 0 == 0 then True
                else False
anyZero f n =   if f n == 0 then True
                else anyZero f (n-1)

anyZero2 :: (Integer -> Integer) -> Integer -> Bool
anyZero2 f n = or (map (\x -> f x == 0) [0..n])

    -- 2
nub :: Eq a => [a] -> [a]
nub   []   = []
nub (x:xs) = x : nub [y | y <- xs, y /= x]

    -- 3 (a)
insert :: Ord a => a -> [a] -> [a]
insert y    []  = [y]
insert y (x:xs) | (y < x)   = y : (x:xs)
                | otherwise = x : insert y xs

    -- 3 (b)
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

    -- 4 (a)
minimum2 :: Ord a => [a] -> a
minimum2 [x] = x
minimum2 (x:xs) = min x (minimum2 xs)

    -- 4 (b)
delete :: Eq a => a -> [a] -> [a]
delete _    []  = []
delete y (x:xs) = if y == x then xs
                else x : delete y xs

    -- 4 (c)
ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort xs = m : ssort (delete m xs)
           where m = minimum2 xs

    -- 5
add :: Bool -> [[Bool]] -> [[Bool]]
add _ [] = []
add b (x:xs) = (x ++ [b]) : add b xs

bits :: Int -> [[Bool]]
bits 0 = []
bits 1 = [[True], [False]]
bits n = (add True (bits (n-1))) ++ (add False (bits(n-1)))
