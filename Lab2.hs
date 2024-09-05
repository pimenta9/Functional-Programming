    -- Useful
-- max3
max3 a b c = max (max a b) c
min3 a b c = min (min a b) c
-- double
double x = x + x
-- one
one = 1
-- apply function n times
nTimes n f x | (n == 1) = f x
             | (n > 1) = f (nTimes (n - 1) f x)

    -- 1 (a)
occurrence x y = if x == y then 2 else 1
maxOccurs :: Integer -> Integer -> (Integer, Integer)
maxOccurs x y = (max x y, occurrence x y)

    -- 1 (b)
orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
orderTriple (x, y, z) = (first, sum[x, y, z] - first - last, last)
                        where first = min3 x y z
                              last = max3 x y z

    -- 2 (a)
diffs :: Int -> Int -> Int
diffs x y   | (x >= y) = x - y
            | (otherwise) = y - x

    -- 3 (a)
segundo xs = head (tail xs)
    -- 3 (b)
trocar (x, y) = (y, x)
    -- 3 (c)
par x y = (x, y)
    -- 3 (d)
dobro x = 2*x
    -- 3 (e)
-- metade :: Fractional a => a -> a
metade x = x/2
    -- 3 (f)
-- minuscula :: Char -> Bool
minuscula x = x >= 'a' && x <= 'z'
    -- 3 (g)
-- intervalo :: Ord a => a -> a -> a -> Bool
intervalo x a b = x >= a && x <= b
    -- 3 (h)
-- palindromo :: Eq a => [a] -> Bool
palindromo xs = reverse xs == xs
    -- 3 (i)
-- twice :: (t -> t) -> t -> t
twice f x = f (f x)

    -- 4

    -- 5 (a)
twice2 :: Int -> (Int -> Int) -> Int
twice2 x f = f (f x)
    -- 5 (b)
maiusculo c b | (c >= 'A' && c <= 'Z') = not b
              | (otherwise) = b
    -- 5 (c)
