-- 2
isTriangle a b c = a + b > c && a + c > b && b + c > a

-- 4
divideList l = (take m l, drop m l)
    where m = (length l) `div` 2

-- 5 (a)
last1 l = l !! i
    where i = length(l) - 1
last2 l = head(reverse l)
-- 5 (b)
removeLast1 l = reverse(drop 1 (reverse l))
removeLast2 l = take (length(l) - 1) l

-- 7 (a)
-- max3 a b c = if (a >=) ...
-- 7 (b)
max3 a b c = max (max a b) c
min3 a b c = min (min a b) c

-- 10
xor :: Bool -> Bool -> Bool
xor True b = not b
xor False b = b

-- 11
safetail1 :: [a] -> [a]
safetail1[] = []
safetail1 l = tail l

safetail2 :: [a] -> [a]
safetail2 l | length(l) > 0 = tail l
            | otherwise = []

-- 12
