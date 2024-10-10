    -- 1
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

    -- 2 (a)
(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (:) ys xs

    -- 2 (b)
myConcat :: [[a]] -> [a]
myConcat xs = foldr (++) [] xs

    -- 2 (c)
myReverseR :: [a] -> [a]
myReverseR xs = foldr (\ y ys -> ys ++ [y]) [] xs

    -- 2 (d)
myReverseL :: [a] -> [a]
myReverseL xs = foldl (\ ys y -> y : ys) [] xs

    -- 3
mdc a b = fst (until (\ (x, y) -> y == 0) (\ (x, y) -> (y, x `mod` y)) (a, b))

    -- 4 (a)
shift (x:xs) = xs ++ [x]

    -- 4 (b)
f x head [xs] = [xs] ++ [shift xs]

rotate :: [a] -> [[a]]
rotate (x:xs) = foldr f [(x:xs)] xs
