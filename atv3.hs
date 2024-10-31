import Data.List (insert)

    -- 1
isort :: Ord a => [a] -> [a]
isort xs = foldr insert [] xs

    -- 2 (a)
add i 0 = i
add i j = succ (add i (pred j))

mult i 0 = 0
mult i j = add i (mult i (pred j))

myExp i 0 = 1
myExp i j = mult i (myExp i (pred j))

f i 0 = i
f i j = myExp i (f i (pred j))

    -- 2 (b) incompleta
foldi :: (a -> a) -> a -> Integer -> a
foldi f q 0 = q
foldi f q i = f (foldi f q (pred i))

add2 i j = foldi succ i j

-- mult2 i j = foldi add2 i j ??

    -- 2 (c)
-- foldi ??

    -- 3
myScanl :: (b -> a -> b) -> b -> [a] -> [b]
myScanl f z [] = [z]
myScanl f z xs = myScanl f z (init xs) ++ [foldl f z xs]
