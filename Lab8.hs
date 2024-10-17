l :: [Int]
l = [1, 2, 3]

teste :: [Int]
teste = l ++ teste

    -- 1
shift :: [a] -> [a]
shift (x:xs) = xs ++ [x]

rotate :: [a] -> [[a]]
rotate xs = xs : rotate (shift xs)

    -- 2
four :: [Double]
four = [4, -4]
alternatingFours :: [Double]
alternatingFours = four ++ alternatingFours

odds :: [Double]
odds = [1, 3..]

calcPi1:: Int -> Double
calcPi1 n = sum (zipWith (/) (take n alternatingFours) (take n odds))

--

build :: [Double] -> [[Double]]
build l = l : build (map (\ x -> x + 2) l)

denoms = map product (build [2, 3, 4])

calcPi2 :: Int -> Double
calcPi2 1 = 3
calcPi2 n = 3 + sum (zipWith (/) (take (n-1) alternatingFours) (take (n-1) denoms))
