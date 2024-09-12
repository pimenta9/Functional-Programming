    -- 1
square x = x*x
-- sum [square x | x<-[1..100]]
-- sum (map square [1..100])
-- sum (map (\x -> x^2) [1..100])
-- sum (map (^2) [1..100])

    -- 2 (a)
piTerm :: Int -> Double
piTerm i = (-1)^i / fromIntegral(2*i + 1)
aprox :: Int -> Double
aprox n = 4 * sum [ piTerm x | x<-[0..(n-1)]]

    -- 2 (b)
aprox2 :: Int -> Double
aprox2 n = sqrt(12 * (sum (map (\x -> (-1)^x / fromIntegral(x + 1)^2) [0..(n-1)])))

    -- 3
divprop :: Int -> [Int]
divprop n = [ x | x<-[1..(n-1)], mod n x == 0]

    -- 4
isPerfect num = if sum (divprop num) == num then True
                else False
perfects :: Int -> [Int]
perfects n = [ x | x<-[1..n], isPerfect x == True]

    -- 5 (a)
doubleLuhn :: Int -> Int
doubleLuhn x =  if x >= 5 then x*2 - 9
                else x*2

    -- 5 (b)
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d =  if mod (a + doubleLuhn b + c + doubleLuhn d) 10 == 0 then True
                else False
