import Data.Char (ord, chr, isLower)

    -- 1 -----------------------------------------------------------------

fat n = product [1..n]
binom :: Int -> Int -> Int
binom n k = div (fat n) ((fat k)*(fat(n-k)))

pascalLevel :: Int -> [Int]
pascalLevel i = [binom i x | x<-[0..i]]

pascal :: Int -> [[Int]]
pascal n | n == 0 = []
         | otherwise = pascal (n-1) ++ [pascalLevel (n-1)]

    -- 2 -----------------------------------------------------------------

-- uppercase
isUppercase :: Char -> Bool
isUppercase c = if c >= 'A' && c <= 'Z' then True
                else False
hasUppercase :: String -> Bool
hasUppercase s = or [isUppercase c | c<-s]

-- lowercase
isLowercase :: Char -> Bool
isLowercase c = if c >= 'a' && c <= 'z' then True
                else False
hasLowercase :: String -> Bool
hasLowercase s = or [isLowercase c | c<-s]

-- number
isNumber :: Char -> Bool
isNumber c = if c >= '0' && c <= '9' then True
                else False
hasNumber :: String -> Bool
hasNumber s = or [isNumber c | c<-s]

-- forte
forte :: String -> Bool
forte s = length s >= 8 && hasUppercase s && hasLowercase s && hasNumber s

    -- 3 -wrong answer----------------------------------------------------

-- FUNÇÃO ALTERADA
let2int :: Char -> Int
let2int c | isLower c = ord c - ord 'a'
          | otherwise = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c = int2let ((let2int c + n) `mod` 26)

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

count :: Char -> String -> Int
count c s = length [c' | c' <- s, c' == c]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

letters :: String -> Int
letters s = length [c | c <- s, c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z']

-- FUNÇÃO ALTERADA (ver função letters acima)
freqs :: String -> [Float]
freqs s = [percent (count c s) n | c <- ['a'..'z']]
        where n = letters s

-- isso deve ser o suficiente para tratar os casos com letra maiúscula
