    -- 1
module Main where

rotChar13 :: Char -> Char
rotChar13 c
    | (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') = toEnum ((mod ((fromEnum c - fromEnum 'a') + 13) 26) + fromEnum 'a')
    | otherwise = c

rot13 :: String -> String
rot13 [] = []
rot13 (c:cs) = rotChar13 c : rot13 cs

main = do
    str <- getLine
    putStr (rot13 str)

    -- 2

begin :: Int -> String
begin 0 = []
begin n = '_' : begin (n - 1)

reveal :: Char -> String -> String -> String
reveal c [] [] = [] -- (x:xs) e (y:ys) têm sempre o mesmo tamanho
reveal c (x:xs) (y:ys)
    | y == c = c : reveal c xs ys
    | otherwise = x : reveal c xs ys

in' :: Eq a => a -> [a] -> Bool
in' _ [] = False
in' x (y:ys)
    | x == y = True
    | otherwise = in' x ys

equals :: Eq a => [a] -> [a] -> Bool
equals [] [] = True
-- não vamos precisar dos casos em que length é diferente
equals (x:xs) (y:ys)
    | x /= y = False
    | otherwise = equals xs ys

adivinhaAux :: String -> String -> Int -> IO ()
adivinhaAux current word n
    | current `equals` word = putStr (word ++ "\nAdivinhou em " ++ (show n) ++ " tentativas\n")
    | otherwise =
        do
        putStr (current ++ "\n? ")
        c <- getChar
        putChar '\n'
        if not (c `in'` word) then putStr "Não ocorre\n" else putStr ""
        adivinhaAux (reveal c current word) word (n + 1)

adivinha :: String -> IO ()
adivinha word = adivinhaAux (begin (length word)) word 0
