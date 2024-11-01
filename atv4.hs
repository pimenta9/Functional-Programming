import Data.List (nub)

data Prop   = Const Bool
            | Var Char
            | Neg Prop
            | Conj Prop Prop
            | Disj Prop Prop
            | Impl Prop Prop
            | BiImpl Prop Prop -- exercício 2
              deriving (Eq, Show)

type Assoc ch v = [(ch, v)]

find :: Eq ch => ch -> Assoc ch v -> v
find ch assocs = head [v | (ch', v)<-assocs, ch == ch']

type Atrib = Assoc Char Bool

valor :: Atrib -> Prop -> Bool
valor s (Const b) = b
valor s (Var x) = find x s
valor s (Neg p) = not (valor s p)
valor s (Conj p q) = valor s p && valor s q
valor s (Disj p q) = valor s p || valor s q
valor s (Impl p q) = not (valor s p) || valor s q
valor s (BiImpl p q) = valor s (Impl p q) && valor s (Impl q p) -- exercício 2

bits :: Int -> [[Bool]]
bits 0 = [[]]
bits n = [ b:bs | bs<-bits(n-1), b<-[False, True]]

vars :: Prop -> [Char]
vars (Const _)  = []
vars (Var x)    = [x]
vars (Neg p)    = vars p
vars (Conj p q) = vars p ++ vars q
vars (Disj p q) = vars p ++ vars q
vars (Impl p q) = vars p ++ vars q

atribs :: Prop -> [Atrib]
atribs p = map (zip vs) (bits (length vs))
    where vs = nub (vars p)

tautologia :: Prop -> Bool
tautologia p = and [valor s p | s<-atribs p]

    -- 1
contraExemplos :: Prop -> [Atrib]
contraExemplos p = [a | a<-atribs p, (valor a p) == False]

    -- 2
-- alterações nas linhas 9 e 26

    -- 3
