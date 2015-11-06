{-------------------------------------------------------
 - Lista de Exercícios II - Programação Funcional 2015/2
 - Aluno: Daniel Carlos Hovadick Félix
 - Matrícula: 2010048967
 --------------------------------------------------------}
 
module Lista02 where
import Test.QuickCheck
 
-- Exercicio 7.8 --
elemNum :: Integer -> [Integer] -> Integer
elemNum _ []     = 0
elemNum x (a:xs) = if x == a then 1 + elemNum x xs else elemNum x xs

-- Exercicio 7.12 --
iSort :: [Integer] -> [Integer]
iSort []     = []
iSort (x:xs) = ins x (iSort xs)

ins :: Integer -> [Integer] -> [Integer]
ins x [] = [x]
ins x (y:ys)
  | x <= y    = x : (y:ys)
  | otherwise = y : ins x ys

min' :: [Integer] -> Integer
min' xs = head (iSort xs)

max' :: [Integer] -> Integer
max' xs = last (iSort xs)

-- MIN & MAX sem utilizar built-in's:
min'' :: [Integer] -> Integer
min'' xs
  | xs == []  = error "empty list"
  | otherwise = y
  where (y:ys) = iSort xs

max'' :: [Integer] -> Integer
max'' xs
  | xs == []  = error "empty list"
  | otherwise = if ys == [] then y else max'' ys
  where (y:ys)= iSort xs

-- Exercicio 7.20 --
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ [] = ([],[])
splitAt' 0 xs = ([],xs)
splitAt' n xs = if n > 0
                then (before n xs, drop' n xs)
                else splitAt' 0 xs

before :: Int -> [a] -> [a]
before _ [] = []
before n (x:xs)
  | n > 0     = x : before (n-1) xs
  | otherwise = []

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' n (x:xs)
  | n >  1    = [] ++ drop' (n-1) xs
  | n <= 0    = (x:xs)
  | otherwise = xs

-- Propriedades do splitAt' para o quickCheck
prop_splitAt' :: (Eq a) => Int -> [a] -> Bool
prop_splitAt' n xs = x1 ++ x2 == xs
  where (x1, x2) = splitAt' n xs

-- Propriedades do drop' para o quickCheck
prop_drop' :: (Eq a) => Int -> [a] -> Bool
prop_drop' n xs = xs == take n xs ++ drop' n xs

-- Exercicio 7.28 --
type Word = String
type Line = [Word]

joinLine :: Line -> String
joinLine []     = ""
joinLine (x:xs) = x ++ " " ++ joinLine xs

-- Exercicio 7.29 --
joinLines :: [Line] -> String
joinLines []     = ""
joinLines (x:xs) = joinLine x ++ "\n" ++ joinLines xs
