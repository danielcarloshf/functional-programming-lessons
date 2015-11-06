{-----------------------------------------------------------
  Trabalho Prático II - Problema PAPRIMAS (SPOJ.com).
  Programação Funcional (DCC030), Prof. Carlos Camarão.
  
  Aluno: Daniel Carlos Hovadick Félix.
  Matrícula: 2010048967.
 -----------------------------------------------------------}

{- Verifica se um número é primo. -}
isPrime :: Int -> Bool
isPrime n = n == 1 || n > 0 && divisors n == [1,n]
  where divisors m
          | m > 0     = [x | x <- [1..(m `div` 2)], m `rem` x == 0] ++ [m]
          | otherwise = []

{- Verifica se uma palavra é prima. -}
checkForPrimeWord :: Int -> String
checkForPrimeWord w
  | isPrime w = "It is a prime word.\n"
  | otherwise = "It is not a prime word.\n"

{- Converte caracteres em número e avalia se o somatório é primo. -}
isPrimeWord :: String -> String
isPrimeWord w = checkForPrimeWord $ sum (map charToNum w)
  where charToNum ch
          | 'a' <= ch && ch <= 'z' = 1  + fromEnum ch `mod` 97
          | 'A' <= ch && ch <= 'Z' = 27 + fromEnum ch `mod` 65
          | otherwise              = 0

{- Parsing da entrada. -}
primeWords :: [String] -> String
primeWords = foldr ((++) . isPrimeWord) ""

{- Lê conteúdo da entrada. -}
main = do
  contents <- getContents
  putStr $ primeWords (words contents)
