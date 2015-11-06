{-----------------------------------------------------------
  Trabalho Prático I - Problema POLEPOS (SPOJ.com).
  Programação Funcional (DCC030), Prof. Carlos Camarão.
  
  Aluno: Daniel Carlos Hovadick Félix.
  Matrícula: 2010048967.
 -----------------------------------------------------------}

{- Formata a saída de dados. -}
formatOutput :: [String] -> String
formatOutput [a]   = a ++ "\n"
formatOutput (a:x) = a ++ " " ++ formatOutput x

{- Ordena pelo método de inserção. -}
iSort :: [(Integer,Integer)] -> [(Integer,Integer)]
iSort []     = []
iSort (x:xs) = ins x (iSort xs)

{- Auxiliar para inserir na lista. -}
ins :: (Integer,Integer) -> [(Integer,Integer)] -> [(Integer,Integer)]
ins x [] = [x]
ins x (y:ys)
  | snd x <= snd y = x : (y:ys)
  | otherwise      = y : ins x ys

{- Calcula a posição de cada piloto. -}
getPositions :: Integer -> Integer -> [(Integer,Integer)] -> [(Integer,Integer)]
getPositions _ _ []           = []
getPositions i n ((p, g):rest) = (p, (i+g)) : getPositions (i+1) n rest

{- Valida a posição do piloto se for única no grid. -}
isUnique :: Integer -> [(Integer,Integer)] -> Bool
isUnique g positions = null (tail (filter (\x -> snd x == g) positions))

{- Forma o grid de acordo com as posições iniciais dos pilotos. -}
makeGrid :: Integer -> [(Integer,Integer)] -> String
makeGrid pilots result =  
  let
    positions = getPositions 1 pilots result
  in
    if foldr (&&) True [(0 < g && g <= pilots && (isUnique g positions)) | (p,g) <- positions]
    then formatOutput [show (fst x) | x <- (iSort positions)]
    else  "-1\n"

{- Formata a entrada numa lista de duplas (piloto, variação). -}
getPerformance :: [String] -> [(Integer,Integer)]
getPerformance []      = []
getPerformance (a:b:x) = (read a :: Integer, read b :: Integer) : getPerformance x

{- Parsing da entrada. -}
polePosition :: [String] -> String
polePosition input =
  let
    p = head input
  in
    if p == "0" then
      ""
    else
      let
        pilots = read p :: Integer
        result = getPerformance (take (2*(fromIntegral pilots)) (drop 1 input))
      in
        (makeGrid pilots result) ++ "\n" ++ polePosition (drop (2*(fromIntegral pilots)+1) input)

{- Lê conteúdo da entrada. -}
main = do
  contents <- getContents
  putStr $ polePosition (words contents)
