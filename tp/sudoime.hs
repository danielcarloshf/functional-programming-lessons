{-----------------------------------------------------------
  Trabalho Prático II - Problema SUDOIME (SPOJ.com).
  Programação Funcional (DCC030), Prof. Carlos Camarão.
  
  Aluno: Daniel Carlos Hovadick Félix.
  Matrícula: 2010048967.
 -----------------------------------------------------------}

{- Retorna um item na posição (i,j). -}
getItem :: [Int] -> Int -> Int -> Int
getItem game i j = game !! (9*i+j)

{- Forma uma lista com items de uma linha. -}
getRow :: Int -> [Int] -> [Int]
getRow i g
  | 0 <= i && i < 9 = [getItem g i j | j <- [0..8]]
  | otherwise       = []

{- Forma uma lista com items de uma coluna. -}
getCol :: Int -> [Int] -> [Int]
getCol j g
  | 0 <= j && j < 9 = [getItem g i j | i <- [0..8]]
  | otherwise       = []

{- Forma uma lista com items de um quadrante. -}
get3X3 :: Int -> [Int] -> [Int]
get3X3 i g
  | i == 0    = [getItem g 0 j | j <- [0..2]] ++ [getItem g 1 j | j <- [0..2]] ++ [getItem g 2 j | j <- [0..2]]
  | i == 1    = [getItem g 3 j | j <- [0..2]] ++ [getItem g 4 j | j <- [0..2]] ++ [getItem g 5 j | j <- [0..2]]
  | i == 2    = [getItem g 6 j | j <- [0..2]] ++ [getItem g 7 j | j <- [0..2]] ++ [getItem g 8 j | j <- [0..2]]
  | i == 3    = [getItem g 0 j | j <- [3..5]] ++ [getItem g 1 j | j <- [3..5]] ++ [getItem g 2 j | j <- [3..5]]
  | i == 4    = [getItem g 3 j | j <- [3..5]] ++ [getItem g 4 j | j <- [3..5]] ++ [getItem g 5 j | j <- [3..5]]
  | i == 5    = [getItem g 6 j | j <- [3..5]] ++ [getItem g 7 j | j <- [3..5]] ++ [getItem g 8 j | j <- [3..5]]
  | i == 6    = [getItem g 0 j | j <- [6..8]] ++ [getItem g 1 j | j <- [6..8]] ++ [getItem g 2 j | j <- [6..8]]
  | i == 7    = [getItem g 3 j | j <- [6..8]] ++ [getItem g 4 j | j <- [6..8]] ++ [getItem g 5 j | j <- [6..8]]
  | i == 8    = [getItem g 6 j | j <- [6..8]] ++ [getItem g 7 j | j <- [6..8]] ++ [getItem g 8 j | j <- [6..8]]
  | otherwise = []

{- Ordenação por inserção. -}
iSort :: [Int] -> [Int]
iSort []    = []
iSort (a:x) = ins a (iSort x)

{- Insere um item numa lista em ordem crescente. -}
ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
  | x <= y    = x : (y:ys)
  | otherwise = y : ins x ys 

{- Valida uma linha. -}
verifyLine :: [Int] -> Bool
verifyLine line = iSort line == [1..9] 

{- Verifica se o jogo é válido. -}
sudokuOK :: [Int] -> Bool
sudokuOK game =
  let
    checkRows = and [verifyLine (getRow i game) | i <- [0..8]]
    checkCols = and [verifyLine (getCol i game) | i <- [0..8]]
    check3X3s = and [verifyLine (get3X3 i game) | i <- [0..8]]
  in
    checkRows && checkCols && check3X3s

{- Verifica unicidade dos algarismos na vertical, horizontal e em quadrantes 3x3. -}
isSudoku :: [Int] -> String
isSudoku game
  | sudokuOK game = "SIM\n\n"
  | otherwise     = "NAO\n\n"

{- Verifica se um jogo é valido. -}
isValid :: Int -> [Int] -> String
isValid i game = "Instancia " ++ (show i) ++ "\n" ++ isSudoku game

{- Verifica cada jogo da entrada. -}
verifySudoku :: Int -> Int -> [String] -> String
verifySudoku t i instances =
  let
    test = (t-1)
    game = [read x :: Int | x <- take 81 instances]
  in
    if test > 0 then
      (isValid i game) ++ verifySudoku test (i+1) (drop 81 instances)
    else
      (isValid i game)

{- Parsing da entrada. -}
checkSudoku :: Int -> [String] -> String
checkSudoku i input  =
  let
    tests     = head input
    instances = tail input
  in
    if tests == "0"
    then ""
    else verifySudoku (read tests :: Int) i instances

{- Lê conteúdo da entrada. -}
main = do
  contents <- getContents
  putStr $ checkSudoku 1 (words contents)