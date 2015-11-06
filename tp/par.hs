{-----------------------------------------------------------
  Trabalho Prático I - Problema PAR (SPOJ.com).
  Programação Funcional (DCC030), Prof. Carlos Camarão.
  
  Aluno: Daniel Carlos Hovadick Félix.
  Matrícula: 2010048967.
 -----------------------------------------------------------}

{- Mostra quem foram os vencedores. -}
play :: String -> String -> [Int] -> String -> String
play p1 p2 rounds test = "Teste " ++ test ++ "\n" ++ getWinner p1 p2 rounds

{- Determina o vencedor de cada rodada. -}
getWinner :: String -> String -> [Int] -> String
getWinner _ _ []       = "\n"
getWinner a b (c1:c2:x)
  | mod (c1+c2) 2 == 0 = a ++ "\n" ++ getWinner a b x
  | otherwise          = b ++ "\n" ++ getWinner a b x

{- Parsing da entrada. -}
choosies :: Int -> [String] -> String
choosies i input =
  let
    played = head input
  in
    if played == "0" then
      ""
    else
      let
        games  = read played :: Int
        p1     = head (drop 1 input)
        p2     = head (drop 2 input)
        rounds = [read x :: Int | x <- (take (2*games) (drop 3 input))]
        test   = show i
      in
        (play p1 p2 rounds test) ++ choosies (i+1) (drop (2*games+3) input)

{- Lê conteúdo da entrada. -}
main = do
  contents <- getContents
  putStr $ choosies 1 (words contents)