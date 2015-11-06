{--------------------------------------------------------
 - Lista de Exercícios IV - Programação Funcional 2015/2
 - Aluno: Daniel Carlos Hovadick Félix
 - Matrícula: 2010048967
 --------------------------------------------------------}

module Lista04 where

-- Exercício 10.15 --
unzip' :: [(a,b)] -> ([a],[b])
unzip' = foldr f ([],[]) where f (a, b) (x, y) = (a:x, b:y)

last' :: [a] -> a
last' = snd . foldr reminder (True, undefined)
  where reminder a (True,  _) = (False, a)
        reminder _ (False, a) = (False, a)

init' :: [a] -> [a]
init' =  snd . foldr begin (False, [])
  where begin _ (False, _) = (True, [])
        begin a (True,  x) = (True, a:x)

-- Exercício 10.17 --
type Line = [String]

{- formatLine :: Line -> String
formatList :: (a -> String) -> [a] -> String
formatLines :: (a -> String) -> [Line] -> String-}

-- Exercício 10.18 --
filterList :: (a -> Bool) -> [a] -> [a]
filterList f = snd . foldr verify ([],[])
  where verify n (all, filtered)
          | f n       = (n:all, n:filtered)
          | otherwise = (n:all, all)

-- Exercício 10.32 --
type Person   = String
type Book     = String
type Database = [(Person,Book)]

books :: Database -> Person -> [Book]
books db person = snd . unzip . filter ((==person) . fst) $ db

borrowers :: Database -> Book -> [Person]
borrowers db book = fst . unzip . filter ((==book) . snd) $ db

borrowed :: Database -> Book -> Bool
borrowed db book = not . null . filter ((==book) . snd) $ db

numBorrowed :: Database -> Person -> Int
numBorrowed db person = length . filter ((==person) . fst) $ db

makeLoan :: Database -> Person -> Book -> Database
makeLoan db person book = foldr (:) db ((person,book):[])

returnLoan :: Database -> Person -> Book -> Database
returnLoan db person book = filter (/=(person,book)) db

-- Exercício 10.34 --
type Name       = String
type Price      = Int
type BarCode    = Int
type TillType   = [BarCode]
type BillType   = [(Name, Price)]
type DatabaseSL = [(BarCode, Name, Price)]

codeIndex :: DatabaseSL
codeIndex = [ (4719, "Fish Fingers" , 121),
              (5643, "Nappies" , 1010),
              (3814, "Orange Jelly", 56),
              (1111, "Hula Hoops", 21),
              (1112, "Hula Hoops (Giant)", 133),
              (1234, "Dry Sherry, 1lt", 540)]

lineLength :: Int
lineLength = 30

formatPence :: Price -> String
formatPence price 
  | digits == 4 = frt(splitAt 2 (show price)) ++ "." ++ scd (splitAt 2 (show price))
  | digits == 3 = frt(splitAt 1 (show price)) ++ "." ++ scd (splitAt 1 (show price))
  | digits == 2 = "."++ (show price)
  | digits == 1 = ".0"++(show price)
  where digits = length (show price)

formatLineBill :: (Name, Price) -> String
formatLineBill (name, price) = name ++ (foldr (++) [] (replicate n ".")) ++ (formatPence price) ++ "\n"
  where n = lineLength - (length name) - (length (formatPence price))  
  
example :: [(Name, Price)]
example = [("Fish Fingers", 1231), ("Nappies" , 1010),("Orange Jelly", 56)]

formatLinesBill :: [(Name, Price)] -> String
formatLinesBill list = foldr (++) [] (map formatLineBill list)

makeTotal :: BillType -> Price
makeTotal list = foldr (+) 0 (map scd list)

formatTotal :: Price -> String
formatTotal total = "\nTotal" ++ (foldr (++) [] (replicate n ".")) ++ (formatPence total)
  where n = lineLength - 5 - (length (formatPence total))

formatBill :: BillType -> String
formatBill list = "\tHaskell Store\n\n"++(formatLinesBill list)++(formatTotal(makeTotal list))
