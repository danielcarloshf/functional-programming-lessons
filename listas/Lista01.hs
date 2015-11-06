{-------------------------------------------------------
 - Lista de Exercícios I - Programação Funcional 2015/2
 - Aluno: Daniel Carlos Hovadick Félix
 - Matrícula: 2010048967
 --------------------------------------------------------}

module Lista01 where

import Pictures
import Test.QuickCheck
import Prelude hiding (elem)

-- Exercicio 4.30 --
drawLine :: Integer -> Picture -> Picture
drawLine 1 colour = colour
drawLine n colour = sideBySide colour (drawLine (n-1) (invertColour colour))

drawBoard :: Integer -> Integer -> Picture -> Picture
drawBoard 1 m colour = drawLine m colour
drawBoard n m colour = above (drawLine m colour) (drawBoard (n-1) m (invertColour colour))

chessBoard :: Integer -> Picture
chessBoard n = drawBoard n n white

-- Exercicio 4.37 --
averageThree :: Integer -> Integer -> Integer -> Float
averageThree a b c = (fromInteger (a+b+c))/3.0

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage a b c = countAbove [fromInteger a, fromInteger b, fromInteger c] (averageThree a b c)

countAbove :: (Num a, Ord a1) => [a1] -> a1 -> a
countAbove [] avg    = 0
countAbove (a:x) avg
  | a < avg   = 1 + countAbove x avg
  | otherwise = 0 + countAbove x avg

-- QuickCheck howManyAboveAverage
prop_avg a b c = (0 <= howManyAboveAverage a b c) && (howManyAboveAverage a b c < 3)

-- Exercicio 5.19 --
capitalize :: Char -> Char
capitalize ch
  | fromEnum ch >= fromEnum 'a' && fromEnum ch <= fromEnum 'z' = toEnum (fromEnum ch + (fromEnum 'A' - fromEnum 'a'))
  | fromEnum ch >= fromEnum 'A' && fromEnum ch <= fromEnum 'Z' = ch
  | otherwise = '\0'

capitalizeLetters :: String -> String
capitalizeLetters chs = [(capitalize ch) | ch <- chs] 

-- Exercicio 5.21 --
matches :: Integer -> [Integer] -> [Integer]
matches a [] = []
matches a (b:x) = if a == b then [a] ++ matches a x else matches a x

elem :: Integer -> [Integer] -> Bool
elem n ns = not ([] == matches n ns)

-- Exercicio 5.23 --
duplicate :: String -> Integer -> String
duplicate s n = if n > 0 then s ++ duplicate s (n-1) else ""