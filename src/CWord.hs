module CWord where
{-# LANGUAGE OverloadedStrings #-}


import qualified          Data.Vector as V
import Data.Char
import System.Random

data Letter = A | B | C | D | E | F |
              G | H | I | J | K | L |
              M | N | O | P | Q | R |
              S | T | U | V | W | X |
              Y | Z deriving(Eq, Ord, Enum, Show)

instance Random Letter where
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (x, g') -> (toEnum x, g')
  random g = randomR (A,Z) g

convertLetter :: Char -> Maybe Letter
convertLetter 'A' = Just A
convertLetter 'B' = Just B
convertLetter 'C' = Just C
convertLetter 'D' = Just D
convertLetter 'E' = Just E
convertLetter 'F' = Just F
convertLetter 'G' = Just G
convertLetter 'H' = Just H
convertLetter 'I' = Just I
convertLetter 'J' = Just J
convertLetter 'K' = Just K
convertLetter 'L' = Just L
convertLetter 'M' = Just M
convertLetter 'N' = Just N
convertLetter 'O' = Just O
convertLetter 'P' = Just P
convertLetter 'Q' = Just Q
convertLetter 'R' = Just R
convertLetter 'S' = Just S
convertLetter 'T' = Just T
convertLetter 'U' = Just U
convertLetter 'V' = Just V
convertLetter 'W' = Just W
convertLetter 'X' = Just X
convertLetter 'Y' = Just Y
convertLetter 'Z' = Just Z
convertLetter _ = Nothing

plus :: Letter -> Letter -> Letter
plus x y
    | s > 25 = toEnum $ mod s 26
    | otherwise = toEnum s
    where s = (fromEnum x) + (fromEnum y)

minus :: Letter -> Letter -> Letter
minus x y
    | s <= 0 = toEnum $ 25 + s
    | otherwise = toEnum (s - 1)
    where s = (fromEnum x) - (fromEnum y)

data Character = Letter Letter | SPACE deriving (Eq)

instance Show Character where
  show SPACE = " "
  show (Letter x) = show x


type CText = [Character]


fromString :: String -> Maybe CText
fromString (x:xs) = do
  l <- if x == ' '
       then Just SPACE
       else do
          y <- convertLetter (toUpper x)
          return $ Letter y
  ls <- fromString xs
  return (l : ls)
fromString [] = Just []

cwords :: CText -> [CText]
cwords = cwords' [] []
  where cwords' :: [CText] -> CText -> CText -> [CText]
        cwords' acc cur (SPACE:xs) = cwords' (acc ++ [cur]) [] xs
        cwords' acc cur (x:xs)     = cwords' acc (cur ++ [x]) xs
        cwords' acc []  []         = acc
        cwords' acc cur []         = acc ++ [cur]

addText :: CText -> CText -> CText
addText ((Letter x):xs) ((Letter y):ys) = (Letter (plus B (plus x y))) : addText xs ys
addText (SPACE:xs) ys = SPACE : addText xs ys
addText xs (SPACE:ys) = SPACE : addText xs ys
addText _ _ = []

subText :: CText -> CText -> CText
subText ((Letter x):xs) ((Letter y):ys) = (Letter (minus x y)) : subText xs ys
subText (SPACE:xs) ys = SPACE : subText xs ys
subText xs (SPACE:ys) = SPACE : subText xs ys
subText _ _ = []
