module Lib
  ( WordParam (..),
    legnthWords,
    takeWordAt,
    listWordsOfSize,
    countLetters,
    someFunc,
    matchLetters,
    matchWords,
    gameLoop,
    gameStart,
  )
where

import System.Random (randomRIO)

data WordParam = WordParam String Int deriving (Show)

instance Semigroup WordParam where
  (WordParam w1 l1) <> (WordParam w2 l2) = WordParam (w1 ++ w2) (l1 + l2)

instance Monoid WordParam where
  mempty = WordParam "" 0

legnthWords :: [WordParam] -> Int
legnthWords [] = 0
legnthWords xs = legnthWords (tail xs)

takeWordAt :: [WordParam] -> Int -> String
takeWordAt words_ n = w
  where
    WordParam w _l = words_ !! n

listWordsOfSize :: [WordParam] -> Int -> [WordParam]
listWordsOfSize words_ n = filter (\(WordParam _ l) -> l == n) words_

countLetters :: String -> WordParam
countLetters s = WordParam s (length s)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

letterInWord :: Char -> String -> Bool
letterInWord _c [] = False
letterInWord c w
  | c == head w = True
  | letterInWord c (tail w) = True
  | otherwise = False

matchLetters :: Char -> String -> String
matchLetters _c [] = "R"
matchLetters c w
  | c == head w = "G"
  | letterInWord c (tail w) = "Y"
  | otherwise = "R"

matchWords :: String -> String -> String
matchWords [] _w2 = []
matchWords _w1 [] = []
matchWords w w2 = matchLetters (head w) w2 ++ matchWords (tail w) (tail w2)

checkWin :: String -> Bool
checkWin [] = True
checkWin s = (head s == 'G') && checkWin (tail s)

gameLoop :: Int -> String -> IO ()
gameLoop 0 s = do
    print s
    putStrLn "Game Over"
gameLoop l s = do
    print "Insira a palavra"
    input2 <- getLine
    let guess = input2
    let matches = matchWords guess s
    let result = checkWin matches
    print matches
    case result of
        True ->  do
            print s
            putStrLn "Win"
        False -> gameLoop (l-1) s

gameStart ::  IO String
gameStart = do
    contents <- readFile "br-sem-acentos.txt"
    let a = map countLetters (words contents)
    print "Entre com a quantidade de letras"
    input <- getLine
    let number = read input :: Int
    let b = listWordsOfSize a number
    let size = length b
    randomNumber <- randomRIO (1, size)
    let word = takeWordAt b randomNumber
    return word