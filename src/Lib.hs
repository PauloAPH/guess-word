module Lib
  ( WordParam (..),
    legnthWords,
    takeWordAt,
    listWordsOfSize,
    countLetters,
    someFunc,
    matchWords,
    letterInWord,
    checkWin,
  )
where

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

letterInWord :: Char -> String -> Int -> String
letterInWord c w n
  | c `elem` w && c == w !! n = "G"
  | c `elem` w = "Y"
  | otherwise = "R"

matchWords :: String -> String -> Int -> String
matchWords [] _w2 _n = []
matchWords _w1 [] _n = []
matchWords w w2 n = letterInWord (head w) w2 n ++ matchWords (tail w) w2 (n + 1)

checkWin :: String -> Bool
checkWin [] = True
checkWin s = (head s == 'G') && checkWin (tail s)
