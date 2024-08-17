module Lib
  ( WordParam (..),
    legnthWords,
    takeWordAt,
    listWordsOfSize,
    countLetters,
    someFunc,
    matchLetters,
    matchWords,
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

letterInWord :: Char -> String -> Bool
letterInWord c w
  | c == head w = True
  | letterInWord c (tail w) = True
  | otherwise = False

matchLetters :: Char -> String -> String
matchLetters c w
  | c == head w = "G"
  | letterInWord c (tail w) = "Y"
  | otherwise = "R"

matchWords :: String -> String -> String
matchWords [] _w2 = []
matchWords w w2 = (matchLetters (head w) w2) ++ matchWords (tail w) (tail w2)