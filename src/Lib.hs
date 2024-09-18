module Lib
  ( WordParam (..),
    legnthWords,
    takeWordAt,
    listWordsOfSize,
    countLetters,
  )
where


data WordParam = WordParam String Int deriving (Show)

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

