module Lib
    ( WordParam(..)
    , legnthWords
    , takeWordAt
    , takeWordListOfSize
    , countLetters
    , mapWords
    , someFunc
    ) where

import System.Random (randomRIO)

data WordParam = WordParam String Int deriving (Show)

legnthWords :: [WordParam] -> Int
legnthWords [] = 0
legnthWords xs = 1 + legnthWords(tail xs)

takeWordAt :: [WordParam] -> Int -> WordParam
takeWordAt words_ n = (words_ !! n)

takeWordListOfSize :: [WordParam] -> Int -> [WordParam]
takeWordListOfSize words_ n = filter (\(WordParam _ l) -> l == n) words_

countLetters :: String -> WordParam
countLetters s = WordParam s (length s)

mapWords :: [String] -> [WordParam]
mapWords s = map countLetters s

someFunc :: IO ()
someFunc = putStrLn "someFunc"
