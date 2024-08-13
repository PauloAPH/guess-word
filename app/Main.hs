module Main (main) where

import Lib 

-- https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-String.html#v:words
-- https://www.ime.usp.br/~pf/dicios/
-- https://stackoverflow.com/questions/30740366/list-with-random-numbers-in-haskell
main :: IO ()
main = do
  contents <- readFile "br-sem-acentos.txt"
  let words_ = words contents
  let a = mapWords words_
  let b = takeWordListOfSize a 3
  let size = legnthWords b
  let word = takeWordAt b 10
  print word
