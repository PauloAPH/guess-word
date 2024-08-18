module Main (main) where

import Lib 
import System.Random (randomRIO)
import Lib (gameLoop)


-- https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-String.html#v:words
-- https://www.ime.usp.br/~pf/dicios/
-- https://stackoverflow.com/questions/30740366/list-with-random-numbers-in-haskell
main :: IO ()
main = do
  contents <- readFile "br-sem-acentos.txt"
  let a = map countLetters (words contents)
  print "Entre com a quantidade de letras"
  input <- getLine
  let number = read input :: Int
  let b = listWordsOfSize a number
  let size = length b
  randomNumber <- randomRIO (1, size)
  let word = takeWordAt b randomNumber
  gameLoop 5 word
