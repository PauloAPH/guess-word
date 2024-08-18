module Main (main) where

import Lib 
import Game


-- https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-String.html#v:words
-- https://www.ime.usp.br/~pf/dicios/
-- https://stackoverflow.com/questions/30740366/list-with-random-numbers-in-haskell
main :: IO ()
main = do
  gameState <- gameStart
  gameLoop gameState
