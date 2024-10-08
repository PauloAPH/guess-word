module Main (main) where

import Lib ()
import Graphics.Gloss ( black, play, Display(InWindow) )
import Game (GameState, createGame, handleInput, render, askSize)
import System.Random.Stateful (mkStdGen)
import System.Random (randomIO)

update :: Float -> GameState -> GameState
update _ gameState = gameState


-- Main function to run the game
main :: IO ()
main = do
  contents <- readFile "/home/paulo/prog_funcional/guess-word/br-sem-acentos.txt"
  wordSizeInput <- askSize
  let wordSize = read wordSizeInput :: Int
  seed <- randomIO :: IO Int
  let gen = mkStdGen seed
  gameState <- createGame wordSize gen contents
  play
         (InWindow "Guess Word" (800, 800) (400, 400)) -- Window size and position
         black                -- Background color
         30                   -- Frames per second
         gameState
         render                -- Render function
         handleInput           -- Handle input function
         update                -- Update function (not used in this case)


{- Fontes
https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-String.html#v:words
https://www.ime.usp.br/~pf/dicios/
https://stackoverflow.com/questions/30740366/list-with-random-numbers-in-haskell
https://hackage.haskell.org/package/random-1.2.1.2/docs/System-Random.html#t:StdGen
-}