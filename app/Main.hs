module Main (main) where

import Lib ()
import Graphics.Gloss ( black, play, Display(InWindow) )
import Game (GameState, criaJogo, handleInput, render, askSize)

update :: Float -> GameState -> GameState
update _ gameState = gameState  
  

-- Main function to run the game
main :: IO ()
main = do
  wordSizeInput <- askSize 
  let wordSize = read wordSizeInput :: Int
  contents <- readFile "/home/paulo/prog_funcional/guess-word/br-sem-acentos.txt"  
  gameState <- criaJogo wordSize contents  
  play 
         (InWindow "Guess Word" (800, 600) (100, 100)) -- Window size and position
         black                -- Background color
         30                   -- Frames per second
         gameState
         render                -- Render function
         handleInput           -- Handle input function
         update                -- Update function (not used in this case)


-- https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-String.html#v:words
-- https://www.ime.usp.br/~pf/dicios/
-- https://stackoverflow.com/questions/30740366/list-with-random-numbers-in-haskell