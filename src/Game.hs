module Game
  ( gameLoop,
    gameStart,
  )
where

import Lib
import System.Random (randomRIO)

data GameState = GameState Int String deriving(Show)

gameLoop :: GameState -> IO ()
gameLoop (GameState 0 s) = do
  print s
  putStrLn "Game Over"
gameLoop (GameState l s) = do
  print "Insira a palavra"
  input2 <- getLine
  let guess = input2
  let matches = matchWords guess s 0
  let result = checkWin matches
  print matches
  case result of
    True -> do
      print s
      putStrLn "Win"
    False -> gameLoop (GameState (l - 1) s)

gameStart :: IO GameState
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
  return (GameState 5 word)