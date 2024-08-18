module Game
  ( gameLoop,
    gameStart,
  )
where

import Lib
import System.Random (randomRIO)

-- GameState Tentativas Tamanho Palavra
data GameState = GameState Int Int String deriving(Show)

gameLoop :: GameState -> IO ()
gameLoop (GameState 0 _max_size s) = do
  print s
  putStrLn "Game Over"
gameLoop (GameState l max_size s) = do
  guess <- askUserForGuess max_size
  let matches = matchWords (stringFromGuess guess) s 0
  let result = checkWin matches
  print matches
  case result of
    True -> do
      print s
      putStrLn "Win"
    False -> gameLoop (GameState (l - 1) max_size s)

gameStart :: IO GameState
gameStart = do
  contents <- readFile "br-sem-acentos.txt"
  let a = map countLetters (words contents)
  print "Entre com a quantidade de letras"
  input <- getLine
  let max_size = read input :: Int
  let b = listWordsOfSize a max_size
  let size = length b
  randomNumber <- randomRIO (1, size)
  let word = takeWordAt b randomNumber
  return (GameState 5 max_size word)