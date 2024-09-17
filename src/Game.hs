module Game
  ( gameLoop,
    gameStart,
    getPalavra,
    criaJogo,
    GameState,
    eval,
  )
where

import Lib
import Control.Monad.State
import System.Random
import Data.Fixed (div')

data GameState = GameState
  { tentativas :: Int,
    tamanho_da_palavra :: Int,
    palavra :: String
  } deriving Show

type Jogo a = State GameState a

eval :: State s a -> s -> a
eval st = fst . runState st

criaJogo :: Int -> String -> GameState
criaJogo size contents = GameState t size word where 
  t = length b `div` size where 
    b = listWordsOfSize a size where
      a = map countLetters (words contents)
  word = takeWordAt b 6 where 
    b = listWordsOfSize a size where
      a = map countLetters (words contents)

getPalavra :: Jogo String
getPalavra = do
  gets palavra

gameStart :: IO GameState
gameStart = do
  contents <- readFile "br-sem-acentos.txt"
  let a = map countLetters (words contents)
  print "Entre com a quantidade de letras"
  input <- getLine
  let max_size = read input :: Int
  let b = listWordsOfSize a 5
  let size = length b
  randomNumber <- randomRIO (1, size)
  let word = takeWordAt b randomNumber
  return (GameState 5 5 word)



-- >>> getPalavra (GameState 5 5 "TESTE")


-- >>> 2 + 2


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
