module Game
  ( criaJogo,
    GameState,
    render,
    drawSquare,
    drawFilledRectangle,
    drawRectangle,
    drawLetter,
    handleInput,
    askSize
  )
where

import Lib (listWordsOfSize, countLetters, takeWordAt)
import GameGraphics
import Graphics.Gloss
import Data.Char (toUpper)
import Data.List (elemIndices)
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import Graphics.Gloss.Interface.IO.Game


data GameState = GameState
  { tentativas :: Int, 
    tamanho_palavra :: Int, 
    palavra :: String, 
    guessedLetters :: [[Maybe Char]], 
    turno :: Int, 
    gameWon :: Bool 
  }

criaJogo :: Int -> String -> IO GameState
criaJogo size contents = do
  let wordList = listWordsOfSize (map countLetters (words contents)) size
  return
    GameState
      { tentativas = size + 1, 
        tamanho_palavra = size, 
        palavra = map toUpper (takeWordAt wordList 3), 
        guessedLetters = replicate (size + 1) (replicate size Nothing), 
        turno = 0, 
        gameWon = False 
      }

-- Renderiza a janela do jogo
render :: GameState -> Picture
render gameState =
  Pictures $ concatMap (drawRow gameState) [0 .. tentativas gameState - 1]

-- Draw a row of squares, with different colors based on the guessed letters
drawRow :: GameState -> Int -> [Picture]
drawRow gameState rowIndex =
  let letters = guessedLetters gameState !! rowIndex
      rowColors = map (colorLetter gameState rowIndex) [0 .. tamanho_palavra gameState - 1]
   in zipWith (drawSquare (tamanho_palavra gameState) (tentativas gameState) rowIndex) [0 ..] (zip letters rowColors)


-- Dado uma letra verifica se ela pertence a palavra na posição passada, na palavra mas em outra posição ou não
letterInWord :: Char -> String -> Int -> Color
letterInWord c w n
  | c `elem` w && c == w !! n = green
  | c `elem` w = yellow
  | otherwise = red

-- Define a cor de fundo da letra do chute
colorLetter :: GameState -> Int -> Int -> Color
colorLetter gameState rowIndex colIndex =
  let palavra_secreta = palavra gameState
      letter = fromMaybe '_' (guessedLetters gameState !! rowIndex !! colIndex)
   in if rowIndex >= turno gameState -- Rows not guessed yet
        then makeColorI 200 200 200 255 -- Light gray for unguessed rows
        else letterInWord letter palavra_secreta colIndex


-- Processa entrada do usuario
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char key) Down _ _) gameState
  | gameWon gameState = gameState -- If the game is already won, do nothing
  | key `elem` ['a' .. 'z'] =
      let updatedLetters = fillNextEmpty (guessedLetters gameState) (toUpper key) (turno gameState)
          newState = gameState {guessedLetters = updatedLetters}
       in if isRowFull updatedLetters (turno gameState)
            then checkGuess newState
            else newState
handleInput _ gameState = gameState

-- Fill the next empty space in the current row with the guessed letter
fillNextEmpty :: [[Maybe Char]] -> Char -> Int -> [[Maybe Char]]
fillNextEmpty rows letter rowIndex =
  let row = rows !! rowIndex
   in case fillFirstEmpty row letter of
        Just updatedRow -> take rowIndex rows ++ [updatedRow] ++ drop (rowIndex + 1) rows
        Nothing -> rows

-- Fill the first empty space in a row
fillFirstEmpty :: [Maybe Char] -> Char -> Maybe [Maybe Char]
fillFirstEmpty (Nothing : rest) letter = Just (Just letter : rest)
fillFirstEmpty (x : rest) letter = fmap (x :) (fillFirstEmpty rest letter)
fillFirstEmpty [] _ = Nothing

-- Check if the current row is full (all letters guessed)
isRowFull :: [[Maybe Char]] -> Int -> Bool
isRowFull rows rowIndex = all (/= Nothing) (rows !! rowIndex)

-- Check the guess after the row is full
checkGuess :: GameState -> GameState
checkGuess gameState =
  let currentGuess = map (fromMaybe '_') (guessedLetters gameState !! turno gameState)
      correct = currentGuess == palavra gameState
      nextRow = turno gameState + 1
   in if correct
        then do 
          gameState {turno = nextRow, gameWon = True}
        else
          if nextRow < tentativas gameState
            then gameState {turno = nextRow}
            else gameState 

-- Solicita ao usuario o tamanho da palavra
askSize :: IO String
askSize = do
  putStr "Entre com o numero de letras: "
  hFlush stdout -- Ensure the prompt is displayed before reading input
  getLine
