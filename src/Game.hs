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


-- Define the game state
data GameState = GameState
  { tentativas :: Int, -- Number of rows
    tamanho_palavra :: Int, -- Number of squares per row
    palavra :: String, -- The word to guess
    guessedLetters :: [[Maybe Char]], -- Grid of guessed letters (Nothing for empty spaces)
    turno :: Int, -- Index of the current row being guessed
    gameWon :: Bool -- Flag to indicate if the game is won
  }

criaJogo :: Int -> String -> IO GameState
criaJogo size contents = do
  let wordList = listWordsOfSize (map countLetters (words contents)) size
  return
    GameState
      { tentativas = size + 1, -- Set attempts based on word list size
        tamanho_palavra = size, -- Word size
        palavra = map toUpper (takeWordAt wordList 3), -- The chosen word
        guessedLetters = replicate (size + 1) (replicate size Nothing), -- Empty guesses grid
        turno = 0, -- Start at turn 0
        gameWon = False -- Game not won initially
      }

-- Rendering the game
render :: GameState -> Picture
render gameState =
  Pictures $ concatMap (drawRow gameState) [0 .. tentativas gameState - 1]

-- Draw a row of squares, with different colors based on the guessed letters
drawRow :: GameState -> Int -> [Picture]
drawRow gameState rowIndex =
  let letters = guessedLetters gameState !! rowIndex
      rowColors = map (colorLetter gameState rowIndex) [0 .. tamanho_palavra gameState - 1]
   in zipWith (drawSquare (tamanho_palavra gameState) (tentativas gameState) rowIndex) [0 ..] (zip letters rowColors)

letterInWord :: Char -> String -> Int -> Color
letterInWord c w n
  | c `elem` w && c == w !! n = green
  | c `elem` w = yellow
  | otherwise = red

-- Determine the color for each letter in the row
colorLetter :: GameState -> Int -> Int -> Color
colorLetter gameState rowIndex colIndex =
  let palavra_secreta = palavra gameState
      letter = fromMaybe '_' (guessedLetters gameState !! rowIndex !! colIndex)
   in if rowIndex >= turno gameState -- Rows not guessed yet
        then makeColorI 200 200 200 255 -- Light gray for unguessed rows
        else letterInWord letter palavra_secreta colIndex

-- Check if the letter is in the target word but not in the correct position
notCorrectPosition :: String -> Char -> Int -> Bool
notCorrectPosition target letter colIndex =
  let correctPositions = elemIndices letter target
   in colIndex `notElem` correctPositions



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
        then do gameState {gameWon = True}
        else
          if nextRow < tentativas gameState
            then gameState {turno = nextRow}
            else gameState -- No more rows to guess, game is over

askSize :: String -> IO String
askSize prompt = do
  putStr prompt
  hFlush stdout -- Ensure the prompt is displayed before reading input
  getLine
