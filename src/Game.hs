module Game
  ( createGame,
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
import Graphics.Gloss.Interface.IO.Game
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import System.Random (randomR)
import System.Random.Stateful (StdGen)


data GameState = GameState
  { tries :: Int,
    wordSize :: Int,
    secretWord :: String,
    guessedLetters :: [[Maybe Char]],
    turn :: Int,
    gameWon :: Bool
  }

-- Cria estancia de jogo com o tamanho da palavra selecionada
createGame :: Int -> StdGen -> String -> IO GameState
createGame size rng contents = do
  let wordList = listWordsOfSize (map countLetters (words contents)) size
  let rn = fst $ randomR (0, length wordList) rng
  return
    GameState
      { tries = size + 1,
        wordSize = size,
        secretWord = map toUpper (takeWordAt wordList rn),
        guessedLetters = replicate (size + 1) (replicate size Nothing),
        turn = 0,
        gameWon = False
      }

-- Renderiza a janela do jogo
render :: GameState -> Picture
render gameState =
  Pictures $ concatMap (drawRow gameState) [0 .. tries gameState - 1]

-- Draw a row of squares, with different colors based on the guessed letters
drawRow :: GameState -> Int -> [Picture]
drawRow gameState currentTurn =
  let letters = guessedLetters gameState !! currentTurn
      rowColors = map (colorLetter gameState currentTurn) [0 .. wordSize gameState - 1]
   in zipWith (drawSquare (wordSize gameState) (tries gameState) currentTurn) [0 ..] (zip letters rowColors)


-- Dado uma letra verifica se ela pertence a secretWord na posição passada, na secretWord mas em outra posição ou não
letterInWord :: Char -> String -> Int -> Color
letterInWord c w n
  | c `elem` w && c == w !! n = green
  | c `elem` w = yellow
  | otherwise = red

-- Define a cor de fundo da letra do chute
colorLetter :: GameState -> Int -> Int -> Color
colorLetter gameState rowIndex colIndex =
  let secretWord_secreta = secretWord gameState
      letter = fromMaybe '_' (guessedLetters gameState !! rowIndex !! colIndex)
   in if rowIndex >= turn gameState -- Rows not guessed yet
        then makeColorI 200 200 200 255 -- Light gray for unguessed rows
        else letterInWord letter secretWord_secreta colIndex


-- Processa entrada do usuario
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char key) Down _ _) gameState
  | gameWon gameState = gameState -- If the game is already won, do nothing
  | key `elem` ['a' .. 'z'] =
      let updatedLetters = fillNextEmpty (guessedLetters gameState) (toUpper key) (turn gameState)
          newState = gameState {guessedLetters = updatedLetters}
       in if isRowFull updatedLetters (turn gameState)
            then checkGuess newState
            else newState
handleInput _ gameState = gameState


-- | Recebe as linhas, concatena a linhas anteriores ao turno, descarta as linhas menores que a do proximo turno, e concatena com a nova linha.  
-- | Ex [["AAAAA"], [], []] -> [["BBBBB"]] -> 1 -> [["AAAAA"], ["BBBBB"], []]
updateRows :: [[Maybe Char]] -> [[Maybe Char]] -> Int -> [[Maybe Char]]
updateRows rows newRow currentTurn = take currentTurn rows ++ newRow ++ drop (currentTurn + 1) rows

-- | Insere a nova letra na linha do turno, e atualiza a linha do turno
fillNextEmpty :: [[Maybe Char]] -> Char -> Int -> [[Maybe Char]]
fillNextEmpty rows letter currentTurn =
  let row = rows !! currentTurn
   in case fillFirstEmpty row letter of
        Just updatedRow -> updateRows rows [updatedRow] currentTurn
        Nothing -> rows

-- | Tenta prencher a letra, enquanto a posição não for Nothing chama recursivo com o resto da lista do turno
fillFirstEmpty :: [Maybe Char] -> Char -> Maybe [Maybe Char]
fillFirstEmpty (Nothing : rest) letter = Just (Just letter : rest)
fillFirstEmpty (x : rest) letter = fmap (x :) (fillFirstEmpty rest letter)
fillFirstEmpty [] _ = Nothing

-- Verifica se todas as letras do turn foram prenchidas
isRowFull :: [[Maybe Char]] -> Int -> Bool
isRowFull rows currentTurn = all (\x -> x /= Nothing) (rows !! currentTurn)

-- Recupera uma palavra do turno atual
recoverWordFromTurn :: GameState -> String
recoverWordFromTurn gameState = map (fromMaybe '_') (guessedLetters gameState !! turn gameState)

-- Verifica se a linha atual acertou a palavra secreta
checkGuess :: GameState -> GameState
checkGuess gameState =
  let currentGuess = recoverWordFromTurn gameState
      nextTurn = turn gameState + 1
   in if currentGuess == secretWord gameState
        then do
          gameState {turn = nextTurn, gameWon = True}
        else
          if nextTurn < tries gameState
            then gameState {turn = nextTurn}
            else gameState

-- Solicita ao usuario o tamanho da secretWord
askSize :: IO String
askSize = do
  putStr "Entre com o numero de letras: "
  hFlush stdout
  getLine
