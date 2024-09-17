module Lib
  ( WordParam (..),
    legnthWords,
    takeWordAt,
    listWordsOfSize,
    countLetters,
    someFunc,
    matchWords,
    letterInWord,
    checkWin,
    stringFromGuess,
    askUserForGuess,
  )
where

data WordParam = WordParam String Int deriving (Show)

data Guess = Guess String deriving (Show)

instance Semigroup WordParam where
  (WordParam w1 l1) <> (WordParam w2 l2) = WordParam (w1 ++ w2) (l1 + l2)

instance Monoid WordParam where
  mempty = WordParam "" 0

legnthWords :: [WordParam] -> Int
legnthWords [] = 0
legnthWords xs = legnthWords (tail xs)

takeWordAt :: [WordParam] -> Int -> String
takeWordAt words_ n = w
  where
    WordParam w _l = words_ !! n

listWordsOfSize :: [WordParam] -> Int -> [WordParam]
listWordsOfSize words_ n = filter (\(WordParam _ l) -> l == n) words_

countLetters :: String -> WordParam
countLetters s = WordParam s (length s)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

letterInWord :: Char -> String -> Int -> String
letterInWord c w n
  | c `elem` w && c == w !! n = "G"
  | c `elem` w = "Y"
  | otherwise = "R"

matchWords :: String -> String -> Int -> String
matchWords [] _w2 _n = []
matchWords _w1 [] _n = []
matchWords w w2 n = letterInWord (head w) w2 n ++ matchWords (tail w) w2 (n + 1)

checkWin :: String -> Bool
checkWin [] = True
checkWin s = (head s == 'G') && checkWin (tail s)

guessFromString :: String -> Int -> Maybe Guess
guessFromString s max_size
  | length s == max_size = Just (Guess s)
  | otherwise     = Nothing

-- this is the only way to access the contents of a Name
stringFromGuess :: Guess -> String
stringFromGuess (Guess s) = s

askUserForGuess :: Int -> IO Guess
askUserForGuess
  max_size = do
    print ("Entre com uma palavra de " ++ show max_size ++ " letras")
    s <- getLine
    case guessFromString s max_size of
      Just n  -> return n
      Nothing -> do
        print "Quantidade errada de caracteres tente de novo."
        askUserForGuess max_size

