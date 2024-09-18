module GameGraphics
  ( drawSquare,
    drawFilledRectangle,
    drawRectangle,
    drawLetter,
  )
where

import Graphics.Gloss

-- Draw a single square with a letter and its determined background color
drawSquare :: Int -> Int -> Int -> Int -> (Maybe Char, Color) -> Picture
drawSquare t ten rowIndex colIndex (maybeLetter, bgColor) =
  Translate
    (fromIntegral (colIndex * 60 - (t * 30)))
    (fromIntegral ((ten * 30) - rowIndex * 60))
    $ Pictures
      [ Color bgColor (drawFilledRectangle 50 50), -- Background color for the square
        Color black drawRectangle, -- Outline of the square
        drawLetter maybeLetter -- Draw the letter on top
      ]

-- Draw a filled rectangle (square) with a background color
drawFilledRectangle :: Float -> Float -> Picture
drawFilledRectangle width height =
  Polygon
    [ (-width / 2, -height / 2),
      (-width / 2, height / 2),
      (width / 2, height / 2),
      (width / 2, -height / 2)
    ]

-- Draw an empty rectangle (square)
drawRectangle :: Picture
drawRectangle = rectangleWire 50 50

-- Draw a letter or underscore inside the square
drawLetter :: Maybe Char -> Picture
drawLetter (Just letter) =
  Translate (-12) (-12) $
    Scale 0.3 0.3 $
      Text [letter] -- Show the letter
drawLetter Nothing =
  Translate (-12) (-12) $
    Scale 0.3 0.3 $
      Text "_" -- Show an underscore for empty space