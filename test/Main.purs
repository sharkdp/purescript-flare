module Test.Main where

import Prelude

import Data.Array
import Data.Foldable
import Data.Int
import Data.Traversable
import Math (pow, cos)

import Signal.DOM

import Flare
import Flare.Drawing

data Language = English | French | German

instance showLanguage :: Show Language where
  show English = "english"
  show French = "french"
  show German = "german"

greet :: Language -> String
greet English = "Hello"
greet French =  "Salut"
greet German =  "Hallo"

main = do
  runFlare "controls1" "output1" $
    pow <$> number "Base" 2.0
        <*> number "Exponent" 10.0

  runFlare "controls2" "output2" $
    string_ "Hello" <> pure " " <> string_ "World"

  runFlare "controls3" "output3" $
    sum (int_ <$> [2, 13, 27, 42])

  runFlare "controls4" "output4" $
    number_ 5.0 / number_ 2.0

  let coloredCircle hue radius =
    filled (fillColor (hsl hue 0.8 0.4)) (circle 50.0 50.0 radius)

  runFlareDrawing "controls5" "output5" $
    coloredCircle <$> (numberRange "Hue" 0.0 360.0 1.0 140.0)
                  <*> (numberRange "Radius" 2.0 45.0 0.1 25.0)

  {--
  runFlare "controls6" "output6" $
       (greet <$> (select "Language" English [French, German]))
    <> pure " " <> string "Name" "Pierre" <> pure "!"
  --}

  let animate time enabled = if enabled then shaded rect else rect
        where s = 50.0 + 25.0 * cos (0.002 * time)
              w = 50.0 - s / 2.0
              o = s / 10.0
              rect = filled (fillColor gray) (rectangle w w s s)
              shaded = shadow (shadowColor black <> shadowOffset o o)

  time <- animationFrame

  runFlareDrawing "controls7" "output7" $
    animate <$> lift time <*> boolean "Shadow" false

  runFlare "controls8" "output8" $
    traverse (intRange_ 1 5) (1..5)

  runFlare "controls9" "output9" $
    boolean_ false && boolean_ true

  let graph xs width = outlined (outlineColor black <> lineWidth width)
                                (path points)
        where points = zipWith point xs (1 .. length xs)
              point x y = { x, y: toNumber y }

  runFlareDrawing "controls10" "output10" $
    graph <$> foldp cons [] (numberRange "Position" 0.0 150.0 1.0 75.0)
          <*> numberRange "Width" 1.0 5.0 0.1 1.0

  let int true  = 1
      int false = 0

  runFlare "controls11" "output11" $
    foldp (+) 0 (int <$> button "Increment")
