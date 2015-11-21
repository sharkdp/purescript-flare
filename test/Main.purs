module Test.Main where

import Prelude

import Data.Array
import Data.Foldable
import Math (pow)

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
    filled (fillColor (hsl hue 0.8 100.0)) (circle 50.0 50.0 radius)

  runFlareDrawing "controls5" "output5" $
    coloredCircle <$> (numberRange "Hue" 140.0 0.0 360.0 1.0)
                  <*> (numberRange "Radius" 25.0 2.0 45.0 0.1)

  runFlare "controls6" "output6" $
       (greet <$> (select "Language" English [French, German]))
    <> pure " " <> string "Name" "Pierre" <> pure "!"
