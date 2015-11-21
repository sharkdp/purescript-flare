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

  runFlareDrawing "controls5" "output5" $
    filled (fillColor red) <$> (circle 50.0 50.0 <$> number "Radius" 25.0)

  runFlare "controls6" "output6" $
       (greet <$> (select "Language" English [French, German]))
    <> pure " " <> string "Name" "Pierre" <> pure "!"
