module Test.Main where

import Prelude

import Data.Array
import Data.Foldable
import Math (pow)

import Flare
import Flare.Drawing

main = do
  runFlare "controls1" "output1" $
    pow <$> number "Base" 2.0
        <*> number "Exponent" 10.0

  let greet name french | french    = "Salut " ++ name ++ "!"
                        | otherwise = "Hello " ++ name ++ "!"

  runFlare "controls2" "output2" $
    greet <$> string "Name" "Pierre"
          <*> boolean "French greeting" true

  runFlare "controls3" "output3" $
    string_ "Hello" <> pure " " <> string_ "World"

  runFlare "controls4" "output4" $
    sum (int_ <$> [2, 13, 27, 42])

  runFlare "controls5" "output5" $
    number_ 5.0 / number_ 2.0

  runFlare "controls6" "output6" $
    replicate <$> int_ 4 <*> string_ "foo"

  runFlareDrawing "controls7" "output7" $
    filled (fillColor red) <$> (circle 50.0 50.0 <$> number "Radius" 25.0)
