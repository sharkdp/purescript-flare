module Test.Main where

import Prelude
import Data.Array
import Data.Foldable
import Flare
import Math (pow)

powUI :: UI Number
powUI = pow <$> number "Base" 2.0
            <*> number "Exponent" 10.0

greetUI :: UI String
greetUI = greet <$> string "Name" "Pierre"
                <*> boolean "French greeting" true
    where greet name french | french    = "Salut " ++ name ++ "!"
                            | otherwise = "Hello " ++ name ++ "!"

concatUI :: UI String
concatUI = string_ "Hello" <> pure " " <> string_ "World"

sumUI :: UI Int
sumUI = sum (int_ <$> [2, 13, 27, 42])

divUI :: UI Number
divUI = number_ 5.0 / number_ 2.0

replicateUI :: UI (Array String)
replicateUI = replicate <$> int_ 4 <*> string_ "foo"

main :: Eff (dom :: DOM, chan :: Chan) Unit
main = do
  runFlare "controls1" "target1" powUI
  runFlare "controls2" "target2" greetUI
  runFlare "controls3" "target3" concatUI
  runFlare "controls4" "target4" sumUI
  runFlare "controls5" "target5" divUI
  runFlare "controls6" "target6" replicateUI
