module Test.Main where

import Prelude
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

main :: Eff (dom :: DOM, chan :: Chan) Unit
main = do
  runFlare "controls1" "target1" powUI
  runFlare "controls2" "target2" greetUI
