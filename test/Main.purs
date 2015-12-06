module Test.Main where

import Prelude

import Data.Array
import Data.Foldable
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Traversable
import Math (pow, sin, cos, pi, abs)

import Signal.DOM

import qualified Text.Smolder.HTML as H
import qualified Text.Smolder.Markup as H

import Flare
import Flare.Drawing
import Flare.Smolder

-- Example 1

ui1 = pow <$> number "Base" 2.0
          <*> number "Exponent" 10.0

-- Example 2

ui2 = string_ "Hello" <> pure " " <> string_ "World"

-- Example 3

ui3 = sum (int_ <$> [2, 13, 27, 42])

-- Example 4
ui4 = number_ 5.0 / number_ 2.0

-- Example 5

coloredCircle hue radius =
  filled (fillColor (hsl hue 0.8 0.4)) (circle 50.0 50.0 radius)

ui5 = coloredCircle <$> (numberSlider "Hue" 0.0 360.0 1.0 140.0)
                    <*> (numberSlider "Radius" 2.0 45.0 0.1 25.0)

-- Example 6

data Language = English | French | German

instance showLanguage :: Show Language where
  show English = "english"
  show French = "french"
  show German = "german"

greet :: Language -> String
greet English = "Hello"
greet French =  "Salut"
greet German =  "Hallo"

ui6 = (greet <$> (select "Language" English [French, German]))
      <> pure " " <> string "Name" "Pierre" <> pure "!"

-- Example 7

plot n s phi0 =
      shadow (fromMaybe mempty s) $ filled (fillColor (hsl 220.0 0.6 0.5)) $
        path (map point angles)

      where point phi = { x: 50.0 + radius phi * cos phi
                        , y: 50.0 + radius phi * sin phi }
            angles = map (\i -> 2.0 * pi / toNumber points * toNumber i) (0 .. points)
            points = 200
            radius phi = 48.0 * abs (cos (0.5 * toNumber n * (phi + phi0)))

shadowStyle = shadowColor black <> shadowOffset 2.0 2.0 <> shadowBlur 2.0

ui7 = plot <$> intSlider "Leaves" 2 10 6
           <*> optional "Shadow" false shadowStyle
           <*> pure 0.001 * lift animationFrame

-- Example 8

ui8 = traverse (intSlider_ 1 5) (1..5)

-- Example 9

ui9 = boolean_ false && boolean_ true

-- Example 10

graph xs width = outlined (outlineColor black <> lineWidth width)
                          (path points)
    where points = zipWith point xs (1 .. length xs)
          point x y = { x, y: toNumber y }

ui10 = graph <$> foldp cons [] (numberSlider "Position" 0.0 150.0 1.0 75.0)
             <*> numberSlider "Width" 1.0 5.0 0.1 1.0

-- Example 11

toInt true  = 1
toInt false = 0

ui11 = foldp (+) 0 (toInt <$> button "Increment")

-- Example 12

table h w = H.table $ foldMap row (0 .. h)
  where row i = H.tr $ foldMap (cell i) (0 .. w)
        cell i j = H.td (H.text (show i ++ "," ++ show j))

ui12 = table <$> intSlider_ 0 9 5 <*> intSlider_ 0 9 5

-- Example 13

inputs = { current: _ , add: _ }
           <$> string "Add item:" "Orange"
           <*> button "Add"

update { current, add } xs = if add then current : xs else xs

list = foldp update ["Apple", "Banana"] inputs

ui13 = (H.ul <<< foldMap (H.li <<< H.text)) <$> list

-- Render everything to the DOM

main = do
  runFlare "controls1" "output1" ui1
  runFlareS "controls2" "output2" ui2
  runFlare "controls3" "output3" ui3
  runFlare "controls4" "output4" ui4
  runFlareDrawing "controls5" "output5" ui5
  runFlareS "controls6" "output6" ui6
  runFlareDrawing "controls7" "output7" ui7
  runFlare "controls8" "output8" ui8
  runFlare "controls9" "output9" ui9
  runFlareDrawing "controls10" "output10" ui10
  runFlare "controls11" "output11" ui11
  runFlareHTML "controls12" "output12" ui12
  runFlareHTML "controls13" "output13" ui13
