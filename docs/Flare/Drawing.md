## Module Flare.Drawing

#### `runFlareDrawing`

``` purescript
runFlareDrawing :: forall e. ElementId -> ElementId -> UI (canvas :: Canvas | e) Drawing -> Eff (dom :: DOM, channel :: CHANNEL, canvas :: Canvas | e) Unit
```

Renders a Flare UI with a `Drawing` as output. The first ID specifies
the DOM element for the controls while the second ID specifies the
canvas for rendering.


### Re-exported from Graphics.Drawing:

#### `Color`

``` purescript
data Color
```

The representation of a color.

##### Instances
``` purescript
Show Color
Eq Color
```

#### `Drawing`

``` purescript
data Drawing
```

A vector `Drawing`.

##### Instances
``` purescript
Semigroup Drawing
Monoid Drawing
Eq Drawing
```

#### `FillStyle`

``` purescript
newtype FillStyle
```

Encapsulates fill color etc.

##### Instances
``` purescript
Semigroup FillStyle
Monoid FillStyle
Eq FillStyle
```

#### `Font`

``` purescript
data Font
```

Fonts.

##### Instances
``` purescript
Eq Font
```

#### `OutlineStyle`

``` purescript
newtype OutlineStyle
```

Encapsulates outline color etc.

##### Instances
``` purescript
Semigroup OutlineStyle
Monoid OutlineStyle
Eq OutlineStyle
```

#### `Point`

``` purescript
type Point = { x :: Number, y :: Number }
```

A `Point` consists of `x` and `y` coordinates.

#### `Shadow`

``` purescript
newtype Shadow
```

Encapsulates shadow settings etc.

##### Instances
``` purescript
Eq Shadow
Semigroup Shadow
Monoid Shadow
```

#### `Shape`

``` purescript
data Shape
```

A single shape.

##### Instances
``` purescript
Semigroup Shape
Monoid Shape
Eq Shape
```

#### `black`

``` purescript
black :: Color
```

The color black.

#### `brightness`

``` purescript
brightness :: Color -> Number
```

The percieved brightness of the color (A number between 0.0 and 1.0).
See: https://www.w3.org/TR/AERT#color-contrast

#### `circle`

``` purescript
circle :: Number -> Number -> Number -> Shape
```

Create a circle from the left, top and radius parameters.

#### `clipped`

``` purescript
clipped :: Shape -> Drawing -> Drawing
```

Clip a `Drawing` to a `Shape`.

#### `closed`

``` purescript
closed :: forall f. (Foldable f) => f Point -> Shape
```

Create a _closed_ path.

#### `complementary`

``` purescript
complementary :: Color -> Color
```

Get the complementary color (hue rotated by 180°).

#### `cssStringHSLA`

``` purescript
cssStringHSLA :: Color -> String
```

The CSS representation of the color in the form `hsl(..)` or `hsla(...)`.

#### `darken`

``` purescript
darken :: Number -> Color -> Color
```

Darken a color by subtracting a certain amount (number between -1.0 and
1.0) from the lightness channel. If the number is negative, the color is
lightened.

#### `desaturate`

``` purescript
desaturate :: Number -> Color -> Color
```

Decrease the saturation of a color by subtracting a certain amount (number
between -1.0 and 1.0) from the saturation channel. If the number is
negative, the color is saturated.

#### `everywhere`

``` purescript
everywhere :: (Drawing -> Drawing) -> Drawing -> Drawing
```

Modify a `Drawing` by applying a transformation to every subdrawing.

#### `fillColor`

``` purescript
fillColor :: Color -> FillStyle
```

Set the fill color.

#### `filled`

``` purescript
filled :: FillStyle -> Shape -> Drawing
```

Fill a `Shape`.

#### `fontString`

``` purescript
fontString :: Font -> String
```

Turn a `Font` into a `String` which can be used with `Graphics.Canvas.setFont`.

#### `fromHexString`

``` purescript
fromHexString :: String -> Maybe Color
```

Parse a hexadecimal RGB code of the form `#rgb` or `#rrggbb`, where the
hexadecimal digits are of the format [0-9a-f] (case insensitive). Returns
`Nothing` if the string is in a wrong format.

#### `grayscale`

``` purescript
grayscale :: Number -> Color
```

Create a gray tone from a lightness values (0.0 is black, 1.0 is white).

#### `hsl`

``` purescript
hsl :: Number -> Number -> Number -> Color
```

Create a `Color` from hue, saturation and lightness values. The hue is
given in degrees, as a `Number` between 0.0 and 360.0. Both saturation and
lightness are numbers between 0.0 and 1.0.

#### `hsla`

``` purescript
hsla :: Number -> Number -> Number -> Number -> Color
```

Create a `Color` from hue, saturation, lightness and alpha values. The
hue is given in degrees, as a `Number` between 0.0 and 360.0. Saturation,
lightness and alpha are numbers between 0.0 and 1.0.

#### `isLight`

``` purescript
isLight :: Color -> Boolean
```

Determine whether a color is perceived as a light color.

#### `lighten`

``` purescript
lighten :: Number -> Color -> Color
```

Lighten a color by adding a certain amount (number between -1.0 and 1.0)
to the lightness channel. If the number is negative, the color is
darkened.

#### `lineWidth`

``` purescript
lineWidth :: Number -> OutlineStyle
```

Set the line width.

#### `outlineColor`

``` purescript
outlineColor :: Color -> OutlineStyle
```

Set the outline color.

#### `outlined`

``` purescript
outlined :: OutlineStyle -> Shape -> Drawing
```

Draw the outline of a `Shape`.

#### `path`

``` purescript
path :: forall f. (Foldable f) => f Point -> Shape
```

Create a path.

#### `rectangle`

``` purescript
rectangle :: Number -> Number -> Number -> Number -> Shape
```

Create a rectangle from the left, top, width and height parameters.

#### `render`

``` purescript
render :: forall eff. Context2D -> Drawing -> Eff (canvas :: Canvas | eff) Unit
```

Render a `Drawing` to a canvas.

#### `rgb`

``` purescript
rgb :: Int -> Int -> Int -> Color
```

Create a `Color` from RGB values between 0 and 255.

#### `rgb'`

``` purescript
rgb' :: Number -> Number -> Number -> Color
```

Create a `Color` from RGB values between 0.0 and 1.0.

#### `rgba`

``` purescript
rgba :: Int -> Int -> Int -> Number -> Color
```

Create a `Color` from integer RGB values between 0 and 255 and a floating
point alpha value between 0.0 and 1.0.

#### `rgba'`

``` purescript
rgba' :: Number -> Number -> Number -> Number -> Color
```

Create a `Color` from RGB values between 0.0 and 1.0 and an alpha value
between 0.0 and 1.0.

#### `rotate`

``` purescript
rotate :: Number -> Drawing -> Drawing
```

Apply a rotation by providing the angle.

#### `saturate`

``` purescript
saturate :: Number -> Color -> Color
```

Increase the saturation of a color by adding a certain amount (number
between -1.0 and 1.0) to the saturation channel. If the number is
negative, the color is desaturated.

#### `scale`

``` purescript
scale :: Number -> Number -> Drawing -> Drawing
```

Apply a scale transformation by providing the x and y scale factors.

#### `shadow`

``` purescript
shadow :: Shadow -> Drawing -> Drawing
```

Apply a `Shadow` to a `Drawing`.

#### `shadowBlur`

``` purescript
shadowBlur :: Number -> Shadow
```

Set the shadow blur.

#### `shadowColor`

``` purescript
shadowColor :: Color -> Shadow
```

Set the shadow color.

#### `shadowOffset`

``` purescript
shadowOffset :: Number -> Number -> Shadow
```

Set the shadow blur.

#### `text`

``` purescript
text :: Font -> Number -> Number -> FillStyle -> String -> Drawing
```

Render some text.

#### `toHSLA`

``` purescript
toHSLA :: Color -> { h :: Number, s :: Number, l :: Number, a :: Number }
```

Convert a `Color` to its hue, saturation, lightness and alpha values.

#### `toHexString`

``` purescript
toHexString :: Color -> String
```

Return a hexadecimal representation of the color in the form `#rrggbb`,
where `rr`, `gg` and `bb` refer to hexadecimal digits corresponding to
the RGB channel values between `00` and `ff`. The alpha channel is not
represented.

#### `toRGBA`

``` purescript
toRGBA :: Color -> { r :: Int, g :: Int, b :: Int, a :: Number }
```

Convert a `Color` to its red, green, blue and alpha values. The RGB values
are integers in the range from 0 to 255.

#### `toRGBA'`

``` purescript
toRGBA' :: Color -> { r :: Number, g :: Number, b :: Number, a :: Number }
```

Convert a `Color` to its red, green, blue and alpha values. All values
are numbers in the range from 0.0 to 1.0.

#### `translate`

``` purescript
translate :: Number -> Number -> Drawing -> Drawing
```

Apply a translation by providing the x and y distances.

#### `white`

``` purescript
white :: Color
```

The color white.

