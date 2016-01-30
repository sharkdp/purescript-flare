## Module Flare.Drawing

#### `runFlareDrawing`

``` purescript
runFlareDrawing :: forall e. ElementId -> ElementId -> UI (canvas :: Canvas | e) Drawing -> Eff (dom :: DOM, chan :: Chan, canvas :: Canvas | e) Unit
```

Renders a Flare UI with a `Drawing` as output. The first ID specifies
the DOM element for the controls while the second ID specifies the
canvas for rendering.


### Re-exported from Graphics.Drawing:

#### `Color`

``` purescript
data Color
```

Colors.

##### Instances
``` purescript
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

#### `aqua`

``` purescript
aqua :: Color
```

#### `black`

``` purescript
black :: Color
```

#### `blue`

``` purescript
blue :: Color
```

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

#### `colorString`

``` purescript
colorString :: Color -> String
```

Render a color as a HTML color string.

#### `darken`

``` purescript
darken :: Number -> Color -> Color
```

Darken a color by the specified amount between 0 and 1.

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

#### `fuchsia`

``` purescript
fuchsia :: Color
```

#### `gray`

``` purescript
gray :: Color
```

#### `green`

``` purescript
green :: Color
```

#### `hsl`

``` purescript
hsl :: Number -> Number -> Number -> Color
```

Create a `Color` from hue (0.0-360.0), saturation (0.0-1) and lightness (0.0-1) values.

#### `lighten`

``` purescript
lighten :: Number -> Color -> Color
```

Lighten a color by the specified amount between 0 and 1.

#### `lime`

``` purescript
lime :: Color
```

#### `lineWidth`

``` purescript
lineWidth :: Number -> OutlineStyle
```

Set the line width.

#### `maroon`

``` purescript
maroon :: Color
```

#### `navy`

``` purescript
navy :: Color
```

#### `olive`

``` purescript
olive :: Color
```

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

#### `purple`

``` purescript
purple :: Color
```

#### `rectangle`

``` purescript
rectangle :: Number -> Number -> Number -> Number -> Shape
```

Create a rectangle from the left, top, width and height parameters.

#### `red`

``` purescript
red :: Color
```

#### `render`

``` purescript
render :: forall eff. Context2D -> Drawing -> Eff (canvas :: Canvas | eff) Unit
```

Render a `Drawing` to a canvas.  

#### `rgb`

``` purescript
rgb :: Number -> Number -> Number -> Color
```

Create a `Color` from RGB values between 0.0 and 255.0.

#### `rgba`

``` purescript
rgba :: Number -> Number -> Number -> Number -> Color
```

Create a `Color` from RGBA values between 0.0 and 255.0 (rgb), 0.0 and 1.0 (a)

#### `rotate`

``` purescript
rotate :: Number -> Drawing -> Drawing
```

Apply a rotation by providing the angle.

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

#### `silver`

``` purescript
silver :: Color
```

#### `teal`

``` purescript
teal :: Color
```

#### `text`

``` purescript
text :: Font -> Number -> Number -> FillStyle -> String -> Drawing
```

Render some text.

#### `translate`

``` purescript
translate :: Number -> Number -> Drawing -> Drawing
```

Apply a translation by providing the x and y distances.

#### `white`

``` purescript
white :: Color
```

#### `yellow`

``` purescript
yellow :: Color
```

