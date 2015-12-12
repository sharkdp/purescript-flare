## Module Flare

#### `ElementId`

``` purescript
type ElementId = String
```

#### `Label`

``` purescript
type Label = String
```

#### `Flare`

``` purescript
data Flare a
  = Flare (Array Element) (Signal a)
```

A `Flare` is a `Signal` with a corresponding list of HTML elements
for the user interface components.

##### Instances
``` purescript
Functor Flare
Apply Flare
Applicative Flare
```

#### `UI`

``` purescript
newtype UI e a
  = UI (Eff (dom :: DOM, chan :: Chan | e) (Flare a))
```

The main data type for a Flare UI. It encapsulates the `Eff` action
which is to be run when setting up the input elements and corresponding
signals.

##### Instances
``` purescript
Functor (UI e)
Apply (UI e)
Applicative (UI e)
(Semigroup a) => Semigroup (UI e a)
(Monoid a) => Monoid (UI e a)
(Semiring a) => Semiring (UI e a)
(Ring a) => Ring (UI e a)
(ModuloSemiring a) => ModuloSemiring (UI e a)
(DivisionRing a) => DivisionRing (UI e a)
(Num a) => Num (UI e a)
(Bounded a) => Bounded (UI e a)
(BooleanAlgebra a) => BooleanAlgebra (UI e a)
```

#### `wrap`

``` purescript
wrap :: forall e a. Signal a -> UI e a
```

Encapsulate a `Signal` within a `UI` component.

#### `lift`

``` purescript
lift :: forall e a. Eff (chan :: Chan, dom :: DOM | e) (Signal a) -> UI e a
```

Lift a `Signal` inside the `Eff` monad to a `UI` component.

#### `foldp`

``` purescript
foldp :: forall a b e. (a -> b -> b) -> b -> UI e a -> UI e b
```

Create a past dependent component. The fold-function takes the current
value of the component and the previous value of the output to produce
the new value of the output.

#### `number`

``` purescript
number :: forall e. Label -> Number -> UI e Number
```

Creates an input field for a `Number` from a given label and default
value.

#### `number_`

``` purescript
number_ :: forall e. Number -> UI e Number
```

Like `number`, but without a label.

#### `numberRange`

``` purescript
numberRange :: forall e. Label -> Number -> Number -> Number -> Number -> UI e Number
```

Creates an input field for a `Number` from a given label,
minimum value, maximum value, step size as well as default value.
The returned value is guaranteed to be within the given range.

#### `numberRange_`

``` purescript
numberRange_ :: forall e. Number -> Number -> Number -> Number -> UI e Number
```

Like `numberRange`, but without a label.

#### `numberSlider`

``` purescript
numberSlider :: forall e. Label -> Number -> Number -> Number -> Number -> UI e Number
```

Creates a slider for a `Number` input from a given label,
minimum value, maximum value, step size as well as default value.

#### `numberSlider_`

``` purescript
numberSlider_ :: forall e. Number -> Number -> Number -> Number -> UI e Number
```

Like `numberSlider`, but without a label.

#### `int`

``` purescript
int :: forall e. Label -> Int -> UI e Int
```

Creates an input field for an `Int` from a given label and default
value. The returned value is guaranteed to be within the allowed integer
range.

#### `int_`

``` purescript
int_ :: forall e. Int -> UI e Int
```

Like `int`, but without a label.

#### `intRange`

``` purescript
intRange :: forall e. Label -> Int -> Int -> Int -> UI e Int
```

Creates an input field for an `Int` from a given label, minimum and
maximum values as well as a default value. The returned value is
guaranteed to be within the given range.

#### `intRange_`

``` purescript
intRange_ :: forall e. Int -> Int -> Int -> UI e Int
```

Like `intRange`, but without a label.

#### `intSlider`

``` purescript
intSlider :: forall e. Label -> Int -> Int -> Int -> UI e Int
```

Creates a slider for an `Int` input from a given label, minimum and
maximum values as well as a default value.

#### `intSlider_`

``` purescript
intSlider_ :: forall e. Int -> Int -> Int -> UI e Int
```

Like `intSlider`, but without a label.

#### `string`

``` purescript
string :: forall e. Label -> String -> UI e String
```

Creates a text field for a `String` input from a given label and default
value.

#### `string_`

``` purescript
string_ :: forall e. String -> UI e String
```

Like `string`, but without a label.

#### `boolean`

``` purescript
boolean :: forall e. Label -> Boolean -> UI e Boolean
```

Creates a checkbox for a `Boolean` input from a given label and default
value.

#### `boolean_`

``` purescript
boolean_ :: forall e. Boolean -> UI e Boolean
```

Like `boolean`, but without a label.

#### `optional`

``` purescript
optional :: forall a e. Label -> Boolean -> a -> UI e (Maybe a)
```

Creates a checkbox that returns `Just x` if enabled and `Nothing` if
disabled. Takes a label, the initial state (enabled or disabled) and
the default value `x`.

#### `optional_`

``` purescript
optional_ :: forall a e. Boolean -> a -> UI e (Maybe a)
```

Like `optional`, but without a label.

#### `button`

``` purescript
button :: forall e. Label -> UI e Boolean
```

Creates a button which yields `true` if is pressed and `false` otherwise.

#### `buttons`

``` purescript
buttons :: forall a e. Array a -> (a -> String) -> UI e (Maybe a)
```

Create a button for each element of the array. The whole component
returns `Nothing` if none of the buttons is pressed and `Just x` if
the button corresponding to the element `x` is pressed.

#### `select`

``` purescript
select :: forall e a. Label -> a -> Array a -> (a -> String) -> UI e a
```

Creates a select box to choose from a list of options. The first option
is selected by default. The rest of the options is given as an array.

#### `select_`

``` purescript
select_ :: forall e a. a -> Array a -> (a -> String) -> UI e a
```

Like `select`, but without a label.

#### `radioGroup`

``` purescript
radioGroup :: forall e a. Label -> a -> Array a -> (a -> String) -> UI e a
```

Creates a group of radio buttons to choose from a list of options. The
first option is selected by default. The rest of the options is given as
an array.

#### `radioGroup_`

``` purescript
radioGroup_ :: forall e a. a -> Array a -> (a -> String) -> UI e a
```

Like `radioGroup`, but without a label.

#### `runFlareWith`

``` purescript
runFlareWith :: forall e a. ElementId -> (a -> Eff (dom :: DOM, chan :: Chan | e) Unit) -> UI e a -> Eff (dom :: DOM, chan :: Chan | e) Unit
```

Renders a Flare UI to the DOM and sets up all event handlers. The ID
specifies the HTML element to which the controls are attached. The
function argument will be mapped over the `Signal` inside the `Flare`.

#### `runFlare`

``` purescript
runFlare :: forall e. ElementId -> ElementId -> UI e String -> Eff (dom :: DOM, chan :: Chan | e) Unit
```

Renders a Flare UI to the DOM and sets up all event handlers. The two IDs
specify the DOM elements to which the controls and the output will be
attached, respectively.

#### `runFlareShow`

``` purescript
runFlareShow :: forall e a. (Show a) => ElementId -> ElementId -> UI e a -> Eff (dom :: DOM, chan :: Chan | e) Unit
```

Like `runFlare` but uses `show` to convert the contained value to a
`String` before rendering to the DOM.


