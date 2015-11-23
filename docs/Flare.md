## Module Flare

#### `ElementId`

``` purescript
type ElementId = String
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

Creates a text field for a `Number` input from a given label and default
value.

#### `number_`

``` purescript
number_ :: forall e. Number -> UI e Number
```

Creates a text field for a `Number` input with a default value.

#### `numberRange`

``` purescript
numberRange :: forall e. Label -> Number -> Number -> Number -> Number -> UI e Number
```

Creates a slider for a `Number` input from a given label,
minimum value, maximum value, step size as well as default value.

#### `numberRange_`

``` purescript
numberRange_ :: forall e. Number -> Number -> Number -> Number -> UI e Number
```

Creates a slider for a `Number` input without a label.

#### `int`

``` purescript
int :: forall e. Label -> Int -> UI e Int
```

Creates a text field for an `Int` input from a given label and default
value.

#### `int_`

``` purescript
int_ :: forall e. Int -> UI e Int
```

Creates a text field for an `Int` input with a default value.

#### `intRange`

``` purescript
intRange :: forall e. Label -> Int -> Int -> Int -> UI e Int
```

Creates a slider for an `Int` input from a given label, minimum and
maximum values as well as a default value.

#### `intRange_`

``` purescript
intRange_ :: forall e. Int -> Int -> Int -> UI e Int
```

Creates a slider for an `Int` input without a label.

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

Creates a text field for a `String` input with a default value.

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

Creates a checkbox for a `Boolean` input with a default value.

#### `select`

``` purescript
select :: forall e a. (Show a) => Label -> a -> Array a -> UI e a
```

Creates a select box to choose from a list of options. The first option
is selected by default. The rest of the options is given as an array.

#### `select_`

``` purescript
select_ :: forall e a. (Show a) => a -> Array a -> UI e a
```

Create a select box without a label.

#### `appendComponents`

``` purescript
appendComponents :: forall e. ElementId -> Array Element -> Eff (dom :: DOM | e) Unit
```

Attach all elements in the array to the specified parent element.

#### `runFlare`

``` purescript
runFlare :: forall e a. (Show a) => ElementId -> ElementId -> UI e a -> Eff (dom :: DOM, chan :: Chan | e) Unit
```

Renders a Flare UI to the DOM and sets up all event handlers. The two IDs
specify the DOM elements to which the controls and the output will be
attached, respectively.


