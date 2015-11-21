## Module Flare

#### `Flare`

``` purescript
data Flare a
```

A `Flare` is a `Signal` with a corresponding list of HTML elements
for the user interface components.

#### `UI`

``` purescript
newtype UI a
```

The main data type for a Flare UI. It encapsulates the `Eff` action
which is to be run when setting up the input elements and corresponding
signals.

##### Instances
``` purescript
Functor UI
Apply UI
Applicative UI
(Semigroup a) => Semigroup (UI a)
(Monoid a) => Monoid (UI a)
(Semiring a) => Semiring (UI a)
(Ring a) => Ring (UI a)
(ModuloSemiring a) => ModuloSemiring (UI a)
(DivisionRing a) => DivisionRing (UI a)
(Num a) => Num (UI a)
```

#### `number`

``` purescript
number :: Label -> Number -> UI Number
```

Creates a text field for a `Number` input from a given label and default
value.

#### `number_`

``` purescript
number_ :: Number -> UI Number
```

Creates a text field for a `Number` input with a default value.

#### `int`

``` purescript
int :: Label -> Int -> UI Int
```

Creates a text field for an `Int` input from a given label and default
value.

#### `int_`

``` purescript
int_ :: Int -> UI Int
```

Creates a text field for an `Int` input with a default value.

#### `string`

``` purescript
string :: Label -> String -> UI String
```

Creates a text field for a `String` input from a given label and default
value.

#### `string_`

``` purescript
string_ :: String -> UI String
```

Creates a text field for a `String` input with a default value.

#### `boolean`

``` purescript
boolean :: Label -> Boolean -> UI Boolean
```

Creates a checkbox for a `Boolean` input from a given label and default
value.

#### `boolean_`

``` purescript
boolean_ :: Boolean -> UI Boolean
```

Creates a checkbox for a `Boolean` input with a default value.

#### `runFlare`

``` purescript
runFlare :: forall a. (Show a) => ElementId -> ElementId -> UI a -> Eff (dom :: DOM, chan :: Chan) Unit
```

Render a Flare UI to the DOM and set up all event handlers.


