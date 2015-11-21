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
```

#### `number`

``` purescript
number :: Label -> Number -> UI Number
```

Creates a text field for a `Number` input from a given label and default
value.

#### `int`

``` purescript
int :: Label -> Int -> UI Int
```

Creates a text field for an `Int` input from a given label and default
value.

#### `string`

``` purescript
string :: Label -> String -> UI String
```

Creates a text field for a `String` input from a given label and default
value.

#### `boolean`

``` purescript
boolean :: Label -> Boolean -> UI Boolean
```

Creates a checkbox for a `Boolean` input from a given label and default
value.

#### `runFlare`

``` purescript
runFlare :: forall a. (Show a) => ElementId -> ElementId -> UI a -> Eff (dom :: DOM, chan :: Chan) Unit
```

Render a Flare UI to the DOM and set up all event handlers.


